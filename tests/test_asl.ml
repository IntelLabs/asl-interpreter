(****************************************************************
 * Test ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2020
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
module TC = Tcheck
module AST = Asl_ast

let format_value f v = Format.fprintf f "%s" (Value.pp_value v)

let format_expr (e : AST.expr) : string =
  Utils.to_string2 (fun fmt -> Asl_fmt.expr fmt e)

let value = Alcotest.testable format_value ( = )

(* test that checks that an expression lexes, parses and typechecks *)
let check_expr_tcheck (tcenv : TC.Env.t) (test_fmt : bool) (what : string)
    (input : string) _ : unit =
  LoadASL.report_type_error
    (fun _ -> Alcotest.fail "type error")
    (fun _ ->
      LoadASL.report_parse_error
        (fun _ -> Alcotest.fail "parse error")
        (fun _ ->
          let loc = AST.Unknown in
          let e = LoadASL.read_expr tcenv loc input in
          if test_fmt then
            Alcotest.(check string) ("format " ^ what) input (format_expr e)))

let extend_tcenv (globals : TC.GlobalEnv.t) (declarations : string) : (TC.Env.t * AST.declaration list) =
  let globals = TC.GlobalEnv.clone globals in
  let tcenv = TC.Env.mkEnv globals in
  LoadASL.report_type_error
    (fun _ -> Alcotest.fail "type error in decls")
    (fun _ ->
      LoadASL.report_parse_error
        (fun _ -> Alcotest.fail "parse error in decls")
        (fun _ ->
          let lexbuf = Lexing.from_string declarations in
          let t = Asl_parser.declarations_start Lexer.token lexbuf in
          let ds = TC.tc_declarations globals false t in
          (tcenv, ds)))

let extend_env (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
  : (TC.Env.t * Eval.Env.t) =
  let (tcenv, ds) = extend_tcenv globals decls in
  let env = Eval.build_evaluation_environment (List.append prelude ds) in
  (tcenv, env)

(* simple test of static semantics: parsing and typechecking of correct expressions
 * and optionally checks that pretty-printing the AST produces exactly the input string
 * optionally extends environment with new declarations first
 *)
let test_static (tcenv : TC.GlobalEnv.t) (test_fmt : bool) (name : string)
    (decls : string) (expr : string) : unit Alcotest.test_case =
  let (tcenv', _) = extend_tcenv tcenv decls in
  (name, `Quick, check_expr_tcheck tcenv' test_fmt name expr)

let eval tcenv env (input : string) : Value.value =
  let loc = AST.Unknown in
  let e = LoadASL.read_expr tcenv loc input in
  Eval.eval_expr loc env e

type xform = AST.declaration list -> AST.declaration list

let test_bool (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : bool) () : unit =
  let (tcenv, env) = extend_env globals prelude decls in
  let what = l ^ " == " ^ Bool.to_string r in
  Alcotest.check value what (Value.VBool r) (eval tcenv env l)

let test_int (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : int) () : unit =
  let (tcenv, env) = extend_env globals prelude decls in
  let what = l ^ " == " ^ string_of_int r in
  Alcotest.check value what (Value.VInt (Z.of_int r)) (eval tcenv env l)

let test_string (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, env) = extend_env globals prelude decls in
  let what = l ^ " == " ^ r in
  Alcotest.check value what (Value.VString r) (eval tcenv env l)

let test_bits (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, env) = extend_env globals prelude decls in
  let what = l ^ " == " ^ r in
  Alcotest.check value what (Value.from_bitsLit r) (eval tcenv env l)

(* Test that a global transformation f does not change the value of expression x *)
let test_xform (globals : TC.GlobalEnv.t) (prelude : AST.declaration list)
    (f : xform) (decls : string) (x : string) () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let ds1 = List.append prelude ds in
  let ds2 = f ds1 in
  let r1 = eval tcenv (Eval.build_evaluation_environment ds1) x in
  let r2 = eval tcenv (Eval.build_evaluation_environment ds2) x in
  Alcotest.check value "transformed code" r1 r2

let tests : unit Alcotest.test_case list =
  let paths = [ "../../.." ] in
  let prelude = LoadASL.read_file paths "prelude.asl" true false in
  let globals = TC.env0 in
  [
    test_static globals true "literals (int)" "" "1234";
    test_static globals true "literals (real)" "" "10.0";
    test_static globals true "literals (bits)" "" "'1111 0000'";
    test_static globals true "literals (string)" "" "\"abc\"";
    test_static globals true "literals (string)" "" "\"ab\\nc\"";
    test_static globals true "literals (string)" "" "\"ab\\tc\"";
    test_static globals true "literals (string)" "" "\"ab\\\\c\"";
    test_static globals true "literals (string)" "" "\"ab\\\"c\"";
    test_static globals true "expressions (records)"
      "record Pair{x :: integer; y :: integer; };" "Pair{x = 1, y = 2}";
    test_static globals true "expressions (UNKNOWN)" "" "UNKNOWN :: bits(4)";
    test_static globals true "expressions (IMPDEF)" ""
      "IMPLEMENTATION_DEFINED \"MaxAddr\" :: bits(64)";
    test_static globals false "expressions (bitfields)"
      "type T of bits(32) { [ 31:16 ] hi, [15:0] lo };\n\
      \                      let t :: T = 0x12345678[31:0];\n\
      \                     " "t.hi";
    test_static globals false "var decls"
      "func F(x :: bits(8*N))
           var a :: bits(8*N) = UNKNOWN :: bits(8*N);
           var b :: bits(8*N) = Zeros(8*N);
           var c :: bits(8*N);
           var d = UNKNOWN :: bits(8*N);
           var _ = 1;
           var (f, g) = (1, TRUE);
           var (h :: integer, i :: boolean) = (1, TRUE);
           var (j :: integer, _ :: boolean) = (1, TRUE);

           let m :: bits(8*N) = UNKNOWN :: bits(8*N);
           let n :: bits(8*N) = Zeros(8*N);
           let o = UNKNOWN :: bits(8*N);
           let _ = 1;
           let (p, q) = (1, TRUE);
           let (r :: integer, s :: boolean) = (1, TRUE);
           let (t :: integer, _ :: boolean) = (1, TRUE);
       end" "1";
    test_static globals false "case statements"
      "func F(x :: bits(3), y :: boolean) => integer
           case x of
               when '000': return 0;
               when '001': return 0;
               when '01x': return 2;
               when '100', '111': return 4;
               when '101' where y: return 5;
               otherwise: return 6;
           end
       end" "1";
    ("operators (implies)",    `Quick, test_bool globals prelude "" "FALSE --> FALSE" true);
    ("operators (implies)",    `Quick, test_bool globals prelude "" "FALSE --> TRUE" true);
    ("operators (implies)",    `Quick, test_bool globals prelude "" "TRUE --> FALSE" false);
    ("operators (implies)",    `Quick, test_bool globals prelude "" "TRUE --> TRUE" true);
    ("operators (iff)",        `Quick, test_bool globals prelude "" "FALSE <-> FALSE" true);
    ("operators (iff)",        `Quick, test_bool globals prelude "" "FALSE <-> TRUE" false);
    ("operators (iff)",        `Quick, test_bool globals prelude "" "TRUE <-> FALSE" false);
    ("operators (iff)",        `Quick, test_bool globals prelude "" "TRUE <-> TRUE" true);
    ("prelude (cvt_bits_str)", `Quick, test_string globals prelude "" "cvt_bits_str(1, '0')" "'0'");
    ("prelude (HexStr (int))", `Quick, test_string globals prelude "" "HexStr(15)" "0xf");
    ("prelude (+ (int))",      `Quick, test_int globals prelude "" "1+1" 2);
    ("prelude (DIV)",          `Quick, test_int globals prelude "" "5 DIV 3" 1);
    ("prelude (mul_bits_int)", `Quick, test_bits globals prelude "" "'00111' * 3" "10101"); (* == 21 *)
    ("prelude (DecStr)",       `Quick, test_string globals prelude "" "DecStr('10101')" "21");
    ("prelude (HexStr)",       `Quick, test_string globals prelude "" "HexStr('10101')" "0x15");
    ("statements (while)",     `Quick, test_int globals prelude
      "func TestWhile(x :: integer) => integer var i = 0; while i < x do i = i + 1; end return i; end"
      "TestWhile(3)" 3);
    ("statements (repeat)",    `Quick, test_int globals prelude
      "func TestRepeat(x :: integer) => integer var i = 0; repeat i = i + 1; until i >= x; return i; end"
      "TestRepeat(3)" 3);
    ("tuple transform",        `Quick, test_xform globals prelude Xform_tuples.xform_decls
       "func F() => (integer, integer) return (1,2); end
       func T() => integer let (x, y) = F(); return x + y; end"
      "T() == 3");
  ]

let () = Alcotest.run "libASL" [ ("asl", tests) ]

(****************************************************************
 * End
 ****************************************************************)
