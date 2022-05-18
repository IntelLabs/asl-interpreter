(****************************************************************
 * Test ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2020
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL

module TC  = Tcheck
module AST = Asl_ast

let format_value f v = Format.fprintf f "%s" (Value.pp_value v)
let format_expr (e : AST.expr) : string = Utils.to_string2 (fun fmt -> Asl_fmt.expr fmt e)
let value = Alcotest.testable format_value ( = )

(* test that checks that an expression lexes, parses and typechecks *)
let check_expr_tcheck (tcenv : TC.Env.t) (test_fmt : bool) (what : string) (input : string) _ : unit =
    LoadASL.report_type_error (fun _ -> Alcotest.fail "type error") (fun _ ->
        LoadASL.report_parse_error (fun _ -> Alcotest.fail "parse error") (fun _ ->
            let loc = AST.Unknown in
            let e = LoadASL.read_expr tcenv loc input in
            if test_fmt then Alcotest.(check string) ("format "^what) input (format_expr e)
        )
    )

let extend_tcenv (globals : TC.GlobalEnv.t) (declarations : string) : TC.Env.t =
  let globals = TC.GlobalEnv.clone globals in
  let tcenv   = TC.Env.mkEnv globals in
  LoadASL.report_type_error (fun _ -> Alcotest.fail "type error in decls") (fun _ ->
    LoadASL.report_parse_error (fun _ -> Alcotest.fail "parse error in decls") (fun _ ->
      let lexbuf = Lexing.from_string declarations in
      let t = Asl_parser.declarations_start Lexer.token lexbuf in
      ignore (TC.tc_declarations globals false t);
      tcenv
    )
  )

(* simple test of static semantics: parsing and typechecking of correct expressions
 * and optionally checks that pretty-printing the AST produces exactly the input string
 * optionally extends environment with new declarations first
 *)
let test_static (tcenv : TC.GlobalEnv.t) (test_fmt : bool) (name : string) (decls : string) (expr : string) : unit Alcotest.test_case =
    (name, `Quick, check_expr_tcheck (extend_tcenv tcenv decls) test_fmt name expr)

let check_int what l r = Alcotest.check value what l (Value.VInt (Z.of_int r))

let check_bool what l r = Alcotest.check value what l (Value.VBool r)

let eval tcenv env (input : string): Value.value =
    let loc = AST.Unknown in
    let e = LoadASL.read_expr tcenv loc input in
    Eval.eval_expr loc env e

let test_primop_boolean (globals : TC.GlobalEnv.t) (env : Eval.Env.t) () : unit =
    let tcenv = TC.Env.mkEnv globals in
    check_bool "implies" (eval tcenv env "FALSE --> FALSE") true;
    check_bool "implies" (eval tcenv env "FALSE --> TRUE") true;
    check_bool "implies" (eval tcenv env "TRUE --> FALSE") false;
    check_bool "implies" (eval tcenv env "TRUE --> TRUE") true;
    check_bool "iff" (eval tcenv env "FALSE <-> FALSE") true;
    check_bool "iff" (eval tcenv env "FALSE <-> TRUE") false;
    check_bool "iff" (eval tcenv env "TRUE <-> FALSE") false;
    check_bool "iff" (eval tcenv env "TRUE <-> TRUE") true;
    ()

let test_primop_integer (globals : TC.GlobalEnv.t) (env : Eval.Env.t) () : unit =
    let tcenv = TC.Env.mkEnv globals in
    check_int "1+1 == 2" (eval tcenv env "1+1") 2;
    check_int "5 DIV 3 == 1" (eval tcenv env "5 DIV 3") 1

let tests : unit Alcotest.test_case list =
    let prelude = LoadASL.read_file "../../../prelude.asl" true false in
    let globals = TC.env0 in
    let env     = Eval.build_evaluation_environment prelude in
    [
        (test_static globals true "literals (int)" "" "1234");
        (test_static globals true "literals (real)" "" "10.0");
        (test_static globals true "literals (bits)" "" "'1111 0000'");
        (test_static globals true "literals (string)" "" "\"abc\"");
        (test_static globals true "literals (string)" "" "\"ab\\nc\"");
        (test_static globals true "literals (string)" "" "\"ab\\tc\"");
        (test_static globals true "literals (string)" "" "\"ab\\\\c\"");
        (test_static globals true "literals (string)" "" "\"ab\\\"c\"");
        (test_static globals true "expressions (records)"
                     "record Pair{x :: integer; y :: integer; };"
                     "Pair{x = 1, y = 2}");
        (test_static globals true "expressions (UNKNOWN)" "" "UNKNOWN :: bits(4)");
        (test_static globals true "expressions (IMPDEF)" "" "IMPLEMENTATION_DEFINED \"MaxAddr\" :: bits(64)");
        (test_static globals false "expressions (bitfields)"
                     "type T of bits(32) { [ 31:16 ] hi, [15:0] lo };
                      let t :: T = 0x12345678[31:0];
                     "
                     "t.hi");
        ("operators (boolean)", `Quick, test_primop_boolean globals env);
        ("operators (integer)", `Quick, test_primop_integer globals env)
    ]

let () = Alcotest.run "libASL" [("asl", tests)]

(****************************************************************
 * End
 ****************************************************************)
