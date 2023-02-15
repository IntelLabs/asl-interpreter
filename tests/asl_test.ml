(****************************************************************
 * Test ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2020
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
module TC = Tcheck
module AST = Asl_ast

(* test that checks that an expression lexes, parses and typechecks *)
let check_expr_tcheck (tcenv : TC.Env.t) (test_fmt : bool) (what : string)
    (input : string) _ : unit =
  let loc = AST.Unknown in
  let e = LoadASL.read_expr tcenv loc input in
  if test_fmt then
    Alcotest.(check string) ("format " ^ what) input (format_expr e)

(* simple test of static semantics: parsing and typechecking of correct expressions
 * and optionally checks that pretty-printing the AST produces exactly the input string
 * optionally extends environment with new declarations first
 *)
let test_static (tcenv : TC.GlobalEnv.t) (test_fmt : bool) (name : string)
    (decls : string) (expr : string) : unit Alcotest.test_case =
  let (tcenv', _) = extend_tcenv tcenv decls in
  (name, `Quick, check_expr_tcheck tcenv' test_fmt name expr)

(* Test that checks that a declaration reports the right error message at the right location
 *
 * Both the error message and the location are optional.
 * If both are omitted, we just check that an error is reported.
 *)
let test_static_error (globals : TC.GlobalEnv.t) (name : string) (declarations : string)
    (oexpect : string option) (oloc : string option) : unit Alcotest.test_case =
  let globals = TC.GlobalEnv.clone globals in
  (name, `Quick, fun _ ->
    let lexbuf = Lexing.from_string declarations in
    let msg = try
        let t = Asl_parser.declarations_start Lexer.token lexbuf in
        let ds = TC.tc_declarations globals false t in
        ignore ds;
        Alcotest.fail "error was not detected"
      with
      | Asl_parser.Error -> "ParseError()"
      | AST.Parse_error_locn(loc, msg) -> Printf.sprintf "Parse_error_locn(%s,%s)" (AST.pp_loc loc) msg
      | AST.PrecedenceError(loc, op1, op2) ->
        Printf.sprintf "PrecedenceError(%s,%s,%s)" (AST.pp_loc loc) (Asl_utils.pp_binop op1) (Asl_utils.pp_binop op2)
      | Lexer.Eof -> "Eof()"
      | TC.UnknownObject(loc, what, x) -> Printf.sprintf "UnknownObject(%s,%s,%s)" (AST.pp_loc loc) what x
      | TC.DoesNotMatch(loc, what, x, y) -> Printf.sprintf "DoesNotMatch(%s,%s,%s,%s)" (AST.pp_loc loc) what x y
      | TC.IsNotA(loc, what, x) -> Printf.sprintf "IsNotA(%s,%s,%s)" (AST.pp_loc loc) what x
      | TC.Ambiguous(loc, what, x) -> Printf.sprintf "Ambiguous(%s,%s,%s)" (AST.pp_loc loc) what x
      | TC.TypeError(loc, msg) -> Printf.sprintf "TypeError(%s,%s)" (AST.pp_loc loc) msg
      | TC.InternalError(msg) -> Printf.sprintf "InternalError(%s)" msg
      | Value.Return(_) -> Printf.sprintf "Return(_)"
      | Value.EvalError(loc, err) -> Printf.sprintf "EvalError(%s,%s)" (AST.pp_loc loc) err
      | Value.Throw(loc, _) -> Printf.sprintf "Throw(%s,_)" (AST.pp_loc loc)
    in
    ( match oexpect with
    | Some expect -> Alcotest.(check string) name expect msg;
    | _ -> ()
    );
    ( match oloc with
    | Some loc ->
        let location = Printf.sprintf "'%s' %d %d"
            lexbuf.lex_start_p.pos_fname lexbuf.lex_start_p.pos_lnum (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
        in
        Alcotest.(check string) (name ^ " location") loc location
    | _ -> ()
    )
  )

let eval tcenv env (input : string) : Value.value =
  let loc = AST.Unknown in
  let e = LoadASL.read_expr tcenv loc input in
  Eval.eval_expr loc env e

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
    (f : (AST.declaration list) xform) (decls : string) (x : string) () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let ds1 = List.append prelude ds in
  let ds2 = f ds1 in
  let r1 = eval tcenv (Eval.build_evaluation_environment ds1) x in
  let r2 = eval tcenv (Eval.build_evaluation_environment ds2) x in
  Alcotest.check value "transformed code" r1 r2

let tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
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
      "record Pair{x : integer; y : integer; };" "Pair{x = 1, y = 2}";
    test_static globals true "expressions (UNKNOWN)" "" "UNKNOWN : bits(4)";
    test_static globals true "expressions (IMPDEF)" ""
      "IMPLEMENTATION_DEFINED \"MaxAddr\" : bits(64)";
    test_static globals false "expressions (bitfields)"
      "type T of bits(32) { [ 31:16 ] hi, [15:0] lo };\n\
      \                      let t : T = 0x12345678[31:0];\n\
      \                     " "t.hi";
    test_static_error globals "return type mismatch"
      "func F(x : boolean) => integer return x; end"
      (Some "DoesNotMatch(file \"\" line 1 char 31 - 40,type,integer,boolean)")
      None;
    test_static_error globals "#line directive"
      "\n# 42 \"foobar.asl\"\nx == 0"
      (Some "ParseError()") (Some "'foobar.asl' 42 0");
    test_static_error globals "record initializer (missing field)"
      "record R{ x : integer; y : integer; };
      func T() => R return R{x=1, z=3}; end"
      (Some "TypeError(file \"\" line 2 char 20 - 39,record initializer is missing fields y and/or has extra fields z)") None;
    test_static_error globals "record initializer (parameterization error)"
      "record R(M) { x : bits(M); };
      func S4(r : R(4)) => bits(4) return r.x; end
      func T() let t = S4(R{x='111'}); end"
      (Some "TypeError(file \"\" line 3 char 15 - 38,wrong number of type parameters)") None;
    test_static_error globals "function missing"
      "func T() UndefinedFunction(); end"
      (Some "UnknownObject(file \"\" line 1 char 9 - 29,procedure,UndefinedFunction)") None;
    test_static_error globals "function type error (args)"
      "func IntFunction(x : integer) end
       func T() IntFunction(TRUE); end"
      (Some "TypeError(file \"\" line 2 char 16 - 34,function arguments)") None;
    test_static_error globals "var setter arg mismatch"
      "setter A = value : integer;
       func T() A = TRUE; end"
      (Some "DoesNotMatch(file \"\" line 2 char 16 - 25,type,integer,boolean)")
      None;
    test_static_error globals "array setter arg mismatch"
      "setter A[] = value : integer;
       func T() A[] = TRUE; end"
      (Some "DoesNotMatch(file \"\" line 2 char 16 - 27,type,integer,boolean)")
      None;
    test_static_error globals "type parameter - argument mismatch"
     "func F{A}(A : integer, src : bits(A))
      end
      func Test()
          let src = Zeros(10);
          F(5, src);
      end
     "
     (Some "TypeError(file \"\" line 5 char 10 - 20,Type mismatch)")
     None;
    test_static globals false "var decls"
      "func F(x : bits(8*N))
           var a : bits(8*N) = UNKNOWN : bits(8*N);
           var b : bits(8*N) = Zeros(8*N);
           var c : bits(8*N);
           var d = UNKNOWN : bits(8*N);
           var _ = 1;
           var (f, g) = (1, TRUE);
           var (h : integer, i : boolean) = (1, TRUE);
           var (j : integer, _ : boolean) = (1, TRUE);
           var arr1 : array [8] of integer;
           var arr2 : array [boolean] of integer;

           let m : bits(8*N) = UNKNOWN : bits(8*N);
           let n : bits(8*N) = Zeros(8*N);
           let o = UNKNOWN : bits(8*N);
           let _ = 1;
           let (p, q) = (1, TRUE);
           let (r : integer, s : boolean) = (1, TRUE);
           let (t : integer, _ : boolean) = (1, TRUE);
       end" "1";
    test_static globals false "case statements"
      "func F(x : bits(3), y : boolean) => integer
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

    (* This regression test is for a typechecker bug where the typechecker did not handle negations
     * correctly (and subtractions 'x-y' were being transformed into 'x + (-y)' *)
    ("tcheck regression (neg_int)", `Quick, test_bits globals prelude
     "func F(i : integer, N : integer) => bits(N)
          var result = Ones(N);
          result[N-1 : i] = Zeros(N-i);
          return result;
      end"
    "F(3,8)" "00000111");

    ("setters (var)",          `Quick, test_bool globals prelude
       "var _A : boolean;
       getter A => boolean return _A; end
       setter A = v : boolean _A = v; end
       func Test(v : boolean) => boolean
           A = v;
           return A;
       end
       "
       "Test(TRUE)" true);
    ("bittuple LExpr_BitTuple", `Quick, test_bool globals prelude
     "func expand() => boolean
        var x : bits(4);
        var y : bits(8);
        var z : bits(12);
        [x, y, z] = '1111 01010101 000000000000';

        return (x == '1111' && y == '01010101' && z == '000000000000');
     end"
     "expand()" true);
    ("setters (array)",        `Quick, test_bool globals prelude
       "var _A : array [4] of boolean;
       getter A[i : integer] => boolean return _A[i]; end
       setter A[i : integer] = v : boolean _A[i] = v; end
       func Test(i : integer, v : boolean) => boolean
           A[i] = v;
           return A[i];
       end
       "
       "Test(1,TRUE)" true);
    ("parameterized record",   `Quick, test_bits globals prelude
       "record R(M) { x : bits(M); };
       func S(b : bits(M)) => R(M) return R(M){x=b}; end
       func T(r : R(M)) => bits(M) return r.x; end"
       "T(S('111'))" "111");
    ("parameterized type",     `Quick, test_bits globals prelude
       "type B(M) of bits(M);
       func T(b : bits(M)) => B(M) return b; end"
       "T('111')" "111");
    ("parameterized record extract", `Quick, test_bits globals prelude
       "record R(M) { x : bits(M); };
       func F() => bits(8)
           let a = R(3){ x = '111' };
           return ZeroExtend(a.x, 8);
       end"
       "F()" "00000111");
    ("statements (while)",     `Quick, test_int globals prelude
      "func TestWhile(x : integer) => integer var i = 0; while i < x do i = i + 1; end return i; end"
      "TestWhile(3)" 3);
    ("statements (repeat)",    `Quick, test_int globals prelude
      "func TestRepeat(x : integer) => integer var i = 0; repeat i = i + 1; until i >= x; return i; end"
      "TestRepeat(3)" 3);
    ("tuple transform",        `Quick, test_xform globals prelude Xform_tuples.xform_decls
      "func F() => (integer, integer) return (1,2); end
       func T() => integer let (x, y) = F(); return x + y; end"
      "T() == 3");
    ("tuple transform (in getter)", `Quick, test_xform globals prelude Xform_tuples.xform_decls
      "func F() => (integer, integer) return (1,2); end
       getter T => integer let (x, y) = F(); return x + y; end"
      "T == 3");
    ("getter & setter transform", `Quick, test_xform globals prelude Xform_getset.xform_decls
      "var i : integer;
       getter G => integer return i; end
       setter S = value : integer i = value; end
       func P() S = G + 1; end
       func T() => integer i = 1; P(); S = G + 1; return i; end"
      "T() == 3");
    ("array getter & setter transform", `Quick, test_xform globals prelude Xform_getset.xform_decls
      "var i : array [2] of integer;
       getter G{N}[x : bits(N)] => integer return i[UInt(x)]; end
       setter S{N}[x : bits(N)] = value : integer i[UInt(x)] = value; end
       func P() S['0'] = G['0'] + 1; end
       func T() => integer i[0] = 1; P(); S['0'] = G['0'] + 1; return i[0]; end"
      "T() == 3");
    ("read-modify-write transform", `Quick, test_xform globals prelude Xform_rmw.xform_decls
      "var i : array [2] of bits(3);
       getter I{N}[x : bits(N)] => bits(3) return i[UInt(x)]; end
       setter I{N}[x : bits(N)] = value : bits(3) i[UInt(x)] = value; end
       func T() => bits(3) i[0] = '001'; I['0'][1 : 0] = '10'; return i[0]; end"
      "T() == '010'");
  ]

let () = Alcotest.run "libASL" [ ("asl", tests) ]

(****************************************************************
 * End
 ****************************************************************)
