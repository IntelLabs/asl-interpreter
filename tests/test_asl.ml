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

(* simple test of static semantics: parsing and typechecking of correct expressions
 * and optionally checks that pretty-printing the AST produces exactly the input string
 *)
let test_static tcenv (test_fmt : bool) (name : string) (expr : string) : unit Alcotest.test_case =
    (name, `Quick, check_expr_tcheck tcenv test_fmt name expr)

let check_int what l r = Alcotest.check value what l (Value.VInt (Z.of_int r))

let eval tcenv env (input : string): Value.value =
    let loc = AST.Unknown in
    let e = LoadASL.read_expr tcenv loc input in
    Eval.eval_expr loc env e

let test_arith tcenv env () : unit =
    check_int "1+1 == 2" (eval tcenv env "1+1") 2;
    check_int "5 DIV 3 == 1" (eval tcenv env "5 DIV 3") 1

let tests : unit Alcotest.test_case list =
    let prelude = LoadASL.read_file "../../../prelude.asl" true false in
    let tcenv   = TC.Env.mkEnv TC.env0 in
    let env     = Eval.build_evaluation_environment prelude in
    [
        (test_static tcenv true "literals (int)" "1234");
        (test_static tcenv true "literals (real)" "10.0");
        (test_static tcenv true "literals (bits)" "'1111 0000'");
        (test_static tcenv true "UNKNOWN" "UNKNOWN :: bits(4)");
        ("arith", `Quick, test_arith tcenv env)
    ]

let () = Alcotest.run "libASL" [("asl", tests)]

(****************************************************************
 * End
 ****************************************************************)
