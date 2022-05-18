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
let value = Alcotest.testable format_value ( = )

let check_int what l r = Alcotest.check value what l (Value.VInt (Z.of_int r))

let eval tcenv env (input : string): Value.value =
    let loc = AST.Unknown in
    let e = LoadASL.read_expr tcenv loc input in
    Eval.eval_expr loc env e

let test_arith tcenv env () : unit =
    check_int "1+1 == 2" (eval tcenv env "1+1") 2;
    check_int "5 DIV 3 == 1" (eval tcenv env "5 DIV 3") 1

let tests : unit Alcotest.test_case list =
    let paths   = ["../../.."] in
    let prelude = LoadASL.read_file paths "prelude.asl" true false in
    let tcenv   = TC.Env.mkEnv TC.env0 in
    let env     = Eval.build_evaluation_environment prelude in
    [
        ("arith", `Quick, test_arith tcenv env)
    ]

let () = Alcotest.run "libASL" [("asl", tests)]

(****************************************************************
 * End
 ****************************************************************)
