(****************************************************************
 * Test tuple lowering transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module AST = Asl_ast
module TC = Tcheck

(****************************************************************
 * Test tuple lowering
 ****************************************************************)

let tuple_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let stmts = test_xform_stmts Xform_tuples.xform_stmts globals prelude in
  [
    ("assign", `Quick, stmts
       "var x : integer; var y : integer;"
       "(x, y) = (1, 2);"
       "x = 1; y = 2;");

    ("let-if", `Quick, stmts
       "var p : boolean;"
       "let (x : integer, y : integer) = if p then (1, 2) else (2, 1);"
       "var __t0 : integer; var __t1 : integer;
        if p then __t0 = 1; __t1 = 2; else __t0 = 2; __t1 = 1; end
        let x : integer = __t0; let y : integer = __t1;
       ");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("tuple", tuple_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
