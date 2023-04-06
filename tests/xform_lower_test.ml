(****************************************************************
 * Test case lowering transform
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
 * Test bitslice lowering
 ****************************************************************)

let bitslices_hilo_tests : unit Alcotest.test_case list =
  let paths = [ "../../.." ] in
  let prelude = LoadASL.read_file paths "prelude.asl" true false in
  let globals = TC.env0 in
  let expr = test_xform_expr Xform_lower.xform_expr globals prelude in
  let stmts = test_xform_stmts Xform_lower.xform_stmts globals prelude in
  [
    ("lower x[63:0] where x is a bitvector", `Quick, expr
       "var x : bits(64);"
       "x[63:0]"
       "x[0 +: 64]");
    ("lower x[hi:lo] where x is a bitvector", `Quick, expr
       "var x : bits(64); var hi : integer; var lo : integer;"
       "x[hi:lo]"
       "x[lo +: add_int(add_int(hi,neg_int(lo)), 1)]");
    ("lower x[2:0] = y[2:0] where x and y are bitvectors", `Quick, stmts
       "var x : bits(8); var y : bits(8);"
       "x[2:0] = y[2:0];"
       "x[0 +: 3] = y[0 +: 3];");
    ("lower x[hi:lo] = y[hi:lo] where x and y are bitvectors", `Quick, stmts
       "var x : bits(8); var y : bits(8); var hi : integer; var lo : integer;"
       "x[hi:lo] = y[hi:lo];"
       "x[lo +: add_int(add_int(hi, neg_int(lo)), 1)] = y[lo +: add_int(add_int(hi, neg_int(lo)), 1)];");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("expr_bitslices_hilo", bitslices_hilo_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
