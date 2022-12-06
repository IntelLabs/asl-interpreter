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

let bitslice_tests : unit Alcotest.test_case list =
  let paths = [ "../../.." ] in
  let prelude = LoadASL.read_file paths "prelude.asl" true false in
  let globals = TC.env0 in
  let expr = test_xform_expr Xform_bitslices.xform_expr globals prelude in
  [
    ("combine (hi:lo)", `Quick, expr
       "var x :: bits(64); var y :: bits(64); var i :: integer;"
       "[x[63:i], y[i-1:0]]"
       "lsl_bits(lsr_bits(x, i), i) OR and_bits(y, mk_mask(i, 64))");
    ("combine (lo:wd)", `Quick, expr
       "var x :: bits(64); var y :: bits(64); var i :: integer;"
       "[x[i +: 64-i], y[0 +: i]]"
       "lsl_bits(lsr_bits(x, i) AND mk_mask(64-i, 64), i) OR and_bits(y, mk_mask(i, 64))");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("bitslice", bitslice_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
