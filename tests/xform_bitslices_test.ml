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
  let stmts = test_xform_stmts Xform_bitslices.xform_stmts globals prelude in
  [
    ("combine (hi:lo)", `Quick, expr
       "var x : bits(64); var y : bits(64); var i : integer;"
       "[x[63:i], y[i-1:0]]"
       "lsl_bits(lsr_bits(x, i), i) OR and_bits(y, mk_mask(i, 64))");
    ("combine (lo:wd)", `Quick, expr
       "var x : bits(64); var y : bits(64); var i : integer;"
       "[x[i +: 64-i], y[0 +: i]]"
       "lsl_bits(lsr_bits(x, i) AND mk_mask(64-i, 64), i) OR and_bits(y, mk_mask(i, 64))");
    ("transform Ones() 1 hi:lo", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[i:0] = Ones(i+1);"
       "x = or_bits(
          and_bits(x, not_bits(mk_mask(add_int(i, 1), 64))),
          mk_mask(add_int(i, 1), 64));");
    ("transform Ones() 2 hi:lo", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[i:1] = Ones(i);"
       "x = or_bits(
          and_bits(x, not_bits(lsl_bits(mk_mask(i, 64), 1))),
          lsl_bits(mk_mask(i, 64), 1));");
    ("transform Ones() 1 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[0 +: i+1] = Ones(i+1);"
       "x = or_bits(
          and_bits(x, not_bits(mk_mask(add_int(i, 1), 64))),
          mk_mask(add_int(i, 1), 64));");
    ("transform Ones() 2 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[1 +: i] = Ones(i);"
       "x = or_bits(
          and_bits(x, not_bits(lsl_bits(mk_mask(i, 64), 1))),
          lsl_bits(mk_mask(i, 64), 1));");
    ("transform Zeros() 1 hi:lo", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[i:0] = Zeros(i+1);"
       "x = and_bits(x, not_bits(mk_mask(add_int(i, 1), 64)));");
    ("transform Zeros() 2 hi:lo", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[i:1] = Zeros(i);"
       "x = and_bits(x, not_bits(lsl_bits(mk_mask(i, 64), 1)));");
    ("transform Zeros() 1 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[0 +: i + 1] = Zeros(i+1);"
       "x = and_bits(x, not_bits(mk_mask(add_int(i, 1), 64)));");
    ("transform Zeros() 2 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[1 +: i] = Zeros(i);"
       "x = and_bits(x, not_bits(lsl_bits(mk_mask(i, 64), 1)));");
    ("transform integer bitslice", `Quick, stmts
       "var result : bits(32); var x : bits(8);"
       "let r = CountLeadingZeroBits(x);
        result[0 +: 8] = r[0 +: 8];"
       "let r = CountLeadingZeroBits(x);
        result = or_bits(and_bits(result, not_bits(mk_mask(8, 32))),
            zero_extend_bits(r[0 +: 8], 32));");
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
