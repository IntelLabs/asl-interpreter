(****************************************************************
 * Test bitslice lowering transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module TC = Tcheck

(****************************************************************
 * Test bitslice lowering
 ****************************************************************)

let bitslice_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let expr = test_xform_expr Xform_bitslices.xform_expr globals prelude in
  let stmts = test_xform_stmts Xform_bitslices.xform_stmts globals prelude in
  [
    ("combine (lo+:wd)", `Quick, expr
       "var x : bits(64); var y : bits(64); var i : integer;"
       "[x[i +: 64-i], y[0 +: i]]"
       "lsl_bits(lsr_bits(x, i) AND mk_mask(64-i, 64), i) OR and_bits(y, mk_mask(i, 64))");
    ("transform Ones() 1 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[0 +: i+1] = Ones(i+1);"
       "x = and_bits(x, NOT mk_mask(add_int(i, 1), 64))
            OR mk_mask(add_int(i, 1), 64);");
    ("transform Ones() 2 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[1 +: i] = Ones(i);"
       "x = and_bits(x, NOT lsl_bits(mk_mask(i, 64), 1))
            OR lsl_bits(mk_mask(i, 64), 1);");
    ("transform Zeros() 1 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[0 +: i + 1] = Zeros(i+1);"
       "x = x AND NOT mk_mask(add_int(i, 1), 64);");
    ("transform Zeros() 2 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[1 +: i] = Zeros(i);"
       "x = x AND NOT lsl_bits(mk_mask(i, 64), 1);");
    ("transform integer bitslice", `Quick, stmts
       "var result : bits(32); var x : bits(8);"
       "let r = CountLeadingZeroBits(x);
        result[0 +: 8] = r[0 +: 8];"
       "let r = CountLeadingZeroBits(x);
        result = and_bits(result, NOT mk_mask(8, 32))
                 OR zero_extend_bits(r[0 +: 8], 32);");
    ("transform bitslice, width of r > width of l", `Quick, stmts
       "var l : bits(8); var r : bits(16);"
       "l[1 +: 7] = r[1 +: 7];"
       "l = and_bits(l, NOT lsl_bits(mk_mask(7, 8), 1))
            OR lsl_bits(zero_extend_bits(r[1 +: 7], 8), 1);");
    ("transform ZeroExtend(Ones(i), n)", `Quick, expr
       "var i : integer;"
       "ZeroExtend(Ones(i), 64)"
       "mk_mask(i, 64)");
    ("transform [Ones(i), Zeros(n-i)]", `Quick, expr
       "var i : integer;"
       "[Ones(i), Zeros(64-i)]"
       "lsl_bits(mk_mask(i, 64), add_int(64, -i))");
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
