(****************************************************************
 * Test getters and setters elimination transform
 *
 * Copyright Intel Inc (c) 2024
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module TC = Tcheck

(****************************************************************
 * Test getters and setters elimination
 ****************************************************************)

let getset_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let decl = test_xform_decls Xform_getset.xform_decls globals prelude in
  [
    ("getter type", `Quick, decl
      ""
      "getter G => integer;"
      "func G_read() => integer;");

    ("getter", `Quick, decl
      "var x : integer;"
      "getter G => integer begin return x; end"
      "func G_read() => integer begin return x; end");

    ("setter type", `Quick, decl
      ""
      "setter S = val : integer;"
      "func S_write(val : integer);");

    ("setter", `Quick, decl
      "var x : integer;"
      "setter S = val : integer begin x = val; end"
      "func S_write(val : integer) begin x = val; end");

    ("array getter type", `Quick, decl
      ""
      "getter G[i : integer] => integer;"
      "func G_read(i : integer) => integer;");

    ("array getter", `Quick, decl
      "var x : array [1] of integer;"
      "getter G[i : integer] => integer begin return x[i]; end"
      "func G_read(i : integer) => integer begin return x[i]; end");

    ("array setter type", `Quick, decl
      ""
      "setter S[i : integer] = val : integer;"
      "func S_set(i : integer, val : integer);");

    ("array setter", `Quick, decl
      "var x : array [1] of integer;"
      "setter S[i : integer] = val : integer begin x[i] = val; end"
      "func S_set(i : integer, val : integer) begin x[i] = val; end");

    ("setter call in procedure", `Quick, decl
      "var x : integer;"
      "setter S = val : integer begin x = val; end
       func F() begin S = 0; end"
      "func S_write(val : integer) begin x = val; end
       func F() begin S_write(0); end");

    ("setter call in function", `Quick, decl
      "var x : integer;"
      "setter S = val : integer begin x = val; end
       func F() => integer begin S = 0; return 0; end"
      "func S_write(val : integer) begin x = val; end
       func F() => integer begin S_write(0); return 0; end");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("getset", getset_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
