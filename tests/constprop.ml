(****************************************************************
 * Test constant propagation transform
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
 * Test constant propagation
 ****************************************************************)

(** Test xform_expr *)
let test_cp_expr (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let genv = Eval.build_constant_environment (prelude @ ds) in
  let env = Xform_constprop.mkEnv genv [] in
  let l' = LoadASL.read_expr tcenv AST.Unknown l in
  let what = l ^ " == " ^ r in
  Alcotest.check Alcotest.string what r (Asl_utils.pp_expr (Xform_constprop.xform_expr env l'))

(** Test xform_stmts *)
let test_cp_stmts (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let genv = Eval.build_constant_environment (prelude @ ds) in
  let env = Xform_constprop.mkEnv genv [] in
  let l' = LoadASL.read_stmt tcenv l in
  let l'' = Xform_constprop.xform_stmts env [l'] in
  let l''' = String.concat " " (List.map Asl_utils.pp_stmt l'') in
  let what = l ^ " == " ^ r in
  Alcotest.check Alcotest.string what r l'''

let constprop_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  [
    ("add", `Quick, test_cp_expr globals prelude "" "1 + 1" "2");
    ("add_mul", `Quick, test_cp_expr globals prelude "" "1 + (2 * 3)" "7");
    ("bool", `Quick, test_cp_expr globals prelude "" "1 == 1" "TRUE");
    ("bool", `Quick, test_cp_expr globals prelude "" "1 == 2" "FALSE");
    ("IN", `Quick, test_cp_expr globals prelude "" "8 IN {8, 16}" "TRUE");
    ("enum", `Quick, test_cp_expr globals prelude
       "enumeration T { E1, E2 };" "if TRUE then E1 else E2" "E1");

    ("let", `Quick, test_cp_stmts globals prelude ""
       "let x = 1 + 1;" "let x :: integer = 2;");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "asl_utils" [
    ("constprop", constprop_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
