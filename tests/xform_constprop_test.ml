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

let constprop_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let test_cp_stmts (decls : string) (l : string) (r : string) () : unit =
    let (tcenv, ds) = extend_tcenv globals decls in
    let genv = Eval.build_constant_environment (prelude @ ds) in
    let env = Xform_constprop.mkEnv genv [] in
    test_xform_stmts (Xform_constprop.xform_stmts env) globals prelude decls l r ()
  in
  [
    ("add", `Quick, test_cp_expr globals prelude "" "1 + 1" "2");
    ("add_mul", `Quick, test_cp_expr globals prelude "" "1 + (2 * 3)" "7");
    ("bool", `Quick, test_cp_expr globals prelude "" "1 == 1" "TRUE");
    ("bool", `Quick, test_cp_expr globals prelude "" "1 == 2" "FALSE");
    ("IN", `Quick, test_cp_expr globals prelude "" "8 IN {8, 16}" "TRUE");
    ("enum", `Quick, test_cp_expr globals prelude
       "enumeration T { E1, E2 };" "if TRUE then E1 else E2" "E1");

    ("let", `Quick, test_cp_stmts ""
       "let x : integer = 1 + 1;" "let x : integer = 2;");
    ("assignment", `Quick, test_cp_stmts ""
      "let c : bits(2) = '11';
       let a : bits(2) = c;"
      "let c : bits(2) = '11'; let a : bits(2) = '11';");
    ("bittuple declaration", `Quick, test_cp_stmts "let N : integer = 2;"
      "let [a : bits(N), b : bits(1)] = '111';"
      "let [a : bits(2), b : bits(1)] = '111';");
    ("if-else", `Quick, test_cp_stmts "var d : boolean;"
      "var c : bits(2);
       if d then
           c = '11';
       else
           c = '11';
       end
       let a = c;"
      "var c : bits(2);
       if d then
           c = '11';
       else
           c = '11';
       end
       let a : bits(2) = '11';");
    ("if-else dead statement elimination 1", `Quick, test_cp_stmts "let b : boolean = TRUE;"
      "if b then
           let c = 1;
       else
           let c = 0;
       end"
      "let c = 1;");
    ("if-else dead statement elimination 2", `Quick, test_cp_stmts "let b : boolean = FALSE;"
      "if b then
           let c = 1;
       else
           let c = 0;
       end"
      "let c = 0;");
    ("if-else dead expression elimination 1", `Quick, test_cp_stmts "let b : boolean = TRUE; var x : integer; var y : integer;"
      "let c = if b then x else y;"
      "let c = x;");
    ("if-else dead expression elimination 2", `Quick, test_cp_stmts "let b : boolean = FALSE; var x : integer; var y : integer;"
      "let c = if b then x else y;"
      "let c = y;");
    (* Make sure c = '11' gets propagated to after loop *)
    ("for loop 1", `Quick, test_cp_stmts "var d : integer;"
      "let c = '11';
       var x : integer;
       for i = 0 to d do
           x = 0;
       end
       let a = c;"
      "let c : bits(2) = '11';
       var x : integer;
       for i = 0 to d do
           x = 0;
       end
       let a : bits(2) = '11';");

    (* Make sure we still except c to be '10' after loop *)
    ("for loop 2", `Quick, test_cp_stmts "var d : integer;"
      "var c : bits(2) = '10';
       for i = 0 to d do
           c = '10';
       end
       let a = c;"
      "var c : bits(2) = '10';
       for i = 0 to d do
           c = '10';
       end
       let a : bits(2) = '10';");

    (* Make sure a = c is intact when altering c inside loop. *)
    ("for loop 3", `Quick, test_cp_stmts "var d : integer;"
      "var c : bits(2) = '10';
       for i = 0 to d do
           c = '11';
       end
       let a = c;"
      "var c : bits(2) = '10';
       for i = 0 to d do
           c = '11';
       end
       let a : bits(2) = c;");

    (* This will trigger more than 2 fixpoint iterations *)
    ("for loop 4", `Quick, test_cp_stmts "var n : integer;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       for i = 0 to n do
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       end
       let a = x1;
       let b = x2;
       let c = x3;
       let d = x4;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       for i = 0 to n do
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       end
       let a = x1;
       let b = x2;
       let c = x3;
       let d : integer = 1;");

    (* Make sure c = '11' gets propagated to after loop *)
    ("while loop 1", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "let c = '11';
       var x : integer;
       while i != d do
           x = 0;
       end
       let a = c;"
      "let c : bits(2) = '11';
       var x : integer;
       while i != d do
           x = 0;
       end
       let a : bits(2) = '11';");

    (* Make sure we still except c to be '10' after loop *)
    ("while loop 2", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "var c : bits(2) = '10';
       while i != d do
           c = '10';
       end
       let a = c;"
      "var c : bits(2) = '10';
       while i != d do
           c = '10';
       end
       let a : bits(2) = '10';");

    (* Make sure a = c is intact when altering c inside loop. *)
    ("while loop 3", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "var c : bits(2) = '10';
       while i != d do
           c = '11';
       end
       let a = c;"
      "var c : bits(2) = '10';
       while i != d do
           c = '11';
       end
       let a : bits(2) = c;");

    (* This will trigger more than 2 fixpoint iterations *)
    ("while loop 4", `Quick, test_cp_stmts "var i : integer; var n : integer;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       while i != n do
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       end
       let a = x1;
       let b = x2;
       let c = x3;
       let d = x4;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       while i != n do
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       end
       let a = x1;
       let b = x2;
       let c = x3;
       let d : integer = 1;");

    (* Make sure c = '11' gets propagated to after loop *)
    ("repeat loop 1", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "let c = '11';
       var x : integer;
       repeat
           x = 0;
       until i != d;
       let a = c;"
      "let c : bits(2) = '11';
       var x : integer;
       repeat
           x = 0;
       until i != d;
       let a : bits(2) = '11';");

    (* Make sure we still except c to be '10' after loop *)
    ("repeat loop 2", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "var c : bits(2) = '10';
       repeat
           c = '10';
       until i != d;
       let a = c;"
      "var c : bits(2) = '10';
       repeat
           c = '10';
       until i != d;
       let a : bits(2) = '10';");

    (* Make sure a = c is intact when altering c inside loop. *)
    ("repeat loop 3", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "var c : bits(2) = '10';
       repeat
           c = '11';
       until i != d;
       let a = c;"
      "var c : bits(2) = '10';
       repeat
           c = '11';
       until i != d;
       let a : bits(2) = c;");

    (* This will trigger more than 2 fixpoint iterations *)
    ("repeat loop 4", `Quick, test_cp_stmts "var i : integer; var n : integer;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       repeat
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       until i != n;
       let a = x1;
       let b = x2;
       let c = x3;
       let d = x4;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       repeat
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       until i != n;
       let a = x1;
       let b = x2;
       let c = x3;
       let d : integer = 1;");
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
