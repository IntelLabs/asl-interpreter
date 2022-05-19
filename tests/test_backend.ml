(****************************************************************
 * Test ASL backends
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
module AST = Asl_ast

let test_builtin_fun decls () : unit =
  Alcotest.(check pass)
    "ignored (no exception thrown)" ()
    (decls Format.std_formatter
       [
         AST.Decl_BuiltinFunction
           ( AST.Ident "id",
             [],
             [],
             AST.Type_Constructor (AST.Ident "id"),
             AST.Unknown );
       ])

let test_fun_defn decls () : unit =
  Alcotest.(check pass)
    "no params, empty body" ()
    (decls Format.std_formatter
       [
         AST.Decl_FunDefn
           ( AST.Ident "name",
             [],
             [],
             AST.Type_Integer None,
             [],
             AST.Unknown );
       ])

let test_proc_defn decls () : unit =
  Alcotest.(check pass)
    "no params, empty body" ()
    (decls Format.std_formatter
       [
         AST.Decl_ProcDefn
           ( AST.Ident "name",
             [],
             [],
             [],
             AST.Unknown );
       ])

let test_cases decls : unit Alcotest.test_case list =
  [
    ("built-in function", `Quick, test_builtin_fun decls);
    ("function definition", `Quick, test_fun_defn decls);
    ("procedure definition", `Quick, test_proc_defn decls);
  ]

let () =
  Alcotest.run "backend"
    [
      ("backend c", test_cases Backend_c.declarations);
      ("backend verilog", test_cases Backend_verilog.declarations);
    ]

(****************************************************************
 * End
 ****************************************************************)
