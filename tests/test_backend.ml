(****************************************************************
 * Test ASL backends
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
module AST = Asl_ast

let check_decl (decls : AST.declaration list -> unit) (name : string)
    (d : AST.declaration) : unit =
  Alcotest.(check pass) name () (decls [ d ])

let test_builtin_fun (decls : AST.declaration list -> unit) () : unit =
  check_decl decls "ignored"
    (AST.Decl_BuiltinFunction
       (AST.Ident "id", [], [], AST.Type_Constructor (AST.Ident "id"), AST.Unknown))

let test_fun_defn (decls : AST.declaration list -> unit) () : unit =
  let fun_defn (params : (AST.ident * AST.ty) list) (body : AST.stmt list) =
    AST.Decl_FunDefn
      (AST.Ident "name", [], params, AST.Type_Integer None, body, AST.Unknown)
  in

  check_decl decls "no params, empty body" (fun_defn [] []);

  let params =
    [
      (AST.Ident "p1", AST.Type_Integer None);
      (AST.Ident "p2", AST.Type_Integer None);
    ]
  in
  check_decl decls "few params, empty body" (fun_defn params [])

let test_proc_defn (decls : AST.declaration list -> unit) () : unit =
  let proc_defn (params : (AST.ident * AST.ty) list) (body : AST.stmt list) =
    AST.Decl_ProcDefn (AST.Ident "name", [], params, body, AST.Unknown)
  in

  check_decl decls "no params, empty body" (proc_defn [] []);

  let params =
    [
      (AST.Ident "p1", AST.Type_Integer None);
      (AST.Ident "p2", AST.Type_Integer None);
    ]
  in
  check_decl decls "few params, empty body" (proc_defn params [])

let test_cases (decls : AST.declaration list -> unit) :
    unit Alcotest.test_case list =
  [
    ("built-in function", `Quick, test_builtin_fun decls);
    ("function definition", `Quick, test_fun_defn decls);
    ("procedure definition", `Quick, test_proc_defn decls);
  ]

let () =
  let fmt = Format.std_formatter in
  Alcotest.run "backend"
    [
      ("backend c", test_cases (Backend_c.declarations fmt));
      ("backend verilog", test_cases (Backend_verilog.declarations fmt));
    ]

(****************************************************************
 * End
 ****************************************************************)
