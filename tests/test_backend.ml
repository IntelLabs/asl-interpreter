(****************************************************************
 * Test ASL backends
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
module AST = Asl_ast
module TC = Tcheck

let try_read_declarations (tcenv : TC.GlobalEnv.t) (s : string) :
    AST.declaration list =
  LoadASL.report_type_error
    (fun _ -> Alcotest.fail "type error")
    (fun _ ->
      LoadASL.report_parse_error
        (fun _ -> Alcotest.fail "parse error")
        (fun _ ->
          let lexbuf = Lexing.from_string s in
          let t = Asl_parser.declarations_start Lexer.token lexbuf in
          TC.tc_declarations tcenv false t))

let check_declaration (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) (name : string) (s : string) : unit =
  let ds = try_read_declarations tcenv s in
  Alcotest.(check pass) name () (decls ds)

let check_decl (decls : AST.declaration list -> unit) (name : string)
    (d : AST.declaration) : unit =
  Alcotest.(check pass) name () (decls [ d ])

let test_builtin_fun (decls : AST.declaration list -> unit) () : unit =
  check_decl decls "ignored"
    (AST.Decl_BuiltinFunction
       ( AST.Ident "id",
         [],
         [],
         AST.Type_Constructor (AST.Ident "id"),
         AST.Unknown ))

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

let test_var (tcenv : TC.GlobalEnv.t) (decls : AST.declaration list -> unit) ()
    : unit =
  check_declaration tcenv decls "bits" "var i :: bits(8);";
  check_declaration tcenv decls "integer" "var i :: integer;";
  ()

let test_cases (decls : AST.declaration list -> unit) :
    unit Alcotest.test_case list =
  let tcenv = TC.env0 in
  [
    ("built-in function", `Quick, test_builtin_fun decls);
    ("function definition", `Quick, test_fun_defn decls);
    ("procedure definition", `Quick, test_proc_defn decls);
    ("variable", `Quick, test_var tcenv decls);
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
