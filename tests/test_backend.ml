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
  let tcenv = TC.GlobalEnv.clone tcenv in
  let ds = try_read_declarations tcenv s in
  Alcotest.(check pass) name () (decls ds)

let test_builtin_fun (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) () : unit =
  check_declaration tcenv decls "ignored" "__builtin func f() => integer;";
  ()

let test_enum (tcenv : TC.GlobalEnv.t) (decls : AST.declaration list -> unit) ()
    : unit =
  check_declaration tcenv decls "boolean (ignored)"
    "enumeration boolean { FALSE, TRUE };";
  check_declaration tcenv decls "few enum literals"
    "enumeration signal { LOW, HIGH };";
  ()

let test_fun_decl (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) () : unit =
  check_declaration tcenv decls "no params, empty body" "func f() => integer;";
  ()

let test_fun_defn (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) () : unit =
  check_declaration tcenv decls "no params, empty body"
    "func f() => integer end";
  check_declaration tcenv decls "few params, empty body"
    "func f(p1 :: integer, p2 :: integer) => integer end";
  check_declaration tcenv decls "statement (return)"
    "func f() => integer return 0; end";
  ()

let test_proc_decl (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) () : unit =
  check_declaration tcenv decls "no params, empty body" "func f();";
  ()

let test_proc_defn (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) () : unit =
  check_declaration tcenv decls "no params, empty body"
    "func f() end";
  check_declaration tcenv decls "few params, empty body"
    "func f(p1 :: integer, p2 :: integer) end";
  check_declaration tcenv decls "uninitialized variables"
    "func f() var i1, i2 :: integer; end";
  check_declaration tcenv decls "variable, literal (integer)"
    "func f() var i :: integer = 0; end";
  check_declaration tcenv decls "let, literal (integer)"
    "func f() let i :: integer = 0; end";
  check_declaration tcenv decls "let, literal (hexadecimal)"
    "func f() let i :: integer = 0x1; end";
  check_declaration tcenv decls "let, literal (bits)"
    "func f() let i :: bits(8) = '1111 0000'; end";
  check_declaration tcenv decls "expression (variable)"
    "func f() var i :: integer = 0; var j = i; end";
  check_declaration tcenv decls "statement (return)"
    "func f() return; end";
  check_declaration tcenv decls "statement (procedure invocation)"
    "func b(); func f() b(); end";
  check_declaration tcenv decls "statement (procedure invocation with arg)"
    "func b(i :: integer); func f() b(0); end";
  check_declaration tcenv decls "statement (block)"
    "func f() begin end end";
  ()

let test_type_decl (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) () : unit =
  check_declaration tcenv decls "type declaration" "type byte of bits(8);";
  ()

let test_var (tcenv : TC.GlobalEnv.t) (decls : AST.declaration list -> unit) ()
    : unit =
  check_declaration tcenv decls "bits" "var i :: bits(8);";
  check_declaration tcenv decls "integer" "var i :: integer;";
  ()

let test_cases (decls : AST.declaration list -> unit) :
    unit Alcotest.test_case list =
  let tcenv = TC.env0 in
  [
    ("built-in function", `Quick, test_builtin_fun tcenv decls);
    ("enumeration", `Quick, test_enum tcenv decls);
    ("function declaration", `Quick, test_fun_decl tcenv decls);
    ("function definition", `Quick, test_fun_defn tcenv decls);
    ("procedure declaration", `Quick, test_proc_decl tcenv decls);
    ("procedure definition", `Quick, test_proc_defn tcenv decls);
    ("type declaration", `Quick, test_type_decl tcenv decls);
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
