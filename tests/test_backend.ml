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
    let lexbuf = Lexing.from_string s in
    let t = Asl_parser.declarations_start Lexer.token lexbuf in
    TC.tc_declarations tcenv false t

let check_declaration (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit)
    (check_ext : string -> string -> unit) (name : string) (s : string) : unit =
  let tcenv = TC.GlobalEnv.clone tcenv in
  let ds = try_read_declarations tcenv s in
  Alcotest.(check pass) name () (decls ds);

  let s = Format.flush_str_formatter () in
  check_ext name s

let check_none (name : string) (s : string) : unit = ()

let check_c_syntax (name : string) (code : string) : unit =
  let prog = "gcc" in
  let args = [| prog; "-std=c99"; "-fsyntax-only"; "-xc"; "-" |] in
  let out = Unix.open_process_args_out prog args in
  let c_header =
    String.concat "\n" [ "#include <stdbool.h>"; "#include <stdint.h>"; "\n" ]
  in

  output_string stdout c_header;
  output_string stdout code;
  flush stdout;

  Out_channel.output_string out c_header;
  Out_channel.output_string out code;

  let status = Unix.close_process_out out in
  let exit_status =
    match status with
    | Unix.WEXITED s ->
        if s <> 0 then Printf.printf "%s exited with %d\n" prog s;
        s
    | Unix.WSIGNALED _ ->
        Printf.printf "%s killed\n" prog;
        1
    | Unix.WSTOPPED _ ->
        Printf.printf "%s stopped\n" prog;
        1
  in
  Alcotest.(check int) ("C syntax: " ^ name) 0 exit_status

let test_builtin_fun (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) (ext : string -> string -> unit) () :
    unit =
  check_declaration tcenv decls ext "ignored" "__builtin func f() => integer;";
  ()

let test_enum (tcenv : TC.GlobalEnv.t) (decls : AST.declaration list -> unit)
    (ext : string -> string -> unit) () : unit =
  check_declaration tcenv decls ext "boolean (ignored)"
    "enumeration boolean { FALSE, TRUE };";
  check_declaration tcenv decls ext "few enum literals"
    "enumeration signal { LOW, HIGH };";
  ()

let test_fun_decl (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) (ext : string -> string -> unit) () :
    unit =
  check_declaration tcenv decls ext "no params, empty body"
    "func F() => integer;";
  ()

let test_fun_defn (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) (ext : string -> string -> unit) () :
    unit =
  check_declaration tcenv decls ext "no params, empty body"
    "func F() => integer end";
  check_declaration tcenv decls ext "few params, empty body"
    "func F(p1 :: integer, p2 :: integer) => integer end";
  check_declaration tcenv decls ext "statement (return)"
    "func F() => integer return 0; end";
  ()

let test_proc_decl (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) (ext : string -> string -> unit) () :
    unit =
  check_declaration tcenv decls ext "no params, empty body" "func F();";
  ()

let test_proc_defn (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) (ext : string -> string -> unit) () :
    unit =
  check_declaration tcenv decls ext "no params, empty body"
    "func F() end";
  check_declaration tcenv decls ext "few params, empty body"
    "func F(p1 :: integer, p2 :: integer) end";
  check_declaration tcenv decls ext "uninitialized variables"
    "func F() var i1, i2 :: integer; end";
  check_declaration tcenv decls ext "variable, literal (integer)"
    "func F() var i :: integer = 0; end";
  check_declaration tcenv decls ext "let, literal (integer)"
    "func F() let i :: integer = 0; end";
  check_declaration tcenv decls ext "let, literal (hexadecimal)"
    "func F() let i :: integer = 0x1; end";
  check_declaration tcenv decls ext "let, literal (bits)"
    "func F() let i :: bits(8) = '1111 0000'; end";
  check_declaration tcenv decls ext "let, literal (string)"
    "func F() let i :: string = \"str\"; end";
  check_declaration tcenv decls ext "expression (variable)"
    "func F() var i :: integer = 0; var j = i; end";
  check_declaration tcenv decls ext "expression (boolean)"
    "func F() var i = FALSE; end";
  check_declaration tcenv decls ext "expression (if)"
    "func F() var i = if FALSE then 0 else 0; end";
  check_declaration tcenv decls ext "expression (if elsif)"
    "func F() var i = if FALSE then 0 elsif FALSE then 0 else 0; end";
  check_declaration tcenv decls ext "expression (parentheses)"
    "func F() var i :: integer = ( 0 ); end";
  check_declaration tcenv decls ext "expression (function invocation)"
    "func B() => integer; func F() var i = B(); end";
  check_declaration tcenv decls ext "expression (builtin function invocation)"
    "func F() var i = 1 + 1; end";
  check_declaration tcenv decls ext "expression (slice, lowd)"
    "func F() var i :: bits(16); var j :: bits(8); j = i[4 +: 8]; end";
  check_declaration tcenv decls ext "expression (slice, hilo)"
    "func F() var i :: bits(16); var j :: bits(8); j = i[11:4]; end";
  check_declaration tcenv decls ext "expression (slice, single)"
    "func F() var i :: bits(16); var j :: bits(1); j = i[4]; end";
  check_declaration tcenv decls ext "expression (record initializer)"
    "record R { i :: integer; }; func F() var r = R { i = 1 }; end";
  check_declaration tcenv decls ext "expression (field selection)"
    "record R { i :: integer; }; func F() var r :: R; var j = r.i; end";
  check_declaration tcenv decls ext "expression (bitvector concatenation)"
    "func F() var i :: bits(8); var j :: bits(4); var k :: bits(2); var r :: bits(14); r = [k, j, i]; end";
  check_declaration tcenv decls ext "statement (return)"
    "func F() return; end";
  check_declaration tcenv decls ext "statement (procedure invocation)"
    "func B(); func F() B(); end";
  check_declaration tcenv decls ext "statement (procedure invocation with arg)"
    "func B(i :: integer); func F() B(0); end";
  check_declaration tcenv decls ext "statement (block)"
    "func F() begin end end";
  check_declaration tcenv decls ext "statement (assignment)"
    "func F() var i, j :: integer; i = j; end";
  check_declaration tcenv decls ext "statement (assignment to slice)"
    "func F() var i :: bits(8); i[4 +: 2] = '10'; end";
  check_declaration tcenv decls ext "statement (assert)"
    "func F() assert FALSE; end";
  check_declaration tcenv decls ext "statement (if)"
    "func F() if FALSE then return; elsif FALSE then return; else return; end end";
  check_declaration tcenv decls ext "statement (if few elsifs)"
    "func F() if FALSE then return; elsif FALSE then return; elsif FALSE then return; end end";
  check_declaration tcenv decls ext "statement (case)"
    "func F() case 0 of when 0: return; otherwise: return; end end";
  check_declaration tcenv decls ext "statement (case few whens)"
    "func F() case 0 of when 0x0: return; when 0x1: return; end end";
  ()

let test_record_decl (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) (ext : string -> string -> unit) () :
    unit =
  check_declaration tcenv decls ext "record declaration"
    "record R { i :: integer; b :: bit; };";
  ()

let test_type_decl (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) (ext : string -> string -> unit) () :
    unit =
  check_declaration tcenv decls ext "type declaration" "type byte of bits(8);";
  check_declaration tcenv decls ext "type declaration (register)"
    "type Reg of bits(9) { [8] a [1] b };";
  ()

let test_var (tcenv : TC.GlobalEnv.t) (decls : AST.declaration list -> unit)
    (ext : string -> string -> unit) () : unit =
  check_declaration tcenv decls ext "bits" "var i :: bits(8);";
  check_declaration tcenv decls ext "integer" "var i :: integer;";
  check_declaration tcenv decls ext "array" "var i :: array [1] of integer;";
  ()

let test_cases (decls : AST.declaration list -> unit)
    (ext : string -> string -> unit) : unit Alcotest.test_case list =
  let tcenv = TC.env0 in
  [
    ("built-in function", `Quick, test_builtin_fun tcenv decls ext);
    ("enumeration", `Quick, test_enum tcenv decls ext);
    ("function declaration", `Quick, test_fun_decl tcenv decls ext);
    ("function definition", `Quick, test_fun_defn tcenv decls ext);
    ("procedure declaration", `Quick, test_proc_decl tcenv decls ext);
    ("procedure definition", `Quick, test_proc_defn tcenv decls ext);
    ("record declaration", `Quick, test_record_decl tcenv decls ext);
    ("type declaration", `Quick, test_type_decl tcenv decls ext);
    ("variable", `Quick, test_var tcenv decls ext);
  ]

let test_proc_defn_c_only (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit) (ext : string -> string -> unit) () :
    unit =
  check_declaration tcenv decls ext "statement (for, direction to)"
    "func F() for i = 0 to 1 do return; end end";
  check_declaration tcenv decls ext "statement (for, direction downto)"
    "func F() for i = 1 downto 0 do return; end end";
  check_declaration tcenv decls ext "statement (wildcard declaration)"
    "func F() var - = 0; end";
  check_declaration tcenv decls ext "statement (assignment to wildcard)"
    "func F() - = 0; end";
  check_declaration tcenv decls ext "statement (repeat loop)"
    "func F() repeat return; until TRUE; end";
  check_declaration tcenv decls ext "statement (while loop)"
    "func F() while TRUE do return; end end";
  ()

let test_cases_c_only (decls : AST.declaration list -> unit)
    (ext : string -> string -> unit) : unit Alcotest.test_case list =
  let tcenv = TC.env0 in
  [ ("procedure definition", `Quick, test_proc_defn_c_only tcenv decls ext) ]

let () =
  let paths = [ "../../.." ] in
  ignore (LoadASL.read_file paths "prelude.asl" true false);

  let fmt = Format.str_formatter in
  Alcotest.run "backend"
    [
      ("backend c", test_cases (Backend_c.declarations fmt) check_c_syntax);
      ("backend c only", test_cases_c_only (Backend_c.declarations fmt) check_c_syntax);
      ("backend verilog", test_cases (Backend_verilog.declarations fmt) check_none);
    ]

(****************************************************************
 * End
 ****************************************************************)
