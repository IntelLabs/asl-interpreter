(****************************************************************
 * Test Verilog backend
 *
 * Copyright Intel Inc (c) 2023
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
open Test_backend
module BE = Backend_verilog
module TC = Tcheck

let check_syntax (name : string) (code : string) : unit =
  let prog = "verilator" in
  let lints = [
    "/* verilator lint_off WIDTH */";
    "/* verilator lint_off UNPACKED */";
    ]
  in
  let header = String.concat "\n" lints in
  let args = [ "--cc" ] in
  check_compiler "System Verilog" ".v" prog args name header code

let test_declaration (name : string) (s : string) : unit =
  let fmt = Format.str_formatter in
  let tcenv = TC.env0 in
  check_declaration tcenv (BE.declarations fmt) check_syntax name s

let make_tests (cases : test_case list) : unit Alcotest.test_case list =
  make_tests Backend_Verilog test_declaration cases

let () =
  ignore (Test_utils.load_test_libraries ());
  Alcotest.run "backend_verilog"
    [
      ("expression", make_tests test_cases_expr);
      ("function_decl", make_tests test_cases_fun_decl);
      ("procedure_decl", make_tests test_cases_proc_decl);
      ("statement", make_tests test_cases_stmt);
      ("type_decl", make_tests test_cases_type_decl);
      ("variable_decl", make_tests test_cases_var_decl);
    ]

(****************************************************************
 * End
 ****************************************************************)
