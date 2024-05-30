(****************************************************************
 * Test Verilog backend
 *
 * Copyright (C) 2023-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
open Test_utils_backend
module Test_cases = Test_cases_backend
module BE = Backend_verilog
module TC = Tcheck

let check_syntax (name : string) (code : string) : unit =
  let prog = "verilator" in
  let lints = [
    "/* verilator lint_off WIDTH */";
    "/* verilator lint_off UNPACKED */";
    "typedef bit [127:0] asl_integer;";
    "typedef bit asl_boolean;";
    ""
    ]
  in
  let header = String.concat "\n" lints in
  let args = [ "--cc" ] in
  check_compiler "System Verilog" ".sv" prog args name header code

let test_declaration (name : string) (s : string) : unit =
  let fmt = Format.str_formatter in
  let tcenv = TC.env0 in
  check_declaration tcenv (BE.declarations fmt) check_syntax name s

let make_cases (cases : Test_cases.test_case list) :
    unit Alcotest.test_case list =
  List.filter_map
    (fun (name, bs, s) ->
      if List.mem Test_cases.Backend_Verilog bs then
        Some (name, `Quick, fun _ -> test_declaration name s)
      else None)
    cases

let () =
  ignore (Test_utils.load_test_libraries ());
  BE.int_width := 128;
  Alcotest.run "backend_verilog"
    [
      ("expression",        make_cases Test_cases.expr);
      ("function_decl",     make_cases Test_cases.fun_decl);
      ("procedure_decl",    make_cases Test_cases.proc_decl);
      ("statement",         make_cases Test_cases.stmt);
      ("type_decl",         make_cases Test_cases.type_decl);
      ("variable_decl",     make_cases Test_cases.var_decl);
    ]

(****************************************************************
 * End
 ****************************************************************)
