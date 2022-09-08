(****************************************************************
 * ASL interactive frontend
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL interactive frontend *)

open LibASL
module AST = Asl_ast
module CP = Xform_constprop
module ASL_FMT = Asl_fmt
module FMT_Utils = Format_utils

type backend = Backend_C | Backend_Verilog

let opt_filenames : string list ref = ref []
let opt_verbose = ref false
let opt_backend = ref Backend_Verilog
let roots : string list ref = ref []
let keeps : string list ref = ref []
let output_file : string ref = ref ""
let backend_pairs = [ ("c", Backend_C); ("verilog", Backend_Verilog) ]
let backend_symbols = List.map fst backend_pairs

let match_backend (symb : string) : unit =
  opt_backend := List.assoc symb backend_pairs

let options =
  Arg.align
    [
      ("-v", Arg.Set opt_verbose, "       Verbose output");
      ( "--backend",
        Arg.Symbol (backend_symbols, match_backend),
        "       Backend type (default: verilog)" );
      ( "--root",
        Arg.String (fun s -> roots := !roots @ [ s ]),
        "       Add function to convert" );
      ( "--keep",
        Arg.String (fun s -> keeps := !keeps @ [ s ]),
        "       Add variable to keep" );
      ("-o", Arg.Set_string output_file, "       Output file");
    ]

let version = "ASL 0.2.0 alpha"
let usage_msg = version ^ "\nusage: asl2v <options> <file1> ... <fileN>\n"

let _ =
  Arg.parse options (fun s -> opt_filenames := !opt_filenames @ [ s ]) usage_msg

let main () =
  let paths = Utils.from_option (Sys.getenv_opt "ASL_PATH") (fun _ -> ".") in
  let paths = String.split_on_char ':' paths in
  let t = LoadASL.read_file paths "prelude.asl" true !opt_verbose in
  let ts =
    List.map
      (fun filename ->
        if Utils.endswith filename ".spec" then
          LoadASL.read_spec paths filename !opt_verbose
        else if Utils.endswith filename ".asl" then
          LoadASL.read_file paths filename false !opt_verbose
        else failwith ("Unrecognized file suffix on " ^ filename))
      !opt_filenames
  in
  let roots = List.map (fun f -> AST.FIdent (f, 0)) !roots in

  let ds = List.concat (t :: ts) in

  let keeps = List.map (fun r -> AST.Ident r) !keeps in

  if true then
    Utils.to_file "tmp.init0.asl" (fun fmt -> ASL_FMT.declarations fmt ds);
  let ds = Asl_utils.reachable_decls (List.append roots keeps) ds in
  if ds = [] then failwith "Couldn't find any roots";

  try
    if !output_file == "" then
      failwith "Output file not specified (use -o foo.v to specify)";

    if true then
      Utils.to_file "tmp.init.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let genv = Eval.build_constant_environment ds in
    let ds = CP.xform_decls genv ds in

    if true then
      Utils.to_file "tmp.constprop.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Xform_mono.monomorphize ds in
    if true then
      Utils.to_file "tmp.mono.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Asl_utils.reachable_decls (List.append roots keeps) ds in
    if ds = [] then failwith "Couldn't find any roots";

    let ds = Xform_mono.monomorphize ds in
    if true then
      Utils.to_file "tmp.mono2.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Xform_tuples.xform_decls ds in
    if true then
      Utils.to_file "tmp.tuples.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    Utils.to_file !output_file (fun fmt ->
        match !opt_backend with
        | Backend_C -> Backend_c.declarations fmt ds
        | Backend_Verilog ->
            Format.pp_print_string fmt "/* verilator lint_off WIDTH */";
            Format.pp_print_cut fmt ();
            Format.pp_print_string fmt "/* verilator lint_off UNPACKED */";
            Format.pp_print_cut fmt ();
            Backend_verilog.declarations fmt ds)
  with
  | Backend_c.Unimplemented (loc, what, pp)
  | Backend_verilog.Unimplemented (loc, what, pp) ->
      let fmt = Format.std_formatter in
      Format.pp_print_newline fmt ();
      FMT_Utils.vbox fmt (fun _ ->
          ASL_FMT.loc fmt loc;
          Format.fprintf fmt ": Unimplemented %s:" what;
          FMT_Utils.indented fmt (fun _ -> pp fmt);
          FMT_Utils.cut fmt)
  | Value.EvalError (loc, msg) ->
      let fmt = Format.std_formatter in
      Format.pp_print_newline fmt ();
      FMT_Utils.vbox fmt (fun _ ->
          ASL_FMT.loc fmt loc;
          Format.fprintf fmt ": Evaluation error %s:" msg;
          FMT_Utils.cut fmt)

let _ = ignore (main ())

(****************************************************************
 * End
 ****************************************************************)
