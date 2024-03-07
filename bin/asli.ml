(****************************************************************
 * ASL interactive frontend
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL interactive frontend *)

open LibASL
open Asl_ast
module Parser = Asl_parser
module TC = Tcheck
module AST = Asl_ast
module FMT = Asl_fmt

open Yojson
open Asl_utils

let opt_filenames : string list ref = ref []
let opt_print_version = ref false
let opt_print_spec = ref false
let opt_print_cflags = ref false
let opt_print_ldflags = ref false
let opt_print_includedir = ref false
let opt_print_runtimedir = ref false
let opt_print_stdlibdir = ref false
let opt_verbose = ref false
let opt_batchmode = ref false
let opt_show_banner = ref true

(* on error, optionally exit if in batchmode *)
let error () : unit =
  if !opt_batchmode then exit 1

let projects : string list ref = ref []

let add_project (prj : string): unit =
  projects := List.append !projects [prj]

let execs : string list ref = ref []

let add_exec (cmd : string): unit =
  execs := List.append !execs [cmd]

(****************************************************************
 * Interactive command support
 ****************************************************************)

let help_msg =
  [
    {|:? :help                            Show this help message|};
    {|:project <file>                     Execute ASLi commands in <file>|};
    {|:q :quit                            Exit the interpreter|};
    {|:set impdef <string> = <expr>       Define implementation defined behavior|};
    {|:set +<flag>                        Set flag|};
    {|:set -<flag>                        Clear flag|};
    {|<expr>                              Execute ASL expression|};
    {|<stmt> ;                            Execute ASL statement|};
  ]

(****************************************************************
 * Read Eval Print Loop
 ****************************************************************)

let mkLoc (fname : string) (input : string) : AST.l =
  let len = String.length input in
  let start : Lexing.position =
    { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  let finish : Lexing.position =
    { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = len }
  in
  AST.Range (start, finish)

let rec process_command (tcenv : TC.Env.t) (cpu : Cpu.cpu) (fname : string) (input0 : string) : unit =
  let input = String.trim input0 in
  match String.split_on_char ' ' input with
  | [ "" ] -> ()
  | ("//"::_) -> () (* comment *)
  | [ ":help" ] | [ ":?" ] ->
      List.iter print_endline help_msg;
      Commands.CommandMap.iter
        (fun nm (fn, args, desc) -> Printf.printf ":%-35s%s\n" (nm^" "^args) desc)
        !Commands.commands;
      print_endline "\nFlags:";
      Flags.FlagMap.iter
        (fun nm (v, desc) -> Printf.printf "  %s%-27s %s\n" (if !v then "+" else "-") nm desc)
        !Flags.flags
  | ":set" :: "impdef" :: rest ->
      let cmd = String.concat " " rest in
      let loc = mkLoc fname cmd in
      let x, e = LoadASL.read_impdef tcenv loc cmd in
      let v = Eval.eval_expr loc cpu.env e in
      cpu.setImpdef x v
 | [ ":set"; flag ] when String.starts_with flag ~prefix:"+" -> (
     match Flags.FlagMap.find_opt (Utils.string_drop 1 flag) !Flags.flags with
     | None -> Printf.printf "Unknown flag %s\n" flag
     | Some (f, _) -> f := true)
 | [ ":set"; flag ] when String.starts_with flag ~prefix:"-" -> (
     match Flags.FlagMap.find_opt (Utils.string_drop 1 flag) !Flags.flags with
     | None -> Printf.printf "Unknown flag %s\n" flag
     | Some (f, _) -> f := false)
  | [ ":project"; prj ] -> (
      let inchan = open_in prj in
      try
        while true do
          process_command tcenv cpu prj (input_line inchan)
        done
      with End_of_file -> close_in inchan)
  | [ ":q" ] | [ ":quit" ] -> exit 0
  | (cmd :: args) when String.starts_with ~prefix:":" cmd ->
     ( match Commands.CommandMap.find_opt (Utils.string_drop 1 cmd) !Commands.commands with
     | None -> Printf.printf "Unknown command %s\n" cmd; error ()
     | Some (cmd, _, _) -> if not (cmd tcenv cpu args) then error()
     )
  | _ ->
      if ';' = String.get input (String.length input - 1) then
        let s = LoadASL.read_stmt tcenv input in
        Eval.eval_stmt cpu.env s
      else
        let loc = mkLoc fname input in
        let e = LoadASL.read_expr tcenv loc input in
        let v = Eval.eval_expr loc cpu.env e in
        print_endline (Value.string_of_value v)

and load_project (tcenv : TC.Env.t) (cpu : Cpu.cpu) (prj : string) : unit =
  let inchan = open_in prj in
  try
    while true do
      process_command tcenv cpu prj (input_line inchan)
    done
  with End_of_file -> close_in inchan

let rec repl (tcenv : TC.Env.t) (cpu : Cpu.cpu) : unit =
  flush stdout;
  match LNoise.linenoise "ASLi> " with
  | None -> ()
  | Some input ->
      LNoise.history_add input |> ignore;
      (try
        process_command tcenv cpu "<stdin>" input
      with e ->
        Error.print_exception e;
        error ();
      );
      repl tcenv cpu

(****************************************************************
 * Command: :bin
 ****************************************************************)

let cmd_bin (tcenv : TC.Env.t) (cpu : Cpu.cpu) (args : string list) : bool =
  ( match args with
  | [ file ; address ]  ->
    let ram_address = Int64.of_string address in
    Printf.printf "Loading BIN file %s to 0x%Lx.\n" file ram_address;
    Bin_file.load_file file cpu.elfwrite8 ram_address;
    true
  | _ ->
    false
  )

let _ = Commands.registerCommand "bin" "<file> <address>" "Load a BIN <file> to <address>" cmd_bin

(****************************************************************
 * Command: :callgraph
 ****************************************************************)

let generate_callgraph (filename : string) (ds : AST.declaration list): unit =
  let cg = ref Bindings.empty in
  List.iter (fun d ->
      ( match decl_name d with
      | None -> ()
      | Some nm ->
          let callees = calls_of_decl d in
          let old = Bindings.find_opt nm !cg |> Option.value ~default:IdentSet.empty in
          cg := Bindings.add nm (IdentSet.union callees old) !cg
      ))
      ds;
  let t = Bindings.bindings !cg
        |> List.map (fun (caller, callees) ->
              let callee_names = IdentSet.elements callees |> List.map Ident.pprint in
              (Ident.pprint caller, `List (List.map (fun s -> `String(s)) callee_names)))
        |> (fun xs -> `Assoc xs)
  in
  let chan = open_out filename in
  Yojson.pretty_to_channel chan t

let cmd_callgraph (tcenv : TC.Env.t) (cpu : Cpu.cpu) (args : string list) : bool =
  ( match args with
  | [ file ] ->
    Printf.printf "Generating callgraph metadata file %s.\n" file;
    generate_callgraph file !Commands.declarations;
    true
  | _ ->
    false
  )

let _ = Commands.registerCommand "callgraph" "<json file>" "Generate json file containing callgraph" cmd_callgraph

(****************************************************************
 * Command: :chekhov
 ****************************************************************)

let cmd_chekhov (tcenv : TC.Env.t) (cpu : Cpu.cpu) (args : string list) : bool =
  ( match args with
  | ( trace_file :: regmap_file :: rest) ->
    let trace_chan = open_out trace_file in
    let o_ami = match rest with [ami] -> Some ami; | _ -> None in
    Value.tracer := Chekhov.chekhovTextTracer [] regmap_file o_ami trace_chan;
    true
  | _ ->
    false
  )

let _ = Commands.registerCommand "chekhov" "<trc> <cfg> [<ami>]" "Enable chekhov tracing" cmd_chekhov

(****************************************************************
 * Command: :elf
 ****************************************************************)

let cmd_elf (tcenv : TC.Env.t) (cpu : Cpu.cpu) (args : string list) : bool =
  ( match args with
  | [ file ] ->
    Printf.printf "Loading ELF file %s.\n" file;
    let entry = Elf.load_file file cpu.elfwrite8 in
    Printf.printf "Entry point = 0x%Lx\n" entry;
    cpu.setPC (Z.of_int64 entry);
    true
  | _ ->
    false
  )

let _ = Commands.registerCommand "elf" "<file>" "Load an ELF file" cmd_elf

(****************************************************************
 * Command: :obj
 ****************************************************************)

let cmd_obj (tcenv : TC.Env.t) (cpu : Cpu.cpu) (args : string list) : bool =
  ( match args with
  | [ file ] ->
    Printf.printf "Loading OBJ32 file %s.\n" file;
    Obj32.load_file file cpu.elfwrite32;
    true
  | _ ->
    false
  )

let _ = Commands.registerCommand "obj" "<file>" "Load an OBJ32 file" cmd_obj

(****************************************************************
 * Command: :run
 ****************************************************************)

let cmd_run (tcenv : TC.Env.t) (cpu : Cpu.cpu) (args : string list) : bool =
  ( try
      while true do
        cpu.step ()
      done
  with
  | e -> Error.print_exception e; error ()
  );
  true

let _ = Commands.registerCommand "run" "" "Execute instructions" cmd_run

(****************************************************************
 * Command: :show
 ****************************************************************)

let cmd_show (tcenv : TC.Env.t) (cpu : Cpu.cpu) (args : string list) : bool =
  (* 'glob' matching against a pattern *)
  let pattern_match (x : string) (pattern : string) : bool =
    let n = String.length pattern in
    if String.get pattern (n-1) = '*' then
      String.starts_with ~prefix:(String.sub pattern 0 (n-1)) x
    else
      String.equal pattern x
  in
  (* declarations whose name matches pattern *)
  let decl_match (d : AST.declaration) (pattern : string) : bool =
    ( match Asl_utils.decl_name d with
    | Some nm -> pattern_match (Ident.name nm) pattern
    | None -> false
    )
  in
  let (ds, output) = match args with
    | ("--output" :: filename :: ds) -> (ds, Some filename)
    | ds -> (ds, None)
  in
  let ds = match ds with
    | [] -> !Commands.declarations
    | patterns -> List.filter (fun d -> List.exists (decl_match d) patterns) !Commands.declarations
  in
  ( match output with
  | Some filename ->
      Utils.to_file filename (fun fmt ->
        List.iter (Format.fprintf fmt "%a@,@." FMT.declaration) ds;
      )
  | None ->
      if Utils.is_empty ds then
        Format.printf "No function selected: try ':show A*'@."
      else
        List.iter (Format.printf "%a@,@." FMT.declaration) ds;
  );
  true

let _ = Commands.registerCommand "show" "[--output <file>] <patterns>*" "Show matching definitions" cmd_show

(****************************************************************
 * Command: :step
 ****************************************************************)

let cmd_step (tcenv : TC.Env.t) (cpu : Cpu.cpu) (args : string list) : bool =
  let n = match args with
    | [n] -> int_of_string n
    | _ -> 1
  in
  ( try
      for i = 1 to n do
        cpu.step ();
      done
  with
  | e -> Error.print_exception e; error ()
  );
  true

let _ = Commands.registerCommand "step" "[<count>]" "Execute <count> instructions" cmd_step

(****************************************************************
 * Main program and command line options
 ****************************************************************)

let options =
  Arg.align
    [
      ("--print_spec", Arg.Set opt_print_spec, "       Print ASL spec");
      ("-v", Arg.Set opt_verbose, "       Verbose output");
      ("--version", Arg.Set opt_print_version, "       Print version");
      ("--print-c-flags",     Arg.Set opt_print_cflags,     "       Print the C flags needed to use the ASL C runtime");
      ("--print-ld-flags",    Arg.Set opt_print_ldflags,    "       Print the flags needed to link against the ASL C runtime");
      ("--print-lib-dir",     Arg.Set opt_print_stdlibdir,  "       Print the installation directory for ASL standard library");
      ("--print-runtime-dir", Arg.Set opt_print_runtimedir, "       Print the installation directory for ASL C runtime");
      ("--print-include-dir", Arg.Set opt_print_includedir, "       Print the installation directory for ASL C runtime include headers");
      ("--nobanner", Arg.Clear opt_show_banner, "       Suppress banner");
      ("--batchmode", Arg.Set opt_batchmode,  "       Fail on error");
      ("--exec",    Arg.String add_exec,        "       Execute command");
      ("--project", Arg.String add_project,     "       Execute project file");
    ]

let version = "ASLi 0.2.0 alpha"

let banner =
  [
    {|            _____  _       _                                       |};
    {|    /\     / ____|| |     (_)   ASL interpreter                    |};
    {|   /  \   | (___  | |      _    Copyright Arm Limited (c) 2017-2019|};
    {|  / /\ \   \___ \ | |     | |   Copyright (C) 2022-2024 Intel Corporation|};
    {| / ____ \  ____) || |____ | |                                      |};
    {|/_/    \_\|_____/ |______||_|   |} ^ version;
  ]

let usage_msg = version ^ "\nusage: asli <options> <file1> ... <fileN>\n"

let _ =
  Arg.parse options (fun s -> opt_filenames := !opt_filenames @ [ s ]) usage_msg

let main () =
  Ocolor_format.prettify_formatter Format.std_formatter;
  Ocolor_config.set_color_capability Ocolor_config.Color4;
  let paths = Option.value (Sys.getenv_opt "ASL_PATH") ~default:"."
    |> String.split_on_char ':' in
  if !opt_print_version then Printf.printf "%s\n" version
  else if !opt_print_cflags then begin
    List.iter (Printf.printf "-I%s ") Sites.Sites.runtime_include;
    print_newline ()
  end else if !opt_print_ldflags then begin
    List.iter (Printf.printf "-L%s ") Sites.Sites.runtime;
    Printf.printf "-lASL\n"
  end else if !opt_print_includedir then print_endline (String.concat ":" Sites.Sites.runtime_include)
  else if !opt_print_runtimedir then print_endline (String.concat ":" Sites.Sites.runtime)
  else if !opt_print_stdlibdir then print_endline (String.concat ":" Sites.Sites.stdlib)
  else begin
    if !opt_show_banner then begin
      List.iter print_endline banner;
      print_endline "\nType :? for help"
    end;
    try (
      let stdlibdirs : string list = Sites.Sites.stdlib @ paths in
      let t = LoadASL.read_file stdlibdirs "prelude.asl" true !opt_verbose in
      let ts = LoadASL.read_files paths !opt_filenames !opt_verbose in
      let ds = t @ ts in
      if !opt_verbose then Printf.printf "Performing global checks\n%!";
      let ds = Global_checks.check_decls ds in
      if !opt_verbose then Printf.printf "Performed global checks\n%!";
      if !opt_print_spec then (
        FMT.comment_list := Lexer.get_comments ();
        FMT.declarations Format.std_formatter ds;
        Format.pp_print_flush Format.std_formatter ());

      if !opt_verbose then Printf.printf "Building evaluation environment\n";
      let env = Eval.build_evaluation_environment ds in
      if !opt_verbose then Printf.printf "Built evaluation environment\n";

      Commands.declarations := ds;
      let tcenv = TC.Env.mkEnv TC.env0 in
      let cpu = Cpu.mkCPU env in

      List.iter (load_project tcenv cpu) !projects;
      List.iter (process_command tcenv cpu "<argv>") !execs;

      if not !opt_batchmode then begin
        LNoise.history_load ~filename:"asl_history" |> ignore;
        LNoise.history_set ~max_length:100 |> ignore;
        repl tcenv cpu
      end
    ) with e -> begin
      Error.print_exception e;
      exit 1
    end
  end

let _ = ignore (main ())

(****************************************************************
 * End
 ****************************************************************)
