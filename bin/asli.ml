(****************************************************************
 * ASL interactive frontend
 *
 * Copyright Arm Limited (c) 2017-2019
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
let opt_verbose = ref false
let opt_batchmode = ref false
let opt_show_banner = ref true

(* on error, optionally exit if in batchmode *)
let error () : unit =
  if !opt_batchmode then exit 1

let projects : string list ref = ref []

let add_project (prj : string): unit =
  projects := List.append !projects [prj]

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

(****************************************************************
 * Interactive command support
 ****************************************************************)

let help_msg =
  [
    {|:? :help                       Show this help message|};
    {|:callgraph <file>              Generate json file containing callgraph|};
    {|:obj <file>                    Load an OBJ file|};
    {|:bin <file> <address>          Load a BIN <file> to <address>|};
    {|:project <file>                Execute ASLi commands in <file>|};
    {|:q :quit                       Exit the interpreter|};
    {|:run                           Execute instructions|};
    {|:step [<count>]                Execute <count> instructions|};
    {|:chekhov <trc> <cfg> [<ami>]   Enable chekhov tracing|};
    {|:set impdef <string> = <expr>  Define implementation defined behavior|};
    {|:set +<flag>                   Set flag|};
    {|:set -<flag>                   Clear flag|};
    {|<expr>                         Execute ASL expression|};
    {|<stmt> ;                       Execute ASL statement|};
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

let rec process_command (ds : AST.declaration list) (tcenv : TC.Env.t) (cpu : Cpu.cpu)
    (fname : string) (input0 : string) : unit =
  let input = String.trim input0 in
  match String.split_on_char ' ' input with
  | [ "" ] -> ()
  | ("//"::_) -> () (* comment *)
  | [ ":callgraph"; file ] ->
      Printf.printf "Generating callgraph metadata file %s.\n" file;
      generate_callgraph file ds
  | [ ":obj"; file ] ->
      Printf.printf "Loading OBJ32 file %s.\n" file;
      Obj32.load_file file cpu.elfwrite32
  | [ ":bin" ; file ; address ]  ->
      let ram_address = Int64.of_string address in
      Printf.printf "Loading BIN file %s to 0x%Lx.\n" file ram_address;
      Bin_file.load_file file cpu.elfwrite8 ram_address
  | [ ":help" ] | [ ":?" ] ->
      List.iter print_endline help_msg;
      Commands.CommandMap.iter
        (fun nm (fn, args, desc) -> Printf.printf ":%-30s%s\n" (nm^" "^args) desc)
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
          process_command ds tcenv cpu prj (input_line inchan)
        done
      with End_of_file -> close_in inchan)
  | [ ":q" ] | [ ":quit" ] -> exit 0
  | [ ":run" ] ->
      ( try
        while true do
          cpu.step ()
        done
      with e -> Error.print_exception e; error ())
  | ":step" :: args ->
      let n = match args with
        | [n] -> int_of_string n
        | _ -> 1
      in
      ( try
          for i = 1 to n do
            cpu.step ();
          done
      with
      | Value.Throw (_, _, _) ->
        Printf.printf "Exception taken\n";
        error ()
      | Value.EvalError (loc, msg) ->
        Printf.printf "  %s: Evaluation error: %s\n" (pp_loc loc) msg;
        error ()
      )
  | ":chekhov" :: trace_file :: regmap_file :: rest ->
    let trace_chan = open_out trace_file in
    let o_ami = match rest with [ami] -> Some ami; | _ -> None in
    Value.tracer := Chekhov.chekhovTextTracer [] regmap_file o_ami trace_chan
  | (cmd :: args) when String.starts_with ~prefix:":" cmd ->
     ( match Commands.CommandMap.find_opt (Utils.string_drop 1 cmd) !Commands.commands with
     | None -> Printf.printf "Unknown command %s\n" cmd
     | Some (cmd, _, _) -> ignore (cmd tcenv cpu args); () (* todo: use the bool *)
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

and load_project (ds : AST.declaration list) (tcenv : TC.Env.t) (cpu : Cpu.cpu) (prj : string) : unit =
  let inchan = open_in prj in
  try
    while true do
      process_command ds tcenv cpu prj (input_line inchan)
    done
  with End_of_file -> close_in inchan

let rec repl (ds : AST.declaration list) (tcenv : TC.Env.t) (cpu : Cpu.cpu) : unit =
  flush stdout;
  match LNoise.linenoise "ASLi> " with
  | None -> ()
  | Some input ->
      LNoise.history_add input |> ignore;
      (try
        process_command ds tcenv cpu "<stdin>" input
      with e ->
        Error.print_exception e;
        error ();
      );
      repl ds tcenv cpu

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
 * Main program and command line options
 ****************************************************************)

let options =
  Arg.align
    [
      ("--print_spec", Arg.Set opt_print_spec, "       Print ASL spec");
      ("-v", Arg.Set opt_verbose, "       Verbose output");
      ("--version", Arg.Set opt_print_version, "       Print version");
      ("--nobanner", Arg.Clear opt_show_banner, "       Suppress banner");
      ("--batchmode", Arg.Set opt_batchmode,  "       Fail on error");
      ("--project", Arg.String add_project,     "       Execute project file");
    ]

let version = "ASL 0.2.0 alpha"

let banner =
  [
    {|            _____  _       _    ___________________________________|};
    {|    /\     / ____|| |     (_)   ASL interpreter                    |};
    {|   /  \   | (___  | |      _    Copyright Arm Limited (c) 2017-2019|};
    {|  / /\ \   \___ \ | |     | |   Copyright Intel Inc (c) 2022-2024  |};
    {| / ____ \  ____) || |____ | |   |} ^ version;
    {|/_/    \_\|_____/ |______||_|   ___________________________________|};
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
  else
    if !opt_show_banner then (
      List.iter print_endline banner;
      print_endline "\nType :? for help");
    try (
      let t = LoadASL.read_file paths "prelude.asl" true !opt_verbose in
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

      let tcenv = TC.Env.mkEnv TC.env0 in
      let cpu = Cpu.mkCPU env in

      List.iter (load_project ds tcenv cpu) !projects;

      LNoise.history_load ~filename:"asl_history" |> ignore;
      LNoise.history_set ~max_length:100 |> ignore;
      repl ds tcenv cpu
    ) with e -> Error.print_exception e; exit 1

let _ = ignore (main ())

(****************************************************************
 * End
 ****************************************************************)
