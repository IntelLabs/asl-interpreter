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

let opt_filenames : string list ref = ref []
let opt_print_version = ref false
let opt_print_spec = ref false
let opt_verbose = ref false
let opt_show_banner = ref true

let projects : string list ref = ref []

let add_project (prj : string): unit =
  projects := List.append !projects [prj]

let help_msg =
  [
    {|:? :help                       Show this help message|};
    {|:elf <file>                    Load an ELF file|};
    {|:project <file>                Execute ASLi commands in <file>|};
    {|:q :quit                       Exit the interpreter|};
    {|:run                           Execute instructions|};
    {|:set impdef <string> = <expr>  Define implementation defined behavior|};
    {|:set +<flag>                   Set flag|};
    {|:set -<flag>                   Clear flag|};
    {|<expr>                         Execute ASL expression|};
    {|<stmt> ;                       Execute ASL statement|};
  ]

let flags =
  [
    ("trace:write", Eval.trace_write);
    ("trace:fun", Eval.trace_funcall);
    ("trace:prim", Eval.trace_primop);
  ]

let mkLoc (fname : string) (input : string) : AST.l =
  let len = String.length input in
  let start : Lexing.position =
    { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  let finish : Lexing.position =
    { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = len }
  in
  AST.Range (start, finish)

let rec process_command (tcenv : TC.Env.t) (cpu : Cpu.cpu) (fname : string)
  (input0 : string) : unit =
  let input = String.trim input0 in
  match String.split_on_char ' ' input with
  | [ "" ] -> ()
  | ("//"::_) -> () (* comment *)
  | [ ":elf"; file ] ->
      Printf.printf "Loading ELF file %s.\n" file;
      let entry = Elf.load_file file cpu.elfwrite in
      Printf.printf "Entry point = 0x%Lx\n" entry;
      cpu.setPC (Z.of_int64 entry)
  | [ ":help" ] | [ ":?" ] ->
      List.iter print_endline help_msg;
      print_endline "\nFlags:";
      List.iter
        (fun (nm, v) -> Printf.printf "  %s%s\n" (if !v then "+" else "-") nm)
        flags
  | ":set" :: "impdef" :: rest ->
      let cmd = String.concat " " rest in
      let loc = mkLoc fname cmd in
      let x, e = LoadASL.read_impdef tcenv loc cmd in
      let v = Eval.eval_expr loc cpu.env e in
      cpu.setImpdef x v
  | [ ":set"; flag ] when Utils.startswith flag "+" -> (
      match List.assoc_opt (Utils.stringDrop 1 flag) flags with
      | None -> Printf.printf "Unknown flag %s\n" flag
      | Some f -> f := true)
  | [ ":set"; flag ] when Utils.startswith flag "-" -> (
      match List.assoc_opt (Utils.stringDrop 1 flag) flags with
      | None -> Printf.printf "Unknown flag %s\n" flag
      | Some f -> f := false)
  | [ ":project"; prj ] ->
      load_project tcenv cpu prj
  | [ ":q" ] | [ ":quit" ] -> exit 0
  | [ ":run" ] -> (
      try
        while true do
          cpu.step ()
        done
      with Value.Throw (_, Primops.Exc_ExceptionTaken) ->
        Printf.printf "Exception taken\n")
  | _ ->
      if ';' = String.get input (String.length input - 1) then
        let s = LoadASL.read_stmt tcenv input in
        Eval.eval_stmt cpu.env s
      else
        let loc = mkLoc fname input in
        let e = LoadASL.read_expr tcenv loc input in
        let v = Eval.eval_expr loc cpu.env e in
        print_endline (Value.pp_value v)

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
         LoadASL.report_eval_error
           (fun _ -> ())
           (fun _ ->
             LoadASL.report_type_error
               (fun _ -> ())
               (fun _ ->
                 LoadASL.report_parse_error
                   (fun _ -> ())
                   (fun _ -> process_command tcenv cpu "<stdin>" input)))
       with exc ->
         Printf.printf "  Error %s\n" (Printexc.to_string exc);
         Printexc.print_backtrace stdout);
      repl tcenv cpu

let options =
  Arg.align
    [
      ("--print_spec", Arg.Set opt_print_spec, "       Print ASL spec");
      ("-v", Arg.Set opt_verbose, "       Verbose output");
      ("--version", Arg.Set opt_print_version, "       Print version");
      ("--nobanner", Arg.Clear opt_show_banner, "       Suppress banner");
      ("--project", Arg.String add_project,     "       Execute project file");
    ]

let version = "ASL 0.2.0 alpha"

let banner =
  [
    {|            _____  _       _    ___________________________________|};
    {|    /\     / ____|| |     (_)   ASL interpreter                    |};
    {|   /  \   | (___  | |      _    Copyright Arm Limited (c) 2017-2019|};
    {|  / /\ \   \___ \ | |     | |                                      |};
    {| / ____ \  ____) || |____ | |   |} ^ version;
    {|/_/    \_\|_____/ |______||_|   ___________________________________|};
  ]

let usage_msg = version ^ "\nusage: asli <options> <file1> ... <fileN>\n"

let _ =
  Arg.parse options (fun s -> opt_filenames := !opt_filenames @ [ s ]) usage_msg

let main () =
  Ocolor_format.prettify_formatter Format.std_formatter;
  Ocolor_config.set_color_capability Ocolor_config.Color4;
  let paths = Option.value (Sys.getenv_opt "ASL_PATH") ~default:"." in
  let paths = String.split_on_char ':' paths in
  if !opt_print_version then Printf.printf "%s\n" version
  else (
    if !opt_show_banner then (
      List.iter print_endline banner;
      print_endline "\nType :? for help");
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

    if !opt_print_spec then (
      FMT.comment_list := Lexer.get_comments ();
      FMT.declarations Format.std_formatter t;
      Format.pp_print_flush Format.std_formatter ());

    if !opt_verbose then Printf.printf "Building evaluation environment\n";
    let env =
      try Eval.build_evaluation_environment (List.concat (t :: ts))
      with Value.EvalError (loc, msg) ->
        Printf.printf "  %s: Evaluation error: %s\n" (pp_loc loc) msg;
        exit 1
    in
    if !opt_verbose then Printf.printf "Built evaluation environment\n";

    let tcenv = TC.Env.mkEnv TC.env0 in
    let cpu = Cpu.mkCPU env in

    List.iter (load_project tcenv cpu) !projects;

    LNoise.history_load ~filename:"asl_history" |> ignore;
    LNoise.history_set ~max_length:100 |> ignore;
    repl tcenv cpu)

let _ = ignore (main ())

(****************************************************************
 * End
 ****************************************************************)
