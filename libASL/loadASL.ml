(****************************************************************
 * Functions for processing ASL files
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Asl_ast
module Lexer = Lexer
module Parser = Asl_parser
module TC = Tcheck
module FMT = Asl_fmt
module AST = Asl_ast
open Lexing
open Asl_utils

exception ParseError of string

let rec find_file (paths : string list) (filename : string) : string =
  match paths with
  | [] -> failwith ("Can't find file '" ^ filename ^ "' on path")
  | f :: paths' ->
      let fname = Filename.concat f filename in
      if Sys.file_exists fname then fname else find_file paths' filename

let parse_file (paths : string list) (filename : string) (isPrelude : bool)
    (verbose : bool) : AST.declaration list =
  let fname = find_file paths filename in
  let inchan = open_in fname in
  if verbose then Printf.printf "Processing %s\n" fname;
  let lexbuf = Lexing.from_channel inchan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  (* Run the parser on this line of input. *)
  if verbose then
    Printf.printf "- Parsing %s\n" fname;
  let t = try
    Parser.declarations_start Lexer.token lexbuf
  with Parser.Error ->
    let msg = pp_loc (Range (lexbuf.lex_start_p, lexbuf.lex_curr_p)) in
    raise (ParseError msg);
  in
  close_in inchan;
  t

let read_file (paths : string list) (filename : string) (isPrelude : bool)
    (verbose : bool) : AST.declaration list =
  let t = parse_file paths filename isPrelude verbose in

  if false then (
    FMT.comment_list := Lexer.get_comments ();
    FMT.declarations Format.std_formatter t;
    Format.pp_print_flush Format.std_formatter ()
  );
  if verbose then (
    Printf.printf "  - Got %d declarations from %s\n%!" (List.length t) filename;
    Printf.printf "- Typechecking %s\n%!" filename;
  );

  let t' = TC.tc_declarations TC.env0 isPrelude t in

  if false then FMT.declarations Format.std_formatter t';
  if verbose then (
    Printf.printf "  - Got %d typechecked declarations from %s\n%!"
      (List.length t') filename;
    Printf.printf "Finished %s\n%!" filename;
  );
  flush stdout;
  t'

let parse_spec (paths : string list) (filename : string) (verbose : bool) :
    AST.declaration list =
  let r : AST.declaration list list ref = ref [] in
  let fname = find_file paths filename in
  let inchan = open_in fname in
  (try
     while true do
       let t = parse_file paths (input_line inchan) false verbose in
       r := t :: !r
     done
   with End_of_file -> close_in inchan);
  List.concat (List.rev !r)

let read_impdef (tcenv : TC.Env.t) (loc : AST.l) (s : string) :
    string * AST.expr =
  let lexbuf = Lexing.from_string s in
  let (CLI_Impdef (x, e)) = Parser.impdef_command_start Lexer.token lexbuf in
  let s, e' =
    TC.with_unify tcenv loc (fun u ->
        let e', _ = TC.tc_expr tcenv u loc e in
        e')
  in
  (x, TC.unify_subst_e s e')

let read_expr (tcenv : TC.Env.t) (loc : AST.l) (s : string) : AST.expr =
  let lexbuf = Lexing.from_string s in
  let e = Parser.expr_command_start Lexer.token lexbuf in
  let s, e' =
    TC.with_unify tcenv loc (fun u ->
        let e', _ = TC.tc_expr tcenv u loc e in
        e')
  in
  TC.unify_subst_e s e'

let read_stmt (tcenv : TC.Env.t) (s : string) : AST.stmt =
  let lexbuf = Lexing.from_string s in
  let s = Parser.stmt_command_start Lexer.token lexbuf in
  TC.tc_stmt tcenv s

let read_stmts (tcenv : TC.Env.t) (s : string) : AST.stmt list =
  let lexbuf = Lexing.from_string s in
  let s = Parser.stmts_command_start Lexer.token lexbuf in
  TC.tc_stmts tcenv AST.Unknown s

let read_declarations (tcenv : TC.GlobalEnv.t) (s : string) :
    AST.declaration list =
  let lexbuf = Lexing.from_string s in
  let s = Parser.declarations_start Lexer.token lexbuf in
  TC.tc_declarations tcenv false s

let read_files (paths : string list) (filenames : string list) (verbose : bool)
    : AST.declaration list =
  let parse fname =
    if String.ends_with fname ~suffix:".spec" then
      parse_spec paths fname verbose
    else if String.ends_with fname ~suffix:".asl" then
      parse_file paths fname false verbose
    else
      failwith ("Unrecognized file suffix on " ^ fname)
  in
  List.map parse filenames
    |> List.concat
    |> TC.tc_declarations TC.env0 false


(****************************************************************
 * End
 ****************************************************************)
