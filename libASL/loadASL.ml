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

let report_parse_error (on_error : unit -> 'a) (f : unit -> 'a) : 'a =
  try f () with
  | Parse_error_locn (l, s) ->
      Printf.printf "  Syntax error %s at %s\n" s (pp_loc l);
      on_error ()
  | PrecedenceError (loc, op1, op2) ->
      Printf.printf
        "  Syntax error: operators %s and %s require parentheses to \
         disambiguate expression at location %s\n"
        (pp_binop op1) (pp_binop op2) (pp_loc loc);
      on_error ()
  | Parser.Error ->
      Printf.printf "  Parser error\n";
      on_error ()

let report_type_error (on_error : unit -> 'a) (f : unit -> 'a) : 'a =
  try f () with
  | TC.UnknownObject (loc, what, x) ->
      Printf.printf "  %s: Type error: Unknown %s %s\n" (pp_loc loc) what x;
      on_error ()
  | TC.DoesNotMatch (loc, what, x, y) ->
      Printf.printf "  %s: Type error: %s %s does not match %s\n" (pp_loc loc)
        what x y;
      on_error ()
  | TC.IsNotA (loc, what, x) ->
      Printf.printf "  %s: Type error: %s is not a %s\n" (pp_loc loc) x what;
      on_error ()
  | TC.Ambiguous (loc, what, x) ->
      Printf.printf "  %s: Type error: %s %s is ambiguous\n" (pp_loc loc) what x;
      on_error ()
  | TC.TypeError (loc, what) ->
      Printf.printf "  %s: Type error: %s\n" (pp_loc loc) what;
      on_error ()

let report_eval_error (on_error : unit -> 'a) (f : unit -> 'a) : 'a =
  try f ()
  with Value.EvalError (loc, msg) ->
    Printf.printf "  %s: Evaluation error: %s\n" (pp_loc loc) msg;
    on_error ()

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
  let t =
    report_parse_error
      (fun _ ->
        print_endline (pp_loc (Range (lexbuf.lex_start_p, lexbuf.lex_curr_p)));
        exit 1)
      (fun _ ->
        (* Run the parser on this line of input. *)
        if verbose then Printf.printf "- Parsing %s\n" fname;
        Parser.declarations_start Lexer.token lexbuf)
  in
  close_in inchan;
  t

let read_file (paths : string list) (filename : string) (isPrelude : bool)
    (verbose : bool) : AST.declaration list =
  let t = parse_file paths filename isPrelude verbose in

  if false then (
    FMT.comment_list := Lexer.get_comments ();
    FMT.declarations Format.std_formatter t;
    Format.pp_print_flush Format.std_formatter ());
  if verbose then
    Printf.printf "  - Got %d declarations from %s\n%!" (List.length t) filename;

  let t' =
    report_type_error
      (fun _ -> exit 1)
      (fun _ ->
        if verbose then Printf.printf "- Typechecking %s\n%!" filename;
        TC.tc_declarations TC.env0 isPrelude t)
  in

  if false then FMT.declarations Format.std_formatter t';
  if verbose then
    Printf.printf "  - Got %d typechecked declarations from %s\n%!"
      (List.length t') filename;

  if verbose then Printf.printf "Finished %s\n%!" filename;
  flush stdout;
  t'

let read_spec (paths : string list) (filename : string) (verbose : bool) :
    AST.declaration list =
  let r : AST.declaration list list ref = ref [] in
  let fname = find_file paths filename in
  let inchan = open_in fname in
  (try
     while true do
       let t = read_file paths (input_line inchan) false verbose in
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

(****************************************************************
 * End
 ****************************************************************)
