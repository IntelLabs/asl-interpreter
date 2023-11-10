(****************************************************************
 * Error
 *
 * Copyright Intel Inc (c) 2021-2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)
open Asl_ast
module TC = Tcheck
module Parser = Asl_parser
module FMT_Utils = Format_utils
module ASL_FMT = Asl_fmt
open Asl_utils

let print_exception (e : exn) : unit =
  match e with
  | Parse_error_locn (l, s) ->
      Printf.printf "  Syntax error %s at %s\n" s (pp_loc l);
  | PrecedenceError (loc, op1, op2) ->
      Printf.printf
        "  Syntax error: operators %s and %s require parentheses to \
         disambiguate expression at location %s\n"
        (pp_binop op1) (pp_binop op2) (pp_loc loc);
  | Parser.Error ->
      Printf.printf "  Parser error\n";
  | LoadASL.ParseError msg ->
      Printf.printf "  Parser error\n%s\n" msg;
  | TC.UnknownObject (loc, what, x) ->
    Printf.printf "  %s: Type error: Unknown %s %s\n" (pp_loc loc) what x
  | TC.DoesNotMatch (loc, what, x, y) ->
    Printf.printf "  %s: Type error: %s %s does not match %s\n" (pp_loc loc) what x y
  | TC.IsNotA (loc, what, x) ->
    Printf.printf "  %s: Type error: %s is not a %s\n" (pp_loc loc) x what
  | TC.Ambiguous (loc, what, x) ->
    Printf.printf "  %s: Type error: %s %s is ambiguous\n" (pp_loc loc) what x
  | TC.TypeError (loc, what) ->
    Printf.printf "  %s: Type error: %s\n" (pp_loc loc) what
  | Value.EvalError (loc, msg) ->
    Printf.printf "  %s: Evaluation error: %s\n" (pp_loc loc) msg
  | Value.Throw (loc, exc, fs) ->
    Printf.printf "Exception taken\n";
  | Backend_c.InternalError (loc, s, pp, ml_loc) ->
      let fmt = Format.std_formatter in
      Format.fprintf fmt "@.%a: internal compiler error: %s" ASL_FMT.loc loc s;
      FMT_Utils.indented fmt (fun _ -> pp fmt);
      FMT_Utils.cut fmt;
      Format.fprintf fmt "Please submit a bug report. %s@," ml_loc
  | Backend_c.Unimplemented (loc, what, pp)
  | Backend_verilog.Unimplemented (loc, what, pp) ->
      let fmt = Format.std_formatter in
      Format.pp_print_newline fmt ();
      FMT_Utils.vbox fmt (fun _ ->
          ASL_FMT.loc fmt loc;
          Format.fprintf fmt ": Unimplemented %s:" what;
          FMT_Utils.indented fmt (fun _ -> pp fmt);
          FMT_Utils.cut fmt)
  | _ ->
    Printf.printf "  Error %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
