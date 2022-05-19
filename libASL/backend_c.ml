(****************************************************************
 * ASL to C backend
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C backend *)

module AST = Asl_ast
module FMTAST = Asl_fmt
module PP = Format
open Format_utils

exception Unimplemented of (AST.l * string * (Format.formatter -> unit))

let delimiter (fmt : PP.formatter) (s : string) : unit =
  PP.pp_print_string fmt s

let keyword (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt s

(* C delimiters *)

let amp                 (fmt : PP.formatter) : unit = delimiter fmt "&"
let amp_amp             (fmt : PP.formatter) : unit = delimiter fmt "&&"
let amp_eq              (fmt : PP.formatter) : unit = delimiter fmt "&="
let bang                (fmt : PP.formatter) : unit = delimiter fmt "!"
let bang_eq             (fmt : PP.formatter) : unit = delimiter fmt "!="
let bar                 (fmt : PP.formatter) : unit = delimiter fmt "|"
let bar_bar             (fmt : PP.formatter) : unit = delimiter fmt "||"
let bar_eq              (fmt : PP.formatter) : unit = delimiter fmt "|="
let caret               (fmt : PP.formatter) : unit = delimiter fmt "^"
let caret_eq            (fmt : PP.formatter) : unit = delimiter fmt "^="
let colon               (fmt : PP.formatter) : unit = delimiter fmt ":"
let comma               (fmt : PP.formatter) : unit = delimiter fmt ","
let dot                 (fmt : PP.formatter) : unit = delimiter fmt "."
let dot_dot_dot         (fmt : PP.formatter) : unit = delimiter fmt "..."
let eq                  (fmt : PP.formatter) : unit = delimiter fmt "="
let eq_eq               (fmt : PP.formatter) : unit = delimiter fmt "=="
let gt                  (fmt : PP.formatter) : unit = delimiter fmt ">"
let gt_eq               (fmt : PP.formatter) : unit = delimiter fmt ">="
let gt_gt               (fmt : PP.formatter) : unit = delimiter fmt ">>"
let gt_gt_eq            (fmt : PP.formatter) : unit = delimiter fmt ">>="
let hash                (fmt : PP.formatter) : unit = delimiter fmt "#"
let hash_hash           (fmt : PP.formatter) : unit = delimiter fmt "##"
let lbrace              (fmt : PP.formatter) : unit = delimiter fmt "{"
let lbrack              (fmt : PP.formatter) : unit = delimiter fmt "["
let lparen              (fmt : PP.formatter) : unit = delimiter fmt "("
let lt                  (fmt : PP.formatter) : unit = delimiter fmt "<"
let lt_eq               (fmt : PP.formatter) : unit = delimiter fmt "<="
let lt_lt               (fmt : PP.formatter) : unit = delimiter fmt "<<"
let lt_lt_eq            (fmt : PP.formatter) : unit = delimiter fmt "<<="
let minus               (fmt : PP.formatter) : unit = delimiter fmt "-"
let minus_eq            (fmt : PP.formatter) : unit = delimiter fmt "-="
let minus_minus         (fmt : PP.formatter) : unit = delimiter fmt "--"
let minus_qt            (fmt : PP.formatter) : unit = delimiter fmt "->"
let percent             (fmt : PP.formatter) : unit = delimiter fmt "%"
let percent_eq          (fmt : PP.formatter) : unit = delimiter fmt "%="
let plus                (fmt : PP.formatter) : unit = delimiter fmt "+"
let plus_eq             (fmt : PP.formatter) : unit = delimiter fmt "+="
let plus_plus           (fmt : PP.formatter) : unit = delimiter fmt "++"
let qmark               (fmt : PP.formatter) : unit = delimiter fmt "?"
let rbrace              (fmt : PP.formatter) : unit = delimiter fmt "}"
let rbrack              (fmt : PP.formatter) : unit = delimiter fmt "]"
let rparen              (fmt : PP.formatter) : unit = delimiter fmt ")"
let semicolon           (fmt : PP.formatter) : unit = delimiter fmt ";"
let slash               (fmt : PP.formatter) : unit = delimiter fmt "/"
let slash_eq            (fmt : PP.formatter) : unit = delimiter fmt "/="
let star                (fmt : PP.formatter) : unit = delimiter fmt "*"
let star_eq             (fmt : PP.formatter) : unit = delimiter fmt "*="
let tilde               (fmt : PP.formatter) : unit = delimiter fmt "~"

(* C keywords *)

let kw_auto             (fmt : PP.formatter) : unit = keyword fmt "auto"
let kw_break            (fmt : PP.formatter) : unit = keyword fmt "break"
let kw_case             (fmt : PP.formatter) : unit = keyword fmt "case"
let kw_char             (fmt : PP.formatter) : unit = keyword fmt "char"
let kw_const            (fmt : PP.formatter) : unit = keyword fmt "const"
let kw_continue         (fmt : PP.formatter) : unit = keyword fmt "continue"
let kw_default          (fmt : PP.formatter) : unit = keyword fmt "default"
let kw_do               (fmt : PP.formatter) : unit = keyword fmt "do"
let kw_double           (fmt : PP.formatter) : unit = keyword fmt "double"
let kw_else             (fmt : PP.formatter) : unit = keyword fmt "else"
let kw_enum             (fmt : PP.formatter) : unit = keyword fmt "enum"
let kw_extern           (fmt : PP.formatter) : unit = keyword fmt "extern"
let kw_float            (fmt : PP.formatter) : unit = keyword fmt "float"
let kw_for              (fmt : PP.formatter) : unit = keyword fmt "for"
let kw_goto             (fmt : PP.formatter) : unit = keyword fmt "goto"
let kw_if               (fmt : PP.formatter) : unit = keyword fmt "if"
let kw_inline           (fmt : PP.formatter) : unit = keyword fmt "inline"
let kw_int              (fmt : PP.formatter) : unit = keyword fmt "int"
let kw_long             (fmt : PP.formatter) : unit = keyword fmt "long"
let kw_register         (fmt : PP.formatter) : unit = keyword fmt "register"
let kw_restrict         (fmt : PP.formatter) : unit = keyword fmt "restrict"
let kw_return           (fmt : PP.formatter) : unit = keyword fmt "return"
let kw_short            (fmt : PP.formatter) : unit = keyword fmt "short"
let kw_signed           (fmt : PP.formatter) : unit = keyword fmt "signed"
let kw_sizeof           (fmt : PP.formatter) : unit = keyword fmt "sizeof"
let kw_static           (fmt : PP.formatter) : unit = keyword fmt "static"
let kw_struct           (fmt : PP.formatter) : unit = keyword fmt "struct"
let kw_switch           (fmt : PP.formatter) : unit = keyword fmt "switch"
let kw_typedef          (fmt : PP.formatter) : unit = keyword fmt "typedef"
let kw_union            (fmt : PP.formatter) : unit = keyword fmt "union"
let kw_unsigned         (fmt : PP.formatter) : unit = keyword fmt "unsigned"
let kw_void             (fmt : PP.formatter) : unit = keyword fmt "void"
let kw_volatile         (fmt : PP.formatter) : unit = keyword fmt "volatile"
let kw_while            (fmt : PP.formatter) : unit = keyword fmt "while"

(* C pseudo-keywords *)

let kw_bool             (fmt : PP.formatter) : unit = keyword fmt "bool"
let kw_false            (fmt : PP.formatter) : unit = keyword fmt "false"
let kw_int64            (fmt : PP.formatter) : unit = keyword fmt "int64_t"
let kw_true             (fmt : PP.formatter) : unit = keyword fmt "true"
let kw_uint16           (fmt : PP.formatter) : unit = keyword fmt "uint16_t"
let kw_uint32           (fmt : PP.formatter) : unit = keyword fmt "uint32_t"
let kw_uint64           (fmt : PP.formatter) : unit = keyword fmt "uint64_t"
let kw_uint8            (fmt : PP.formatter) : unit = keyword fmt "uint8_t"

let declaration (fmt : PP.formatter) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      match x with
      | Decl_BuiltinFunction (f, ps, args, t, loc) -> ()
      | _ ->
          raise
            (Unimplemented
               (AST.Unknown, "declaration", fun fmt -> FMTAST.declaration fmt x)))

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ -> map fmt (declaration fmt) xs)

(****************************************************************
 * End
 ****************************************************************)
