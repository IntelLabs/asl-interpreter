(****************************************************************
 * Functions for processing ASL files
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
module TC = Tcheck

exception ParseError of string

val read_file :
  string list -> string -> bool -> bool -> Asl_ast.declaration list
(** Parse and typecheck ASL file *)

val parse_spec : string list -> string -> bool -> Asl_ast.declaration list

val parse_file :
  string list -> string -> bool -> bool -> Asl_ast.declaration list
(** Parse ASL file, but do not typecheck *)

val read_files :
  string list -> string list -> bool -> Asl_ast.declaration list
(** Parse and typecheck ASL files. All files are first parsed then typechecked
    together.*)

val read_impdef : TC.Env.t -> AST.l -> string -> string * AST.expr
val read_expr : TC.Env.t -> AST.l -> string -> AST.expr
val read_stmt : TC.Env.t -> string -> AST.stmt

(****************************************************************
 * End
 ****************************************************************)
