(****************************************************************
 * ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

(* global symbol table *)
module GlobalEnv : sig
  type t

  val getGlobalConstOpt : t -> AST.ident -> Value.value option
  val isEnumEq : t -> AST.ident -> bool
  val isEnumNeq : t -> AST.ident -> bool

  val getFun :
    AST.l ->
    t ->
    AST.ident ->
    (AST.ident list * AST.ident list * AST.l * AST.stmt list) option

  val getDecoder : t -> AST.ident -> AST.decode_case
  val setImpdef : t -> string -> Value.value -> unit
  val pp : t -> unit
end

(* global + local symbol table *)
module Env : sig
  type t

  val mkEnv : GlobalEnv.t -> Value.value Asl_utils.ScopeStack.t -> t
  val newEnv : GlobalEnv.t -> t
  val globals : t -> GlobalEnv.t
  val pp : Format.formatter -> t -> unit
end

val trace_write : bool ref
(** Debugging output on every variable write *)

val trace_funcall : bool ref
(** Debugging output on every function call *)

val trace_primop : bool ref
(** Debugging output on every primitive function or function call *)

val trace_instruction : bool ref
(** Debugging output on every instruction execution *)

val eval_expr : AST.l -> Env.t -> AST.expr -> Value.value
val eval_stmt : Env.t -> AST.stmt -> unit

val eval_proccall :
  AST.l -> Env.t -> AST.ident -> Value.value list -> Value.value list -> unit
(** Evaluate call to procedure *)

val eval_funcall :
  AST.l ->
  Env.t ->
  AST.ident ->
  Value.value list ->
  Value.value list ->
  Value.value
(** Evaluate call to function *)

val eval_decode_case : AST.l -> Env.t -> AST.decode_case -> Value.value -> unit
(** Evaluate instruction decode case *)

val build_evaluation_environment : AST.declaration list -> Env.t
val build_constant_environment : AST.declaration list -> GlobalEnv.t

(****************************************************************
 * End
 ****************************************************************)
