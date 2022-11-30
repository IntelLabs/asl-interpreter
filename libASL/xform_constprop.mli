(****************************************************************
 * ASL constant propagation transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

module Env : sig
  type t
  val pp : t -> unit
end

val unroll_loops : bool ref

val mkEnv : Eval.GlobalEnv.t -> (AST.ident * Value.value) list -> Env.t

val xform_ty : Env.t -> AST.ty -> AST.ty
val xform_expr : Env.t -> AST.expr -> AST.expr
val xform_stmts : Env.t -> AST.stmt list -> AST.stmt list

val xform_decls : Eval.GlobalEnv.t -> AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
