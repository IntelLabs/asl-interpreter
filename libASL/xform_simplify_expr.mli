(****************************************************************
 * Expression simplifier
 *
 * This simplifies expressions such as
 *
 *     x + 1 - x   ==>   1
 *     2 * x - x   ==>   x
 *
 * which makes it easier for constant propagation, etc.
 * to eliminate expressions.
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

(** Simplify an expression by simplifying integer +,-,*,c,v *)
val simplify : AST.expr -> AST.expr

(****************************************************************)
(* End                                                          *)
(****************************************************************)
