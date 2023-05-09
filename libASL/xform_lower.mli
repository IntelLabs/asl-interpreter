(****************************************************************
 * ASL lowering transform
 *
 * Transforms
 * - Slice_HiLo to Slice_LoWd
 * - Slice_Single to Slice_LoWd
 * - Integer expression slices to asl_extract_int
 *
 * Copyright Intel Inc (c) 2023
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val xform_decls : AST.declaration list -> AST.declaration list
val xform_expr : AST.expr -> AST.expr
val xform_stmts : AST.stmt list -> AST.stmt list

(****************************************************************
 * End
 ****************************************************************)
