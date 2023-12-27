(****************************************************************
 * ASL transform to track valid bits
 *
 * Copyright Intel Inc (c) 2024
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val xform_stmts : Ident.t list -> AST.stmt list -> AST.stmt list
val xform_decls : Ident.t list -> AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
