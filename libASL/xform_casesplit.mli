(****************************************************************
 * ASL case split transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val xform_decl : AST.declaration -> (Ident.t * AST.expr list) list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
