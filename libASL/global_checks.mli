(****************************************************************
 * Global checks
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val check_decls : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
