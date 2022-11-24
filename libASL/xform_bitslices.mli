(****************************************************************
 * ASL bitslice transform
 *
 * This simplifies bitslice expressions where the width is not
 * a literal constant.
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val xform_decls : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
