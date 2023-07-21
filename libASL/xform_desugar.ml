(****************************************************************
 * ASL desugaring transformations
 *
 * This performs some simple desugaring transformations:
 *
 * 1) add_bits_int{N}(x, y) -> add_bits(x, y[0 +: N])
 * 2) sub_bits_int{N}(x, y) -> sub_bits(x, y[0 +: N])
 * 3) mul_bits_int{N}(x, y) -> mul_bits(x, y[0 +: N])
 *
 * Copyright Intel Inc (c) 2023
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_ast

class desugar (ds : AST.declaration list option) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      ( match x with
      | Expr_TApply (FIdent ("add_bits_int", _), [n], [x; y], _) ->
        Visitor.ChangeTo (Asl_utils.mk_add_bits n x (Expr_Slices (Asl_utils.type_bits n, y, [Slice_LoWd (Asl_utils.zero, n)])))
      | Expr_TApply (FIdent ("sub_bits_int", _), [n], [x; y], _) ->
        Visitor.ChangeTo (Asl_utils.mk_sub_bits n x (Expr_Slices (Asl_utils.type_bits n, y, [Slice_LoWd (Asl_utils.zero, n)])))
      | Expr_TApply (FIdent ("mul_bits_int", _), [n], [x; y], _) ->
        Visitor.ChangeTo (Asl_utils.mk_mul_bits n x (Expr_Slices (Asl_utils.type_bits n, y, [Slice_LoWd (Asl_utils.zero, n)])))
      | _ -> DoChildren
      )
  end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let xform = new desugar (Some ds) in
  List.map (Asl_visitor.visit_decl (xform :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * End
 ****************************************************************)
