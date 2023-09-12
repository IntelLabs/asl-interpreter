(****************************************************************
 * ASL lowering transform
 *
 * Transforms
 * - Slice_HiLo to Slice_LoWd
 * - Slice_Single to Slice_LoWd
 *
 * Copyright Intel Inc (c) 2023
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils

let transform_hi_lo hi lo =
  let wd = Xform_simplify_expr.mk_add_int (mk_sub_int hi lo) one in
  AST.Slice_LoWd (lo, wd)

let transform_single s =
  AST.Slice_LoWd (s, one)

class lower_class =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      match x with
      | Expr_Slices (ty, expr, [Slice_HiLo (hi, lo)]) ->
          let lo_wd = transform_hi_lo hi lo in
          Visitor.ChangeDoChildrenPost ((Expr_Slices (ty, expr, [lo_wd])), Fun.id)
      | Expr_Slices (ty, expr, [Slice_Single s]) ->
          let lo_wd = transform_single s in
          Visitor.ChangeDoChildrenPost ((Expr_Slices (ty, expr, [lo_wd])), Fun.id)
      | _ -> DoChildren

    method! vlexpr l =
      match l with
      | LExpr_Slices (ty, lexpr, [Slice_HiLo (hi, lo)]) ->
          let lo_wd = transform_hi_lo hi lo in
          Visitor.ChangeDoChildrenPost ((LExpr_Slices (ty, lexpr, [lo_wd])), Fun.id)
      | LExpr_Slices (ty, lexpr, [Slice_Single s]) ->
          let lo_wd = transform_single s in
          Visitor.ChangeDoChildrenPost ((LExpr_Slices (ty, lexpr, [lo_wd])), Fun.id)
      | _ -> DoChildren

  end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let simplify = new lower_class in
  List.map (Asl_visitor.visit_decl (simplify :> Asl_visitor.aslVisitor)) ds

let xform_expr (x : AST.expr) : AST.expr =
  let simplify = new lower_class in
  Asl_visitor.visit_expr (simplify :> Asl_visitor.aslVisitor) x

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let simplify = new lower_class in
  Asl_visitor.visit_stmts (simplify :> Asl_visitor.aslVisitor) ss

(****************************************************************
 * End
 ****************************************************************)
