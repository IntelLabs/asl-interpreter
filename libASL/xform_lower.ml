(****************************************************************
 * ASL lowering transform
 *
 * Transforms
 * - Slice_HiLo to Slice_LoWd
 * - Slice_Single to Slice_LoWd
 * - Slice_Element to Slice_LoWd
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

let assign_var = new Asl_utils.nameSupply "__l"

let xform_expr_slices
    (ty : AST.ty)
    (e : AST.expr)
    (slices : AST.slice list)
    : AST.expr =
  let rec xform' (slices : AST.slice list) (acc : AST.slice list) : AST.expr =
  match slices with
  | [] -> AST.Expr_Slices (ty, e, List.rev acc)
  | Slice_Element (lo, wd) :: xs ->
     let f e' = xform' xs (AST.Slice_LoWd (mk_mul_int lo e', e') :: acc) in
     mk_expr_safe_to_replicate assign_var wd type_integer f
  | x :: xs -> xform' xs (x :: acc)
  in
  xform' slices []

let xform_lexpr_slices
    (ty : AST.ty)
    (lexpr : AST.lexpr)
    (slices : AST.slice list)
    (loc : AST.l)
    : (AST.lexpr * AST.stmt list) =
  let rec xform'
      (slices : AST.slice list)
      ((acc_slices, acc_stmts) : (AST.slice list * AST.stmt list))
      : (AST.lexpr * AST.stmt list) =
    match slices with
    | [] ->
      (AST.LExpr_Slices (ty, lexpr, List.rev acc_slices), List.rev acc_stmts)
    | Slice_Element (lo, wd) :: xs ->
      if is_safe_to_replicate wd then
        let slice =  AST.Slice_LoWd ((mk_mul_int lo wd), wd) in
        xform' xs (slice :: acc_slices, acc_stmts)
      else
        let v = assign_var#fresh in
        let decl_item = AST.DeclItem_Var (v, Some type_integer) in
        let stmt = AST.Stmt_ConstDecl (decl_item, wd, loc) in
        let slice = AST.Slice_LoWd (mk_mul_int lo (Expr_Var v), (Expr_Var v)) in
        xform' xs (slice :: acc_slices, stmt :: acc_stmts)
    | x :: xs -> xform' xs (x :: acc_slices, acc_stmts)
  in
  xform' slices ([], [])

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
      | Expr_Slices (ty, e, slices) -> ChangeTo (xform_expr_slices ty e slices)
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

    method! vstmt s =
      match s with
      | Stmt_Assign ((LExpr_Slices (ty, l, slices)), e, loc) ->
          let (l', let_stmts) = xform_lexpr_slices ty l slices loc in
          ChangeDoChildrenPost (let_stmts @ [Stmt_Assign (l', e, loc) ], Fun.id)
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
