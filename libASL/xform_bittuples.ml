(****************************************************************
 * ASL bittuple transform
 *
 * This simplifies 'bittuple' L-expression assignments such as
 *
 *     var x :: bits(8);
 *     var y :: bits(8);
 *     var z :: bits(16);
 *     ...
 *     [x,y,z] = e;
 * ==>
 *     let tmp = e;
 *     x = tmp[24 +: 8];
 *     y = tmp[16 +: 8];
 *     z = tmp[0 +: 16];
 *
 * Note that, in the general case, x, y and z can be any L-expressions.
 *
 * Copyright Intel Inc (c) 2022-2023
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_ast

let assign_var = new Asl_utils.nameSupply "__a"

(* Transform '[l1, ... ln] = r;' where, for each 'i', 'li :: bits(wi) *)
let xform
    (loc : AST.l)
    (ws : AST.expr list)
    (ls : AST.lexpr list)
    (r : AST.expr)
    : AST.stmt list =
    let tmp_ident = assign_var#fresh in
    let total_width = Xform_simplify_expr.mk_add_ints ws in
    let ty = Asl_utils.type_bits total_width in

    let (ss, _) = List.fold_right2 (fun l w (ss, idx) ->
      let slice = Slice_LoWd (idx, w) in
      let r' = Expr_Slices (ty, Expr_Var tmp_ident, [slice]) in
      (Stmt_Assign (l, r', loc) :: ss, Asl_utils.mk_add_int idx w)
    ) ls ws ([], Asl_utils.zero) in

    let tmp_var = DeclItem_Var (tmp_ident, Some ty) in
    let tmp_const_decl = Stmt_ConstDecl (tmp_var, r, loc) in
    tmp_const_decl :: ss

class replace_bittuples (ds : AST.declaration list option) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vstmt s =
      match s with
      | Stmt_Assign (LExpr_BitTuple (ws, ls), r, loc) ->
          Visitor.ChangeTo (xform loc ws ls r)
      | _ -> DoChildren
  end

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let replacer = new replace_bittuples None in
  Asl_visitor.visit_stmts (replacer :> Asl_visitor.aslVisitor) ss

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let replacer = new replace_bittuples (Some ds) in
  List.map (Asl_visitor.visit_decl (replacer :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * End
 ****************************************************************)
