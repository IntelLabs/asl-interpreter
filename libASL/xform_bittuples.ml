(****************************************************************
 * ASL bittuple transforms
 *
 * This simplifies assignments and expressions involving multiple slices
 *
 * 1) 'bittuple' L-expression assignments such as
 *
 *        var x :: bits(8);
 *        var y :: bits(8);
 *        var z :: bits(16);
 *        ...
 *        [x,y,z] = e;
 *    ==>
 *        let tmp = e;
 *        x = tmp[24 +: 8];
 *        y = tmp[16 +: 8];
 *        z = tmp[0 +: 16];
 *
 *    Note that, in the general case, x, y and z can be any L-expressions.
 *
 * 2) multiple slice L-expression assignments such as
 *
 *        x[0 +: 8, 8 +: 8, 16 +: 8, 24 +: 8] = e; // byte-reverse
 *    ==>
 *        [x[0 +: 8], x[8 +: 8], x[16 +: 8], x[24 +: 8]] = e; // byte-reverse
 *    ==>
 *        let tmp = e;
 *        x[0 +: 8] = tmp[24 +: 8];
 *        x[8 +: 8] = tmp[16 +: 8];
 *        x[16 +: 8] = tmp[8 +: 8];
 *        x[24 +: 8] = tmp[0 +: 8];
 *
 * 3) multiple slice R-expressions such as
 *
 *        ... = e[0 +: 8, 8 +: 8];
 *    ==>
 *        ... = [e[0 +: 8], e[8 +: 8]];
 *
 *    todo: this should really make sure that e is atomic
 *    by assigning to a tmp if needed
 *
 *    ==>
 *        tmp = e;
 *        ... = [tmp[0 +: 8], tmp[8 +: 8]];
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

let slice_width (x : AST.slice) : AST.expr =
  ( match x with
  | Slice_Single e -> Asl_utils.one
  | Slice_HiLo (hi, lo) -> Xform_simplify_expr.mk_add_int (Asl_utils.mk_sub_int hi lo) Asl_utils.one
  | Slice_LoWd (lo, wd) -> wd
  )

class replace_bittuples (ds : AST.declaration list option) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      ( match x with
      | Expr_Slices (ty, e, ss) when List.length ss > 1 -> (* todo: only if e is atomic *)
        let ws = List.map slice_width ss in
        let es = List.map (fun s -> Expr_Slices (ty, e, [s])) ss in
        Visitor.ChangeTo (Expr_Concat (ws, es))
      | _ -> DoChildren
      )

    method! vstmt s =
      match s with
      | Stmt_Assign (LExpr_BitTuple (ws, ls), r, loc) ->
          Visitor.ChangeTo (xform loc ws ls r)
      | Stmt_Assign (LExpr_Slices (ty, l, ss), r, loc) when List.length ss > 1 ->
          let (ws, ls) =
            List.map (fun s -> (slice_width s, LExpr_Slices (ty, l, [s]))) ss
            |> List.split
          in
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
