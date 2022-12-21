module AST = Asl_ast
module Utils = Asl_utils
open Asl_ast

let assign_var = new Asl_utils.nameSupply "__a"

let xform
    (loc : AST.l)
    (ws : AST.expr list)
    (ls : AST.lexpr list)
    (r : AST.expr)
    : AST.stmt list =
    let tmp_ident = assign_var#fresh in

    let (ss, sum) = List.fold_right2 (fun l w (ss, idx) ->
      let slice = Slice_LoWd (idx, w) in
      let r' = Expr_Slices (Expr_Var tmp_ident, [slice]) in
      (Stmt_Assign (l, r', loc) :: ss, Utils.mk_add_int idx w)
    ) ls ws ([], Utils.zero) in

    let tmp_type = sum |> Xform_simplify_expr.simplify |> Utils.type_bits in
    let tmp_var = DeclItem_Var (tmp_ident, Some tmp_type) in
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
