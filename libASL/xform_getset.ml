(****************************************************************
 * ASL getters and setters elimination transform
 *
 * It also:
 * - replaces assignment to LExpr_Write with procedure call
 *
 * - replaces LExpr_ReadWrite by LExpr_Var and wraps the
 *   assignment statement containing the l-expr with two
 *   statements:
 *     (1) function call which reads the initial value into
 *         the variable,
 *     (2) procedure call which writes the modified value back.
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils

let getFunReturnType (d : AST.declaration) : AST.ty option =
  match d with
  | Decl_ArrayGetterDefn (f, ps, atys, rty, body, loc) -> Some rty
  | Decl_FunDefn (f, ps, atys, rty, body, loc) -> Some rty
  | Decl_VarGetterDefn (f, ps, rty, body, loc) -> Some rty
  | _ -> None

let rmwVariables = new Asl_utils.nameSupply "__rmw"

class replaceClass (ds : AST.declaration list) =
  object (self)
    inherit Asl_visitor.nopAslVisitor
    val mutable le_vars : (AST.lexpr * Ident.t) list = []

    val decl_lookup_table =
      ds
      |> List.to_seq
      |> Seq.filter_map monomorphizable_decl_to_ident_and_decl
      |> IdentTable.of_seq

    method! vlexpr e =
      match e with
      | LExpr_ReadWrite (f, g, tes, es, throws) ->
          let v = rmwVariables#fresh in
          le_vars <- (e, v) :: le_vars;
          ChangeTo (AST.LExpr_Var v)
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_Assign (AST.LExpr_Write (f, tes, es, throws), r, loc) ->
          let s = AST.Stmt_TCall (f, tes, es @ [ r ], throws, loc) in
          Visitor.ChangeTo [ s ]
      | Stmt_Assign (_, _, loc) ->
          let post_action (ss : AST.stmt list) : AST.stmt list =
            let wrap_stmts (ss : AST.stmt list) = function
              | AST.LExpr_ReadWrite (f, g, tes, es, throws), v ->
                  let e = AST.Expr_TApply (f, tes, es, throws) in
                  let fd = Option.get (IdentTable.find_opt decl_lookup_table f) in
                  let rty = Option.get (getFunReturnType fd) in
                  let r =
                    AST.Stmt_VarDecl (AST.DeclItem_Var (v, Some rty), e, loc)
                  in
                  let w = AST.Stmt_TCall (g, tes, es @ [ Expr_Var v ], throws, loc) in
                  [ r ] @ ss @ [ w ]
              | _ -> ss
            in
            let ss' = List.fold_left wrap_stmts ss le_vars in
            le_vars <- [];
            ss'
          in
          ChangeDoChildrenPost ([ s ], post_action)
      | _ -> DoChildren
  end

let replace (cl : replaceClass) (ss : AST.stmt list) : AST.stmt list =
  Asl_visitor.visit_stmts (cl :> Asl_visitor.aslVisitor) ss

let xform_decl (replacer : replaceClass) (d : AST.declaration) :
    AST.declaration list =
  match d with
  | Decl_FunDefn (f, ps, args, rty, body, loc) ->
      let body' = replace replacer body in
      let d' = AST.Decl_FunDefn (f, ps, args, rty, body', loc) in
      [ d' ]
  | Decl_ProcDefn (f, ps, args, body, loc) ->
      let body' = replace replacer body in
      let d' = AST.Decl_ProcDefn (f, ps, args, body', loc) in
      [ d' ]
  | Decl_ArrayGetterDefn (f, ps, args, rty, body, loc) ->
      let body' = replace replacer body in
      let d' = AST.Decl_FunDefn (f, ps, args, rty, body', loc) in
      [ d' ]
  | Decl_ArrayGetterType (f, ps, args, rty, loc) ->
      let d' = AST.Decl_FunType (f, ps, args, rty, loc) in
      [ d' ]
  | Decl_ArraySetterDefn (f, ps, args, v, t, body, loc) ->
      let body' = replace replacer body in
      let d' = AST.Decl_ProcDefn (f, ps, args @ [ (v, t) ], body', loc) in
      [ d' ]
  | Decl_ArraySetterType (f, ps, args, v, t, loc) ->
      let d' = AST.Decl_ProcType (f, ps, args @ [ (v, t) ], loc) in
      [ d' ]
  | Decl_VarGetterDefn (f, ps, rty, body, loc) ->
      let body' = replace replacer body in
      let d' = AST.Decl_FunDefn (f, ps, [], rty, body', loc) in
      [ d' ]
  | Decl_VarGetterType (f, ps, rty, loc) ->
      let d' = AST.Decl_FunType (f, ps, [], rty, loc) in
      [ d' ]
  | Decl_VarSetterDefn (f, ps, v, t, body, loc) ->
      let body' = replace replacer body in
      let d' = AST.Decl_ProcDefn (f, ps, [ (v, t) ], body', loc) in
      [ d' ]
  | Decl_VarSetterType (f, ps, v, t, loc) ->
      let d' = AST.Decl_ProcType (f, ps, [ (v, t) ], loc) in
      [ d' ]
  | _ -> [ d ]

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let replacer = new replaceClass ds in
  List.flatten (List.map (xform_decl replacer) ds)

(****************************************************************
 * End
 ****************************************************************)
