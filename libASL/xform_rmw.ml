(****************************************************************
 * ASL read-modify-write function + procedure call transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

let getFunReturnType (d : AST.declaration) : AST.ty option =
  match d with
  | Decl_ArrayGetterDefn (f, ps, atys, rty, body, loc) -> Some rty
  | Decl_FunDefn (f, ps, atys, rty, body, loc) -> Some rty
  | Decl_VarGetterDefn (f, ps, rty, body, loc) -> Some rty
  | _ -> None

let rmwVariables = new Asl_utils.nameSupply "__rmw"

class replaceRmwClass (ds : AST.declaration list) =
  object (self)
    inherit Asl_visitor.nopAslVisitor
    val mutable le_vars : (AST.lexpr * Ident.t) list = []

    method! vlexpr e =
      match e with
      | LExpr_ReadWrite (f, g, tes, es, throws) ->
          let v = rmwVariables#fresh in
          le_vars <- (e, v) :: le_vars;
          ChangeTo (AST.LExpr_Var v)
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_Assign (_, _, loc) ->
          let post_action (ss : AST.stmt list) : AST.stmt list =
            let wrap_stmts (ss : AST.stmt list) = function
              | AST.LExpr_ReadWrite (f, g, tes, es, throws), v ->
                  let e = AST.Expr_TApply (f, tes, es, throws) in
                  let fd = Option.get (Asl_utils.find_decl f ds) in
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

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let replacer = new replaceRmwClass ds in
  List.map (Asl_visitor.visit_decl (replacer :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * End
 ****************************************************************)
