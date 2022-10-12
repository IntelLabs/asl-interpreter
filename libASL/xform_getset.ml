(****************************************************************
 * ASL getters and setters elimination transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

class replaceSetterClass =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vstmt s =
      match s with
      | Stmt_Assign (AST.LExpr_Write (f, [], []), r, loc) ->
          let s = AST.Stmt_TCall (f, [], [ r ], loc) in
          Visitor.ChangeTo [ s ]
      | _ -> DoChildren
  end

let xform_decl (d : AST.declaration) : AST.declaration list =
  match d with
  | Decl_FunDefn (f, ps, args, rty, body, loc) ->
      let replacer = new replaceSetterClass in
      let body' =
        Asl_visitor.visit_stmts (replacer :> Asl_visitor.aslVisitor) body
      in
      let d' = AST.Decl_FunDefn (f, ps, args, rty, body', loc) in
      [ d' ]
  | Decl_ProcDefn (f, ps, args, body, loc) ->
      let replacer = new replaceSetterClass in
      let body' =
        Asl_visitor.visit_stmts (replacer :> Asl_visitor.aslVisitor) body
      in
      let d' = AST.Decl_ProcDefn (f, ps, args, body', loc) in
      [ d' ]
  | Decl_VarGetterDefn (f, ps, rty, body, loc) ->
      let d' = AST.Decl_FunDefn (f, ps, [], rty, body, loc) in
      [ d' ]
  | Decl_VarGetterType (f, ps, rty, loc) ->
      let d' = AST.Decl_FunType (f, ps, [], rty, loc) in
      [ d' ]
  | Decl_VarSetterDefn (f, ps, v, t, body, loc) ->
      let d' = AST.Decl_ProcDefn (f, ps, [ (v, t) ], body, loc) in
      [ d' ]
  | Decl_VarSetterType (f, ps, v, t, loc) ->
      let d' = AST.Decl_ProcType (f, ps, [ (v, t) ], loc) in
      [ d' ]
  | _ -> [ d ]

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  List.flatten (List.map xform_decl ds)

(****************************************************************
 * End
 ****************************************************************)
