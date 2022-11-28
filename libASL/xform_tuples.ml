(****************************************************************
 * ASL tuple elimination transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

exception Unimplemented of (AST.l * string * (Format.formatter -> unit))

let mkReturnTypeName (f : AST.ident) : AST.ident = AST.addPrefix "__Return_" f

let mkReturnFieldName (i : int) : AST.ident = AST.Ident ("r" ^ string_of_int i)

let mkReturnRecord (tyname : AST.ident) (rtys : AST.ty list) (loc : AST.l) : AST.declaration =
  let fs = List.mapi (fun i ty -> (mkReturnFieldName i, ty)) rtys in
  Decl_Record (tyname, fs, loc)

let returnVariables = new Asl_utils.nameSupply "__r"

let ifVariables = new Asl_utils.nameSupply "__t"

class replaceTupleClass (tc : AST.ident option) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vstmt s =
      match s with
      | Stmt_FunReturn (Expr_Tuple es, loc) when Option.is_some tc && List.length es > 1 ->
        let tc = Option.get tc in
        let fas = List.mapi (fun i e -> (mkReturnFieldName i, e)) es in
        let r = AST.Expr_RecordInit (tc, fas) in
        Visitor.ChangeTo [AST.Stmt_FunReturn (r, loc)]

      | Stmt_VarDecl (AST.DeclItem_Tuple dis, (AST.Expr_TApply (f, _, _) as i), loc) ->
        let vty = AST.Type_Constructor (mkReturnTypeName f) in
        let v = returnVariables#fresh in
        let s = AST.Stmt_ConstDecl (AST.DeclItem_Var (v, Some vty), i, loc) in
        let ss = List.mapi (fun i di ->
            AST.Stmt_VarDecl (di, Expr_Field (Expr_Var v, mkReturnFieldName i), loc)
          ) dis in
        Visitor.ChangeTo (s :: ss)

      | Stmt_ConstDecl (AST.DeclItem_Tuple dis, (AST.Expr_TApply (f, _, _) as i), loc) ->
        let vty = AST.Type_Constructor (mkReturnTypeName f) in
        let v = returnVariables#fresh in
        let s = AST.Stmt_ConstDecl (AST.DeclItem_Var (v, Some vty), i, loc) in
        let ss = List.mapi (fun i di ->
            AST.Stmt_ConstDecl (di, Expr_Field (Expr_Var v, mkReturnFieldName i), loc)
          ) dis in
        Visitor.ChangeTo (s :: ss)

      | Stmt_Assign (AST.LExpr_Tuple es, (AST.Expr_TApply (f, _, _) as i), loc) ->
        let vty = AST.Type_Constructor (mkReturnTypeName f) in
        let v = returnVariables#fresh in
        let s = AST.Stmt_ConstDecl (AST.DeclItem_Var (v, Some vty), i, loc) in
        let ss = List.mapi (fun i e ->
            AST.Stmt_Assign (e, Expr_Field (Expr_Var v, mkReturnFieldName i), loc)
          ) es in
        Visitor.ChangeTo (s :: ss)

      | Stmt_Assign (AST.LExpr_Tuple ls, AST.Expr_Tuple es, loc) ->
        assert (List.length ls = List.length es);
        let ss = List.map2 (fun l e -> AST.Stmt_Assign (l, e, loc)) ls es in
        Visitor.ChangeTo ss

      | Stmt_ConstDecl (AST.DeclItem_Tuple dis, AST.Expr_If (c, t, els, e), loc) ->
        let (vs, ds, ss) = List.map (fun di ->
            ( match di with
            | AST.DeclItem_Var (v, Some vty) ->
                let v' = ifVariables#fresh in
                let s1 = AST.Stmt_VarDeclsNoInit ([v'], vty, loc) in
                let s2 = AST.Stmt_ConstDecl (AST.DeclItem_Var (v, Some vty), Expr_Var v', loc) in
                (AST.LExpr_Var v', s1, s2)
            | _ ->
                raise (Unimplemented (loc, "tuple let-if", (fun fmt -> Asl_fmt.stmt fmt s)))
            )
          )
          dis
          |> Utils.split3
        in
        let t' = [AST.Stmt_Assign (AST.LExpr_Tuple vs, t, loc)] in
        let els' = List.map (fun el ->
            let AST.E_Elsif_Cond (c, t) = el in
            AST.S_Elsif_Cond (c, [AST.Stmt_Assign (AST.LExpr_Tuple vs, t, loc)], loc)
          )
          els
        in
        let e' = [AST.Stmt_Assign (AST.LExpr_Tuple vs, e, loc)] in
        let s' = AST.Stmt_If (c, t', els', (e', loc), loc) in
        let ss' = Asl_visitor.visit_stmt (self :> Asl_visitor.aslVisitor) s' in
        Visitor.ChangeTo (ds @ ss' @ ss)

      | _ -> DoChildren

  end

let xform_decl (d : AST.declaration) : AST.declaration list =
  match d with
  | Decl_FunDefn (f, ps, atys, rty, body, loc) ->

      let (tydecls, tyname, rty') = if Asl_utils.isTupleType rty then
          let tyname = mkReturnTypeName f in
          let tydecl = mkReturnRecord tyname (Asl_utils.tupleTypes rty) loc in
          ([tydecl], Some tyname, AST.Type_Constructor tyname)
        else
          ([], None, rty)
      in
      let replacer = new replaceTupleClass tyname in
      let body' = Asl_visitor.visit_stmts (replacer :> Asl_visitor.aslVisitor) body in
      let d' = AST.Decl_FunDefn (f, ps, atys, rty', body', loc) in
      List.append tydecls [d']

  | _ ->
      let replacer = new replaceTupleClass None in
      let d' = Asl_visitor.visit_decl (replacer :> Asl_visitor.aslVisitor) d in
      [d']

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  List.flatten (List.map xform_decl ds)

(****************************************************************
 * End
 ****************************************************************)
