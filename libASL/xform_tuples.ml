(****************************************************************
 * ASL tuple elimination transform
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

let mkReturnTypeName (f : Ident.t) : Ident.t =
  Ident.add_prefix f ~prefix:"__Return"

let mkReturnFieldName (i : int) : Ident.t = Ident.mk_ident ("r" ^ string_of_int i)

let mkReturnRecord (tyname : Ident.t) (rtys : AST.ty list) (loc : Loc.t) : AST.declaration =
  let fs = List.mapi (fun i ty -> (mkReturnFieldName i, ty)) rtys in
  Decl_Record (tyname, [], fs, loc)

let returnVariables = new Asl_utils.nameSupply "__r"

(* Transform a conditional assignment 'l = if c then t elsif els else e;'
   to an if statement 'if c then l = t; elsif ... else l = e; end'
 *)
let cond_assign (l : AST.lexpr) (c : AST.expr) (t : AST.expr) (els : AST.e_elsif list) (e : AST.expr) (loc : Loc.t) : AST.stmt =
  let t' = AST.Stmt_Assign(l, t, loc) in
  let els' = List.map (function AST.E_Elsif_Cond (c, e) ->
          AST.S_Elsif_Cond (c, [AST.Stmt_Assign(l, e, loc)], loc)
      )
      els
  in
  let e' = AST.Stmt_Assign(l, e, loc) in
  AST.Stmt_If(c, [t'], els', ([e'], loc), loc)

class replaceTupleClass (tc : Ident.t option) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vstmt s =
      match s with
      (* function return *)
      | Stmt_FunReturn (Expr_Tuple es, loc) when Option.is_some tc && List.length es > 1 ->
        let tc = Option.get tc in
        let fas = List.mapi (fun i e -> (mkReturnFieldName i, e)) es in
        let r = AST.Expr_RecordInit (tc, [], fas) in
        Visitor.ChangeTo [AST.Stmt_FunReturn (r, loc)]

      (* function calls *)
      | Stmt_VarDecl (AST.DeclItem_Tuple dis, (AST.Expr_TApply (f, _, _, _) as i), loc) ->
        let vty = AST.Type_Constructor (mkReturnTypeName f, []) in
        let v = returnVariables#fresh in
        let s = AST.Stmt_ConstDecl (AST.DeclItem_Var (v, Some vty), i, loc) in
        let ss = List.mapi (fun i di ->
            AST.Stmt_VarDecl (di, Expr_Field (Expr_Var v, mkReturnFieldName i), loc)
          ) dis in
        Visitor.ChangeTo (s :: ss)

      | Stmt_ConstDecl (AST.DeclItem_Tuple dis, (AST.Expr_TApply (f, _, _, _) as i), loc) ->
        let vty = AST.Type_Constructor (mkReturnTypeName f, []) in
        let v = returnVariables#fresh in
        let s = AST.Stmt_ConstDecl (AST.DeclItem_Var (v, Some vty), i, loc) in
        let ss = List.mapi (fun i di ->
            AST.Stmt_ConstDecl (di, Expr_Field (Expr_Var v, mkReturnFieldName i), loc)
          ) dis in
        Visitor.ChangeTo (s :: ss)

      | Stmt_Assign (AST.LExpr_Tuple es, (AST.Expr_TApply (f, _, _, _) as i), loc) ->
        let vty = AST.Type_Constructor (mkReturnTypeName f, []) in
        let v = returnVariables#fresh in
        let s = AST.Stmt_ConstDecl (AST.DeclItem_Var (v, Some vty), i, loc) in
        let ss = List.mapi (fun i e ->
            AST.Stmt_Assign (e, Expr_Field (Expr_Var v, mkReturnFieldName i), loc)
          ) es in
        Visitor.ChangeTo (s :: ss)

      (* tuple assignment: (a, b) = (x, y); *)
      | Stmt_ConstDecl (AST.DeclItem_Tuple dis, AST.Expr_Tuple es, loc) ->
        let ss = List.map2 (fun di e -> AST.Stmt_ConstDecl (di, e, loc)) dis es in
        Visitor.ChangeTo ss

      | Stmt_VarDecl (AST.DeclItem_Tuple dis, AST.Expr_Tuple es, loc) ->
        let ss = List.map2 (fun di e -> AST.Stmt_VarDecl (di, e, loc)) dis es in
        Visitor.ChangeTo ss

      | Stmt_Assign (AST.LExpr_Tuple ls, AST.Expr_Tuple es, loc) ->
        let ss = List.map2 (fun l e -> AST.Stmt_Assign (l, e, loc)) ls es in
        Visitor.ChangeTo ss

      (* conditional tuple assignment: (a, b) = if _ then _ else _ *)
      | Stmt_Assign (AST.LExpr_Tuple ls as l, AST.Expr_If(c, t, els, e), loc) ->
        let s = cond_assign l c t els e loc in
        Visitor.ChangeTo (Asl_visitor.visit_stmt (self :> Asl_visitor.aslVisitor) s)

      | Stmt_ConstDecl (AST.DeclItem_Tuple dis, AST.Expr_If (c, t, els, e), loc) ->
        let (vs, ds) = List.map (fun di ->
            ( match di with
            | AST.DeclItem_Var (v, Some vty) ->
                let s = AST.Stmt_VarDeclsNoInit ([v], vty, loc) in
                (AST.LExpr_Var v, s)
            | _ ->
                raise (Error.Unimplemented (loc, "tuple let-if", (fun fmt -> Asl_fmt.stmt fmt s)))
            )
          )
          dis
          |> List.split
        in
        let l' = AST.LExpr_Tuple vs in
        let s = cond_assign l' c t els e loc in
        Visitor.ChangeTo (ds @ Asl_visitor.visit_stmt (self :> Asl_visitor.aslVisitor) s)

      | Stmt_VarDecl (AST.DeclItem_Tuple dis, AST.Expr_If (c, t, els, e), loc) ->
        let (vs, ds) = List.map (fun di ->
            ( match di with
            | AST.DeclItem_Var (v, Some vty) ->
                let s = AST.Stmt_VarDeclsNoInit ([v], vty, loc) in
                (AST.LExpr_Var v, s)
            | _ ->
                raise (Error.Unimplemented (loc, "tuple var-if", (fun fmt -> Asl_fmt.stmt fmt s)))
            )
          )
          dis
          |> List.split
        in
        let l' = AST.LExpr_Tuple vs in
        let s = cond_assign l' c t els e loc in
        Visitor.ChangeTo (ds @ Asl_visitor.visit_stmt (self :> Asl_visitor.aslVisitor) s)

      | _ -> DoChildren

  end

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let replacer = new replaceTupleClass None in
  Asl_visitor.visit_stmts (replacer :> Asl_visitor.aslVisitor) ss

let isTupleReturnType (fty : AST.function_type) : bool =
  ( match fty.rty with
  | None -> false
  | Some ty -> Asl_utils.isTupleType ty
  )

let xform_decl (d : AST.declaration) : AST.declaration list =
  match d with
  | Decl_FunDefn (f, fty, body, loc) when isTupleReturnType fty ->
      let tyname = mkReturnTypeName f in
      let tydecl = mkReturnRecord tyname (Asl_utils.tupleTypes (Option.get fty.rty)) loc in
      let rty' = AST.Type_Constructor (tyname, []) in
      let fty' = { fty with rty = Some rty' } in
      let replacer = new replaceTupleClass (Some tyname) in
      let body' = Asl_visitor.visit_stmts (replacer :> Asl_visitor.aslVisitor) body in
      let d' = AST.Decl_FunDefn (f, fty', body', loc) in
      [tydecl; d']

  | Decl_FunType (f, fty, loc) when isTupleReturnType fty ->
      let tyname = mkReturnTypeName f in
      let rty' = AST.Type_Constructor (tyname, []) in
      let fty' = { fty with rty = Some rty' } in
      let d' = AST.Decl_FunType (f, fty', loc) in
      [d']

  | _ ->
      let replacer = new replaceTupleClass None in
      let d' = Asl_visitor.visit_decl (replacer :> Asl_visitor.aslVisitor) d in
      [d']

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  List.flatten (List.map xform_decl ds)

(****************************************************************
 * Command: :xform_tuples
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_tuples" [] [] [] "Eliminate anonymous tuples" cmd

(****************************************************************
 * End
 ****************************************************************)
