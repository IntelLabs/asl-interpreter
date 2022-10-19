(****************************************************************
 * ASL visitor class
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 *
 * This code follows the pattern used in the cilVisitor class in
 * George Necula's excellent CIL (https://people.eecs.berkeley.edu/~necula/cil/)
 * and makes use of the generic Visitor module that is copied from CIL.
 ****************************************************************)

(** ASL visitor class *)

open Asl_ast
open Visitor

(****************************************************************)
(** {2 ASL visitor class}                                       *)
(****************************************************************)

(** For each datatype in the ASL AST, a visitor defines what actions
    it wants to perform on values of that type.
 *)

class type aslVisitor =
  object
    method vvar : ident -> ident visitAction
    method ve_elsif : e_elsif -> e_elsif visitAction
    method vslice : slice -> slice visitAction
    method vpattern : pattern -> pattern visitAction
    method vexpr : expr -> expr visitAction
    method vconstraint : constraint_range -> constraint_range visitAction
    method vtype : ty -> ty visitAction
    method vlvar : ident -> ident visitAction
    method vlexpr : lexpr -> lexpr visitAction
    method vdeclitem : decl_item -> decl_item visitAction
    method vstmt : stmt -> stmt list visitAction
    method vs_elsif : s_elsif -> s_elsif visitAction
    method valt : alt -> alt visitAction
    method vcatcher : catcher -> catcher visitAction
    method vmapfield : mapfield -> mapfield visitAction
    method vdecl : declaration -> declaration visitAction
    method enter_scope : ident list -> unit
    method leave_scope : ident list -> unit
  end

(****************************************************************)
(** {2 ASL visitor functions}                                   *)
(****************************************************************)

(** The following set of recursive functions are the ASL specific
    part of the visitor class.
    For each data constructor of each datatype, they invoke visitors
    on each field of the data constructor and then reconstruct
    the corresponding data constructor.

    These functions implement the space-saving optimisation of
    only reconstructing the constructor if the sub-values are
    different.
 *)

let rec visit_exprs (vis : aslVisitor) (xs : expr list) : expr list =
  mapNoCopy (visit_expr vis) xs

and visit_var (vis : aslVisitor) (x : ident) : ident =
  let aux (_ : aslVisitor) (x : ident) : ident = x in
  doVisit vis (vis#vvar x) aux x

and visit_lvar (vis : aslVisitor) (x : ident) : ident =
  let aux (_ : aslVisitor) (x : ident) : ident = x in
  doVisit vis (vis#vlvar x) aux x

and visit_e_elsif (vis : aslVisitor) (x : e_elsif) : e_elsif =
  let aux (vis : aslVisitor) (x : e_elsif) : e_elsif =
    match x with
    | E_Elsif_Cond (c, e) ->
        let c' = visit_expr vis c in
        let e' = visit_expr vis e in
        if c == c' && e == e' then x else E_Elsif_Cond (c', e')
  in
  doVisit vis (vis#ve_elsif x) aux x

and visit_slice (vis : aslVisitor) (x : slice) : slice =
  let aux (vis : aslVisitor) (x : slice) : slice =
    match x with
    | Slice_Single e ->
        let e' = visit_expr vis e in
        if e == e' then x else Slice_Single e'
    | Slice_HiLo (hi, lo) ->
        let hi' = visit_expr vis hi in
        let lo' = visit_expr vis lo in
        if hi == hi' && lo == lo' then x else Slice_HiLo (hi', lo')
    | Slice_LoWd (lo, wd) ->
        let lo' = visit_expr vis lo in
        let wd' = visit_expr vis wd in
        if lo == lo' && wd == wd' then x else Slice_LoWd (lo', wd')
  in
  doVisit vis (vis#vslice x) aux x

and visit_patterns (vis : aslVisitor) (xs : pattern list) : pattern list =
  mapNoCopy (visit_pattern vis) xs

and visit_pattern (vis : aslVisitor) (x : pattern) : pattern =
  let aux (vis : aslVisitor) (x : pattern) : pattern =
    match x with
    | Pat_LitInt _ -> x
    | Pat_LitHex _ -> x
    | Pat_LitBits _ -> x
    | Pat_LitMask _ -> x
    | Pat_Const _ -> x
    | Pat_Wildcard -> x
    | Pat_Tuple ps ->
        let ps' = visit_patterns vis ps in
        if ps == ps' then x else Pat_Tuple ps'
    | Pat_Set ps ->
        let ps' = visit_patterns vis ps in
        if ps == ps' then x else Pat_Set ps'
    | Pat_Single e ->
        let e' = visit_expr vis e in
        if e == e' then x else Pat_Single e'
    | Pat_Range (lo, hi) ->
        let lo' = visit_expr vis lo in
        let hi' = visit_expr vis hi in
        if lo == lo' && hi == hi' then x else Pat_Range (lo', hi')
  in
  doVisit vis (vis#vpattern x) aux x

and visit_expr (vis : aslVisitor) (x : expr) : expr =
  let aux (vis : aslVisitor) (x : expr) : expr =
    match x with
    | Expr_If (c, t, els, e) ->
        let c' = visit_expr vis c in
        let t' = visit_expr vis t in
        let els' = mapNoCopy (visit_e_elsif vis) els in
        let e' = visit_expr vis e in
        if c == c' && t == t' && els == els' && e == e' then x
        else Expr_If (c', t', els', e')
    | Expr_Binop (a, op, b) ->
        let a' = visit_expr vis a in
        let b' = visit_expr vis b in
        if a == a' && b == b' then x else Expr_Binop (a', op, b')
    | Expr_Field (e, f) ->
        let e' = visit_expr vis e in
        if e == e' then x else Expr_Field (e', f)
    | Expr_Fields (e, fs) ->
        let e' = visit_expr vis e in
        if e == e' then x else Expr_Fields (e', fs)
    | Expr_Slices (e, ss) ->
        let e' = visit_expr vis e in
        let ss' = mapNoCopy (visit_slice vis) ss in
        if e == e' && ss == ss' then x else Expr_Slices (e', ss')
    | Expr_RecordInit (tc, fas) ->
        let fas' = mapNoCopy (visit_fieldassignment vis) fas in
        if fas == fas' then x else Expr_RecordInit (tc, fas')
    | Expr_In (e, p) ->
        let e' = visit_expr vis e in
        let p' = visit_pattern vis p in
        if e == e' && p == p' then x else Expr_In (e', p')
    | Expr_Var v ->
        let v' = visit_var vis v in
        if v == v' then x else Expr_Var v'
    | Expr_Parens e ->
        let e' = visit_expr vis e in
        if e == e' then x else Expr_Parens e'
    | Expr_TApply (f, tes, es) ->
        let f' = visit_var vis f in
        let tes' = visit_exprs vis tes in
        let es' = visit_exprs vis es in
        if f == f' && tes == tes' && es == es' then x
        else Expr_TApply (f', tes', es')
    | Expr_Tuple es ->
        let es' = visit_exprs vis es in
        if es == es' then x else Expr_Tuple es'
    | Expr_Concat (ws, es) ->
        let ws' = visit_exprs vis ws in
        let es' = visit_exprs vis es in
        if ws == ws' && es == es' then x else Expr_Concat (ws', es')
    | Expr_Unop (op, e) ->
        let e' = visit_expr vis e in
        if e == e' then x else Expr_Unop (op, e')
    | Expr_Unknown t ->
        let t' = visit_type vis t in
        if t == t' then x else Expr_Unknown t'
    | Expr_ImpDef (os, t) ->
        let t' = visit_type vis t in
        if t == t' then x else Expr_ImpDef (os, t')
    | Expr_Array (a, e) ->
        let a' = visit_expr vis a in
        let e' = visit_expr vis e in
        if a == a' && e == e' then x else Expr_Array (a', e')
    | Expr_LitInt _ -> x
    | Expr_LitHex _ -> x
    | Expr_LitReal _ -> x
    | Expr_LitBits _ -> x
    | Expr_LitMask _ -> x
    | Expr_LitString _ -> x
    | Expr_AsConstraint (e, c) ->
        let e' = visit_expr vis e in
        let c' = visit_constraints vis c in
        if e == e' && c == c' then x else Expr_AsConstraint (e', c')
    | Expr_AsType (e, t) ->
        let e' = visit_expr vis e in
        let t' = visit_type vis t in
        if e == e' && t == t' then x else Expr_AsType (e', t')
  in
  doVisit vis (vis#vexpr x) aux x

and visit_fieldassignment (vis : aslVisitor) (x : ident * expr) : ident * expr =
  match x with
  | f, e ->
      let e' = visit_expr vis e in
      if e == e' then x else (f, e')

and visit_constraint_range (vis : aslVisitor) (x : constraint_range) :
    constraint_range =
  let aux (vis : aslVisitor) (x : constraint_range) : constraint_range =
    match x with
    | Constraint_Single e ->
        let e' = visit_expr vis e in
        if e == e' then x else Constraint_Single e
    | Constraint_Range (lo, hi) ->
        let lo' = visit_expr vis lo in
        let hi' = visit_expr vis hi in
        if lo == lo' && hi == hi' then x else Constraint_Range (lo', hi')
  in
  doVisit vis (vis#vconstraint x) aux x

and visit_constraints (vis : aslVisitor) (x : constraint_range list) :
    constraint_range list =
  mapNoCopy (visit_constraint_range vis) x

and visit_types (vis : aslVisitor) (xs : ty list) : ty list =
  mapNoCopy (visit_type vis) xs

and visit_type (vis : aslVisitor) (x : ty) : ty =
  let aux (vis : aslVisitor) (x : ty) : ty =
    match x with
    | Type_Constructor _ -> x
    | Type_Integer ocrs ->
        let ocrs' = mapOptionNoCopy (visit_constraints vis) ocrs in
        if ocrs == ocrs' then x else Type_Integer ocrs'
    | Type_Bits n ->
        let n' = visit_expr vis n in
        if n == n' then x else Type_Bits n'
    | Type_App (tc, es) ->
        let es' = visit_exprs vis es in
        if es == es' then x else Type_App (tc, es')
    | Type_OfExpr e ->
        let e' = visit_expr vis e in
        if e == e' then x else Type_OfExpr e'
    | Type_Register (wd, fs) ->
        let fs' =
          mapNoCopy
            (fun ((ss, f) as r) ->
              let ss' = mapNoCopy (visit_slice vis) ss in
              if ss == ss' then r else (ss', f))
            fs
        in
        if fs == fs' then x else Type_Register (wd, fs')
    | Type_Array (Index_Enum tc, ety) ->
        let ety' = visit_type vis ety in
        if ety == ety' then x else Type_Array (Index_Enum tc, ety')
    | Type_Array (Index_Int sz, ety) ->
        let sz' = visit_expr vis sz in
        let ety' = visit_type vis ety in
        if sz == sz' && ety == ety' then x else Type_Array (Index_Int sz', ety')
    | Type_Tuple tys ->
        let tys' = visit_types vis tys in
        if tys == tys' then x else Type_Tuple tys'
  in
  doVisit vis (vis#vtype x) aux x

let rec visit_lexprs (vis : aslVisitor) (xs : lexpr list) : lexpr list =
  mapNoCopy (visit_lexpr vis) xs

and visit_lexpr (vis : aslVisitor) (x : lexpr) : lexpr =
  let aux (vis : aslVisitor) (x : lexpr) : lexpr =
    match x with
    | LExpr_Wildcard -> x
    | LExpr_Var v ->
        let v' = visit_lvar vis v in
        if v == v' then x else LExpr_Var v'
    | LExpr_Field (e, f) ->
        let e' = visit_lexpr vis e in
        if e == e' then x else LExpr_Field (e', f)
    | LExpr_Fields (e, fs) ->
        let e' = visit_lexpr vis e in
        if e == e' then x else LExpr_Fields (e', fs)
    | LExpr_Slices (e, ss) ->
        let e' = visit_lexpr vis e in
        let ss' = mapNoCopy (visit_slice vis) ss in
        if e == e' && ss == ss' then x else LExpr_Slices (e', ss')
    | LExpr_BitTuple es ->
        let es' = mapNoCopy (visit_lexpr vis) es in
        if es == es' then x else LExpr_BitTuple es'
    | LExpr_Tuple es ->
        let es' = mapNoCopy (visit_lexpr vis) es in
        if es == es' then x else LExpr_Tuple es'
    | LExpr_Array (a, e) ->
        let a' = visit_lexpr vis a in
        let e' = visit_expr vis e in
        if a == a' && e == e' then x else LExpr_Array (a', e')
    | LExpr_Write (f, tes, es) ->
        let f' = visit_var vis f in
        let tes' = visit_exprs vis tes in
        let es' = visit_exprs vis es in
        if f == f' && tes == tes' && es == es' then x
        else LExpr_Write (f, tes', es')
    | LExpr_ReadWrite (f, g, tes, es) ->
        let f' = visit_var vis f in
        let g' = visit_var vis g in
        let tes' = visit_exprs vis tes in
        let es' = visit_exprs vis es in
        if f == f' && g == g' && tes == tes' && es == es' then x
        else LExpr_ReadWrite (f, g, tes', es')
  in
  doVisit vis (vis#vlexpr x) aux x

let with_locals (ls : ident list) (vis : aslVisitor) (f : aslVisitor -> 'a) : 'a
    =
  vis#enter_scope ls;
  let result = f vis in
  vis#leave_scope ls;
  result

let rec locals_of_declitem (x : decl_item) : ident list =
  match x with
  | DeclItem_Var (v, _) -> [ v ]
  | DeclItem_Tuple dis -> List.concat_map locals_of_declitem dis
  | DeclItem_Wildcard _ -> []

let locals_of_stmt (x : stmt) : ident list =
  match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) -> vs
  | Stmt_VarDecl (dis, i, loc)
  | Stmt_ConstDecl (dis, i, loc) -> locals_of_declitem dis
  | _ -> []

let rec visit_decl_item (vis : aslVisitor) (x : decl_item) : decl_item =
  match x with
  | DeclItem_Var (v, oty) ->
      let v' = visit_lvar vis v in
      let oty' = mapOptionNoCopy (visit_type vis) oty in
      if v == v' && oty == oty' then x else DeclItem_Var (v', oty')
  | DeclItem_Tuple dis ->
      let dis' = mapNoCopy (visit_decl_item vis) dis in
      if dis == dis' then x else DeclItem_Tuple dis'
  | DeclItem_Wildcard oty ->
      let oty' = mapOptionNoCopy (visit_type vis) oty in
      if oty == oty' then x else DeclItem_Wildcard oty'

(* todo: should probably make this more like cil visitor and allow
 * visit_stmt to generate a list of statements and provide a mechanism to emit
 * statements to be inserted before/after the statement being transformed
 *)
let rec visit_stmts (vis : aslVisitor) (xs : stmt list) : stmt list =
  with_locals (List.concat_map locals_of_stmt xs) vis (fun vis ->
      mapNoCopyList (visit_stmt vis) xs)

and visit_stmt (vis : aslVisitor) (x : stmt) : stmt list =
  let aux (vis : aslVisitor) (x : stmt) : stmt =
    match x with
    | Stmt_VarDeclsNoInit (vs, ty, loc) ->
        let ty' = visit_type vis ty in
        let vs' = mapNoCopy (visit_lvar vis) vs in
        if ty == ty' && vs == vs' then x else Stmt_VarDeclsNoInit (vs', ty', loc)
    | Stmt_VarDecl (di, i, loc) ->
        let di' = visit_decl_item vis di in
        let i' = visit_expr vis i in
        if di == di' && i == i' then x else Stmt_VarDecl (di', i', loc)
    | Stmt_ConstDecl (di, i, loc) ->
        let di' = visit_decl_item vis di in
        let i' = visit_expr vis i in
        if di == di' && i == i' then x else Stmt_ConstDecl (di', i', loc)
    | Stmt_Assign (l, r, loc) ->
        let l' = visit_lexpr vis l in
        let r' = visit_expr vis r in
        if l == l' && r == r' then x else Stmt_Assign (l', r', loc)
    | Stmt_TCall (f, tes, args, loc) ->
        let f' = visit_var vis f in
        let tes' = visit_exprs vis tes in
        let args' = visit_exprs vis args in
        if f == f' && tes == tes' && args == args' then x
        else Stmt_TCall (f', tes', args', loc)
    | Stmt_FunReturn (e, loc) ->
        let e' = visit_expr vis e in
        if e == e' then x else Stmt_FunReturn (e', loc)
    | Stmt_ProcReturn _ -> x
    | Stmt_Assert (e, loc) ->
        let e' = visit_expr vis e in
        if e == e' then x else Stmt_Assert (e', loc)
    | Stmt_Throw (v, loc) ->
        let v' = visit_var vis v in
        if v == v' then x else Stmt_Throw (v', loc)
    | Stmt_Block (b, loc) ->
        let b' = visit_stmts vis b in
        if b == b' then x else Stmt_Block (b', loc)
    | Stmt_If (c, t, els, (e, el), loc) ->
        let c' = visit_expr vis c in
        let t' = visit_stmts vis t in
        let els' = mapNoCopy (visit_s_elsif vis) els in
        let e' = visit_stmts vis e in
        if c == c' && t == t' && els == els' && e == e' then x
        else Stmt_If (c', t', els', (e', el), loc)
    | Stmt_Case (e, alts, ob, loc) ->
        let e' = visit_expr vis e in
        let alts' = mapNoCopy (visit_alt vis) alts in
        let ob' = mapOptionNoCopy (fun (b, bl) -> (visit_stmts vis b, bl)) ob in
        if e == e' && alts == alts' && ob == ob' then x
        else Stmt_Case (e', alts', ob', loc)
    | Stmt_For (v, f, dir, t, b, loc) ->
        let v' = visit_lvar vis v in
        let f' = visit_expr vis f in
        let t' = visit_expr vis t in
        let b' = with_locals [ v' ] vis visit_stmts b in
        if v == v' && f == f' && t == t' && b == b' then x
        else Stmt_For (v', f', dir, t', b', loc)
    | Stmt_While (c, b, loc) ->
        let c' = visit_expr vis c in
        let b' = visit_stmts vis b in
        if c == c' && b == b' then x else Stmt_While (c', b', loc)
    | Stmt_Repeat (b, c, pos, loc) ->
        let b' = visit_stmts vis b in
        let c' = visit_expr vis c in
        if b == b' && c == c' then x else Stmt_Repeat (b', c', pos, loc)
    | Stmt_Try (b, v, pos, cs, ob, loc) ->
        let b' = visit_stmts vis b in
        let v' = visit_lvar vis v in
        let cs' = mapNoCopy (with_locals [ v' ] vis visit_catcher) cs in
        let ob' =
          mapOptionNoCopy
            (fun (b, bl) -> (with_locals [ v' ] vis visit_stmts b, bl))
            ob
        in
        if b == b' && v == v' && cs == cs' && ob == ob' then x
        else Stmt_Try (b', v', pos, cs', ob', loc)
  in
  doVisitList vis (vis#vstmt x) aux x

and visit_s_elsif (vis : aslVisitor) (x : s_elsif) : s_elsif =
  let aux (vis : aslVisitor) (x : s_elsif) : s_elsif =
    match x with
    | S_Elsif_Cond (c, b, loc) ->
        let c' = visit_expr vis c in
        let b' = visit_stmts vis b in
        if c == c' && b == b' then x else S_Elsif_Cond (c', b', loc)
  in
  doVisit vis (vis#vs_elsif x) aux x

and visit_alt (vis : aslVisitor) (x : alt) : alt =
  let aux (vis : aslVisitor) (x : alt) : alt =
    match x with
    | Alt_Alt (ps, oc, b, loc) ->
        let ps' = visit_patterns vis ps in
        let oc' = mapOptionNoCopy (visit_expr vis) oc in
        let b' = visit_stmts vis b in
        if ps == ps' && oc == oc' && b == b' then x
        else Alt_Alt (ps', oc', b', loc)
  in
  doVisit vis (vis#valt x) aux x

and visit_catcher (vis : aslVisitor) (x : catcher) : catcher =
  let aux (vis : aslVisitor) (x : catcher) : catcher =
    match x with
    | Catcher_Guarded (c, b, loc) ->
        let c' = visit_expr vis c in
        let b' = visit_stmts vis b in
        if c == c' && b == b' then x else Catcher_Guarded (c', b', loc)
  in
  doVisit vis (vis#vcatcher x) aux x

let visit_mapfield (vis : aslVisitor) (x : mapfield) : mapfield =
  let aux (vis : aslVisitor) (x : mapfield) : mapfield =
    match x with
    | MapField_Field (v, p) ->
        let v' = visit_var vis v in
        let p' = visit_pattern vis p in
        if v == v' && p == p' then x else MapField_Field (v', p')
  in
  doVisit vis (vis#vmapfield x) aux x

let visit_parameter (vis : aslVisitor) (x : ident * ty option) :
    ident * ty option =
  match x with
  | v, oty ->
      let oty' = mapOptionNoCopy (visit_type vis) oty in
      let v' = visit_var vis v in
      if oty == oty' && v == v' then x else (v', oty')

let visit_parameters (vis : aslVisitor) (xs : (ident * ty option) list) :
    (ident * ty option) list =
  mapNoCopy (visit_parameter vis) xs

let visit_arg (vis : aslVisitor) (x : ident * ty) : ident * ty =
  match x with
  | v, ty ->
      let ty' = visit_type vis ty in
      let v' = visit_var vis v in
      if ty == ty' && v == v' then x else (v', ty')

let visit_args (vis : aslVisitor) (xs : (ident * ty) list) : (ident * ty) list =
  mapNoCopy (visit_arg vis) xs

let visit_decl (vis : aslVisitor) (x : declaration) : declaration =
  let aux (vis : aslVisitor) (x : declaration) : declaration =
    match x with
    | Decl_BuiltinType (v, loc) ->
        let v' = visit_var vis v in
        if v == v' then x else Decl_BuiltinType (v', loc)
    | Decl_Forward (v, loc) ->
        let v' = visit_var vis v in
        if v == v' then x else Decl_Forward (v', loc)
    | Decl_Record (v, fs, loc) ->
        let v' = visit_var vis v in
        let fs' = visit_args vis fs in
        if v == v' && fs == fs' then x else Decl_Record (v', fs', loc)
    | Decl_Typedef (v, ty, loc) ->
        let v' = visit_var vis v in
        let ty' = visit_type vis ty in
        if v == v' && ty == ty' then x else Decl_Typedef (v', ty', loc)
    | Decl_Enum (v, es, loc) ->
        let v' = visit_var vis v in
        let es' = mapNoCopy (visit_var vis) es in
        if v == v' && es == es' then x else Decl_Enum (v', es', loc)
    | Decl_Var (v, ty, loc) ->
        let ty' = visit_type vis ty in
        let v' = visit_var vis v in
        if ty == ty' && v == v' then x else Decl_Var (v', ty', loc)
    | Decl_Const (v, ty, e, loc) ->
        let ty' = visit_type vis ty in
        let v' = visit_var vis v in
        let e' = visit_expr vis e in
        if ty == ty' && v == v' && e == e' then x
        else Decl_Const (v', ty', e', loc)
    | Decl_BuiltinFunction (f, ps, args, ty, loc) ->
        let ty' = visit_type vis ty in
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        if ty == ty' && f == f' && ps == ps' && args == args' then x
        else Decl_BuiltinFunction (f', ps', args', ty', loc)
    | Decl_FunType (f, ps, args, ty, loc) ->
        let ty' = visit_type vis ty in
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        if ty == ty' && f == f' && ps == ps' && args == args' then x
        else Decl_FunType (f', ps', args', ty', loc)
    | Decl_FunDefn (f, ps, args, ty, b, loc) ->
        let ty' = visit_type vis ty in
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        let b' = with_locals (List.map fst args') vis visit_stmts b in
        if ty == ty' && f == f' && ps == ps' && args == args' && b == b' then x
        else Decl_FunDefn (f', ps', args', ty', b', loc)
    | Decl_ProcType (f, ps, args, loc) ->
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        if f == f' && ps == ps' && args == args' then x
        else Decl_ProcType (f', ps', args', loc)
    | Decl_ProcDefn (f, ps, args, b, loc) ->
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        let b' = with_locals (List.map fst args') vis visit_stmts b in
        if f == f' && ps == ps' && args == args' && b == b' then x
        else Decl_ProcDefn (f', ps', args', b', loc)
    | Decl_VarGetterType (f, ps, ty, loc) ->
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let ty' = visit_type vis ty in
        if ty == ty' && f == f' then x
        else Decl_VarGetterType (f', ps', ty', loc)
    | Decl_VarGetterDefn (f, ps, ty, b, loc) ->
        let ty' = visit_type vis ty in
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let b' = visit_stmts vis b in
        if ty == ty' && f == f' && b == b' then x
        else Decl_VarGetterDefn (f', ps', ty', b', loc)
    | Decl_ArrayGetterType (f, ps, args, ty, loc) ->
        let ty' = visit_type vis ty in
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        if ty == ty' && f == f' && ps == ps' && args == args' then x
        else Decl_ArrayGetterType (f', ps', args', ty', loc)
    | Decl_ArrayGetterDefn (f, ps, args, ty, b, loc) ->
        let ty' = visit_type vis ty in
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        let b' = with_locals (List.map fst args') vis visit_stmts b in
        if ty == ty' && f == f' && ps == ps' && args == args' && b == b' then x
        else Decl_ArrayGetterDefn (f', ps', args', ty', b', loc)
    | Decl_VarSetterType (f, ps, v, ty, loc) ->
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let ty' = visit_type vis ty in
        let v' = visit_var vis v in
        if f == f' && ty == ty' && v == v' then x
        else Decl_VarSetterType (f', ps', v', ty', loc)
    | Decl_VarSetterDefn (f, ps, v, ty, b, loc) ->
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let ty' = visit_type vis ty in
        let v' = visit_var vis v in
        let b' = with_locals [ v' ] vis visit_stmts b in
        if f == f' && ty == ty' && v == v' && b == b' then x
        else Decl_VarSetterDefn (f', ps', v', ty', b', loc)
    | Decl_ArraySetterType (f, ps, args, v, ty, loc) ->
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        let ty' = visit_type vis ty in
        let v' = visit_var vis v in
        if f == f' && ps == ps' && args == args' && ty == ty' && v == v' then x
        else Decl_ArraySetterType (f', ps', args', v', ty', loc)
    | Decl_ArraySetterDefn (f, ps, args, v, ty, b, loc) ->
        let f' = visit_var vis f in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        let ty' = visit_type vis ty in
        let v' = visit_var vis v in
        let b' = with_locals (List.map fst args' @ [ v' ]) vis visit_stmts b in
        if f == f' && args == args' && ty == ty' && v == v' && b == b' then x
        else Decl_ArraySetterDefn (f', ps', args', v', ty', b', loc)
    | Decl_Operator1 (op, vs, loc) ->
        let vs' = mapNoCopy (visit_var vis) vs in
        if vs == vs' then x else Decl_Operator1 (op, vs', loc)
    | Decl_Operator2 (op, vs, loc) ->
        let vs' = mapNoCopy (visit_var vis) vs in
        if vs == vs' then x else Decl_Operator2 (op, vs', loc)
    | Decl_NewEventDefn (v, ps, args, loc) ->
        let v' = visit_var vis v in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        if v == v' && ps == ps' && args == args' then x
        else Decl_NewEventDefn (v', ps', args', loc)
    | Decl_EventClause (v, b, loc) ->
        let v' = visit_var vis v in
        let b' = visit_stmts vis b in
        if v == v' && b == b' then x else Decl_EventClause (v', b', loc)
    | Decl_NewMapDefn (v, ps, args, ty, b, loc) ->
        let ty' = visit_type vis ty in
        let v' = visit_var vis v in
        let ps' = visit_parameters vis ps in
        let args' = visit_args vis args in
        let b' = with_locals (List.map fst args') vis visit_stmts b in
        if v == v' && ps == ps' && args == args' && b == b' then x
        else Decl_NewMapDefn (v', ps', args', ty', b', loc)
    | Decl_MapClause (v, fs, oc, b, loc) ->
        let v' = visit_var vis v in
        let fs' = mapNoCopy (visit_mapfield vis) fs in
        let oc' = mapOptionNoCopy (visit_expr vis) oc in
        let b' = visit_stmts vis b in
        if v == v' && fs == fs' && oc == oc' && b == b' then x
        else Decl_MapClause (v', fs', oc', b', loc)
    | Decl_Config (v, ty, e, loc) ->
        let ty' = visit_type vis ty in
        let v' = visit_var vis v in
        let e' = visit_expr vis e in
        if ty == ty' && v == v' && e == e' then x
        else Decl_Config (v', ty', e', loc)
  in

  doVisit vis (vis#vdecl x) aux x

(****************************************************************)
(** {2 nopAslVisitor class}                                     *)
(****************************************************************)

(** The nopAslVisitor class defines a visitor that recursively
    visits the entire tree making no change.
    In practice, all uses of the visitor framework are based on defining
    a subclass of this type.
 *)

class nopAslVisitor : aslVisitor =
  object
    method vvar (_ : ident) = DoChildren
    method ve_elsif (_ : e_elsif) = DoChildren
    method vslice (_ : slice) = DoChildren
    method vpattern (_ : pattern) = DoChildren
    method vexpr (_ : expr) = DoChildren
    method vconstraint (_ : constraint_range) = DoChildren
    method vtype (_ : ty) = DoChildren
    method vlvar (_ : ident) = DoChildren
    method vlexpr (_ : lexpr) = DoChildren
    method vdeclitem (_ : decl_item) = DoChildren
    method vstmt (_ : stmt) = DoChildren
    method vs_elsif (_ : s_elsif) = DoChildren
    method valt (_ : alt) = DoChildren
    method vcatcher (_ : catcher) = DoChildren
    method vmapfield (_ : mapfield) = DoChildren
    method vdecl (_ : declaration) = DoChildren
    method enter_scope _ = ()
    method leave_scope _ = ()
  end

(****************************************************************
 * End
 ****************************************************************)
