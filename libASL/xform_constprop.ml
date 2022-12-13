(****************************************************************
 * ASL constant propagation transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL constant propagation transform *)

module AST = Asl_ast
open Asl_utils
open AST
open Utils
module Concrete = Value

let unroll_loops : bool ref = ref true

module Value = struct
  type t = Concrete.value

  let pp = Concrete.pp_value
  let equal (x : t) (y : t) : bool = Concrete.eq_value x y
end

module Values = Lattice.Const (Value)

module Env : sig
  type value = Values.t
  type t

  val globals : t -> Eval.GlobalEnv.t
  val newEnv : Eval.GlobalEnv.t -> t
  val nest : t -> (t -> 'a) -> 'a
  val seq : (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b
  val fork_join : (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b
  val to_concrete : t -> Eval.Env.t
  val set_bottom : t -> unit
  val fun_return : t -> value -> unit
  val proc_return : t -> unit
  val addLocalVar : t -> ident -> value -> unit
  val addLocalConst : t -> ident -> value -> unit
  val getVar : t -> ident -> value
  val setVar : t -> ident -> value -> unit
  val pp : t -> unit
end = struct
  type value = Values.t

  type t = {
    globalConsts : Eval.GlobalEnv.t;
    mutable locals : value ScopeStack.t;
    mutable return_value : value;
  }

  let pp (env : t) : unit =
    Printf.printf "globals\n";
    Eval.GlobalEnv.pp env.globalConsts;
    Printf.printf "locals\n";
    ScopeStack.pp Values.pp_abstract env.locals;
    Printf.printf "return = %s\n" (Values.pp_abstract env.return_value)

  let globals (env : t) : Eval.GlobalEnv.t = env.globalConsts

  let newEnv (genv : Eval.GlobalEnv.t) =
    {
      globalConsts = genv;
      locals = ScopeStack.empty ();
      return_value = Values.top;
    }

  let nest (env : t) (k : t -> 'a) : 'a =
    ScopeStack.nest env.locals (fun newenv ->
        let env' =
          {
            globalConsts = env.globalConsts;
            locals = newenv;
            return_value = env.return_value;
          }
        in
        let r = k env' in
        env.return_value <- env'.return_value;
        r)

  let seq (m : t -> 'a) (k : t -> 'b) (env : t) : 'a * 'b =
    let a = m env in
    let r = env.return_value in
    let b = k env in
    env.return_value <- Values.lub env.return_value r;
    (* REVISIT *)
    (a, b)

  let fork_join (f : t -> 'a) (g : t -> 'b) (env : t) : 'a * 'b =
    let env' =
      {
        globalConsts = env.globalConsts;
        locals = ScopeStack.clone env.locals;
        return_value = env.return_value;
      }
    in
    let a = f env in
    let b = g env' in
    ScopeStack.merge_inplace Values.glb env.locals env'.locals;
    env.return_value <- Values.glb env.return_value env'.return_value;
    (a, b)

  let set_bottom (env : t) : unit =
    ScopeStack.map_inplace (Fun.const Values.bottom) env.locals;
    env.return_value <- Values.bottom

  let to_concrete (env : t) : Eval.Env.t =
    let locals = ScopeStack.filter_map Values.to_concrete env.locals in
    Eval.Env.mkEnv env.globalConsts locals

  let fun_return (env : t) (r : value) : unit =
    env.return_value <- r;
    ScopeStack.map_inplace (Fun.const Values.top) env.locals

  let proc_return (env : t) : unit =
    ScopeStack.map_inplace (Fun.const Values.top) env.locals

  let addLocalVar (env : t) (x : ident) (v : value) : unit =
    ScopeStack.add env.locals x v

  let addLocalConst (env : t) (x : ident) (v : value) : unit =
    (* todo: should constants be held separately from local vars? *)
    ScopeStack.add env.locals x v

  let getVar (env : t) (x : ident) : value =
    from_option (ScopeStack.get env.locals x) (fun _ ->
        from_option
          (Option.map Values.singleton
             (Eval.GlobalEnv.getGlobalConstOpt env.globalConsts x))
          (fun _ ->
            raise (Concrete.EvalError (Unknown, "getVar: " ^ pprint_ident x))))

  let setVar (env : t) (x : ident) (v : value) : unit =
    ignore (ScopeStack.set env.locals x v)
end

let mkEnv (genv : Eval.GlobalEnv.t) (values: (AST.ident * Concrete.value) list) : Env.t =
  let env = Env.newEnv genv in
  List.iter (fun (x, v) -> Env.addLocalConst env x (Values.singleton v)) values;
  env

let isConstant (env : Env.t) (x : expr) : bool =
  match x with
  | Expr_Var v -> Option.is_some (Eval.GlobalEnv.getGlobalConstOpt (Env.globals env) v)
  | _ -> Asl_utils.is_literal_constant x

let value_of_constant (x : expr) : Concrete.value option =
  match x with
  | Expr_LitInt i -> Some (Concrete.from_intLit i)
  | Expr_LitHex i -> Some (Concrete.from_hexLit i)
  | Expr_LitReal r -> Some (Concrete.from_realLit r)
  | Expr_LitBits b -> Some (Concrete.from_bitsLit b)
  | Expr_LitMask b -> Some (Concrete.from_maskLit b)
  | Expr_LitString s -> Some (Concrete.from_stringLit s)
  | _ -> None

let expr_value (env : Env.t) (x : AST.expr) : Values.t =
  if isConstant env x then
    let env0 = Eval.Env.newEnv (Env.globals env) in
    Values.singleton (Eval.eval_expr Unknown env0 x)
  else Values.bottom

let rec value_to_expr (x : Concrete.value) : expr option =
  match x with
  | VBool b -> Some (Expr_Var (Ident (if b then "TRUE" else "FALSE")))
  | VEnum (v, _) -> Some (Expr_Var v)
  | VInt v -> Some (Asl_utils.mk_litbigint v)
  | VBits v ->
      if v.n = 0 then Some (Expr_LitBits "")
      else
        let s = Z.format "%0b" v.v in
        let pad = String.make (v.n - String.length s) '0' in
        Some (Expr_LitBits (pad ^ s))
  (*
    | VReal v -> _
    *)
  | VString v -> Some (Expr_LitString v)
  | VTuple vs ->
      Option.map (fun es -> Expr_Tuple es) (flatten_map_option value_to_expr vs)
  | _ -> None

let algebraic_simplifications (x : expr) : expr =
  match x with
  (* [x, '', y] == [x,y] *)
  (* [] == '' *)
  | Expr_Concat (ws, xs) ->
      let (ws', xs') =
        List.combine ws xs |>
        List.filter (function (_, Expr_LitBits "") -> false | _ -> true) |>
        List.split
      in
      if xs' = [] then Expr_LitBits "" else Expr_Concat (ws', xs')
  (* '' : x == x == x : '' *)
  | Expr_TApply (FIdent ("append_bits", _), [ Expr_LitInt "0"; _ ], [ _; y ]) ->
      y
  | Expr_TApply (FIdent ("append_bits", _), [ _; Expr_LitInt "0" ], [ x; _ ]) ->
      x
  (* x + 0 == x == 0 + x *)
  | Expr_TApply (FIdent ("add_int", _), [], [ x; Expr_LitInt "0" ]) -> x
  | Expr_TApply (FIdent ("add_int", _), [], [ Expr_LitInt "0"; x ]) -> x
  (* x - 0 == x *)
  | Expr_TApply (FIdent ("sub_int", _), [], [ x; Expr_LitInt "0" ]) -> x
  | _ -> x

(* impure functions: set during initialization *)
let impure_funs : IdentSet.t ref = ref IdentSet.empty

let isPure (x : expr) : bool =
  match x with
  | Expr_TApply (f, _, _) -> not (IdentSet.mem f !impure_funs)
  | _ -> true

class constEvalClass (env : Env.t) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      match x with
      | Expr_Var v -> (
          let r = try Env.getVar env v with _ -> Values.bottom in
          match Option.bind (Values.to_concrete r) value_to_expr with
          | Some r -> ChangeTo r
          | None -> SkipChildren)
      | Expr_Array (a, i) ->
          let i' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) i in
          ChangeTo (Expr_Array (a, i'))
      | _ -> (
          try
            let eval (x : expr) : expr =
              if isConstant env x then x
              else if isPure x && List.for_all (isConstant env) (subexprs_of_expr x)
              then
                let env0 = Env.to_concrete env in
                let x' =
                    Option.value
                    (value_to_expr (Eval.eval_expr Unknown env0 x))
                    ~default:x
                in
                (*
                        let fmt = Format.std_formatter in
                        Format.pp_print_string fmt "const: "; Asl_fmt.expr fmt x; Format.pp_print_string fmt " -> "; Asl_fmt.expr fmt x'; Format.pp_print_string fmt "\n";
                        *)
                x'
              else
                (* Format.pp_print_string Format.std_formatter "\nnonconst "; Asl_fmt.expr fmt x; *)
                algebraic_simplifications x
            in
            (*
                let fmt = Format.std_formatter in
                List.iter (fun bs ->
                    List.iter (fun (k, v) ->
                        Format.pp_print_string fmt "\n"; Asl_fmt.varname fmt k
                    ) bs)
                    (ScopeStack.bindings (Env.locals env));
                Eval.Env.pp fmt (Env.to_concrete env);
                *)
            ChangeDoChildrenPost (x, eval)
          with _ -> DoChildren)
  end

let xform_expr (env : Env.t) (x : AST.expr) : AST.expr =
  (* bottom up rewrite of expression substituting constants and evaluating
     constant subexpressions *)
  let ce = new constEvalClass env in
  Asl_visitor.visit_expr (ce :> Asl_visitor.aslVisitor) x

let xform_exprs (env : Env.t) (es : AST.expr list) : AST.expr list =
  List.map (xform_expr env) es

let xform_ty (env : Env.t) (x : AST.ty) : AST.ty =
  let ce = new constEvalClass env in
  Asl_visitor.visit_type (ce :> Asl_visitor.aslVisitor) x

let xform_slice (env : Env.t) (x : AST.slice) : AST.slice =
  match x with
  | Slice_Single e -> Slice_Single (xform_expr env e)
  | Slice_HiLo (hi, lo) -> Slice_HiLo (xform_expr env hi, xform_expr env lo)
  | Slice_LoWd (lo, wd) -> Slice_LoWd (xform_expr env lo, xform_expr env wd)

(* todo: this combines abstract interpretation with transformation
 * would it be cleaner to just use a visitior to perform the transformation
 * and then this bit just updates environments?
 *)
let rec xform_lexpr (env : Env.t) (x : AST.lexpr) (r : Values.t) : AST.lexpr =
  match x with
  | LExpr_Wildcard -> x
  | LExpr_Var v ->
      Env.setVar env v r;
      x
  | LExpr_Field(l, f) ->
      let l' = xform_lexpr env l Values.bottom in
      LExpr_Field(l', f)
  (*
    | LExpr_Fields(l, fs) ->
            let rec set_fields (i: int) (fs: ident list) (prev: value): value =
                (match fs with
                | [] -> prev
                | (f::fs') ->
                        let p = get_field prev f in (* read previous value to get width *)
                        let w = Primops.prim_length_bits (Value.to_bits p) in
                        let y = extract_bits' r i w in
                        let v' = set_field prev f y in
                        set_fields (i + w) fs' v'
                )
            in
            xform_lexpr_modify env l (set_fields 0 fs)
    *)
  | LExpr_Slices (l, ss) ->
      let l' = xform_lexpr env l Values.bottom in
      let ss' = List.map (xform_slice env) ss in
      LExpr_Slices (l', ss')
  | LExpr_BitTuple (ws, ls) ->
      let ws' = xform_exprs env ws in
      let ls' = List.map (fun l -> xform_lexpr env l Values.bottom) ls in
      LExpr_BitTuple(ws', ls')
  | LExpr_Tuple(ls) ->
      let ls' = List.map (fun l -> xform_lexpr env l Values.bottom) ls in
      LExpr_Tuple(ls')
  | LExpr_Array (l, i) ->
      let i' = xform_expr env i in
      let l' = xform_lexpr env l Values.bottom in
      LExpr_Array (l', i')
  | LExpr_Write (setter, tes, es) ->
      let tes' = xform_exprs env tes in
      let es' = xform_exprs env es in
      LExpr_Write (setter, tes', es')
  | LExpr_ReadWrite (getter, setter, tes, es) ->
      let tes' = xform_exprs env tes in
      let es' = xform_exprs env es in
      LExpr_ReadWrite (getter, setter, tes', es')
  | _ -> failwith ("xform_lexpr: " ^ pp_lexpr x)

(** Evaluate pattern match *)
let rec xform_pattern (env : Env.t) (x : AST.pattern) : AST.pattern =
  match x with
  | Pat_Const c -> Pat_Const c
  | Pat_Tuple ps -> Pat_Tuple (List.map (xform_pattern env) ps)
  | Pat_Set ps -> Pat_Set (xform_patterns env ps)
  | Pat_Single e -> Pat_Single (xform_expr env e)
  | Pat_Range (lo, hi) ->
      let lo' = xform_expr env lo in
      let hi' = xform_expr env hi in
      Pat_Range (lo', hi')
  | p -> p

and xform_patterns (env : Env.t) (ps : AST.pattern list) : AST.pattern list =
  List.map (xform_pattern env) ps

let rec xform_declitem (env : Env.t) (isConst : bool) (x : AST.decl_item)
    (r : AST.expr option) : AST.decl_item =
  match (x, r) with
  | DeclItem_Var (v, oty), _ ->
      let oty' = Option.map (xform_ty env) oty in
      let r' = Option.map (expr_value env) r in
      let _ =
        Option.map
          (fun r'' ->
            if isConst then Env.addLocalConst env v r''
            else Env.addLocalVar env v r'')
          r'
      in
      DeclItem_Var (v, oty')
  | DeclItem_Tuple dis, Some (Expr_Tuple rs) ->
      let dis' =
        List.map2 (fun di r -> xform_declitem env isConst di (Some r)) dis rs
      in
      DeclItem_Tuple dis'
  | DeclItem_Tuple dis, _ ->
      let dis' = List.map (fun di -> xform_declitem env isConst di None) dis in
      DeclItem_Tuple dis'
  | DeclItem_Wildcard oty, _ ->
      let oty' = Option.map (xform_ty env) oty in
      DeclItem_Wildcard oty'

let rec xform_stmts (env : Env.t) (xs : AST.stmt list) : AST.stmt list =
  let rec xform e (ss : AST.stmt list) : AST.stmt list list =
    match ss with
    | [] -> []
    | s :: ss ->
        let s', ss' =
          Env.seq (fun e' -> xform_stmt e' s) (fun e' -> xform e' ss) e
        in
        s' :: ss'
  in
  Env.nest env (fun env' -> List.concat (xform env' xs))

(** Evaluate statement *)
and xform_stmt (env : Env.t) (x : AST.stmt) : AST.stmt list =
  match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) ->
      let ty' = xform_ty env ty in
      List.iter (fun v -> Env.addLocalVar env v Values.bottom) vs;
      [ Stmt_VarDeclsNoInit (vs, ty', loc) ]
  | Stmt_VarDecl (di, i, loc) ->
      let i' = xform_expr env i in
      let di' = xform_declitem env false di (Some i') in
      [ Stmt_VarDecl (di', i', loc) ]
  | Stmt_ConstDecl (di, i, loc) ->
      let i' = xform_expr env i in
      let di' = xform_declitem env true di (Some i') in
      (* todo: we should always be able to delete this declaration *)
      [ Stmt_ConstDecl (di', i', loc) ]
  | Stmt_Assign (l, r, loc) ->
      let r' = xform_expr env r in
      let l' = xform_lexpr env l (expr_value env r') in
      (* todo: delete assignment if possible *)
      [ Stmt_Assign (l', r', loc) ]
  | Stmt_TCall (f, tes, es, loc) ->
      let tes' = xform_exprs env tes in
      let es' = xform_exprs env es in
      [ Stmt_TCall (f, tes', es', loc) ]
  | Stmt_FunReturn (e, loc) ->
      let e' = xform_expr env e in
      Env.fun_return env (expr_value env e');
      [ Stmt_FunReturn (e', loc) ]
  | Stmt_ProcReturn loc ->
      Env.proc_return env;
      [ Stmt_ProcReturn loc ]
  | Stmt_Assert (e, loc) ->
      let e' = xform_expr env e in
      (* todo: add 'e' to the environment *)
      (* todo: if e is false, dead code eliminate *)
      [ Stmt_Assert (e', loc) ]
  (*
    | Stmt_Unpred(loc) ->
            raise (Throw (loc, Exc_Unpredictable))
    | Stmt_ConstrainedUnpred(loc) ->
            raise (Throw (loc, Exc_ConstrainedUnpredictable))
    | Stmt_ImpDef(v, loc) ->
            raise (Throw (loc, Exc_ImpDefined (pprint_ident v)))
    | Stmt_Undefined(loc) ->
            raise (Throw (loc, Exc_Undefined))
    | Stmt_ExceptionTaken(loc) ->
            raise (Throw (loc, Exc_ExceptionTaken))
    | Stmt_Dep_Unpred(loc) ->
            raise (Throw (loc, Exc_Unpredictable))
    | Stmt_Dep_ImpDef(s, loc) ->
            raise (Throw (loc, Exc_ImpDefined s))
    | Stmt_Dep_Undefined(loc) ->
            raise (Throw (loc, Exc_Undefined))
    | Stmt_See(e, loc) ->
            let s = to_string (xform_expr env e) in
            raise (Throw (loc, Exc_SEE s))
    | Stmt_Throw(v, loc) ->
            let ex = to_exc (Env.getVar env v) in
            raise (Throw ex)
    *)
  | Stmt_Block (ss, loc) -> [ Stmt_Block (xform_stmts env ss, loc) ]
  | Stmt_If (c, t, els, (e, el), loc) -> (
      let rec xform env css =
        match css with
        | [] -> ([], xform_stmts env e)
        | S_Elsif_Cond (c, s, loc) :: css' ->
            (* todo: each branch should assert c or not c *)
            (* todo: add dead code elimination *)
            let c' = xform_expr env c in
            let s', (css'', e') =
              Env.fork_join
                (fun env' -> xform_stmts env' s)
                (fun env' -> xform env' css')
                env
            in
            (S_Elsif_Cond (c', s', loc) :: css'', e')
      in
      match xform env (S_Elsif_Cond (c, t, loc) :: els) with
      | [], e' -> e'
      | S_Elsif_Cond (c', t', loc) :: els', e' ->
          [ Stmt_If (c', t', els', (e', el), loc) ])
  | Stmt_Case (e, alts, odefault, loc) ->
      let e' = xform_expr env e in
      let rec xform env alts =
        match alts with
        | [] ->
            let odefault' =
              Option.map (fun (s, loc) -> (xform_stmts env s, loc)) odefault
            in
            ([], odefault')
        | Alt_Alt (ps, oc, s, loc) :: alts' ->
            let ps' = xform_patterns env ps in
            let oc' = Option.map (xform_expr env) oc in
            let s', (alts'', odefault') =
              Env.fork_join
                (fun env -> xform_stmts env s)
                (fun env -> xform env alts')
                env
            in
            (Alt_Alt (ps', oc', s', loc) :: alts'', odefault')
      in
      let alts', odefault' = xform env alts in
      [ Stmt_Case (e', alts', odefault', loc) ]
  | Stmt_For (v, start, dir, stop, b, loc) -> (
      let start' = xform_expr env start in
      let stop' = xform_expr env stop in
      match (value_of_constant start', value_of_constant stop') with
      | Some x, Some y when !unroll_loops ->
          let rec eval (i : Concrete.value) =
            let c =
              match dir with
              | Direction_Up -> Concrete.eval_leq loc i y
              | Direction_Down -> Concrete.eval_leq loc y i
            in
            if c then
              let b' =
                Env.nest env (fun env' ->
                    Env.addLocalConst env' v (Values.singleton i);
                    xform_stmts env' b)
              in
              let i' =
                match dir with
                | Direction_Up -> Concrete.eval_add_int loc i Concrete.int_one
                | Direction_Down -> Concrete.eval_sub_int loc i Concrete.int_one
              in
              Stmt_Block (b', loc) :: eval i'
            else []
          in
          eval x
      | _ ->
          (* todo: this is overkill: only need to set variables modified to b *)
          Env.set_bottom env;
          let b' =
            Env.nest env (fun env' ->
                Env.addLocalVar env' v Values.bottom;
                xform_stmts env' b)
          in
          (* todo: this is overkill: only need to set variables modified to b *)
          Env.set_bottom env;
          [ Stmt_For (v, start', dir, stop', b', loc) ])
    | Stmt_While(c, b, loc) ->
          (* todo: this is overkill: only need to set variables modified to b *)
          Env.set_bottom env;
          let c' = xform_expr env c in
          let b' = xform_stmts env b in
          (* todo: this is overkill: only need to set variables modified to b *)
          Env.set_bottom env;
          [ Stmt_While(c', b', loc) ]
    | Stmt_Repeat(b, c, pos, loc) ->
          (* todo: this is overkill: only need to set variables modified to b *)
          Env.set_bottom env;
          let b' = xform_stmts env b in
          let c' = xform_expr env c in
          (* todo: this is overkill: only need to set variables modified to b *)
          Env.set_bottom env;
          [ Stmt_Repeat(b', c', pos, loc) ]
  (*
    | Stmt_Try(tb, ev, pos, catchers, odefault, loc) ->
            (try
                xform_stmts env tb
            with
            | Return v -> raise (Return v)
            | Throw (l, ex) ->
                Env.nest (fun env' ->
                    let rec eval cs =
                        (match cs with
                        | [] ->
                            (match odefault with
                            | None   -> raise (Throw (l, ex))
                            | Some (s, _) -> xform_stmts env' s
                            )
                        | (Catcher_Guarded(c, b, loc) :: cs') ->
                            if to_bool (xform_expr env' c) then
                                xform_stmts env' b
                            else
                                eval cs'
                        )
                    in
                    Env.addLocalVar env' ev (VExc (l, ex));
                    eval catchers
                ) env
            )
    *)
  | _ -> failwith ("xform_stmt: " ^ pp_stmt x)

let xform_tvs (env : Env.t) (loc : AST.l) (f : AST.ident) : unit =
  let genv = Env.globals env in
  let tvs =
    match Eval.GlobalEnv.getFun genv f with
    | Some (tvs, _, _, _) -> tvs
    | _ -> failwith "xform_decl"
  in
  List.iter (fun tv -> Env.addLocalConst env tv Values.bottom) tvs

let xform_atys (env : Env.t) (atys : (AST.ident * AST.ty) list) : unit =
  List.iter (fun (v, ty) -> Env.addLocalVar env v Values.bottom) atys

let xform_decl (genv : Eval.GlobalEnv.t) (d : AST.declaration) : AST.declaration
    =
  match d with
  | Decl_FunDefn (f, ps, atys, rty, body, loc) ->
      let env = Env.newEnv genv in
      xform_tvs env loc f;
      xform_atys env atys;
      let body' = xform_stmts env body in
      Decl_FunDefn (f, ps, atys, rty, body', loc)
  | Decl_ProcDefn (f, ps, atys, body, loc) ->
      let env = Env.newEnv genv in
      xform_tvs env loc f;
      xform_atys env atys;
      let body' = xform_stmts env body in
      Decl_ProcDefn (f, ps, atys, body', loc)
  | _ -> d

let xform_decls (genv : Eval.GlobalEnv.t) (ds : AST.declaration list) :
    AST.declaration list =
  let isConstant (v : ident) : bool =
    Option.is_some (Eval.GlobalEnv.getGlobalConstOpt genv v)
  in
  let isImpurePrim (v : ident) : bool =
    List.exists (fun p -> Id.matches p v) Concrete.impure_prims
  in
  impure_funs := identify_impure_funs isConstant isImpurePrim ds;
  List.map (xform_decl genv) ds

(****************************************************************
 * End
 ****************************************************************)
