(****************************************************************
 * ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL evaluator *)

module AST  = Asl_ast
module FMT  = Asl_fmt
module TC   = Tcheck

open AST
open Utils
open Asl_utils
open Value

(****************************************************************
 * Flags to control behaviour (mostly for debugging)
 ****************************************************************)

(** Debugging output on every variable write *)
let trace_write = ref false

(** Debugging output on every function call *)
let trace_funcall = ref false

(** Debugging output on every primitive function or function call *)
let trace_primop = ref false

(** Debugging output on every instruction execution *)
let trace_instruction = ref false


(** It is an error to have multiple function definitions with conflicting types.
 *  But, for historical reasons, we still allow multiple definitions and later
 *  definitions override earlier definitions.
 *)
let override_conflicts = true


(****************************************************************)
(** {2 Lookup table for IMPLEMENTATION_DEFINED values}          *)
(****************************************************************)

module ImpDefs = struct
    include Map.Make(struct
        type t = string
        let compare = String.compare
    end)
end


(****************************************************************)
(** {2 Mutable bindings}                                        *)
(****************************************************************)

(** Environment representing immutable global state of the system
 * Note that it is mutable so that we can construct the initial state
 * as each declaration is processed.
 *)
module GlobalEnv : sig
    type t
    val empty               : t

    val addGlobalConst      : t -> ident -> value -> unit
    val getGlobalConst      : t -> ident -> value
    val getGlobalConstOpt   : t -> ident -> value option

    (* to support generation of unknown values, we need to remember the structure
     * of user-defined types such as enumerations and records
     *)
    val addEnum             : t -> ident -> value list -> unit
    val getEnum             : t -> ident -> (value list) option
    val isEnumEq            : t -> ident -> bool
    val isEnumNeq           : t -> ident -> bool

    val addRecord           : t -> ident -> (ident * AST.ty) list -> unit
    val getRecord           : t -> ident -> (ident * AST.ty) list option

    val addTypedef          : t -> ident -> AST.ty -> unit
    val getTypedef          : t -> ident -> AST.ty option

    val getFun              : AST.l -> t -> ident -> (ident list * ident list * AST.l * stmt list) option
    val addFun              : AST.l -> t -> ident -> (ident list * ident list * AST.l * stmt list) -> unit

    val getInstruction      : AST.l -> t -> ident -> (encoding * (stmt list) option * bool * stmt list)
    val addInstruction      : AST.l -> t -> ident -> (encoding * (stmt list) option * bool * stmt list) -> unit

    val getDecoder          : t -> ident -> decode_case
    val addDecoder          : t -> ident -> decode_case -> unit

    val setImpdef           : t -> string -> value -> unit
    val getImpdef           : AST.l -> t -> string -> value

    val pp                  : t -> unit
end = struct
    type t = {
        mutable instructions : (encoding * (stmt list) option * bool * stmt list) Bindings.t;
        mutable decoders     : decode_case Bindings.t;
        mutable functions    : (ident list * ident list * AST.l * stmt list) Bindings.t;
        mutable enums        : (value list) Bindings.t;
        mutable enumEqs      : IdentSet.t;
        mutable enumNeqs     : IdentSet.t;
        mutable records      : ((ident * AST.ty) list) Bindings.t;
        mutable typedefs     : AST.ty Bindings.t;
        mutable constants    : value Scope.t;
        mutable impdefs      : value ImpDefs.t;
    }

    let pp (env: t): unit =
        Scope.pp pp_value env.constants

    let empty = {
        decoders     = Bindings.empty;
        instructions = Bindings.empty;
        functions    = Bindings.empty;
        enums        = Bindings.empty;
        enumEqs      = IdentSet.empty;
        enumNeqs     = IdentSet.empty;
        records      = Bindings.empty;
        typedefs     = Bindings.empty;
        constants    = Scope.empty();
        impdefs      = ImpDefs.empty;
    }

    let addGlobalConst (env: t) (x: ident) (v: value): unit =
        Scope.set env.constants x v

    let getGlobalConst (env: t) (x: ident): value =
        (match Scope.get env.constants x with
        | Some v -> v
        | None   -> failwith "getGlobalConst"
        )

    let getGlobalConstOpt (env: t) (x: ident): value option =
        Scope.get env.constants x

    let addEnum (env: t) (x: ident) (vs: value list): unit =
        env.enums    <- Bindings.add x vs env.enums

    let getEnum (env: t) (x: ident): (value list) option =
        Bindings.find_opt x env.enums

    let isEnumEq  (env: t) (x: ident): bool = IdentSet.mem x env.enumEqs
    let isEnumNeq (env: t) (x: ident): bool = IdentSet.mem x env.enumNeqs

    let addRecord (env: t) (x: ident) (fs: (ident * AST.ty) list): unit =
        env.records <- Bindings.add x fs env.records

    let getRecord (env: t) (x: ident): ((ident * AST.ty) list) option =
        Bindings.find_opt x env.records

    let addTypedef (env: t) (x: ident) (ty: AST.ty): unit =
        env.typedefs <- Bindings.add x ty env.typedefs

    let getTypedef (env: t) (x: ident): AST.ty option =
        Bindings.find_opt x env.typedefs

    let getFun (loc: l) (env: t) (x: ident): (ident list * ident list * AST.l * stmt list) option =
        Bindings.find_opt x env.functions

    let addFun (loc: l) (env: t) (x: ident) (def: (ident list * ident list * AST.l * stmt list)): unit =
        if false then Printf.printf "Adding function %s\n" (pprint_ident x);
        if Bindings.mem x env.functions then begin
            if true then begin
                () (* silently override *)
            end else if override_conflicts then begin
                (* backward compatibility mode: only report a stern warning *)
                Printf.printf "Stern warning: %s function %s conflicts with earlier definition - discarding earlier definition\n"
                    (pp_loc loc) (pprint_ident x);
            end else begin
                raise (TC.Ambiguous (loc, "function definition", pprint_ident x))
            end
        end;
        env.functions <- Bindings.add x def env.functions

    let getInstruction (loc: AST.l) (env: t) (x: ident): (encoding * (stmt list) option * bool * stmt list) =
        Bindings.find x env.instructions

    let addInstruction (loc: AST.l) (env: t) (x: ident) (instr: encoding * (stmt list) option * bool * stmt list): unit =
        env.instructions <- Bindings.add x instr env.instructions

    let getDecoder (env: t) (x: ident): decode_case =
        Bindings.find x env.decoders

    let addDecoder (env: t) (x: ident) (d: decode_case): unit =
        env.decoders <- Bindings.add x d env.decoders

    let setImpdef (env: t) (x: string) (v: value): unit =
        env.impdefs <- ImpDefs.add x v env.impdefs

    let getImpdef (loc: l) (env: t) (x: string): value =
        (match ImpDefs.find_opt x env.impdefs with
        | Some v -> v
        | None ->
                raise (EvalError (loc, "Unknown value for IMPLEMENTATION_DEFINED \""^x^"\""))
        )
end


(** Environment representing both global and local state of the system *)
module Env : sig
    type t
    val mkEnv               : GlobalEnv.t -> value ScopeStack.t -> t
    val newEnv              : GlobalEnv.t -> t
    val nestTop             : t -> (t -> 'a) -> 'a
    val nest                : t -> (t -> 'a) -> 'a

    val globals             : t -> GlobalEnv.t

    val addLocalVar         : AST.l -> t -> ident -> value -> unit
    val addLocalConst       : AST.l -> t -> ident -> value -> unit

    val addGlobalVar        : t -> ident -> value -> unit
    val getVar              : AST.l -> t -> ident -> value
    val setVar              : AST.l -> t -> ident -> value -> unit

    val pp                  : Format.formatter -> t -> unit
end = struct
    type t = {
                globalConsts : GlobalEnv.t;
        mutable globalVars   : value Scope.t;
        mutable locals       : value ScopeStack.t
    }

    let mkEnv (genv: GlobalEnv.t) (env: value ScopeStack.t) = {
        globalConsts = genv;
        globalVars   = Scope.empty();
        locals       = env;
    }

    let newEnv (genv: GlobalEnv.t) = mkEnv genv (ScopeStack.empty ())

    let nestTop (parent: t) (k: t -> 'a): 'a =
        let child = {
            globalConsts = parent.globalConsts;
            globalVars   = parent.globalVars;
            locals       = ScopeStack.empty ();  (* only change *)
        } in
        k child

    let nest (parent: t) (k: t -> 'a): 'a =
        ScopeStack.nest parent.locals (fun newenv ->
            let child = {
                globalConsts = parent.globalConsts;
                globalVars   = parent.globalVars;
                locals       = newenv;  (* only change *)
            } in
            k child
        )

    let globals (env: t): GlobalEnv.t = env.globalConsts

    let addLocalVar (loc: l) (env: t) (x: ident) (v: value): unit =
        if !trace_write then Printf.printf "TRACE: fresh %s = %s\n" (pprint_ident x) (pp_value v);
        ScopeStack.add env.locals x v

    let addLocalConst (loc: l) (env: t) (x: ident) (v: value): unit =
        (* todo: should constants be held separately from local vars? *)
        ScopeStack.add env.locals x v

    let addGlobalVar (env: t) (x: ident) (v: value): unit =
        Scope.set env.globalVars x v

    let getVar (loc: l) (env: t) (x: ident): value =
        from_option (ScopeStack.get env.locals x) (fun _ ->
        from_option (Scope.get env.globalVars x) (fun _ ->
        from_option ( GlobalEnv.getGlobalConstOpt env.globalConsts x) (fun _ ->
        raise (EvalError (loc, "getVar: " ^ pprint_ident x))
        )))

    let setVar (loc: l) (env: t) (x: ident) (v: value): unit =
        if !trace_write then Printf.printf "TRACE: write %s = %s\n" (pprint_ident x) (pp_value v);
        if ScopeStack.set env.locals x v then ()
        else Scope.set env.globalVars x v

    let pp (fmt: Format.formatter) (env: t): unit =
        List.iter (fun bs ->
            List.iter (fun (k, v) ->
                let fmt = Format.std_formatter in
                Asl_fmt.varname fmt k;
                Format.pp_print_string fmt " -> ";
                Format.pp_print_string fmt (pp_value v);
                Format.pp_print_string fmt ", ";
            ) bs)
            (ScopeStack.bindings (env.locals))

end

(****************************************************************)
(** {2 Evaluation functions}                                    *)
(****************************************************************)

(** Evaluate bitslice of instruction opcode *)
let eval_decode_slice (loc: l) (env: Env.t) (x: decode_slice) (op: value): value =
    (match x with
    | DecoderSlice_Slice (lo, wd) -> extract_bits' loc op lo wd
    | DecoderSlice_FieldName f -> Env.getVar loc env f
    | DecoderSlice_Concat fs -> eval_concat loc (List.map (Env.getVar loc env) fs)
    )

(** Evaluate instruction decode pattern match *)
let rec eval_decode_pattern (loc: AST.l) (x: decode_pattern) (op: value): bool =
    (match x with
    | DecoderPattern_Bits     b -> eval_eq     loc op (from_bitsLit b)
    | DecoderPattern_Mask     m -> eval_inmask loc op (from_maskLit m)
    | DecoderPattern_Wildcard _ -> true
    | DecoderPattern_Not      p -> not (eval_decode_pattern loc p op)
    )


(** Evaluate list of expressions *)
let rec eval_exprs (loc: l) (env: Env.t) (xs: AST.expr list): value list =
    List.map (eval_expr loc env) xs

(** Create uninitialized value at given type

    - For any scalar type, this is the value VUninitialized.
    - For any composite type, all elements are set to uninitialized values

    todo: bitvectors are currently set to UNKNOWN because the bitvector
    representation currently in use cannot track uninitialized bits
 *)
and mk_uninitialized (loc: l) (env: Env.t) (x: AST.ty): value =
    ( match x with
    | Type_Constructor(tc) ->
        (match GlobalEnv.getRecord (Env.globals env) tc with
        | Some fs ->
            mkrecord (List.map (fun (f, ty) -> (f, mk_uninitialized loc env ty)) fs)
        | None ->
            (match GlobalEnv.getTypedef (Env.globals env) tc with
            | Some ty' -> mk_uninitialized loc env ty'
            | None     -> VUninitialized
            )
        )
    | Type_Array(Index_Enum(tc),ety) ->
            Value.empty_array (mk_uninitialized loc env ety)
    | Type_Array(Index_Range(lo,hi),ety) ->
            Value.empty_array (mk_uninitialized loc env ety)
    | Type_Tuple(tys) ->
            VTuple (List.map (mk_uninitialized loc env) tys)
    | Type_Integer _ -> eval_unknown_integer ()
    (* bitvectors and registers should really track whether a bit is initialized individually *)
    | Type_Bits(n) -> eval_unknown_bits (to_integer loc (eval_expr loc env n))
    | Type_Register(wd, _) -> eval_unknown_bits (Z.of_string wd)
    | _ ->
            VUninitialized (* should only be used for scalar types *)
    )

(** Evaluate UNKNOWN at given type *)
and eval_unknown (loc: l) (env: Env.t) (x: AST.ty): value =
    ( match x with
    | Type_Constructor(Ident "real")    -> eval_unknown_real ()
    | Type_Constructor(Ident "string")  -> eval_unknown_string ()
    | Type_Constructor(tc) ->
        (match GlobalEnv.getEnum (Env.globals env) tc with
        | Some (e::_) -> e
        | Some [] -> raise (EvalError (loc, "eval_unknown unknown type constructor " ^ pp_type x))
        | None ->
            (match GlobalEnv.getRecord (Env.globals env) tc with
            | Some fs ->
                mkrecord (List.map (fun (f, ty) -> (f, eval_unknown loc env ty)) fs)
            | None ->
                (match GlobalEnv.getTypedef (Env.globals env) tc with
                | Some ty' -> eval_unknown loc env ty'
                | None ->
                    raise (EvalError (loc, "eval_unknown " ^ pp_type x))
                )
            )
        )
    | Type_Integer _ -> eval_unknown_integer ()
    | Type_Bits(n) -> eval_unknown_bits (to_integer loc (eval_expr loc env n))
    | Type_App(Ident "__RAM", [a]) ->
            let a' = to_integer loc (eval_expr loc env a) in
            eval_unknown_ram a'
    | Type_App(tc, es) ->
            raise (EvalError (loc, "eval_unknown App " ^ pp_type x))
    | Type_OfExpr(e) ->
            raise (EvalError (loc, "eval_unknown typeof " ^ pp_type x))
    | Type_Register(wd, _) -> eval_unknown_bits (Z.of_string wd)
    | Type_Array(Index_Enum(tc),ety) ->
            Value.empty_array (eval_unknown loc env ety)
    | Type_Array(Index_Range(lo,hi),ety) ->
            Value.empty_array (eval_unknown loc env ety)
    | Type_Tuple(tys) ->
            VTuple (List.map (eval_unknown loc env) tys)
    )

(** Evaluate pattern match *)
and eval_pattern (loc: l) (env: Env.t) (v: value) (x: AST.pattern): bool =
    ( match x with
    | Pat_LitInt(l)  -> eval_eq_int  loc v (from_intLit l)
    | Pat_LitHex(l)  -> eval_eq_int  loc v (from_hexLit l)
    | Pat_LitBits(l) -> eval_eq_bits loc v (from_bitsLit l)
    | Pat_LitMask(l) -> eval_inmask  loc v (from_maskLit l)
    | Pat_Const(c)   -> eval_eq      loc v (GlobalEnv.getGlobalConst (Env.globals env) c)
    | Pat_Wildcard   -> true
    | Pat_Tuple(ps) ->
            let vs = of_tuple loc v in
            assert (List.length vs = List.length ps);
            List.for_all2 (eval_pattern loc env) vs ps
    | Pat_Set(ps) ->
            List.exists (eval_pattern loc env v) ps
    | Pat_Single(e) ->
            let v' = eval_expr loc env e in
            eval_eq loc v v'
    | Pat_Range(lo, hi) ->
            let lo' = eval_expr loc env lo in
            let hi' = eval_expr loc env hi in
            eval_leq loc lo' v && eval_leq loc v hi'
    )

(** Evaluate bitslice bounds *)
and eval_slice (loc: l) (env: Env.t) (x: AST.slice): (value * value) =
    (match x with
    | Slice_Single(i) ->
            let i' = eval_expr loc env i in
            (i', VInt Z.one)
    | Slice_HiLo(hi, lo) ->
            let hi' = eval_expr loc env hi in
            let lo' = eval_expr loc env lo in
            let wd' = eval_add_int loc (eval_sub_int loc hi' lo') (VInt Z.one) in
            (lo', wd')
    | Slice_LoWd(lo, wd) ->
            let lo' = eval_expr loc env lo in
            let wd' = eval_expr loc env wd in
            (lo', wd')
    )

(** Evaluate expression *)
and eval_expr (loc: l) (env: Env.t) (x: AST.expr): value =
    (match x with
    | Expr_If(c, t, els, e) ->
            let rec eval_if xs d = match xs with
                | [] -> eval_expr loc env d
                | AST.E_Elsif_Cond (cond, b)::xs' ->
                    if to_bool loc (eval_expr loc env cond) then
                        eval_expr loc env b
                    else
                        eval_if xs' d
            in
            eval_if (E_Elsif_Cond(c, t)::els) e
    | Expr_Binop(a, op, b) ->
            raise (EvalError (loc, "binary operation should have been removed in expression "
                   ^ pp_expr x))
    | Expr_Field(e, f) ->
            get_field loc (eval_expr loc env e) f
    | Expr_Fields(e, fs) ->
            let v  = eval_expr loc env e in
            let vs = List.map (get_field loc v) fs in
            eval_concat loc vs
    | Expr_Slices(e, ss) ->
            let v  = eval_expr loc env e in
            let vs = List.map (fun s ->
                let (i, w) = eval_slice loc env s in
                extract_bits'' loc v i w
            ) ss in
            eval_concat loc vs
    | Expr_In(e, p) ->
            from_bool (eval_pattern loc env (eval_expr loc env e) p)
    | Expr_Var(v) ->
            Env.getVar loc env v
    | Expr_Parens(e) ->
            let v = eval_expr loc env e in
            v
    | Expr_TApply(f, tes, es) ->
            (* First deal with &&, || and IMPLIES all of which only evaluate
             * their second argument if they need to
             *)
            if name_of_FIdent f = "and_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    if to_bool loc (eval_expr loc env x) then
                        eval_expr loc env y
                    else
                        from_bool false
                | _ ->
                    raise (EvalError (loc, "malformed and_bool expression "
                       ^ pp_expr x))
                )
            end else if name_of_FIdent f = "or_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    if to_bool loc (eval_expr loc env x) then
                        from_bool true
                    else
                        eval_expr loc env y
                | _ ->
                    raise (EvalError (loc, "malformed or_bool expression "
                       ^ pp_expr x))
                )
            end else if name_of_FIdent f = "implies_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    if to_bool loc (eval_expr loc env x) then
                        eval_expr loc env y
                    else
                        from_bool true
                | _ ->
                    raise (EvalError (loc, "malformed implies_bool expression "
                       ^ pp_expr x))
                )
            end else begin
                let tvs = eval_exprs loc env tes in
                let vs  = eval_exprs loc env es in
                eval_funcall loc env f tvs vs
            end
    | Expr_Tuple(es) ->
            let vs = List.map (eval_expr loc env) es in
            VTuple vs
    | Expr_Concat(es) ->
            let vs = List.map (eval_expr loc env) es in
            eval_concat loc vs
    | Expr_Unop(op, e) ->
            raise (EvalError (loc, "unary operation should have been removed"))
    | Expr_Unknown(t) ->
            eval_unknown loc env t
    | Expr_ImpDef(Some(s), t) ->
            GlobalEnv.getImpdef loc (Env.globals env) s
    | Expr_ImpDef(None, t) ->
            raise (EvalError (loc, "unnamed IMPLEMENTATION_DEFINED behavior"))
    | Expr_Array(a, i) ->
            let a' = eval_expr loc env a in
            let i' = eval_expr loc env i in
            get_array loc a' i'
    | Expr_LitInt(i) -> from_intLit i
    | Expr_LitHex(i) -> from_hexLit i
    | Expr_LitReal(r) -> from_realLit r
    | Expr_LitBits(b) -> from_bitsLit b
    | Expr_LitMask(b) -> from_maskLit b (* todo: masks should not be expressions *)
    | Expr_LitString(s) -> from_stringLit s
    | Expr_AsConstraint(e, c) ->
         (* todo: dynamic constraint check *)
         eval_expr loc env e
    | Expr_AsType(e, t) ->
         (* todo: dynamic type/constraint check *)
         eval_expr loc env e
    )

(** Evaluate L-expression in write-mode (i.e., this is not a read-modify-write) *)
and eval_lexpr (loc: l) (env: Env.t) (x: AST.lexpr) (r: value): unit =
    ( match x with
    | LExpr_Wildcard ->
            ()
    | LExpr_Var(v) ->
            Env.setVar loc env v r
    | LExpr_Field(l, f) ->
            eval_lexpr_modify loc env l (fun prev -> set_field loc prev f r)
    | LExpr_Fields(l, fs) ->
            let rec set_fields (i: int) (fs: ident list) (prev: value): value =
                (match fs with
                | [] -> prev
                | (f::fs') ->
                        let p = get_field loc prev f in (* read previous value to get width *)
                        let w = Primops.prim_length_bits (Value.to_bits loc p) in
                        let y = extract_bits' loc r i w in
                        let v' = set_field loc prev f y in
                        set_fields (i + w) fs' v'
                )
            in
            eval_lexpr_modify loc env l (set_fields 0 fs)
    | LExpr_Slices(l, ss) ->
            let rec eval (o: value) (ss': AST.slice list) (prev: value): value =
                (match ss' with
                | [] -> prev
                | (s :: ss) ->
                        let (i, w) = eval_slice loc env s in
                        let v      = extract_bits'' loc r o w in
                        eval (eval_add_int loc o w) ss (insert_bits loc prev i w v)
                )
            in
            eval_lexpr_modify loc env l (eval (VInt Z.zero) (List.rev ss))
    | LExpr_BitTuple(ls) ->
            failwith "eval_lexpr: bittuple"
    | LExpr_Tuple(ls) ->
            let rs = of_tuple loc r in
            assert (List.length ls = List.length rs);
            List.iter2 (eval_lexpr loc env) ls rs
    | LExpr_Array(l, i) ->
            let i' = eval_expr loc env i in
            eval_lexpr_modify loc env l (fun prev -> set_array loc prev i' r)
    | LExpr_Write(setter, tes, es) ->
            let tvs = eval_exprs loc env tes in
            let vs  = eval_exprs loc env es in
            eval_proccall loc env setter tvs (List.append vs [r])
    | _ ->
            failwith ("eval_lexpr: "^ pp_lexpr x)
    )

(** Evaluate L-expression in read-modify-write mode.

    1. The old value of the L-expression is read.
    2. The function 'modify' is applied to the old value
    3. The result is written back to the L-expression.
 *)
and eval_lexpr_modify (loc: l) (env: Env.t) (x: AST.lexpr) (modify: value -> value): unit =
    (match x with
    | LExpr_Var(v) ->
            Env.setVar loc env v (modify (Env.getVar loc env v))
    | LExpr_Field(l, f) ->
            let modify' (prev: value): value =
                let old = get_field loc prev f in
                set_field loc prev f (modify old)
            in
            eval_lexpr_modify loc env l modify'
    | LExpr_Array(l, i) ->
            let i' = eval_expr loc env i in
            let modify' (prev: value): value =
                let old = get_array loc prev i' in
                set_array loc prev i' (modify old)
            in
            eval_lexpr_modify loc env l modify'
    | LExpr_ReadWrite (getter, setter, tes, es) ->
            let tvs = eval_exprs loc env tes in
            let vs  = eval_exprs loc env es in
            let old = eval_funcall loc env getter tvs vs in
            eval_proccall loc env setter tvs (List.append vs [modify old])
    | _ ->
            failwith "eval_lexpr_modify"
    )

(** Evaluate list of statements *)
and eval_stmts (env: Env.t) (xs: AST.stmt list): unit =
    Env.nest env (fun env' -> List.iter (eval_stmt env') xs)

(** Evaluate statement *)
and eval_stmt (env: Env.t) (x: AST.stmt): unit =
    (match x with
    | Stmt_VarDeclsNoInit(vs, ty, loc) ->
            List.iter (fun v -> Env.addLocalVar loc env v (mk_uninitialized loc env ty)) vs
    | Stmt_VarDecl(v, ty, i, loc) ->
            let i' = eval_expr loc env i in
            Env.addLocalVar loc env v i'
    | Stmt_ConstDecl(v, ty, i, loc) ->
            let i' = eval_expr loc env i in
            Env.addLocalConst loc env v i'
    | Stmt_Assign(l, r, loc) ->
            let r' = eval_expr loc env r in
            eval_lexpr loc env l r'
    | Stmt_TCall(f, tes, es, loc) ->
            let tvs = eval_exprs loc env tes in
            let vs  = eval_exprs loc env es in
            eval_proccall loc env f tvs vs
    | Stmt_FunReturn(e, loc) ->
            let v = eval_expr loc env e in
            raise (Return (Some v))
    | Stmt_ProcReturn(loc) ->
            raise (Return None)
    | Stmt_Assert(e, loc) ->
            if not (to_bool loc (eval_expr loc env e)) then
                raise (EvalError (loc, "assertion failure"))
    | Stmt_Throw(v, loc) ->
            let ex = to_exc loc (Env.getVar loc env v) in
            raise (Throw ex)
    | Stmt_DecodeExecute(i, e, loc) ->
            let dec = GlobalEnv.getDecoder (Env.globals env) i in
            let op  = eval_expr loc env e in
            eval_decode_case loc env dec op
    | Stmt_Block(b, loc) ->
            eval_stmts env b
    | Stmt_If(c, t, els, (e, el), loc) ->
            let rec eval css d =
                (match css with
                | [] -> eval_stmts env d
                | (S_Elsif_Cond(c, s, loc) :: css') ->
                        if to_bool loc (eval_expr loc env c) then
                            eval_stmts env s
                        else
                            eval css' d
                )
            in
            eval (S_Elsif_Cond(c, t, loc) :: els) e
    | Stmt_Case(e, alts, odefault, loc) ->
            let rec eval v alts =
                (match alts with
                | [] ->
                        (match odefault with
                        | None -> raise (EvalError (loc, "unmatched case"))
                        | Some (s, _) -> eval_stmts env s
                        )
                | (Alt_Alt(ps, oc, s, loc) :: alts') ->
                        if List.exists (eval_pattern loc env v) ps && from_option
                        (map_option (to_bool loc) (map_option (eval_expr loc env) oc)) (fun _ -> true) then
                            eval_stmts env s
                        else
                            eval v alts'
                )
            in
            eval (eval_expr loc env e) alts
    | Stmt_For(v, start, dir, stop, b, loc) ->
            let start' = eval_expr loc env start in
            let stop'  = eval_expr loc env stop in
            let rec eval i =
                let c = (match dir with
                | Direction_Up   -> eval_leq loc i stop'
                | Direction_Down -> eval_leq loc stop' i
                ) in
                if c then begin
                    Env.nest env (fun env' ->
                        Env.addLocalVar loc env' v i;
                        eval_stmts env' b
                    );
                    let i' = (match dir with
                    | Direction_Up   -> eval_add_int loc i (VInt Z.one)
                    | Direction_Down -> eval_sub_int loc i (VInt Z.one)
                    ) in
                    eval i'
                end
            in
            eval start'

    | Stmt_While(c, b, loc) ->
            let rec eval _ =
                if to_bool loc (eval_expr loc env c) then
                    eval_stmts env b;
                    eval ()
            in
            eval ()
    | Stmt_Repeat(b, c, pos, loc) ->
            let rec eval _ =
                eval_stmts env b;
                if to_bool loc (eval_expr loc env c) then
                    eval ()
            in
            eval ()
    | Stmt_Try(tb, ev, pos, catchers, odefault, loc) ->
            (try
                eval_stmts env tb
            with
            | Return v -> raise (Return v)
            | Throw (l, ex) ->
                Env.nest env (fun env' ->
                    let rec eval cs =
                        (match cs with
                        | [] ->
                            (match odefault with
                            | None   -> raise (Throw (l, ex))
                            | Some (s, _) -> eval_stmts env' s
                            )
                        | (Catcher_Guarded(c, b, loc) :: cs') ->
                            if to_bool loc (eval_expr loc env' c) then
                                eval_stmts env' b
                            else
                                eval cs'
                        )
                    in
                    Env.addLocalVar loc env' ev (VExc (l, ex));
                    eval catchers
                )
            )
    )

(** Evaluate call to function or procedure *)
and eval_call (loc: l) (env: Env.t) (f: ident) (tvs: value list) (vs: value list): unit =
    (match eval_prim (name_of_FIdent f) tvs vs with
    | Some r ->
        if !trace_primop then begin
            Printf.printf "TRACE primop: %s " (pprint_ident f);
            List.iter (fun v -> Printf.printf " [%s]" (pp_value v)) tvs;
            List.iter (fun v -> Printf.printf " %s" (pp_value v)) vs;
            Printf.printf " --> %s\n" (pp_value r);
        end;
        raise (Return (Some r))
    | None ->
        begin
            if !trace_funcall then begin
                Printf.printf "TRACE funcall: %s " (pprint_ident f);
                List.iter (fun v -> Printf.printf " [%s]" (pp_value v)) tvs;
                List.iter (fun v -> Printf.printf " %s" (pp_value v)) vs;
                Printf.printf "\n"
            end;
            let (targs, args, loc, b) = Utils.from_option (GlobalEnv.getFun loc (Env.globals env) f)
                (fun _ -> raise (EvalError (loc, "Undeclared function " ^ pprint_ident f)))
            in
            assert (List.length targs = List.length tvs);
            assert (List.length args  = List.length vs);
            Env.nestTop env (fun env' ->
                List.iter2 (fun arg v -> Env.addLocalVar loc env' arg v) targs tvs;
                List.iter2 (fun arg v -> Env.addLocalVar loc env' arg v) args vs;
                eval_stmts env' b
            )
        end
    )

(** Evaluate call to function *)
and eval_funcall (loc: l) (env: Env.t) (f: ident) (tvs: value list) (vs: value list): value =
    (try
        eval_call loc env f tvs vs;
        raise (EvalError (loc, "no return statement"))
    with
    | Return (Some v) -> v
    | Throw (l, ex) -> raise (Throw (l, ex))
    )

(** Evaluate call to procedure *)
and eval_proccall (loc: l) (env: Env.t) (f: ident) (tvs: value list) (vs: value list): unit =
    (try
        eval_call loc env f tvs vs
    with
    | Return None -> ()
    | Return (Some (VTuple [])) -> ()
    | Throw (l, ex) -> raise (Throw (l, ex))
    )

(** Evaluate instruction decode case *)
and eval_decode_case (loc: AST.l) (env: Env.t) (x: decode_case) (op: value): unit =
    (match x with
    | DecoderCase_Case (ss, alts, loc) ->
            let vs = List.map (fun s -> eval_decode_slice loc env s op) ss in
            let rec eval alts =
                (match alts with
                | (alt :: alts') ->
                        if eval_decode_alt loc env alt vs op then
                            ()
                        else
                            eval alts'
                | [] ->
                        raise (EvalError (loc, "unmatched decode pattern"))
                )
            in
            eval alts
    )

(** Evaluate instruction decode case alternative *)
and eval_decode_alt (loc: AST.l) (env: Env.t) (DecoderAlt_Alt (ps, b)) (vs: value list) (op: value): bool =
    if List.for_all2 (eval_decode_pattern loc) ps vs then
        (match b with
        | DecoderBody_UNPRED loc -> raise (Throw (loc, Exc_Unpredictable))
        | DecoderBody_UNALLOC loc -> raise (Throw (loc, Exc_Undefined))
        | DecoderBody_NOP loc -> true
        | DecoderBody_Encoding (enc, l) ->
                let (enc, opost, cond, exec) = GlobalEnv.getInstruction loc (Env.globals env) enc in
                if eval_encoding env enc op then begin
                    (match opost with
                    | Some post -> List.iter (eval_stmt env) post
                    | None -> ()
                    );
                    (* todo: should evaluate ConditionHolds to decide whether to execute body *)
                    List.iter (eval_stmt env) exec;
                    true
                end else begin
                    false
                end
        | DecoderBody_Decoder (fs, c, loc) ->
                Env.nestTop env (fun env ->
                    List.iter (function (IField_Field (f, lo, wd)) ->
                        Env.addLocalVar loc env f (extract_bits' loc op lo wd)
                    ) fs;
                    eval_decode_case loc env c op
                );
                true
        )
    else
        false

(** Evaluate instruction encoding *)
and eval_encoding (env: Env.t) (x: encoding) (op: value): bool =
    let Encoding_Block (nm, iset, fields, opcode, guard, unpreds, b, loc) = x in
    (* todo: consider checking iset *)
    (* Printf.printf "Checking opcode match %s == %s\n" (Utils.to_string (PP.pp_opcode_value opcode)) (pp_value op); *)
    let ok = (match opcode with
    | Opcode_Bits b -> eval_eq     loc op (from_bitsLit b)
    | Opcode_Mask m -> eval_inmask loc op (from_maskLit m)
    ) in
    if ok then begin
        if !trace_instruction then Printf.printf "TRACE: instruction %s\n" (pprint_ident nm);
        List.iter (function (IField_Field (f, lo, wd)) ->
            let v = extract_bits' loc op lo wd in
            if !trace_instruction then Printf.printf "      %s = %s\n" (pprint_ident f) (pp_value v);
            Env.addLocalVar loc env f v
        ) fields;
        if to_bool loc (eval_expr loc env guard) then begin
            List.iter (fun (i, b) ->
                if eval_eq loc (extract_bits' loc op i 1) (from_bitsLit b) then
                    raise (Throw (loc, Exc_Unpredictable))
            ) unpreds;
            List.iter (eval_stmt env) b;
            true
        end else begin
            false
        end
    end else begin
        false
    end


(****************************************************************)
(** {2 Creating environment from global declarations}           *)
(****************************************************************)

(* Uninitialized global variables are UNKNOWN by default *)
let eval_uninitialized (loc: l) (env: Env.t) (x: AST.ty): value = eval_unknown loc env x

(** Construct global constant environment from global declarations *)
let build_constant_environment (ds: AST.declaration list): GlobalEnv.t = begin
    if false then Printf.printf "Building environment from %d declarations\n" (List.length ds);
    let genv = GlobalEnv.empty in
    (* todo?: first pull out the constants/configs and evaluate all of them
     * lazily?
     *)
    List.iter (fun d ->
        (match d with
        | Decl_Record (v, fs, loc) ->
                GlobalEnv.addRecord genv v fs
        | Decl_Enum(qid, es, loc) ->
                let evs = if qid = Ident "boolean" then begin (* optimized special case *)
                              [ (Ident "FALSE", VBool false); (Ident "TRUE", VBool true) ]
                          end else begin
                              List.mapi (fun i e -> (e, VEnum (e, i))) es;
                          end
                in
                List.iter (fun (e, v) -> GlobalEnv.addGlobalConst genv e v) evs;
                GlobalEnv.addEnum genv qid (List.map (fun (e, v) -> v) evs)
        | Decl_Typedef (v, ty, loc) ->
                GlobalEnv.addTypedef genv v ty
        | Decl_Const(v, ty, i, loc) ->
                (* todo: constants need to be lazily evaluated or need to be
                 * sorted by dependencies
                 *)
                let init = eval_expr loc (Env.newEnv genv) i in
                GlobalEnv.addGlobalConst genv v init
        | Decl_FunDefn(f, ps, atys, rty, body, loc) ->
                let tvs  = List.map fst ps in
                let args = List.map fst atys in
                GlobalEnv.addFun loc genv f (tvs, args, loc, body)
        | Decl_ProcDefn(f, ps, atys, body, loc) ->
                let tvs  = List.map fst ps in
                let args = List.map fst atys in
                GlobalEnv.addFun loc genv f (tvs, args, loc, body)
        | Decl_VarGetterDefn(f, ps, ty, body, loc) ->
                let tvs  = List.map fst ps in
                let args = [] in
                GlobalEnv.addFun loc genv f (tvs, args, loc, body)
        | Decl_ArrayGetterDefn(f, ps, atys, rty, body, loc) ->
                let tvs  = List.map fst ps in
                let args = List.map fst atys in
                GlobalEnv.addFun loc genv f (tvs, args, loc, body)
        | Decl_VarSetterDefn(f, ps, v, ty, body, loc) ->
                let tvs  = List.map fst ps in
                let args = [v] in
                GlobalEnv.addFun loc genv f (tvs, args, loc, body)
        | Decl_ArraySetterDefn(f, ps, atys, v, ty, body, loc) ->
                let tvs  = List.map fst ps in
                let name_of (x: AST.sformal): ident =
                    (match x with
                    | Formal_In (nm, _) -> nm
                    | Formal_InOut (nm, _) -> nm
                    )
                in
                let args = List.map name_of atys in
                GlobalEnv.addFun loc genv f (tvs, List.append args [v], loc, body)
        | Decl_InstructionDefn(nm, encs, opost, conditional, exec, loc) ->
                (* Instructions are looked up by their encoding name *)
                List.iter (fun enc ->
                    let Encoding_Block (nm, _, _, _, _, _, _, _) = enc in
                    GlobalEnv.addInstruction loc genv nm (enc, opost, conditional, exec)
                ) encs
        | Decl_DecoderDefn(nm, case, loc) ->
                GlobalEnv.addDecoder genv nm case
        | Decl_NewMapDefn(f, ps, atys, rty, body, loc) ->
                let tvs  = List.map fst ps in
                let args = List.map fst atys in
                GlobalEnv.addFun loc genv f (tvs, args, loc, body)
        (*
        | Decl_MapClause(f, ps, atys, cond, body, loc) ->
                let tvs   = List.map fst ps in
                let args' = List.map fst args in
                GlobalEnv.addFun loc genv f (tvs, args', loc, body)
        *)
        | Decl_NewEventDefn (f, ps, atys, loc) ->
                let tvs  = List.map fst ps in
                let args = List.map fst atys in
                GlobalEnv.addFun loc genv f (tvs, args, loc, [])
        | Decl_EventClause (f, body, loc) ->
                let (tvs, args, _, body0) = Utils.from_option (GlobalEnv.getFun loc genv f)
                    (fun _ -> raise (EvalError (loc, "Undeclared event " ^ pprint_ident f)))
                in
                GlobalEnv.addFun loc genv f (tvs, args, loc, List.append body body0)
        (* todo: when creating initial environment, should pass in a set of configuration
         * options that will override any default values given in definition
         *)
        | Decl_Config(v, ty, i, loc) ->
                (* todo: config constants need to be lazily evaluated or need to be
                 * sorted by dependencies
                 *)
                let init = eval_expr loc (Env.newEnv genv) i in
                GlobalEnv.addGlobalConst genv v init

        (* The following declarations are part of the mutable global state *)
        | Decl_Var(ty, v, loc)
        -> ()

        (* The following declarations have no impact on execution *)
        | Decl_BuiltinType (_, _)           | Decl_Forward (_, _)
        | Decl_BuiltinFunction (_, _, _, _, _)
        | Decl_FunType (_, _, _, _, _)         | Decl_ProcType (_, _, _, _)
        | Decl_VarGetterType (_, _, _, _)      | Decl_ArrayGetterType (_, _, _, _, _)
        | Decl_VarSetterType (_, _, _, _, _)   | Decl_ArraySetterType (_, _, _, _, _, _)
        | Decl_Operator1 (_, _, _)
        | Decl_Operator2 (_, _, _)
        | Decl_MapClause (_, _, _, _, _)
        -> ()
        )
    ) ds;
    genv
end

(** Construct environment from global declarations *)
let build_evaluation_environment (ds: AST.declaration list): Env.t = begin
    let genv = build_constant_environment ds in
    let env = Env.newEnv genv in
    List.iter (fun d ->
        (match d with
        | Decl_Var(v, ty, loc) ->
                let init = eval_uninitialized loc (Env.newEnv genv) ty in
                Env.addGlobalVar env v init

        | _ -> ()
        )
    ) ds;
    env
end


(****************************************************************
 * End
 ****************************************************************)
