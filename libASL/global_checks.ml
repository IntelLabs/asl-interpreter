(****************************************************************
 * Global checks
 *
 * This module performs global checks that rely on analyzing
 * the call-graph. For example, to check whether a function
 * transitively accesses a global variable or transitively
 * throws an exception.
 *
 * Checks performed:
 *
 * - Does the meaning of an expression depend on evaluation order?
 * - Are calls to functions that can throw exceptions marked as rethrowing?
 *
 * Copyright (C) 2023-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
module FMT = Asl_fmt
open Asl_utils
open Builtin_idents

(** Add one entry to a set *)
let add_one (f : Ident.t) (x : Ident.t) (bs : IdentSet.t Bindings.t) : IdentSet.t Bindings.t =
  Bindings.update
    f
    (function
    | None -> Some (IdentSet.singleton x)
    | Some s -> Some (IdentSet.add x s)
    )
    bs

(** Add multiple entries to a set *)
let add (f : Ident.t) (xs : IdentSet.t) (bs : IdentSet.t Bindings.t) : IdentSet.t Bindings.t =
  Bindings.update
    f
    (function
    | None -> Some xs
    | Some s -> Some (IdentSet.union s xs)
    )
    bs

let get (f : Ident.t) (bs : IdentSet.t Bindings.t) : IdentSet.t =
  Option.value ~default:IdentSet.empty (Bindings.find_opt f bs)

(** Calculates the effects of all functions in a set of declarations.
 *
 * Effects include
 * - any global variables read
 * - any global variables written
 * - whether it throws an exception
 *
 *)
class effects_class
    (is_constant : Ident.t -> bool)
    (is_impure_prim : Ident.t -> bool)
    (ds : AST.declaration list) =

  object (self)
    val mutable globals : IdentSet.t = IdentSet.empty
    val mutable throws : IdentSet.t = IdentSet.empty
    val mutable callees : IdentSet.t Bindings.t = Bindings.empty
    val mutable callers : IdentSet.t Bindings.t = Bindings.empty
    val mutable reads : IdentSet.t Bindings.t = Bindings.empty
    val mutable writes : IdentSet.t Bindings.t = Bindings.empty

    method is_global (x : Ident.t) : bool = IdentSet.mem x globals

    method fun_effects (f : Ident.t) : (IdentSet.t * IdentSet.t * bool) =
      (get f reads, get f writes, IdentSet.mem f throws)

    method add_effects (f : Ident.t) (rds : IdentSet.t) (wrs : IdentSet.t) (throws_exn : bool) : bool =
      let changed = ref false in
      changed := !changed || (throws_exn && not (IdentSet.mem f throws));
      changed := !changed || not (IdentSet.subset rds (get f reads));
      changed := !changed || not (IdentSet.subset wrs (get f writes));
      if throws_exn then throws <- IdentSet.add f throws;
      reads <- add f rds reads;
      writes <- add f wrs writes;
      !changed

    (* Update the effects of 'f' with the effects of all functions that it calls.
     * Return 'true' if this adds anything new.
     *)
    method private update (f : Ident.t) : bool =
      let changed = ref false in
      IdentSet.iter (fun g ->
          let (rds, wrs, throws_exn) = self#fun_effects g in
          changed := self#add_effects f rds wrs throws_exn || !changed
        )
        (get f callees);
      !changed

    (* Propagate the effects of any changed functions to their callers *)
    method private fixpoint (fs : IdentSet.t) : unit =
      if not (IdentSet.is_empty fs) then begin
        let parents =
          IdentSet.elements fs
          |> List.map (fun f -> get f callers)
          |> Asl_utils.unionSets
        in
        let next = IdentSet.filter self#update parents in
        self#fixpoint next
      end

    initializer begin
      (* start by calculating the local effects of each function *)
      List.iter (fun d ->
        decl_name d |>
        Option.iter (fun nm ->
            let (rds, wrs, calls, throws_exn) = side_effects_of_decl d in
            globals <- IdentSet.add nm globals;
            if throws_exn then throws <- IdentSet.add nm throws;
            reads <- add nm rds reads;
            writes <- add nm wrs writes;
            callees <- add nm calls callees;
            IdentSet.iter (fun f -> callers <- add_one f nm callers) calls
        )
      ) ds;

      (* then compute the fixpoint *)
      self#fixpoint globals
    end
  end

(** Check that expression value does not depend on evaluation order
 *
 * This check is done by checking that we don't have two disjoint
 * subexpressions 'e1' and 'e2' such that
 *
 * - 'e1' writes a global variable that 'e2' reads or writes
 * - 'e1' writes a global variable and 'e2' can throw an exception
 * - 'e1' and 'e2' can both throw an exception
 *)
let check_effect_conflicts
    (loc : AST.l)
    ((e1, fx1) : AST.expr * (IdentSet.t * IdentSet.t * bool))
    ((e2, fx2) : AST.expr * (IdentSet.t * IdentSet.t * bool))
  : unit
  =
  let (rds1, wrs1, throws1) = fx1 in
  let (rds2, wrs2, throws2) = fx2 in
  let ww_conflicts = IdentSet.inter wrs1 wrs2 in
  let wr_conflicts = IdentSet.inter wrs1 rds2 in
  let wt_conflicts = throws1 && not (IdentSet.is_empty wrs2) in
  let throw_conflicts = throws1 && throws2 in
  if not (IdentSet.is_empty ww_conflicts) then begin
    let msg = Format.asprintf
        "expression behaviour depends on evaluation order:\
         @,    global variable(s) `%a` can be \
         written to by both subexpression `%a` \
         and subexpression `%a`"
        pp_identset ww_conflicts
        FMT.expr e1
        FMT.expr e2
    in
    raise (Tcheck.TypeError (loc, msg))
  end;
  if not (IdentSet.is_empty wr_conflicts) then begin
    let msg = Format.asprintf
        "expression behaviour depends on evaluation order:\
         @,   global variable(s) `%a` can be \
         written to by subexpression `%a` \
         and read from by subexpression `%a`"
        pp_identset wr_conflicts
        FMT.expr e1
        FMT.expr e2
    in
    raise (Tcheck.TypeError (loc, msg))
  end;
  if wt_conflicts then begin
    let msg = Format.asprintf
        "expression behaviour depends on evaluation order:\
         @,    subexpression `%a` can throw an exception \
         and subexpression `%a` can write to global variable(s) `%a`"
        FMT.expr e1
        FMT.expr e2
        pp_identset wrs2
    in
    raise (Tcheck.TypeError (loc, msg))
  end;
  if throw_conflicts then begin
    let msg = Format.asprintf
        "expression behaviour depends on evaluation order:\
         @,    subexpression `%a` \
         and subexpression `%a` can both throw exceptions"
        FMT.expr e1
        FMT.expr e2
    in
    raise (Tcheck.TypeError (loc, msg))
  end


(** Check immediate subexpressions for expression order dependencies
 *
 * Returns the set of reads, writes and whether 'e' throws an exception
 * Reports an error on failure.
 *)
let rec check_expression_order (loc : AST.l) (effects : effects_class) (e : AST.expr) : (IdentSet.t * IdentSet.t * bool) =
  ( match e with
  | Expr_Var v ->
      if effects#is_global v then
        (IdentSet.singleton v, IdentSet.empty, false)
      else
        (IdentSet.empty, IdentSet.empty, false)
  | _ ->
    let ordered = (* is evaluation order specified by ASL language? *)
      ( match e with
      | Expr_If _ -> true
      | Expr_TApply (i, _, _, _) when Ident.in_list i [
          and_bool;
          or_bool;
          implies_bool
        ]
        -> true
      | _
        -> false
      )
    in
    let es = subexprs_of_expr e in
    let fxs = List.map (fun e -> (e, check_expression_order loc effects e)) es in
    if not ordered then begin
      Utils.iter_pairs (check_effect_conflicts loc) fxs;
      Utils.iter_pairs (Fun.flip (check_effect_conflicts loc)) fxs
    end;
    let rds = Asl_utils.unionSets (List.map (fun (_, (r, _, _)) -> r) fxs) in
    let wrs = Asl_utils.unionSets (List.map (fun (_, (_, w, _)) -> w) fxs) in
    let throws = List.exists (fun (_, (_, _, t)) -> t) fxs in
    let (frds, fwrs, fthrows) =
      ( match e with
      | Expr_TApply (f, _, _, _) -> effects#fun_effects f
      | _ -> (IdentSet.empty, IdentSet.empty, false)
      )
    in
    (IdentSet.union frds rds, IdentSet.union fwrs wrs, fthrows || throws)
  )

(** Perform rethrow checks on the specification *)
class rethrow_checks_class (effects : effects_class) (loc : AST.l) =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      ( match x with
      | Expr_TApply (f, _, _, throws) ->
        let (_, _, fthrows) = effects#fun_effects f in
        if throws && not fthrows then begin
            let msg = Format.asprintf
                "call to function `%a` is incorrectly marked with `?` but it cannot throw an exception"
                FMT.varname f
            in
            raise (Tcheck.TypeError (loc, msg))
        end else
        if not throws && fthrows then begin
            let msg = Format.asprintf
                "call to function `%a` should be marked with `?` because it can throw an exception"
                FMT.varname f
            in
            raise (Tcheck.TypeError (loc, msg))
        end
      | _ -> ()
      );
      DoChildren

    method! vstmt x =
      ( match x with
      | Stmt_TCall (f, _, _, throws, loc) ->
        let (_, _, fthrows) = effects#fun_effects f in
        if throws && not fthrows then begin
            let msg = Format.asprintf
                "call to procedure `%a` is incorrectly marked with `?` but it cannot throw an exception"
                FMT.varname f
            in
            raise (Tcheck.TypeError (loc, msg))
        end else if not throws && fthrows then begin
            let msg = Format.asprintf
                "call to procedure `%a` should be marked with `?` because it can throw an exception"
                FMT.varname f
            in
            raise (Tcheck.TypeError (loc, msg))
        end
      | _ -> ()
      );
      SkipChildren
  end

(** Perform global checks on the specification *)
class global_checks_class (effects : effects_class) (loc : AST.l) =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      ignore (check_expression_order loc effects x);
      ignore (Asl_visitor.visit_expr (new rethrow_checks_class effects loc) x);
      SkipChildren

    method! vstmt x =
      ignore (Asl_visitor.visit_stmt (new rethrow_checks_class effects (stmt_loc x)) x);
      DoChildren
  end

(** Wrapper around global_checks_class that adds location information *)
class global_checks_class_wrapper (effects : effects_class) =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vstmt x =
      ignore (Asl_visitor.visit_stmt (new global_checks_class effects (stmt_loc x)) x);
      SkipChildren
  end


let check_decls (ds : AST.declaration list) : AST.declaration list =
  let genv = Eval.build_constant_environment ds in
  let is_constant (v : Ident.t) : bool =
    Option.is_some (Eval.GlobalEnv.get_global_constant genv v)
  in
  let is_impure_prim (v : Ident.t) : bool =
    List.exists (fun name -> Ident.matches v ~name) Value.impure_prims
  in
  let effects = new effects_class is_constant is_impure_prim ds in
  let checker = new global_checks_class_wrapper effects in
  List.iter (fun d -> ignore (Asl_visitor.visit_decl checker d)) ds;
  ds

(****************************************************************
 * End
 ****************************************************************)
