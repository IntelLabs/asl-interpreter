(****************************************************************
 * ASL utility functions
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL utility functions *)

module AST = Asl_ast
module FMT = Asl_fmt
module FMTUtils = Format_utils
open AST
open Asl_visitor

(****************************************************************)
(** {2 Bindings and IdentSet}                                   *)
(****************************************************************)

module Bindings = Map.Make (AST.Id)
(** {2 Bindings: maps indexed by identifiers} *)

(** add association list to bindings *)
let add_bindings (bs : 'a Bindings.t) (xs : (ident * 'a) list) : 'a Bindings.t =
  List.fold_left (fun a (k, v) -> Bindings.add k v a) bs xs

(** create bindings from association list *)
let mk_bindings (xs : (ident * 'a) list) : 'a Bindings.t =
  add_bindings Bindings.empty xs

(** print bindings *)
let pp_bindings (pp : 'a -> string) (bs : 'a Bindings.t) : unit =
  Bindings.iter (fun k v -> Printf.printf "%s: %s\n" (pprint_ident k) (pp v)) bs

(****************************************************************)
(** {2 Scopes}                                                  *)
(****************************************************************)

(** A mutable binding *)
module Scope : sig
  type 'a t

  val empty : unit -> 'a t

  (* make a clean copy that can be independently mutated *)
  val clone : 'a t -> 'a t
  val mem : 'a t -> ident -> bool
  val get : 'a t -> ident -> 'a option
  val set : 'a t -> ident -> 'a -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val map_inplace : ('a -> 'a) -> 'a t -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val merge_inplace : ('a -> 'b -> 'a) -> 'a t -> 'b t -> unit
  val bindings : 'a t -> (ident * 'a) list
  val pp : ('a -> string) -> 'a t -> unit
end = struct
  type 'a t = { mutable bs : 'a Bindings.t }

  let pp (pp_value : 'a -> string) (env : 'a t) : unit =
    Printf.printf "{\n";
    pp_bindings pp_value env.bs;
    Printf.printf "}\n"

  let empty () : 'a t =
    let bs = Bindings.empty in
    { bs }

  (* create copy of a scope that can be independently mutated *)
  let clone (s : 'a t) : 'a t = { bs = s.bs }
  let mem (s : 'a t) (k : ident) : bool = Bindings.mem k s.bs
  let get (s : 'a t) (k : ident) : 'a option = Bindings.find_opt k s.bs
  let set (s : 'a t) (k : ident) (v : 'a) : unit = s.bs <- Bindings.add k v s.bs
  let map (f : 'a -> 'b) (s : 'a t) : 'b t = { bs = Bindings.map f s.bs }

  let filter_map (f : 'a -> 'b option) (s : 'a t) : 'b t =
    { bs = Bindings.filter_map (fun k a -> f a) s.bs }

  let map_inplace (f : 'a -> 'a) (s : 'a t) : unit = s.bs <- Bindings.map f s.bs

  let map2 (f : 'a -> 'b -> 'c) (env1 : 'a t) (env2 : 'b t) : 'c t =
    let merge v oa ob =
      match (oa, ob) with Some a, Some b -> Some (f a b) | _ -> None
    in
    { bs = Bindings.merge merge env1.bs env2.bs }

  let merge_inplace (f : 'a -> 'b -> 'a) (env1 : 'a t) (env2 : 'b t) : unit =
    let merge v oa ob =
      match (oa, ob) with Some a, Some b -> Some (f a b) | _ -> None
    in
    env1.bs <- Bindings.merge merge env1.bs env2.bs

  let bindings (env : 'a t) : (ident * 'a) list = Bindings.bindings env.bs
end

(* A collection of nested mutable scopes *)
module ScopeStack : sig
  type 'a t

  val empty : unit -> 'a t

  (* make a clean copy that can be independently mutated *)
  val clone : 'a t -> 'a t
  val add : 'a t -> ident -> 'a -> unit
  val mem : 'a t -> ident -> bool
  val get : 'a t -> ident -> 'a option
  val set : 'a t -> ident -> 'a -> bool
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val map_inplace : ('a -> 'a) -> 'a t -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val merge_inplace : ('a -> 'b -> 'a) -> 'a t -> 'b t -> unit
  val nest : 'a t -> ('a t -> 'b) -> 'b
  val bindings : 'a t -> (ident * 'a) list list
  val pp : ('a -> string) -> 'a t -> unit
end = struct
  type 'a t = 'a Scope.t list

  let pp (pp_value : 'a -> string) (env : 'a t) : unit =
    List.iter (Scope.pp pp_value) env

  let empty () : 'a t =
    let base : 'a Scope.t = Scope.empty () in
    [ base ]

  let clone (ss : 'a t) : 'a Scope.t list = List.map Scope.clone ss

  let add (ss : 'a t) (x : ident) (v : 'a) : unit =
    match ss with
    | s :: _ -> Scope.set s x v
    | [] -> failwith "ScopeStack.add: broken invariant"

  let rec get (ss : 'a t) (x : ident) : 'a option =
    match ss with
    | s :: ss' -> (
        match Scope.get s x with Some v -> Some v | None -> get ss' x)
    | [] -> None

  let mem (ss : 'a t) (x : ident) : bool = Option.is_some (get ss x)

  let rec set (ss : 'a t) (x : ident) (v : 'a) : bool =
    match ss with
    | s :: ss' ->
        if Scope.mem s x then (
          Scope.set s x v;
          true)
        else set ss' x v
    | [] -> false

  let map (f : 'a -> 'b) (env : 'a t) : 'b t = List.map (Scope.map f) env

  let filter_map (f : 'a -> 'b option) (env : 'a t) : 'b t =
    List.map (Scope.filter_map f) env

  let map_inplace (f : 'a -> 'a) (env : 'a t) : unit =
    List.iter (Scope.map_inplace f) env

  let map2 (f : 'a -> 'b -> 'c) (env1 : 'a t) (env2 : 'b t) : 'c t =
    List.map2 (Scope.map2 f) env1 env2

  let merge_inplace (f : 'a -> 'b -> 'a) (env1 : 'a t) (env2 : 'b t) : unit =
    List.iter2 (Scope.merge_inplace f) env1 env2

  let nest (env : 'a t) (k : 'a t -> 'b) : 'b =
    let newscope = Scope.empty () in
    k (newscope :: env)

  let bindings (env : 'a t) : (ident * 'a) list list =
    List.map Scope.bindings env
end

module IdentSet = Set.Make (Id)
(** {2 Sets of identifiers} *)

(** merge a list of sets *)
let unionSets (idss : IdentSet.t list) : IdentSet.t =
  List.fold_left IdentSet.union IdentSet.empty idss

(** add v to set of identifiers mapped to k *)
let addToBindingSet (k : ident) (v : ident) (bs : IdentSet.t Bindings.t) :
    IdentSet.t Bindings.t =
  Bindings.update k
    (fun old ->
      match old with
      | None -> Some (IdentSet.singleton v)
      | Some vs -> Some (IdentSet.add v vs))
    bs

(** convert identifier set to sorted list of identifiers

    The implementation is trivial and exists mostly to emphasize that the
    resulting list is sorted
 *)
let to_sorted_list (s : IdentSet.t) : ident list = IdentSet.elements s

(****************************************************************)
(** {2 Equivalence classes}                                     *)
(****************************************************************)

type tree = { mutable parent : tree; data : ident }
(** Equivalence classes are represented by trees.

    The root of the tree is the canonical member of the class.
    Traversing the parent node takes you closer to the canonical member.
    The root is its own parent.
 *)

(** Equivalence class support (to support unification, and similar)

    The implementation is based on
    {{:https://en.wikipedia.org/wiki/Disjoint-set_data_structure}Wikipedia: Union-Find}.
    I have not implemented all the optimizations they suggest
    because I expect sets to be quite small in practice.
 *)

class equivalences =
  object (self)
    (* Mapping from elements to the set containing them *)
    val mutable forest : tree Bindings.t = Bindings.empty

    (* Find the root (canonical member of) the set.
     * Implements "path-splitting" optimisation that makes every node
     * point to its grandfather so each traversal reduces height of tree.
     *)
    method private find (x : tree) : tree =
      let r = ref x in
      while !r.parent != !r do
        let next = !r.parent in
        !r.parent <- next.parent;
        r := next
      done;
      !r

    (* Find the root of the set containing 'x' - creating a new
     * set if not already known *)
    method private find_ident (x : ident) : tree =
      let s =
        match Bindings.find_opt x forest with
        | None ->
            let rec t = { parent = t; data = x } in
            t
        | Some t -> self#find t
      in
      forest <- Bindings.add x s forest;
      s

    (* Find the canonical member of the set containing 'x' *)
    method canonicalize (x : ident) : ident =
      let s = self#find_ident x in
      s.data

    (* Merge the sets containing 'x' and 'y' *)
    method merge (x : ident) (y : ident) : unit =
      let x' = self#find_ident x in
      let y' = self#find_ident y in
      if x != y then y'.parent <- x'

    (* Optimization: short circuit every tree so that they all point directly at
       root *)
    method private normalize : unit = forest <- Bindings.map self#find forest

    (* Return mapping from identifiers to the canonical representation of their
     * equivalence class
     *)
    method mapping : ident Bindings.t =
      self#normalize;
      Bindings.map (fun t -> (self#find t).data) forest

    (* Construct equivalence classes for each canonical member of a class.
     *
     * The implementation of this could be made more efficient by adding
     * pointers to trees so that we can map each canonical member to a
     * tree containing all the nodes that point to it.
     * But this implementation just does a linear scan over all the members
     * of the forest.
     *)
    method classes : IdentSet.t Bindings.t =
      Bindings.fold (fun k v -> addToBindingSet v k) self#mapping Bindings.empty

    (* Print equivalence classes adding a prefix at the start of every line of
     * output.
     *)
    method pp (fmt : Format.formatter) (prefix : string) : unit =
      Format.pp_open_vbox fmt 0;
      Bindings.iter
        (fun v vs ->
          Format.pp_print_string fmt prefix;
          FMT.varname fmt v;
          Format.fprintf fmt "-> {";
          IdentSet.iter
            (fun w ->
              FMT.varname fmt w;
              FMTUtils.nbsp fmt)
            vs;
          Format.fprintf fmt "}";
          Format.pp_print_space fmt ())
        self#classes;
      Format.pp_close_box fmt ()
  end

(****************************************************************)
(** {1 AST Transformation Utilities}                            *)
(****************************************************************)

(****************************************************************)
(** {2 Calculating free variables of expressions and types}     *)
(****************************************************************)

class freevarClass =
  object
    inherit nopAslVisitor
    val mutable free_vars = IdentSet.empty
    val mutable free_tcs = IdentSet.empty
    val mutable free_funs = IdentSet.empty
    method vars = free_vars
    method tycons = free_tcs
    method funs = free_funs

    method! vvar x =
      free_vars <- IdentSet.add x free_vars;
      SkipChildren

    method! vtype ty =
      match ty with
      | Type_Constructor tc ->
          free_tcs <- IdentSet.add tc free_tcs;
          DoChildren
      | Type_App (tc, _) ->
          free_tcs <- IdentSet.add tc free_tcs;
          DoChildren
      | Type_Register _ ->
          (* Free variables in register types are not supported and will
             lead to a type error.

             Uses of global constants and variables in the indices of field
             declarations of a register type are allowed, though, and will
             be checked by the type checker as usual.  Note that they will
             not be evaluated at register declaration time, but every time
             the respective register field is accessed (the type checker
             desugars register field accesses to slice expressions, copying
             the field indices). *)
          SkipChildren
      | _ -> DoChildren

    method! vexpr e =
      match e with
      | Expr_TApply (f, tes, es) ->
          free_funs <- IdentSet.add f free_funs;
          DoChildren
      | _ -> DoChildren

    method! vlexpr =
      function
      | LExpr_Var v ->
          free_vars <- IdentSet.add v free_vars;
          SkipChildren
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_TCall (f, tes, args, loc) ->
          free_funs <- IdentSet.add f free_funs;
          DoChildren
      | _ -> DoChildren

    method! leave_scope (vs : ident list) =
      (* remove reads/writes to local variables *)
      List.iter (fun v -> free_vars <- IdentSet.remove v free_vars) vs
  end

let fv_expr (x : expr) : IdentSet.t =
  let fv = new freevarClass in
  ignore (visit_expr (fv :> aslVisitor) x);
  fv#vars

let fv_type (x : ty) : IdentSet.t =
  let fv = new freevarClass in
  ignore (visit_type (fv :> aslVisitor) x);
  fv#vars

let fv_args (atys : (ident * ty) list) : IdentSet.t =
  unionSets (List.map (fun (_, ty) -> fv_type ty) atys)

let fv_stmts stmts =
  let fvs = new freevarClass in
  ignore (visit_stmts (fvs :> aslVisitor) stmts);
  fvs#vars

let fv_decl decl =
  let fvs = new freevarClass in
  ignore (visit_decl (fvs :> aslVisitor) decl);
  fvs#vars

(****************************************************************)
(** {2 Calculating assigned variables in statements}            *)
(****************************************************************)

class assignedVarsClass =
  object
    inherit nopAslVisitor
    val mutable avs = IdentSet.empty
    method result = avs

    method! vlvar x =
      avs <- IdentSet.add x avs;
      SkipChildren
  end

let assigned_vars_of_stmts stmts =
  let avs = new assignedVarsClass in
  ignore (visit_stmts (avs :> aslVisitor) stmts);
  avs#result

let assigned_vars_of_decl decl =
  let avs = new assignedVarsClass in
  ignore (visit_decl (avs :> aslVisitor) decl);
  avs#result

(****************************************************************)
(** {2 Calculating subexpressions}                              *)
(****************************************************************)

(** Collect subexpressions of an expression *)
class subExprsClass (root : expr) =
  object
    inherit nopAslVisitor
    val mutable exprs : expr list = []
    method result = exprs

    method! vexpr x =
      if x == root then DoChildren
      else (
        exprs <- x :: exprs;
        SkipChildren)
  end

let subexprs_of_expr (x : expr) : expr list =
  let subs = new subExprsClass x in
  ignore (visit_expr (subs :> aslVisitor) x);
  subs#result

(****************************************************************)
(** {2 Collect local bindings (variables and constants)}        *)
(****************************************************************)

(*
class localsClass = object (self)
    inherit nopAslVisitor

    val mutable stack = [(Bindings.empty : ty Bindings.t)]
    method locals =
        let merge _ x y = Some x in
        List.fold_right (Bindings.union merge) stack Bindings.empty

    method add_local (id, ty) =
        match stack with
        | s :: ss -> stack <- (Bindings.add id ty s :: ss)
        | [] -> failwith "addLocal: empty stack"
    method! enter_scope vars =
        stack <- Bindings.empty :: stack;
        List.iter self#add_local vars
    method! leave_scope () =
        match stack with
        | s :: ss -> stack <- ss
        | [] -> failwith "leave_scope: empty stack"
    method! vstmt = function
        | Stmt_VarDecl (id, ty, _, _)
        | Stmt_ConstDecl (id, ty, _, _) ->
            self#add_local (id, ty);
            DoChildren
        | Stmt_VarDeclsNoInit (ids, ty, _) ->
            List.iter (fun id -> self#add_local (id, ty)) ids;
            DoChildren
        | _ ->
            DoChildren
end

let locals_of_stmts stmts =
    let lc = new localsClass in
    ignore (Visitor.mapNoCopy (visit_stmt (lc :> aslVisitor)) stmts);
    lc#locals

let locals_of_decl decl =
    let lc = new localsClass in
    ignore (Visitor.mapNoCopy (visit_decl (lc :> aslVisitor)) decl);
    lc#locals
*)

(****************************************************************)
(** {2 Calculate types used in expressions and statements}      *)
(****************************************************************)

class typesClass =
  object
    inherit nopAslVisitor
    val mutable types = IdentSet.empty
    method result = types

    method! vtype ty =
      match ty with
      | Type_Constructor id | Type_App (id, _) ->
          types <- IdentSet.add id types;
          DoChildren
      | _ -> DoChildren
  end

let types_of_expr expr =
  let cc = new typesClass in
  ignore (visit_expr (cc :> aslVisitor) expr);
  cc#result

let types_of_stmts stmts =
  let cc = new typesClass in
  ignore (visit_stmts (cc :> aslVisitor) stmts);
  cc#result

let types_of_decl decl =
  let cc = new typesClass in
  ignore (visit_decl (cc :> aslVisitor) decl);
  cc#result

(****************************************************************)
(** {2 Calculate functions and procedures called in statements} *)
(****************************************************************)

class callsClass =
  object
    inherit nopAslVisitor
    val mutable calls = IdentSet.empty
    method result = calls

    method! vexpr =
      function
      | Expr_TApply (f, _, _) ->
          calls <- IdentSet.add f calls;
          DoChildren
      | _ -> DoChildren

    method! vstmt =
      function
      | Stmt_TCall (id, _, _, _) ->
          calls <- IdentSet.add id calls;
          DoChildren
      | _ -> DoChildren

    method! vlexpr =
      function
      | LExpr_Write (id, _, _) ->
          calls <- IdentSet.add id calls;
          DoChildren
      | LExpr_ReadWrite (id1, id2, _, _) ->
          calls <- IdentSet.add id1 calls |> IdentSet.add id2;
          DoChildren
      | _ -> DoChildren
  end

let calls_of_expr expr =
  let cc = new callsClass in
  ignore (visit_expr (cc :> aslVisitor) expr);
  cc#result

let calls_of_stmts stmts =
  let cc = new callsClass in
  ignore (visit_stmts (cc :> aslVisitor) stmts);
  cc#result

let calls_of_decl decl =
  let cc = new callsClass in
  ignore (visit_decl (cc :> aslVisitor) decl);
  cc#result

(****************************************************************)
(** {2 Keep definitions reachable from roots}                   *)
(****************************************************************)

let decl_name (x : declaration) : ident option =
  match x with
  | Decl_BuiltinType (v, loc) -> Some v
  | Decl_Forward (v, loc) -> Some v
  | Decl_Record (v, fs, loc) -> Some v
  | Decl_Typedef (v, ty, loc) -> Some v
  | Decl_Enum (v, es, loc) -> Some v
  | Decl_Var (v, ty, loc) -> Some v
  | Decl_Const (v, ty, e, loc) -> Some v
  | Decl_BuiltinFunction (f, ps, args, ty, loc) -> Some f
  | Decl_FunType (f, ps, args, ty, loc) -> Some f
  | Decl_FunDefn (f, ps, args, ty, b, loc) -> Some f
  | Decl_ProcType (f, ps, args, loc) -> Some f
  | Decl_ProcDefn (f, ps, args, b, loc) -> Some f
  | Decl_VarGetterType (f, ps, ty, loc) -> Some f
  | Decl_VarGetterDefn (f, ps, ty, b, loc) -> Some f
  | Decl_ArrayGetterType (f, ps, args, ty, loc) -> Some f
  | Decl_ArrayGetterDefn (f, ps, args, ty, b, loc) -> Some f
  | Decl_VarSetterType (f, ps, v, ty, loc) -> Some f
  | Decl_VarSetterDefn (f, ps, v, ty, b, loc) -> Some f
  | Decl_ArraySetterType (f, ps, args, v, ty, loc) -> Some f
  | Decl_ArraySetterDefn (f, ps, args, v, ty, b, loc) -> Some f
  | Decl_Operator1 (op, vs, loc) -> None
  | Decl_Operator2 (op, vs, loc) -> None
  | Decl_NewEventDefn (v, ps, args, loc) -> Some v
  | Decl_EventClause (v, b, loc) -> Some v
  | Decl_NewMapDefn (v, ps, args, ty, b, loc) -> Some v
  | Decl_MapClause (v, fs, oc, b, loc) -> Some v
  | Decl_Config (v, ty, e, loc) -> Some v

let decl_map_of (ds : declaration list) : declaration Bindings.t =
  (* Map of declarations *)
  let decls : declaration Bindings.t ref = ref Bindings.empty in
  List.iter
    (fun d ->
      match decl_name d with
      | Some nm -> decls := Bindings.add nm d !decls
      | None -> ())
    ds;
  !decls

(* construct map of Union { x -> f x | for x in xs } *)
let memoize (xs : ident list) (f : ident -> IdentSet.t) : IdentSet.t Bindings.t
    =
  let results = ref Bindings.empty in
  List.iter (fun x -> results := Bindings.add x (f x) !results) xs;
  !results

(* construct map of Union { f x -> x | for x in xs } *)
let rev_memoize (xs : ident list) (f : ident -> IdentSet.t) :
    IdentSet.t Bindings.t =
  let results = ref Bindings.empty in
  List.iter
    (fun x ->
      let ys = f x in
      IdentSet.iter
        (fun y ->
          let prev =
            Option.value (Bindings.find_opt y !results) ~default:IdentSet.empty
          in
          results := Bindings.add y (IdentSet.add x prev) !results)
        ys)
    xs;
  !results

(* topologically sorted list of objects reachable from roots *)
let reach (next : ident -> IdentSet.t) (roots : ident list) : ident list =
  let result : ident list ref = ref [] in
  let seen : IdentSet.t ref = ref IdentSet.empty in
  let rec visit (ls : ident list) : unit =
    match ls with
    | [] -> ()
    | h :: t when IdentSet.mem h !seen -> visit t
    | h :: t ->
        seen := IdentSet.add h !seen;
        result := h :: !result;
        visit (IdentSet.elements (next h) @ t)
  in
  visit roots;
  !result

(* f (find x bs) if x in bs, empty otherwise *)
let bindings_to_function (bs : 'a Bindings.t) (f : 'a -> IdentSet.t) (x : ident)
    : IdentSet.t =
  Option.value
    (Option.map f (Bindings.find_opt x bs))
    ~default:IdentSet.empty

(* Result is topologically sorted list of everything reachable from roots *)
let reachable_decls (roots : ident list) (ds : declaration list) :
    declaration list =
  let next (d : declaration) : IdentSet.t =
    let fvs = new freevarClass in
    ignore (visit_decl (fvs :> aslVisitor) d);
    let refs = fvs#vars in
    let refs = IdentSet.union fvs#funs refs in
    let refs = IdentSet.union fvs#tycons refs in
    refs
  in

  let decls = decl_map_of ds in
  let reachable = reach (bindings_to_function decls next) roots in
  (* List.iter (fun r -> Printf.printf "%s\n" (pprint_ident r)) reachable; *)
  let r = Utils.flatmap_option (fun x -> Bindings.find_opt x decls) reachable in
  List.rev r

let callers (leaves : ident list) (ds : declaration list) : IdentSet.t =
  let next (d : declaration) : IdentSet.t =
    let fvs = new freevarClass in
    ignore (visit_decl (fvs :> aslVisitor) d);
    fvs#funs
  in

  let decls = decl_map_of ds in
  let fs = List.filter Id.isFunction (List.map fst (Bindings.bindings decls)) in
  let rev_next = rev_memoize fs (bindings_to_function decls next) in
  IdentSet.of_list (reach (bindings_to_function rev_next Fun.id) leaves)

(****************************************************************)
(** {2 Side effect detection}                                   *)
(****************************************************************)

class sideEffectClass =
  object
    inherit nopAslVisitor
    val mutable reads = IdentSet.empty
    val mutable writes = IdentSet.empty
    val mutable functions_called = IdentSet.empty
    val mutable throws_exceptions = false

    method sideEffects : IdentSet.t * IdentSet.t * IdentSet.t * bool =
      (reads, writes, functions_called, throws_exceptions)

    method! vlvar x =
      writes <- IdentSet.add x writes;
      SkipChildren

    method! vvar x =
      reads <- IdentSet.add x reads;
      SkipChildren

    method! vexpr e =
      match e with
      | Expr_TApply (f, tes, es) ->
          functions_called <- IdentSet.add f functions_called;
          DoChildren
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_TCall (f, tes, args, loc) ->
          functions_called <- IdentSet.add f functions_called;
          DoChildren
      | Stmt_Throw _ ->
          throws_exceptions <- true;
          DoChildren
      | _ -> DoChildren

    method! leave_scope (vs : ident list) =
      (* remove reads/writes to local variables *)
      List.iter (fun v -> writes <- IdentSet.remove v writes) vs;
      List.iter (fun v -> reads <- IdentSet.remove v reads) vs
  end

let identify_impure_funs (isConstant : ident -> bool)
    (isImpurePrim : ident -> bool) (ds : declaration list) : IdentSet.t =
  let is_pure_prim (d : declaration) : bool =
    match d with
    | Decl_BuiltinFunction (f, ps, args, ty, loc) -> not (isImpurePrim f)
    | _ -> false
  in
  let locally_impure (d : declaration) : bool =
    let se = new sideEffectClass in
    ignore (visit_decl (se :> aslVisitor) d);
    let rds, wrs, callees, throws = se#sideEffects in
    let vrds = IdentSet.filter (fun v -> not (isConstant v)) rds in
    throws || (not (IdentSet.is_empty vrds)) || not (IdentSet.is_empty wrs)
  in

  let impure = ref IdentSet.empty in
  List.iter
    (fun d ->
      match decl_name d with
      | Some x when IdentSet.mem x !impure || (not (is_pure_prim d) && locally_impure d)
        ->
          impure := IdentSet.add x !impure
      | _ -> ())
    ds;

  (* globally impure if it calls a locally impure function *)
  callers (IdentSet.elements !impure) ds

(****************************************************************)
(** {2 Substitutions}                                           *)
(****************************************************************)

(** Performing variable substitutions in expressions and types

    Note that it does not replace type constructors, global constants
    or enumerations in patterns, array indexes and types so this is
    limited to replacing local variables.
    It also does not replace variables used as l-expressions though
    that it easily changed if we think it should.               *)
class substClass (s : expr Bindings.t) =
  object
    inherit nopAslVisitor

    method! vexpr x =
      match x with
      | Expr_Var v -> (
          match Bindings.find_opt v s with
          | Some r -> ChangeTo r
          | None -> DoChildren)
      | _ -> DoChildren
  end

let subst_expr (s : expr Bindings.t) (x : expr) : expr =
  let subst = new substClass s in
  visit_expr subst x

let subst_lexpr (s : expr Bindings.t) (x : lexpr) : lexpr =
  let subst = new substClass s in
  visit_lexpr subst x

let subst_var (s : expr Bindings.t) (x : ident) : ident =
  let subst = new substClass s in
  visit_var subst x

let subst_slice (s : expr Bindings.t) (x : slice) : slice =
  let subst = new substClass s in
  visit_slice subst x

let subst_type (s : expr Bindings.t) (x : ty) : ty =
  let subst = new substClass s in
  visit_type subst x

let subst_decl_item (s : expr Bindings.t) (x : decl_item) : decl_item =
  let subst = new substClass s in
  visit_decl_item subst x

(** More flexible substitution class - takes a function instead
    of a binding set.
 *)
class substFunClass (replace : ident -> expr option) =
  object
    inherit nopAslVisitor

    method! vexpr x =
      match x with
      | Expr_Var v -> (
          match replace v with Some r -> ChangeTo r | None -> DoChildren)
      | _ -> DoChildren
  end

let subst_fun_expr (replace : ident -> expr option) (x : expr) : expr =
  let subst = new substFunClass replace in
  visit_expr subst x

let subst_fun_lexpr (replace : ident -> expr option) (x : lexpr) : lexpr =
  let subst = new substFunClass replace in
  visit_lexpr subst x

let subst_fun_slice (replace : ident -> expr option) (x : slice) : slice =
  let subst = new substFunClass replace in
  visit_slice subst x

let subst_fun_type (replace : ident -> expr option) (x : ty) : ty =
  let subst = new substFunClass replace in
  visit_type subst x

(****************************************************************)
(** {2 Expression transformation}                               *)
(****************************************************************)

(** Expression transformation class

    Applies replace function to any subexpression.
    (Especially useful for expressions in types)                *)
class replaceExprClass (replace : expr -> expr option) =
  object
    inherit nopAslVisitor

    method! vexpr x =
      match replace x with Some r -> ChangeTo r | None -> SkipChildren
  end

(****************************************************************)
(** {2 Resugaring}                                              *)
(****************************************************************)

(** Resugaring transform

    The typechecker desugars infix syntax to make it absolutely explicit
    what it means.  This is good for tools but bad for humans.

    This transformation re-introduces the infix syntax - the intention
    being that you might use this in error messages.
    It also deletes type parameters - so this is (more or less)
    the reverse of typechecking.                                *)
class resugarClass (ops : AST.binop Bindings.t) =
  object (self)
    inherit nopAslVisitor

    method! vexpr x =
      match x with
      | Expr_TApply (f, tys, args) -> (
          let args' = List.map (visit_expr (self :> aslVisitor)) args in
          match (Bindings.find_opt f ops, args') with
          | Some op, [ a; b ] -> ChangeTo (Expr_Binop (a, op, b))
          (* | (Some op, [a]) -> ChangeTo (Expr_Unop(op, a)) *)
          | _ -> ChangeTo (Expr_TApply (f, [], args')))
      | _ -> DoChildren
  end

let resugar_expr (ops : AST.binop Bindings.t) (x : expr) : expr =
  let resugar = new resugarClass ops in
  visit_expr resugar x

let resugar_type (ops : AST.binop Bindings.t) (x : AST.ty) : AST.ty =
  let resugar = new resugarClass ops in
  visit_type resugar x

(****************************************************************)
(** {2 Pretty printing wrappers}                                *)
(****************************************************************)

let pp_unop (x : unop) : string = Utils.to_string2 (Fun.flip FMT.unop x)
let pp_binop (x : binop) : string = Utils.to_string2 (Fun.flip FMT.binop x)
let pp_type (x : ty) : string = Utils.to_string2 (Fun.flip FMT.ty x)
let pp_expr (x : expr) : string = Utils.to_string2 (Fun.flip FMT.expr x)
let pp_lexpr (x : lexpr) : string = Utils.to_string2 (Fun.flip FMT.lexpr x)
let pp_stmt (x : stmt) : string = Utils.to_string2 (Fun.flip FMT.stmt x)

(****************************************************************)
(** {2 Misc}                                                    *)
(****************************************************************)

(** Length of bitstring or mask literal.

    ASL bit and mask literals allow spaces to be included - these
    do not count towards the length of the literal.
 *)
let masklength (x : string) : int =
  let r = ref 0 in
  String.iter (function ' ' -> () | _ -> r := !r + 1) x;
  !r

(** Test whether a function returns a tuple (of 2 or more elements). *)
let isTupleType (t : AST.ty) : bool =
  match t with
  | AST.Type_Tuple (_ :: _ :: _) -> true
  | _ -> false

(** Deconstruct a tuple type (return `[t]` if not a tuple type *)
let tupleTypes (t : AST.ty) : AST.ty list =
  match t with
  | AST.Type_Tuple ts -> ts
  | _ -> [t]

(****************************************************************
 * End
 ****************************************************************)
