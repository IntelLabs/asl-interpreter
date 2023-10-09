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

module Bindings = Map.Make (Ident)
(** {2 Bindings: maps indexed by identifiers} *)

(** add association list to bindings *)
let add_bindings (bs : 'a Bindings.t) (xs : (Ident.t * 'a) list) : 'a Bindings.t =
  List.fold_left (fun a (k, v) -> Bindings.add k v a) bs xs

(** create bindings from association list *)
let mk_bindings (xs : (Ident.t * 'a) list) : 'a Bindings.t =
  add_bindings Bindings.empty xs

(** format bindings *)
let pp_bindings (f : Format.formatter -> 'a -> unit) (fmt : Format.formatter) (bs : 'a Bindings.t) : unit =
  Bindings.iter (fun k v -> Format.fprintf fmt "%a: %a\n" Asl_fmt.varname k f v) bs


(****************************************************************)
(** {2 Scopes}                                                  *)
(****************************************************************)

(** A mutable binding *)
module Scope = struct
  type 'a t = { mutable bs : 'a Bindings.t }

  let pp (f : Format.formatter -> 'a -> unit) (fmt : Format.formatter) (s : 'a t) : unit =
    Format.fprintf fmt "{\n";
    Format_utils.vbox fmt (fun _ -> pp_bindings f fmt s.bs);
    Format.fprintf fmt "}\n"

  let empty () : 'a t =
    let bs = Bindings.empty in
    { bs }

  let equal (eq : 'a -> 'a -> bool) (s1 : 'a t) (s2 : 'a t) : bool =
    Bindings.equal eq s1.bs s2.bs

  (* create copy of a scope that can be independently mutated *)
  let clone (s : 'a t) : 'a t = { bs = s.bs }
  let mem (s : 'a t) (k : Ident.t) : bool = Bindings.mem k s.bs
  let get (s : 'a t) (k : Ident.t) : 'a option = Bindings.find_opt k s.bs
  let set (s : 'a t) (k : Ident.t) (v : 'a) : unit = s.bs <- Bindings.add k v s.bs
  let map (f : 'a -> 'b) (s : 'a t) : 'b t = { bs = Bindings.map f s.bs }

  let filter_map (f : 'a -> 'b option) (s : 'a t) : 'b t =
    { bs = Bindings.filter_map (fun k a -> f a) s.bs }

  let map_inplace (f : 'a -> 'a) (s : 'a t) : unit = s.bs <- Bindings.map f s.bs

  let map2 (f : 'a -> 'b -> 'c) (s1 : 'a t) (s2 : 'b t) : 'c t =
    let merge v oa ob =
      match (oa, ob) with Some a, Some b -> Some (f a b) | _ -> None
    in
    { bs = Bindings.merge merge s1.bs s2.bs }

  let merge_inplace (f : 'a -> 'b -> 'a) (s1 : 'a t) (s2 : 'b t) : unit =
    let merge v oa ob =
      match (oa, ob) with Some a, Some b -> Some (f a b) | _ -> None
    in
    s1.bs <- Bindings.merge merge s1.bs s2.bs

  let bindings (s : 'a t) : (Ident.t * 'a) list = Bindings.bindings s.bs
end

(* A collection of nested mutable scopes *)
module ScopeStack = struct
  type 'a t = 'a Scope.t list

  let pp (pp_value : Format.formatter -> 'a -> unit) (fmt : Format.formatter) (ss : 'a t) : unit =
    List.iter (Scope.pp pp_value fmt) ss

  let empty () : 'a t =
    let base : 'a Scope.t = Scope.empty () in
    [ base ]

  let clone (ss : 'a t) : 'a Scope.t list = List.map Scope.clone ss

  let add (ss : 'a t) (x : Ident.t) (v : 'a) : unit =
    match ss with
    | s :: _ -> Scope.set s x v
    | [] -> failwith "ScopeStack.add: broken invariant"

  let rec get (ss : 'a t) (x : Ident.t) : 'a option =
    match ss with
    | s :: ss' -> (
        match Scope.get s x with Some v -> Some v | None -> get ss' x)
    | [] -> None

  let mem (ss : 'a t) (x : Ident.t) : bool = Option.is_some (get ss x)

  let rec set (ss : 'a t) (x : Ident.t) (v : 'a) : bool =
    match ss with
    | s :: ss' ->
        if Scope.mem s x then (
          Scope.set s x v;
          true)
        else
          set ss' x v
    | [] -> false

  let map (f : 'a -> 'b) (ss : 'a t) : 'b t = List.map (Scope.map f) ss

  let filter_map (f : 'a -> 'b option) (ss : 'a t) : 'b t =
    List.map (Scope.filter_map f) ss

  let map_inplace (f : 'a -> 'a) (ss : 'a t) : unit =
    List.iter (Scope.map_inplace f) ss

  let map2 (f : 'a -> 'b -> 'c) (ss1 : 'a t) (ss2 : 'b t) : 'c t =
    List.map2 (Scope.map2 f) ss1 ss2

  let merge_inplace (f : 'a -> 'b -> 'a) (ss1 : 'a t) (ss2 : 'b t) : unit =
    List.iter2 (Scope.merge_inplace f) ss1 ss2

  let nest (ss : 'a t) (k : 'a t -> 'b) : 'b =
    let newscope = Scope.empty () in
    k (newscope :: ss)

  let bindings (ss : 'a t) : (Ident.t * 'a) list list =
    List.map Scope.bindings ss

  let equal (eq : 'a -> 'a -> bool) (ss1 : 'a t) (ss2 : 'a t) : bool =
    List.equal (Scope.equal eq) ss1 ss2
end

module IdentSet = Set.Make (Ident)
(** {2 Sets of identifiers} *)

(** merge a list of sets *)
let unionSets (idss : IdentSet.t list) : IdentSet.t =
  List.fold_left IdentSet.union IdentSet.empty idss

(** add v to set of identifiers mapped to k *)
let addToBindingSet (k : Ident.t) (v : Ident.t) (bs : IdentSet.t Bindings.t) :
    IdentSet.t Bindings.t =
  Bindings.update k
    (fun old ->
      match old with
      | None -> Some (IdentSet.singleton v)
      | Some vs -> Some (IdentSet.add v vs))
    bs

let pp_identset (fmt : Format.formatter) (xs : IdentSet.t) : unit =
    Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.pp_print_string fmt ", ") FMT.varname fmt (IdentSet.elements xs)

(** convert identifier set to sorted list of identifiers

    The implementation is trivial and exists mostly to emphasize that the
    resulting list is sorted
 *)
let to_sorted_list (s : IdentSet.t) : Ident.t list = IdentSet.elements s

(****************************************************************)
(** {2 Name supply}                                             *)
(****************************************************************)

(* This is used to generate unique names by generating a new name
 * each time it is invoked.
 *
 * Uniqueness depends on
 * - the prefix being different from names in the original source
 *   code and different from names generated by other nameSupply
 *   instances.
 * - in almost all cases, the class should be instantiated as a global
 *   variable so that all names are unique across the entire application.
 *)
class nameSupply (prefix : string) =
  object
    val mutable vnum = 0

    method fresh : Ident.t =
      let v = Ident.Ident (prefix ^ string_of_int vnum) in
      vnum <- vnum + 1;
      v
  end

(****************************************************************)
(** {2 Equivalence classes}                                     *)
(****************************************************************)

type tree = { mutable parent : tree; data : Ident.t }
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
     *
     * Efficiency of this data structure critically depends on the
     * use of pointer equality in the search.
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
    method private find_ident (x : Ident.t) : tree =
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
    method canonicalize (x : Ident.t) : Ident.t =
      let s = self#find_ident x in
      s.data

    (* Merge the sets containing 'x' and 'y'
     *
     * Efficiency of this data structure critically depends on the
     * use of pointer equality when merging nodes.
     *)
    method merge (x : Ident.t) (y : Ident.t) : unit =
      let x' = self#find_ident x in
      let y' = self#find_ident y in
      if x != y then y'.parent <- x'

    (* Optimization: short circuit every tree so that they all point directly at
       root *)
    method private normalize : unit = forest <- Bindings.map self#find forest

    (* Return mapping from identifiers to the canonical representation of their
     * equivalence class
     *)
    method mapping : Ident.t Bindings.t =
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

    method! vvar access x =
      if access = Read then free_vars <- IdentSet.add x free_vars;
      SkipChildren

    method! vtype ty =
      match ty with
      | Type_Constructor (tc, _) ->
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
      | Expr_TApply (f, _, _, _) ->
          free_funs <- IdentSet.add f free_funs;
          DoChildren
      | Expr_RecordInit (tc, _, _) ->
          free_tcs <- IdentSet.add tc free_tcs;
          DoChildren
      | _ -> DoChildren

    method! vlexpr =
      function
      | LExpr_Var v ->
          free_vars <- IdentSet.add v free_vars;
          SkipChildren
      | LExpr_Write (f, _, _, _) ->
          free_funs <- IdentSet.add f free_funs;
          DoChildren
      | LExpr_ReadWrite (f, g, _, _, _) ->
          free_funs <- IdentSet.add f free_funs;
          free_funs <- IdentSet.add g free_funs;
          DoChildren
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_TCall (f, _, _, _, _) ->
          free_funs <- IdentSet.add f free_funs;
          DoChildren
      | _ -> DoChildren

    method! vcatcher (Catcher_Guarded (_, tc, _, _)) =
      free_tcs <- IdentSet.add tc free_tcs;
      DoChildren

    method! leave_scope (vs : Ident.t list) =
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

let fv_args (atys : (Ident.t * ty) list) : IdentSet.t =
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
      (* Use of pointer equality (==) is deliberate here.
       * We are only interested in checking for the root object itself
       * not some object that is structurally identical to it
       * and we don't want to avoid the quadratic cost that would occur
       * if we performed a structural equality check as we recurse down
       * the tree.
       *)
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
      | Type_Constructor (id, _) ->
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
      | Expr_TApply (f, _, _, _) ->
          calls <- IdentSet.add f calls;
          DoChildren
      | _ -> DoChildren

    method! vstmt =
      function
      | Stmt_TCall (id, _, _, _, _) ->
          calls <- IdentSet.add id calls;
          DoChildren
      | _ -> DoChildren

    method! vlexpr =
      function
      | LExpr_Write (id, _, _, _) ->
          calls <- IdentSet.add id calls;
          DoChildren
      | LExpr_ReadWrite (id1, id2, _, _, _) ->
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
(** {2 Extract location info from AST nodes}                    *)
(****************************************************************)

let decl_loc (x : AST.declaration) : AST.l =
  ( match x with
  | Decl_BuiltinType (v, loc) -> loc
  | Decl_Forward (v, loc) -> loc
  | Decl_Record (v, ps, fs, loc) -> loc
  | Decl_Exception (v, fs, loc) -> loc
  | Decl_Typedef (v, ps, ty, loc) -> loc
  | Decl_Enum (v, es, loc) -> loc
  | Decl_Var (v, ty, loc) -> loc
  | Decl_Const (v, ty, e, loc) -> loc
  | Decl_BuiltinFunction (f, ps, args, ty, loc) -> loc
  | Decl_FunType (f, ps, args, ty, loc) -> loc
  | Decl_FunDefn (f, ps, args, ty, b, loc) -> loc
  | Decl_ProcType (f, ps, args, loc) -> loc
  | Decl_ProcDefn (f, ps, args, b, loc) -> loc
  | Decl_VarGetterType (f, ps, ty, loc) -> loc
  | Decl_VarGetterDefn (f, ps, ty, b, loc) -> loc
  | Decl_ArrayGetterType (f, ps, args, ty, loc) -> loc
  | Decl_ArrayGetterDefn (f, ps, args, ty, b, loc) -> loc
  | Decl_VarSetterType (f, ps, v, ty, loc) -> loc
  | Decl_VarSetterDefn (f, ps, v, ty, b, loc) -> loc
  | Decl_ArraySetterType (f, ps, args, v, ty, loc) -> loc
  | Decl_ArraySetterDefn (f, ps, args, v, ty, b, loc) -> loc
  | Decl_Operator1 (op, vs, loc) -> loc
  | Decl_Operator2 (op, vs, loc) -> loc
  | Decl_NewEventDefn (v, ps, args, loc) -> loc
  | Decl_EventClause (v, b, loc) -> loc
  | Decl_NewMapDefn (v, ps, args, ty, b, loc) -> loc
  | Decl_MapClause (v, fs, oc, b, loc) -> loc
  | Decl_Config (v, ty, e, loc) -> loc
  )

let stmt_loc (x : AST.stmt) : AST.l =
  ( match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) -> loc
  | Stmt_VarDecl (di, i, loc) -> loc
  | Stmt_ConstDecl (di, i, loc) -> loc
  | Stmt_Assign (l, r, loc) -> loc
  | Stmt_TCall (f, tes, args, throws, loc) -> loc
  | Stmt_FunReturn (e, loc) -> loc
  | Stmt_ProcReturn loc -> loc
  | Stmt_Assert (e, loc) -> loc
  | Stmt_Throw (v, loc) -> loc
  | Stmt_Block (b, loc) -> loc
  | Stmt_If (c, t, els, (e, el), loc) -> loc
  | Stmt_Case (e, alts, ob, loc) -> loc
  | Stmt_For (v, f, dir, t, b, loc) -> loc
  | Stmt_While (c, b, loc) -> loc
  | Stmt_Repeat (b, c, pos, loc) -> loc
  | Stmt_Try (b, pos, cs, ob, loc) -> loc
  )

(****************************************************************)
(** {2 Keep definitions reachable from roots}                   *)
(****************************************************************)

let decl_name (x : declaration) : Ident.t option =
  match x with
  | Decl_BuiltinType (v, loc) -> Some v
  | Decl_Forward (v, loc) -> Some v
  | Decl_Record (v, ps, fs, loc) -> Some v
  | Decl_Exception (v, fs, loc) -> Some v
  | Decl_Typedef (v, ps, ty, loc) -> Some v
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
let memoize (xs : Ident.t list) (f : Ident.t -> IdentSet.t) : IdentSet.t Bindings.t
    =
  let results = ref Bindings.empty in
  List.iter (fun x -> results := Bindings.add x (f x) !results) xs;
  !results

(* construct map of Union { f x -> x | for x in xs } *)
let rev_memoize (xs : Ident.t list) (f : Ident.t -> IdentSet.t) :
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

(* Generate list of objects reachable from roots
 *
 * If the graph is acyclic, the resulting list will be
 * sorted in topological order such that 'x' occurs before 'y'
 * in the result if 'y' is transitively reachable from 'x'.
 * (Note that this is the reverse of the order that is
 * needed for code generation.)
 *
 * If the graph is cyclic, there are no ordering guarantees.
 *)
let reach (next : Ident.t -> IdentSet.t) (roots : Ident.t list) : Ident.t list =
  let result : Ident.t list ref = ref [] in
  let visited = ref IdentSet.empty in

  let rec dfs (x : Ident.t) : unit =
    if not (IdentSet.mem x !visited) then begin
      visited := IdentSet.add x !visited;
      IdentSet.iter dfs (next x);
      result := x :: !result
    end
  in

  List.iter dfs roots;
  !result

(* f (find x bs) if x in bs, empty otherwise *)
let bindings_to_function (bs : 'a Bindings.t) (f : 'a -> IdentSet.t) (x : Ident.t)
    : IdentSet.t =
  Option.value
    (Option.map f (Bindings.find_opt x bs))
    ~default:IdentSet.empty

(* Generate list of declarations reachable from roots
 *
 * If the graph is acyclic, the resulting list will be
 * sorted in topological order such that 'x' occurs before 'y'
 * in the result if 'y' is transitively reachable from 'x'.
 * (Note that this is the reverse of the order that is
 * needed for code generation.)
 *
 * If the graph is cyclic, there are no ordering guarantees.
 *)
let reachable_decls (roots : Ident.t list) (ds : declaration list) :
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
  List.filter_map (fun x -> Bindings.find_opt x decls) reachable

(* Topological sort of declarations
 *
 * The declarations should be acyclic
 *)
let topological_sort (ds : declaration list) : declaration list =
  let roots = List.filter_map decl_name ds in
  reachable_decls roots ds

let callers (leaves : Ident.t list) (ds : declaration list) : IdentSet.t =
  let next (d : declaration) : IdentSet.t =
    let fvs = new freevarClass in
    ignore (visit_decl (fvs :> aslVisitor) d);
    fvs#funs
  in

  let decls = decl_map_of ds in
  let fs = List.filter Ident.is_function (List.map fst (Bindings.bindings decls)) in
  let rev_next = rev_memoize fs (bindings_to_function decls next) in
  IdentSet.of_list (reach (bindings_to_function rev_next Fun.id) leaves)

(****************************************************************)
(** {2 Side effect detection}                                   *)
(****************************************************************)

(* All RAM accesses are considered to modify a single RAM variable
 * Note that the name is the same as the __RAM type in the Prelude.
 *)
let dummy_ram_variable = Ident.Ident "__RAM"

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

    method! vvar access x =
      if access = Read then reads <- IdentSet.add x reads;
      SkipChildren

    method! vexpr e =
      match e with
      | Expr_TApply (f, _, _, _) ->
          functions_called <- IdentSet.add f functions_called;
          if Ident.matches ~name:"ram_read" f then begin
            reads <- IdentSet.add dummy_ram_variable reads
          end;
          DoChildren
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_TCall (f, _, _, _, _) ->
          functions_called <- IdentSet.add f functions_called;
          if Ident.matches ~name:"ram_init" f || Ident.matches ~name:"ram_write" f then begin
            writes <- IdentSet.add dummy_ram_variable writes
          end;
          DoChildren
      | Stmt_Throw _ ->
          throws_exceptions <- true;
          DoChildren
      | _ -> DoChildren

    method! leave_scope (vs : Ident.t list) =
      (* remove reads/writes to local variables *)
      List.iter (fun v -> writes <- IdentSet.remove v writes) vs;
      List.iter (fun v -> reads <- IdentSet.remove v reads) vs
  end

let side_effects_of_decl (d : declaration) : (IdentSet.t * IdentSet.t * IdentSet.t * bool) =
  let se = new sideEffectClass in
  ignore (visit_decl (se :> aslVisitor) d);
  se#sideEffects

let identify_impure_funs (isConstant : Ident.t -> bool)
    (isImpurePrim : Ident.t -> bool) (ds : declaration list) : IdentSet.t =
  let is_impure_prim (d : declaration) : bool =
    match d with
    | Decl_BuiltinFunction (f, ps, args, ty, loc) -> isImpurePrim f
    | _ -> false
  in
  let locally_impure (d : declaration) : bool =
    let (rds, wrs, callees, throws) = side_effects_of_decl d in
    let vrds = IdentSet.filter (fun v -> not (isConstant v)) rds in
    throws || (not (IdentSet.is_empty vrds)) || not (IdentSet.is_empty wrs)
  in

  let impure = ref IdentSet.empty in
  List.iter
    (fun d ->
      match decl_name d with
      | Some x when IdentSet.mem x !impure || is_impure_prim d || locally_impure d
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

let subst_var (s : expr Bindings.t) (kind : access_kind) (x : Ident.t) : Ident.t =
  let subst = new substClass s in
  visit_var subst kind x

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
class substFunClass (replace : Ident.t -> expr option) =
  object
    inherit nopAslVisitor

    method! vexpr x =
      match x with
      | Expr_Var v -> (
          match replace v with Some r -> ChangeTo r | None -> DoChildren)
      | _ -> DoChildren
  end

let subst_fun_expr (replace : Ident.t -> expr option) (x : expr) : expr =
  let subst = new substFunClass replace in
  visit_expr subst x

let subst_fun_lexpr (replace : Ident.t -> expr option) (x : lexpr) : lexpr =
  let subst = new substFunClass replace in
  visit_lexpr subst x

let subst_fun_slice (replace : Ident.t -> expr option) (x : slice) : slice =
  let subst = new substFunClass replace in
  visit_slice subst x

let subst_fun_type (replace : Ident.t -> expr option) (x : ty) : ty =
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
      | Expr_TApply (f, tys, args, throws) -> (
          let args' = List.map (visit_expr (self :> aslVisitor)) args in
          match (Bindings.find_opt f ops, args') with
          | Some op, [ a; b ] -> ChangeTo (Expr_Binop (a, op, b))
          (* | (Some op, [a]) -> ChangeTo (Expr_Unop(op, a)) *)
          | _ -> ChangeTo (Expr_TApply (f, [], args', throws)))
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
let pp_ixtype (x : ixtype) : string = Utils.to_string2 (Fun.flip FMT.ixtype x)
let pp_expr (x : expr) : string = Utils.to_string2 (Fun.flip FMT.expr x)
let pp_lexpr (x : lexpr) : string = Utils.to_string2 (Fun.flip FMT.lexpr x)
let pp_stmt (x : stmt) : string = Utils.to_string2 (Fun.flip FMT.stmt x)

(****************************************************************)
(** {2 AST expression constructors}                             *)
(****************************************************************)

let type_unit = Type_Tuple []
let type_integer = Type_Integer None
let type_bool = Type_Constructor (Ident "boolean", [])
let type_real = Type_Constructor (Ident "real", [])
let type_string = Type_Constructor (Ident "string", [])
let type_bits (n : expr) = Type_Bits n
let type_exn = Type_Constructor (Ident "__Exception", [])

let mk_enum (nm : string) : AST.expr = AST.Expr_Var (Ident nm)

let asl_false = mk_enum "FALSE"
let asl_true = mk_enum "TRUE"

let mk_litint (x : int) : AST.expr = Expr_LitInt (string_of_int x)
let mk_litbigint (x : Z.t) : AST.expr = Expr_LitInt (Z.to_string x)

let minus_one = Expr_LitInt "-1"
let zero = Expr_LitInt "0"
let one = Expr_LitInt "1"
let two = Expr_LitInt "2"

let mk_unop (op : string) (tys : AST.expr list) (x : AST.expr) : AST.expr =
  Expr_TApply (FIdent (op, 0), tys, [x], false)

let mk_binop (op : string) (tys : AST.expr list) (x : AST.expr) (y : AST.expr) : AST.expr =
  Expr_TApply (FIdent (op, 0), tys, [x; y], false)

(** Construct "!x" *)
let mk_not (x : AST.expr) : AST.expr =
  if x = asl_false then asl_true
  else if x = asl_true then asl_false
  else mk_unop "not_bool" [] x

(** Construct "x && y" *)
let mk_and (x : AST.expr) (y : AST.expr) : AST.expr =
  if x = asl_false then asl_false
  else if x = asl_true then y
  else if y = asl_true then x
  else mk_binop "and_bool" [] x y

(** Construct "x || y" *)
let mk_or (x : AST.expr) (y : AST.expr) : AST.expr =
  if x = asl_true then asl_true
  else if x = asl_false then y
  else if y = asl_false then x
  else mk_binop "or_bool" [] x y

(** Construct "x --> y" *)
let mk_implies (x : AST.expr) (y : AST.expr) : AST.expr =
  if x = asl_false then asl_true
  else if x = asl_true then y
  else if y = asl_false then mk_not x
  else mk_binop "implies_bool" [] x y

(** Construct "eq_enum(x, y)" *)
let mk_eq_enum (x : AST.expr) (y : AST.expr) : AST.expr = mk_binop "eq_enum" [] x y

(** Construct "eq_int(x, y)" *)
let mk_eq_int (x : AST.expr) (y : AST.expr) : AST.expr = mk_binop "eq_int" [] x y

(** Construct "le_int(x, y)" *)
let mk_le_int (x : AST.expr) (y : AST.expr) : AST.expr = mk_binop "le_int" [] x y

(** Construct "add_int(x, y)" *)
let mk_add_int (x : AST.expr) (y : AST.expr) : AST.expr =
  if x = zero then y
  else if y = zero then x
  else mk_binop "add_int" [] x y

(** Construct "sub_int(x, y)" *)
let mk_sub_int (x : AST.expr) (y : AST.expr) : AST.expr =
  if y = zero then x
  else mk_binop "sub_int" [] x y

(** Construct "neg_int(x)" *)
let mk_neg_int (x : AST.expr) : AST.expr =
  mk_unop "neg_int" [] x

(** Construct "mul_int(x, y)" *)
let mk_mul_int (x : AST.expr) (y : AST.expr) : AST.expr =
  if x = one then y
  else if y = one then x
  else if x = minus_one then mk_neg_int y
  else if y = minus_one then mk_neg_int x
  else mk_binop "mul_int" [] x y

(** Construct "fdiv_int(x, y)" *)
let mk_fdiv_int (x : AST.expr) (y : AST.expr) : AST.expr =
  mk_binop "fdiv_int" [] x y

(** Construct "Max(x, y)" *)
let mk_max_int (x : AST.expr) (y : AST.expr) : AST.expr =
  if x = y then x
  else mk_binop "Max" [] x y

(** Construct "Min(x, y)" *)
let mk_min_int (x : AST.expr) (y : AST.expr) : AST.expr =
  if x = y then x
  else mk_binop "Min" [] x y

(** Construct "eq_bits{w}(x, y)" *)
let mk_eq_bits (w : AST.expr) (x : AST.expr) (y : AST.expr) : AST.expr =
  mk_binop "eq_bits" [w] x y

(** Construct "add_bits{w}(x, y)" *)
let mk_add_bits (w : AST.expr) (x : AST.expr) (y : AST.expr) : AST.expr =
  mk_binop "add_bits" [w] x y

(** Construct "sub_bits{w}(x, y)" *)
let mk_sub_bits (w : AST.expr) (x : AST.expr) (y : AST.expr) : AST.expr =
  mk_binop "sub_bits" [w] x y

(** Construct "mul_bits{w}(x, y)" *)
let mk_mul_bits (w : AST.expr) (x : AST.expr) (y : AST.expr) : AST.expr =
  mk_binop "mul_bits" [w] x y

(** Construct "in_mask{w}(x, y)" *)
let mk_in_mask (w : AST.expr) (x : AST.expr) (y : AST.expr) : AST.expr =
  mk_binop "in_mask" [w] x y

(** Construct "and_bits{N}(x, y)" *)
let mk_and_bits (n : AST.expr) (x : AST.expr) (y : AST.expr) : AST.expr =
  ( match (x, y) with
  | (Expr_TApply (FIdent ("ones_bits", _), _, _, _), _) -> y
  | (_, Expr_TApply (FIdent ("ones_bits", _), _, _, _)) -> x
  | _ -> mk_binop "and_bits" [n] x y
  )

(** Construct "or_bits{N}(x, y)" *)
let mk_or_bits (n : AST.expr) (x : AST.expr) (y : AST.expr) : AST.expr =
  ( match (x, y) with
  | (Expr_TApply (FIdent ("zeros_bits", _), _, _, _), _) -> y
  | (_, Expr_TApply (FIdent ("zeros_bits", _), _, _, _)) -> x
  | _ -> mk_binop "or_bits" [n] x y
  )

(** Construct "lsr_bits{N}(x, y)" *)
let mk_lsr_bits (n : AST.expr) (x : AST.expr) (y : AST.expr) : AST.expr =
  if y = zero then
    x
  else
    mk_binop "lsr_bits" [n] x y

(** Construct "lsl_bits{N}(x, y)" *)
let mk_lsl_bits (n : AST.expr) (x : AST.expr) (y : AST.expr) : AST.expr =
  if y = zero then
    x
  else
    mk_binop "lsl_bits" [n] x y

(** Construct "zeros_bits{N}(N)" *)
let mk_zero_bits (n : AST.expr) : AST.expr =
  mk_unop "zeros_bits" [n] n

(** Construct "ones_bits{N}(N)" *)
let mk_ones_bits (n : AST.expr) : AST.expr =
  mk_unop "ones_bits" [n] n

(** Construct "asl_extract_bits{w,n}(x, lo, w)" *)
let mk_bits_select (w : AST.expr) (n : AST.expr) (x : AST.expr) (lo : AST.expr) : AST.expr =
  Expr_TApply (FIdent ("asl_extract_bits", 0), [ w; n ], [ x; lo; w ], false)

(** Construct "zero_extend_bits{w, n}(x, n)" *)
let mk_zero_extend_bits (w : AST.expr) (n : AST.expr) (x : AST.expr) : AST.expr =
  if w = n then
    x
  else
    Expr_TApply (FIdent ("zero_extend_bits", 0), [ w; n ], [ x; n ], false)

(** Construct "mk_mask{n}(w, n)" which is equivalent to
 *  'ZeroExtend{n}(Ones(w), n)'
 *)
let mk_mask (w : AST.expr) (n : AST.expr) =
  if w = n then
    mk_ones_bits n
  else
    Expr_TApply (FIdent ("mk_mask", 0), [n], [w; n], false)

let mk_not_mask(m : AST.expr) (n : AST.expr) =
  Expr_TApply (FIdent ("not_bits", 0), [n], [m], false)

(** Construct "(0 + x1) + ... + xn" *)
let mk_add_ints (xs : AST.expr list) : AST.expr =
  List.fold_left mk_add_int zero xs

(** Construct "(z * x1) * ... * xn" *)
let mk_mul_ints (z : AST.expr) (xs : AST.expr list) : AST.expr =
  List.fold_left mk_mul_int z xs

(** Construct "(TRUE && x1) && ... && xn" *)
let mk_ands (xs : AST.expr list) : AST.expr =
  List.fold_left mk_and asl_true xs

(** Construct "(FALSE || x1) || ... || xn" *)
let mk_ors  (xs : AST.expr list) : AST.expr =
  List.fold_left mk_or asl_false xs

(** Construct "cvt_int_bits{n}(x, n)" *)
let mk_cvt_int_bits (n : AST.expr) (x : AST.expr) : AST.expr =
  Expr_TApply (FIdent ("cvt_int_bits", 0), [ n ], [ x; n ], false)

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

let masklength_expr (x : string) : AST.expr =
  mk_litint (masklength x)

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

(** Bitwidth of type (which is expected to be a bitvector) *)
let width_of_type (ty : AST.ty) : AST.expr option =
  ( match ty with
  | Type_Bits n
  | Type_Register (n, _)
    -> Some n
  | _
    -> None
  )

(** Is an expression a literal constant? *)
let is_literal_constant (x : expr) : bool =
  ( match x with
  | Expr_LitInt _
  | Expr_LitHex _
  | Expr_LitReal _
  | Expr_LitBits _
  | Expr_LitMask _
  | Expr_LitString _ -> true
  | _ -> false
  )

(** Find declaration (type, function, procedure, getters and setters)
    definition by an identifier *)
let rec find_decl (f : Ident.t) (ds : AST.declaration list) :
    AST.declaration option =
  ( match ds with
  | [] -> None
  | (Decl_Record (tc, ps, fs, loc) as d) :: ds' ->
      if f = tc then Some d else find_decl f ds'
  | (Decl_Typedef (tc, ps, ty, loc) as d) :: ds' ->
      if f = tc then Some d else find_decl f ds'
  | (Decl_FunDefn (f', ps, atys, rty, body, loc) as d) :: ds' ->
      if f = f' then Some d else find_decl f ds'
  | (Decl_ProcDefn (f', ps, atys, body, loc) as d) :: ds' ->
      if f = f' then Some d else find_decl f ds'
  | (Decl_ArrayGetterDefn (f', ps, atys, rty, body, loc) as d) :: ds' ->
      if f = f' then Some d else find_decl f ds'
  | (Decl_ArraySetterDefn (f', ps, atys, v, t, body, loc) as d) :: ds' ->
      if f = f' then Some d else find_decl f ds'
  | (Decl_VarGetterDefn (f', ps, rty, body, loc) as d) :: ds' ->
      if f = f' then Some d else find_decl f ds'
  | (Decl_VarSetterDefn (f', ps, v, t, body, loc) as d) :: ds' ->
      if f = f' then Some d else find_decl f ds'
  | _ :: ds' -> find_decl f ds'
  )

(****************************************************************
 * End
 ****************************************************************)
