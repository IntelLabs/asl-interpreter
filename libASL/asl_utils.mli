(****************************************************************
 * ASL utility functions
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL utility functions *)

module AST = Asl_ast
module FMT = Asl_fmt
module FMTUtils = Format_utils

(****************************************************************)
(** {2 Bindings and IdentSet}                                   *)
(****************************************************************)

module Bindings : Map.S with type key = AST.ident
(** {2 Bindings: maps indexed by identifiers} *)

(** add association list to bindings *)
val add_bindings : 'a Bindings.t -> (AST.ident * 'a) list -> 'a Bindings.t

(** create bindings from association list *)
val mk_bindings : (AST.ident * 'a) list -> 'a Bindings.t

(** print bindings *)
val pp_bindings : ('a -> string) -> 'a Bindings.t -> unit

(****************************************************************)
(** {2 Scopes}                                                  *)
(****************************************************************)

(** A mutable binding *)
module Scope : sig
  type 'a t

  val empty : unit -> 'a t

  (* make a clean copy that can be independently mutated *)
  val clone : 'a t -> 'a t
  val mem : 'a t -> AST.ident -> bool
  val get : 'a t -> AST.ident -> 'a option
  val set : 'a t -> AST.ident -> 'a -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val map_inplace : ('a -> 'a) -> 'a t -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val merge_inplace : ('a -> 'b -> 'a) -> 'a t -> 'b t -> unit
  val bindings : 'a t -> (AST.ident * 'a) list
  val pp : ('a -> string) -> 'a t -> unit
end

(* A collection of nested mutable scopes *)
module ScopeStack : sig
  type 'a t

  val empty : unit -> 'a t

  (* make a clean copy that can be independently mutated *)
  val clone : 'a t -> 'a t
  val add : 'a t -> AST.ident -> 'a -> unit
  val mem : 'a t -> AST.ident -> bool
  val get : 'a t -> AST.ident -> 'a option
  val set : 'a t -> AST.ident -> 'a -> bool
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val map_inplace : ('a -> 'a) -> 'a t -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val merge_inplace : ('a -> 'b -> 'a) -> 'a t -> 'b t -> unit
  val nest : 'a t -> ('a t -> 'b) -> 'b
  val bindings : 'a t -> (AST.ident * 'a) list list
  val pp : ('a -> string) -> 'a t -> unit
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module IdentSet : Set.S with type elt = AST.ident

(** {2 Sets of identifiers} *)

(** merge a list of sets *)
val unionSets : IdentSet.t list -> IdentSet.t

(** add v to set of identifiers mapped to k *)
val addToBindingSet : AST.ident -> AST.ident -> IdentSet.t Bindings.t -> IdentSet.t Bindings.t

(** convert identifier set to sorted list of identifiers

    The implementation is trivial and exists mostly to emphasize that the
    resulting list is sorted
 *)
val to_sorted_list : IdentSet.t -> AST.ident list

(****************************************************************)
(** {2 Name supply}                                             *)
(****************************************************************)

class nameSupply : string ->
  object
    method fresh : AST.ident
  end

(****************************************************************)
(** {2 Equivalence classes}                                     *)
(****************************************************************)

type tree = { mutable parent : tree; data : AST.ident }
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

class equivalences :
  object
    (* Find the canonical member of the set containing 'x' *)
    method canonicalize : AST.ident -> AST.ident

    (* Merge the sets containing 'x' and 'y' *)
    method merge : AST.ident -> AST.ident -> unit

    (* Return mapping from identifiers to the canonical representation of their
     * equivalence class
     *)
    method mapping : AST.ident Bindings.t

    (* Construct equivalence classes for each canonical member of a class.
     *
     * The implementation of this could be made more efficient by adding
     * pointers to trees so that we can map each canonical member to a
     * tree containing all the nodes that point to it.
     * But this implementation just does a linear scan over all the members
     * of the forest.
     *)
    method classes : IdentSet.t Bindings.t

    (* Print equivalence classes adding a prefix at the start of every line of
     * output.
     *)
    method pp : Format.formatter -> string -> unit
  end

(****************************************************************)
(** {1 AST Transformation Utilities}                            *)
(****************************************************************)

(****************************************************************)
(** {2 Calculating free variables of expressions and types}     *)
(****************************************************************)

val fv_expr : AST.expr -> IdentSet.t
val fv_type : AST.ty -> IdentSet.t
val fv_args : (AST.ident * AST.ty) list -> IdentSet.t
val fv_stmts : AST.stmt list -> IdentSet.t
val fv_decl : AST.declaration -> IdentSet.t

(****************************************************************)
(** {2 Calculating assigned variables in statements}            *)
(****************************************************************)

val assigned_vars_of_stmts : AST.stmt list -> IdentSet.t
val assigned_vars_of_decl : AST.declaration -> IdentSet.t

(****************************************************************)
(** {2 Calculating subexpressions}                              *)
(****************************************************************)

val subexprs_of_expr : AST.expr -> AST.expr list

(****************************************************************)
(** {2 Calculate types used in expressions and statements}      *)
(****************************************************************)

val types_of_expr : AST.expr -> IdentSet.t
val types_of_stmts : AST.stmt list -> IdentSet.t
val types_of_decl : AST.declaration -> IdentSet.t

(****************************************************************)
(** {2 Calculate functions and procedures called in statements} *)
(****************************************************************)

val calls_of_expr : AST.expr -> IdentSet.t
val calls_of_stmts : AST.stmt list -> IdentSet.t
val calls_of_decl : AST.declaration -> IdentSet.t

(****************************************************************)
(** {2 Keep definitions reachable from roots}                   *)
(****************************************************************)

val decl_name : AST.declaration -> AST.ident option

(* Map of declarations *)
val decl_map_of : AST.declaration list -> AST.declaration Bindings.t

(* construct map of Union { x -> f x | for x in xs } *)
val memoize : AST.ident list -> (AST.ident -> IdentSet.t) -> IdentSet.t Bindings.t

(* construct map of Union { f x -> x | for x in xs } *)
val rev_memoize : AST.ident list -> (AST.ident -> IdentSet.t) -> IdentSet.t Bindings.t

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
val reach : (AST.ident -> IdentSet.t) -> AST.ident list -> AST.ident list

(* f (find x bs) if x in bs, empty otherwise *)
val bindings_to_function : 'a Bindings.t -> ('a -> IdentSet.t) -> AST.ident -> IdentSet.t

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
val reachable_decls : AST.ident list -> AST.declaration list -> AST.declaration list

(* Topological sort of declarations
 * The declarations should be acyclic
 *)
val topological_sort : AST.declaration list -> AST.declaration list

val callers : AST.ident list -> AST.declaration list -> IdentSet.t

(****************************************************************)
(** {2 Side effect detection}                                   *)
(****************************************************************)

val side_effects_of_decl : AST.declaration -> (IdentSet.t * IdentSet.t * IdentSet.t * bool)

(* `identify_impure_funs is_const isImpurePrim ds` returns the set of impure functions.
 * That is, functions that
 * - read or write non-constant globals
 * - or call impure primops
 * - or call an impure function.
 *)
val identify_impure_funs : (AST.ident -> bool) -> (AST.ident -> bool) -> AST.declaration list -> IdentSet.t

(****************************************************************)
(** {2 Substitutions}                                           *)
(****************************************************************)

(** Performing variable substitutions in expressions and types

    Note that it does not replace type constructors, global constants
    or enumerations in patterns, array indexes and types so this is
    limited to replacing local variables.
    It also does not replace variables used as l-expressions though
    that it easily changed if we think it should.               *)

val subst_expr : AST.expr Bindings.t -> AST.expr -> AST.expr
val subst_lexpr : AST.expr Bindings.t -> AST.lexpr -> AST.lexpr
val subst_var : AST.expr Bindings.t -> Asl_visitor.access_kind -> AST.ident -> AST.ident
val subst_slice : AST.expr Bindings.t -> AST.slice -> AST.slice
val subst_type : AST.expr Bindings.t -> AST.ty -> AST.ty
val subst_decl_item : AST.expr Bindings.t -> AST.decl_item -> AST.decl_item

(** More flexible substitution class - takes a function instead
    of a binding set.
 *)
class substFunClass : (AST.ident -> AST.expr option) -> Asl_visitor.aslVisitor

val subst_fun_expr : (AST.ident -> AST.expr option) -> AST.expr -> AST.expr
val subst_fun_lexpr : (AST.ident -> AST.expr option) -> AST.lexpr -> AST.lexpr
val subst_fun_slice : (AST.ident -> AST.expr option) -> AST.slice -> AST.slice
val subst_fun_type : (AST.ident -> AST.expr option) -> AST.ty -> AST.ty

(****************************************************************)
(** {2 Expression transformation}                               *)
(****************************************************************)

(** Expression transformation class

    Applies replace function to any subexpression.
    (Especially useful for expressions in types)                *)
class replaceExprClass : (AST.expr -> AST.expr option) -> Asl_visitor.aslVisitor

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

val resugar_expr : AST.binop Bindings.t -> AST.expr -> AST.expr
val resugar_type : AST.binop Bindings.t -> AST.ty -> AST.ty

(****************************************************************)
(** {2 Pretty printing wrappers}                                *)
(****************************************************************)

val pp_unop : AST.unop -> string
val pp_binop : AST.binop -> string
val pp_type : AST.ty -> string
val pp_expr : AST.expr -> string
val pp_lexpr : AST.lexpr -> string
val pp_stmt : AST.stmt -> string

(****************************************************************)
(** {2 AST expression constructors}                             *)
(****************************************************************)

val type_unit : AST.ty
val type_integer : AST.ty
val type_bool : AST.ty
val type_real : AST.ty
val type_string : AST.ty
val type_bits : AST.expr -> AST.ty
val type_exn : AST.ty

val asl_false : AST.expr
val asl_true : AST.expr

val mk_litint : int -> AST.expr
val mk_litbigint : Z.t -> AST.expr

val zero : AST.expr
val one : AST.expr
val two : AST.expr

(** Construct "!x" *)
val mk_not : AST.expr -> AST.expr

(** Construct "x && y" *)
val mk_and : AST.expr -> AST.expr -> AST.expr

(** Construct "x || y" *)
val mk_or : AST.expr -> AST.expr -> AST.expr

(** Construct "eq_enum(x, y)" *)
val mk_eq_enum : AST.expr -> AST.expr -> AST.expr

(** Construct "eq_int(x, y)" *)
val mk_eq_int : AST.expr -> AST.expr -> AST.expr

(** Construct "le_int(x, y)" *)
val mk_le_int : AST.expr -> AST.expr -> AST.expr

(** Construct "add_int(x, y)" *)
val mk_add_int : AST.expr -> AST.expr -> AST.expr

(** Construct "sub_int(x, y)" *)
val mk_sub_int : AST.expr -> AST.expr -> AST.expr

(** Construct "neg_int(x, y)" *)
val mk_neg_int : AST.expr -> AST.expr

(** Construct "mul_int(x, y)" *)
val mk_mul_int : AST.expr -> AST.expr -> AST.expr

(** Construct "eq_bits{w}(x, y)" *)
val mk_eq_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "in_mask{w}(x, y)" *)
val mk_in_mask : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "and_bits{N}(x, y)" *)
val mk_and_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "or_bits{N}(x, y)" *)
val mk_or_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "lsr_bits{N}(x, y)" *)
val mk_lsr_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "lsl_bits{N}(x, y)" *)
val mk_lsl_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "zero_bits{N}(N)" *)
val mk_zero_bits : AST.expr -> AST.expr

(** Construct "ones_bits{N}(N)" *)
val mk_ones_bits : AST.expr -> AST.expr

(** Construct "asl_extract_bits{w,n}(x, lo, w)" *)
val mk_bits_select : AST.expr -> AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "asl_extract_int{w}(x, lo, w)" *)
val mk_int_select : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "asl_zero_extend{w, n}(x)" which is equivalent to "ZeroExtend{w,n}(x, w)" *)
val mk_zero_extend : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "mk_mask{n}()" which is equivalent to "ZeroExtend{w,n}(Ones(w), n)" *)
val mk_mask : AST.expr -> AST.expr -> AST.expr

(** Construct "(0 + x1) + ... + xn" *)
val mk_add_ints : AST.expr list -> AST.expr

(** Construct "(z + x1) + ... + xn" *)
val mk_mul_ints : AST.expr -> AST.expr list -> AST.expr

(** Construct "(TRUE && x1) && ... && xn" *)
val mk_ands : AST.expr list -> AST.expr

(** Construct "(FALSE || x1) || ... || xn" *)
val mk_ors : AST.expr list -> AST.expr

(****************************************************************)
(** {2 Misc}                                                    *)
(****************************************************************)

(** Length of bitstring or mask literal.

    ASL bit and mask literals allow spaces to be included - these
    do not count towards the length of the literal.
 *)
val masklength : string -> int

val masklength_expr : string -> AST.expr

(** Is an expression a literal constant? *)
val is_literal_constant : AST.expr -> bool

(** Test whether a function returns a tuple (of 2 or more elements). *)
val isTupleType : AST.ty -> bool

(** Deconstruct a tuple type (return `[t]` if not a tuple type *)
val tupleTypes : AST.ty -> AST.ty list

(** Bitwidth of type (which is expected to be a bitvector) *)
val width_of_type : AST.ty -> AST.expr option

(** Find subprogram (function, procedure, getters and setters)
    definition by an identifier *)
val findFun : AST.ident -> AST.declaration list -> AST.declaration option

(****************************************************************
 * End
 ****************************************************************)
