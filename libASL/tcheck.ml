(****************************************************************
 * ASL typechecker
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Type inference and checker for ASL language *)

module AST = Asl_ast
module Visitor = Asl_visitor
module FMT = Asl_fmt
module FMTUtils = Format_utils
open Builtin_idents
open AST
open Utils
open Asl_utils
open Identset
open Error
open Format

let verbose = false
let fmt = std_formatter
let enable_constraint_checks = ref false
let enable_runtime_checks = ref false
let max_errors = ref 0

(****************************************************************)
(** {3 AST construction utilities}                              *)
(****************************************************************)

let slice_width (x : AST.slice) : AST.expr =
  match x with
  | Slice_Single e -> one
  | Slice_HiLo (hi, lo) -> Xform_simplify_expr.mk_add_int (mk_sub_int hi lo) one
  | Slice_LoWd (lo, wd) -> wd
  | Slice_HiWd (hi, wd) -> wd
  | Slice_Element (_, wd) -> wd

let slices_width (xs : AST.slice list) : AST.expr =
  mk_add_ints (List.map slice_width xs)

let ixtype_basetype (ty : AST.ixtype) : AST.ty =
  match ty with
  | Index_Enum tc -> Type_Constructor (tc, [])
  | Index_Int sz -> type_integer

(****************************************************************)
(** {2 Environment representing global and local objects}       *)
(****************************************************************)

type typedef =
  | Type_Builtin of Ident.t
  | Type_Forward
  | Type_Record of (Ident.t list * (Ident.t * ty) list)
  | Type_Exception of (Ident.t * ty) list
  | Type_Enumeration of Ident.t list
  | Type_Abbreviation of (Ident.t list * ty)

let pp_typedef (x : typedef) (fmt : formatter) : unit =
  match x with
  | Type_Builtin t ->
      FMT.kw_underscore_builtin fmt;
      FMTUtils.nbsp fmt;
      FMT.tycon fmt t
  | Type_Forward -> pp_print_string fmt "forward"
  | Type_Record (ps, fs) ->
      FMT.kw_record fmt;
      FMTUtils.nbsp fmt;
      FMTUtils.parens fmt (fun _ -> FMTUtils.commasep fmt (FMT.varname fmt) ps);
      FMTUtils.nbsp fmt;
      FMTUtils.braces fmt (fun _ ->
          FMTUtils.vbox fmt (fun _ ->
              FMTUtils.cutsep fmt
                (fun (f, ty) ->
                  FMT.fieldname fmt f;
                  FMTUtils.nbsp fmt;
                  FMT.colon fmt;
                  FMTUtils.nbsp fmt;
                  FMT.ty fmt ty;
                  FMT.semicolon fmt)
                fs))
  | Type_Exception fs ->
      Format.fprintf fmt "exception {";
      FMTUtils.vbox fmt (fun _ ->
        FMTUtils.cutsep fmt
          (fun (f, ty) ->
             Format.fprintf fmt "%a : %a;"
               FMT.fieldname f
               FMT.ty ty)
          fs);
      Format.fprintf fmt "}"
  | Type_Enumeration es ->
      FMT.kw_enumeration fmt;
      FMTUtils.nbsp fmt;
      FMTUtils.braces fmt (fun _ ->
          FMTUtils.vbox fmt (fun _ ->
              FMTUtils.commasep fmt (FMT.varname fmt) es))
  | Type_Abbreviation (ps, ty) ->
      FMT.keyword fmt "type";
      FMTUtils.nbsp fmt;
      FMTUtils.parens fmt (fun _ -> FMTUtils.commasep fmt (FMT.varname fmt) ps);
      FMTUtils.nbsp fmt;
      FMT.eq_gt fmt;
      FMTUtils.nbsp fmt;
      FMT.ty fmt ty


(* Information about variables *)
type var_info =
  { name        : Ident.t;
    loc         : Loc.t;
    ty          : AST.ty;
    is_local    : bool;
    is_constant : bool;
  }

let pp_var_info (fmt : Format.formatter) (x : var_info) : unit =
  Format.fprintf fmt "Var{%a, %a, %a, %s, %s}"
    FMT.varname x.name
    FMT.loc x.loc
    FMT.ty x.ty
    (if x.is_local then "local" else "global")
    (if x.is_constant then "constant" else "variable")

(* Information about functions *)
type funtype =
  { funname : Ident.t;
    loc     : Loc.t;
    isArray : bool;
    params  : (Ident.t * AST.ty option) list;
    atys    : (Ident.t * AST.ty * AST.expr option) list;
    ovty    : AST.ty option; (* type of rhs in setter functions *)
    rty     : AST.ty;
  }

let pp_funtype (fmt : formatter) (fty : funtype) : unit =
    Format.fprintf fmt "@[<v>%a%s{%a}(%a)%a => %a [defined at %a]@]"
    Ident.pp_untagged fty.funname
    (if fty.isArray then "[]" else "")
    FMT.parameters fty.params
    FMT.formals fty.atys
    (Format.pp_print_option (fun fmt -> Format.fprintf fmt " = %a" FMT.ty)) fty.ovty
    FMT.ty fty.rty
    FMT.loc fty.loc

module Operator1 = struct
  type t = AST.unop

  let compare x y = Stdlib.compare x y
end

module Operators1 = Map.Make (Operator1)

module Operator2 = struct
  type t = AST.binop

  let compare x y = Stdlib.compare x y
end

module Operators2 = Map.Make (Operator2)

(****************************************************************)
(** {3 Global Environment (aka the Global Symbol Table)}        *)
(****************************************************************)

module GlobalEnv : sig
  type t

  val mkempty : unit -> t
  val clone : t -> t
  val addType : t -> Loc.t -> Ident.t -> typedef -> unit
  val getType : t -> Ident.t -> typedef option
  val isType : t -> Ident.t -> bool
  val isTycon : t -> Ident.t -> bool
  val isEnum : t -> Ident.t -> bool
  val addFuns : t -> Loc.t -> Ident.t -> funtype list -> unit
  val getFuns : t -> Ident.t -> funtype list
  val addSetterFuns : t -> Ident.t -> funtype list -> unit
  val getSetterFun : t -> Ident.t -> funtype list
  val addOperators1 : t -> Loc.t -> AST.unop -> funtype list -> unit
  val getOperators1 : t -> Loc.t -> AST.unop -> funtype list
  val addOperators2 : t -> Loc.t -> AST.binop -> funtype list -> unit
  val getOperators2 : t -> Loc.t -> AST.binop -> funtype list
  val addGlobalVar : t -> var_info -> unit
  val getGlobalVar : t -> Ident.t -> var_info option
  val addConstant : t -> Ident.t -> AST.expr -> unit
  val getConstant : t -> Ident.t -> AST.expr option
end = struct
  type t = {
    mutable types : typedef Bindings.t;
    mutable functions : funtype list Bindings.t;
    mutable setters : funtype list Bindings.t;
    mutable operators1 : funtype list Operators1.t;
    mutable operators2 : funtype list Operators2.t;
    mutable globals : var_info Bindings.t;
    mutable constants : AST.expr Bindings.t;
  }

  let mkempty _ : t =
    {
      types = Bindings.empty;
      functions = Bindings.empty;
      setters = Bindings.empty;
      operators1 = Operators1.empty;
      operators2 = Operators2.empty;
      globals = Bindings.empty;
      constants = Bindings.empty;
    }

  let clone (env : t) : t =
    {
      types = env.types;
      functions = env.functions;
      setters = env.setters;
      operators1 = env.operators1;
      operators2 = env.operators2;
      globals = env.globals;
      constants = env.constants;
    }

  let addType (env : t) (loc : Loc.t) (qid : Ident.t) (t : typedef) : unit =
    (* Format.fprintf fmt "New type %a at %a\n" FMT.varname FMT.loc loc; *)
    let t' =
      match (Bindings.find_opt qid env.types, t) with
      | None, _ -> t
      | Some Type_Forward, _ -> t
      | Some p, Type_Forward -> p
      | Some p, _ when p <> t ->
          raise
            (DoesNotMatch
               ( loc,
                 "type definition",
                 to_string2 (pp_typedef t),
                 to_string2 (pp_typedef p) ))
      | _ -> t
    in
    env.types <- Bindings.add qid t' env.types

  let getType (env : t) (qid : Ident.t) : typedef option =
    Bindings.find_opt qid env.types

  let isType (env : t) (qid : Ident.t) : bool = true (* todo *)
  let isTycon (env : t) (qid : Ident.t) : bool = true (* todo *)
  let isEnum (env : t) (qid : Ident.t) : bool = true (* todo *)

  let addFuns (env : t) (loc : Loc.t) (qid : Ident.t) (ftys : funtype list) :
      unit =
    env.functions <- Bindings.add qid ftys env.functions

  let getFuns (env : t) (qid : Ident.t) : funtype list =
    match Bindings.find_opt qid env.functions with
    | None -> []
    | Some tys -> tys

  let addSetterFuns (env : t) (qid : Ident.t) (ftys : funtype list) : unit =
    env.setters <- Bindings.add qid ftys env.setters

  let getSetterFun (env : t) (qid : Ident.t) : funtype list =
    match Bindings.find_opt qid env.setters with None -> [] | Some tys -> tys

  let addOperators1 (env : t) (loc : Loc.t) (op : AST.unop)
      (funs : funtype list) : unit =
    List.iter (function fty -> FMT.add_unop op fty.funname) funs;
    env.operators1 <-
      Operators1.update op
        (fun ov ->
          let old = Option.value ov ~default:[] in
          Some (funs @ old))
        env.operators1

  let getOperators1 (env : t) (loc : Loc.t) (op : AST.unop) : funtype list =
    Option.value (Operators1.find_opt op env.operators1) ~default:[]

  let addOperators2 (env : t) (loc : Loc.t) (op : AST.binop)
      (funs : funtype list) : unit =
    List.iter (function fty -> FMT.add_binop op fty.funname) funs;
    env.operators2 <-
      Operators2.update op
        (fun ov ->
          let old = Option.value ov ~default:[] in
          Some (funs @ old))
        env.operators2

  let getOperators2 (env : t) (loc : Loc.t) (op : AST.binop) : funtype list =
    Option.value (Operators2.find_opt op env.operators2) ~default:[]

  let addGlobalVar (env : t) (v : var_info) : unit =
    (* Format.fprintf fmt "New global %a\n" pp_var_info v; *)
    env.globals <- Bindings.add v.name v env.globals

  let getGlobalVar (env : t) (v : Ident.t) : var_info option =
    (* Format.fprintf fmt "Looking for global variable %a\n" FMT.varname v; *)
    Bindings.find_opt v env.globals

  let getConstant (env : t) (v : Ident.t) : AST.expr option =
    Bindings.find_opt v env.constants

  let addConstant (env : t) (v : Ident.t) (e : AST.expr) : unit =
    let e' = subst_fun_expr (getConstant env) e in
    env.constants <- Bindings.add v e' env.constants
end

let subst_consts_expr (env : GlobalEnv.t) (e : AST.expr) : AST.expr =
  subst_fun_expr (GlobalEnv.getConstant env) e

let subst_consts_type (env : GlobalEnv.t) (ty : AST.ty) : AST.ty =
  subst_fun_type (GlobalEnv.getConstant env) ty

(** expand a type definition using type parameters *)
let expand_type (loc : Loc.t) (ps : Ident.t list) (ty : AST.ty) (es : expr list) : AST.ty =
  if List.length ps <> List.length es then begin
    raise (TypeError (loc, "wrong number of type parameters"))
  end;
  let bs = mk_bindings (List.combine ps es) in
  subst_type bs ty

(** dereference typedef *)
let rec derefType (env : GlobalEnv.t) (loc : Loc.t) (ty : AST.ty) : AST.ty =
  match ty with
  | Type_Constructor (tc, es) -> (
      match GlobalEnv.getType env tc with
      | Some (Type_Abbreviation (ps, ty')) ->
        let ty'' = expand_type loc ps ty' es in
        derefType env loc ty''
      | _ -> ty)
  | _ -> ty

let width_of_type (env : GlobalEnv.t) (loc : Loc.t) (ty : AST.ty) : AST.expr =
  let ty' = derefType env loc ty in
  Utils.from_option (Asl_utils.width_of_type ty')
    (fun _ -> raise (InternalError
      (loc, "width_of_type", (fun fmt -> FMT.ty fmt ty'), __LOC__)))


(****************************************************************)
(** {3 Field typechecking support}                              *)
(****************************************************************)

(** Field accesses can be either record fields or fields of registers

    This type captures the information needed to typecheck either of these
    - a list of fieldname/type pairs for records
    - a list of fieldname/slice pairs for registers
 *)
type fieldtypes =
  | FT_Record of (Ident.t * ty) list
  | FT_Register of (AST.slice list * Ident.t) list

(** Get fieldtype information for a record/register type *)
let typeFields (env : GlobalEnv.t) (loc : Loc.t) (x : ty) : fieldtypes =
  match derefType env loc x with
  | Type_Constructor (tc, es) -> (
      match GlobalEnv.getType env tc with
      | Some (Type_Record (ps, fs)) ->
        let fs' = List.map (fun (f, ty) -> (f, expand_type loc ps ty es)) fs in
        FT_Record fs'
      | Some (Type_Exception fs) ->
        FT_Record fs
      | _ -> raise (IsNotA (loc, "record or exception", Ident.to_string tc)))
  | Type_Bits (n, fs) -> FT_Register fs
  | Type_OfExpr e ->
      raise (InternalError
        (loc, "typeFields: Type_OfExpr", (fun fmt -> FMT.expr fmt e), __LOC__))
  | _ -> raise (IsNotA (loc, "record/register", pp_type x))

(** Get fieldtype information for a named field of a record *)
let get_recordfield (loc : Loc.t) (rfs : (Ident.t * ty) list) (f : Ident.t) : AST.ty
    =
  match List.filter (fun (fnm, _) -> fnm = f) rfs with
  | [ (_, fty) ] -> fty
  | [] -> raise (UnknownObject (loc, "field", Ident.to_string f))
  | fs -> raise (Ambiguous (loc, "field", Ident.to_string f))

(** Get fieldtype information for a named field of a slice *)
let get_regfield_info (loc : Loc.t) (rfs : (AST.slice list * Ident.t) list)
    (f : Ident.t) : AST.slice list =
  match List.filter (fun (_, fnm) -> fnm = f) rfs with
  | [ (ss, _) ] -> ss
  | [] -> raise (UnknownObject (loc, "field", Ident.to_string f))
  | fs -> raise (Ambiguous (loc, "field", Ident.to_string f))

(** Get named field of a register and calculate type *)
let get_regfield (loc : Loc.t) (rfs : (AST.slice list * Ident.t) list) (f : Ident.t)
    : AST.slice list * AST.ty =
  let ss = get_regfield_info loc rfs f in
  (ss, type_bits (slices_width ss))

(** Get named fields of a register and calculate type of concatenating them *)
let get_regfields (loc : Loc.t) (rfs : (AST.slice list * Ident.t) list)
    (fs : Ident.t list) : AST.slice list * AST.ty =
  let ss = List.flatten (List.map (get_regfield_info loc rfs) fs) in
  (ss, type_bits (slices_width ss))

(****************************************************************)
(** {3 Environment (aka the Local+Global Symbol Table)}         *)
(****************************************************************)

module Env : sig
  type t

  val mkEnv : GlobalEnv.t -> t
  val globals : t -> GlobalEnv.t
  val nest : (t -> 'a) -> t -> 'a
  val addLocalVar : t -> var_info -> unit
  val getVar : t -> Ident.t -> var_info option
  val markModified : t -> Ident.t -> unit
  val addConstraint : t -> Loc.t -> AST.expr -> unit
  val getConstraints : t -> AST.expr list
  val setReturnType : t -> AST.ty -> unit
  val getReturnType : t -> AST.ty
end = struct
  type t = {
    globals : GlobalEnv.t;
    mutable rty : AST.ty;
    (* a stack of nested scopes representing the local type environment *)
    (* Invariant: the stack is never empty *)
    mutable locals : var_info Bindings.t list;
    mutable modified : IdentSet.t;
    (* constraints collected while typechecking current expression/assignment *)
    mutable constraints : AST.expr list;
  }

  let mkEnv (globalEnv : GlobalEnv.t) =
    {
      globals = globalEnv;
      rty = type_unit;
      locals = [ Bindings.empty ];
      modified = IdentSet.empty;
      constraints = [];
    }

  (* todo: would it be better to make Env a subclass of GlobalEnv
   * Doing that would eliminate many, many calls to this function
   *)
  let globals (env : t) : GlobalEnv.t = env.globals

  let nest (k : t -> 'a) (parent : t) : 'a =
    let child =
      {
        globals = parent.globals;
        rty = parent.rty;
        locals = Bindings.empty :: parent.locals;
        modified = IdentSet.empty;
        constraints = parent.constraints;
      }
    in
    let r = k child in
    parent.modified <- IdentSet.union parent.modified child.modified;
    r

  let rec search_var (env : GlobalEnv.t) (bss : var_info Bindings.t list) (v : Ident.t) : var_info option =
    ( match bss with
    | bs :: bss' -> orelse_option (Bindings.find_opt v bs) (fun _ -> search_var env bss' v)
    | [] -> GlobalEnv.getGlobalVar env v
    )

  let addLocalVar (env : t) (v : var_info) : unit =
    (* Format.fprintf fmt "New local var %a : %a at %a\n" FMT.varname v.name FMT.ty (ppp_type v.ty) FMT.loc v.loc; *)
    Option.iter (fun (w : var_info) ->
        let msg = Format.asprintf "variable `%a` previously declared at `%a`"
            FMT.varname v.name
            FMT.loc w.loc
        in
        raise (TypeError (v.loc, msg))
      )
      (search_var env.globals env.locals v.name);
    (match env.locals with
    | bs :: bss -> env.locals <- Bindings.add v.name v bs :: bss
    | [] ->
        raise (InternalError
          (v.loc, "addLocalVar", (fun fmt -> FMT.varname fmt v.name), __LOC__))
    );
    if not v.is_constant then env.modified <- IdentSet.add v.name env.modified

  let getVar (env : t) (v : Ident.t) : var_info option =
    (* Format.fprintf fmt "Looking for variable %a\n" FMT.varname v; *)
    search_var env.globals env.locals v

  let markModified (env : t) (v : Ident.t) : unit =
    env.modified <- IdentSet.add v env.modified

  let addConstraint (env : t) (loc : Loc.t) (c : AST.expr) : unit =
    env.constraints <- c :: env.constraints

  let getConstraints (env : t) : AST.expr list = env.constraints
  let setReturnType (env : t) (ty : AST.ty) : unit = env.rty <- ty
  let getReturnType (env : t) : AST.ty = env.rty
end

(****************************************************************)
(** {2 Subtype satisfaction}                                    *)
(****************************************************************)

(****************************************************************)
(** {3 Expression simplification}                               *)
(****************************************************************)

(** Perform simple constant folding of expression

    It's primary role is to enable the 'DIV' hacks in
    z3_of_expr which rely on shallow syntactic transforms.
    It has a secondary benefit of sometimes causing constraints
    to become so trivial that we don't even need to invoke Z3
    which gives a performance benefit.
 *)

let rec const_fold_expr (x : AST.expr) : AST.expr =
  let rec eval (x : AST.expr) : Z.t option =
    ( match x with
    | Expr_Lit (VInt x') -> Some x'
    | Expr_Assert (e1, e2, loc) -> eval e2
    | _ -> None
    )
  in
  let to_expr (x : Z.t) : AST.expr = Expr_Lit (VInt x) in

  ( match x with
  | Expr_TApply (f, [], [a; b], _) when Ident.equal f eq_int && a = b -> asl_true
  | Expr_TApply (f, tes, es, throws) ->
      let es' = List.map const_fold_expr es in
      ( match flatten_map_option eval es' with
      | Some [a]    when Ident.equal f neg_int -> to_expr (Primops.prim_neg_int a)
      | Some [a; b] when Ident.equal f add_int -> to_expr (Primops.prim_add_int a b)
      | Some [a; b] when Ident.equal f sub_int -> to_expr (Primops.prim_sub_int a b)
      | Some [a; b] when Ident.equal f mul_int -> to_expr (Primops.prim_mul_int a b)
      | Some [a; b] when Ident.equal f exact_div_int -> to_expr (Primops.prim_exact_div_int a b)
      | Some [a; b] when Ident.equal f shl_int -> to_expr (Primops.prim_shl_int a b)
      | Some [a; b] when Ident.equal f shr_int -> to_expr (Primops.prim_shr_int a b)
      | Some [a; b] when Ident.equal f min -> to_expr (Z.min a b)
      | Some [a; b] when Ident.equal f max -> to_expr (Z.max a b)
      | Some [a]    when Ident.equal f pow2_int -> to_expr (Primops.prim_pow2_int a)
      | Some [a; b] when Ident.equal f pow_int_int -> to_expr (Primops.prim_pow_int_int a b)
      | _ -> Expr_TApply (f, tes, es', throws)
      )
  | Expr_Assert (e1, e2, loc) ->
      const_fold_expr e2
  | _ -> x
  )

(* Simplify use of __assert and __let in an expression
 * as preparation for generating a Z3 check.
 *
 * The primary reason for doing this is to enable the exact_div
 * handling in z3_of_expr to work.
 *)
class simplifyExprClass = object(self)
  inherit Asl_visitor.nopAslVisitor

  val mutable env = Bindings.empty

  method! vexpr e =
    ( match e with
    | Expr_Var v ->
        ( match Bindings.find_opt v env with
        | Some r -> ChangeTo r
        | None -> SkipChildren
        )
    | Expr_Let (v, ty, e1, e2) ->
        env <- Bindings.add v e1 env;
        (* Note: there is no need to remove 'v' after transforming e2
         * because __let-bound variables are unique in the expression.
         *)
        ChangeTo e2
    | Expr_Assert (e1, e2, loc) ->
        ChangeDoChildrenPost (e2, Fun.id)
    | _ ->
        DoChildren
    )
end

let simplify_expr (x : AST.expr) : AST.expr =
  let v = new simplifyExprClass in
  Asl_visitor.visit_expr v x

(****************************************************************)
(** {3 Z3 support code}                                         *)
(****************************************************************)

(** Convert ASL expression to Z3 expression.
    This only copes with a limited set of operations:
    ==, +, -, negate, *, and DIV.
    (It is possible that we will need to extend this list in the future but
    it is sufficient for the current ASL specifications.)

    The support for DIV is not sound - it is a hack needed to cope with
    the way ASL code is written and generally needs a side condition
    that the division is exact (no remainder).

    ufs is a mutable list of conversions used to handle subexpressions
    that cannot be translated.  We treat such subexpressions as
    uninterpreted functions and add them to the 'ufs' list so that
    we can reason that "F(x) == F(x)" without knowing what the function
    "F" does.
 *)

let rec z3_of_expr
    (ctx : Z3.context)
    (ufs : (AST.expr * Z3.Expr.expr) list ref)
    (x : AST.expr)
    : Z3.Expr.expr =
  ( match x with
  | Expr_Var v ->
      if v = false_ident then Z3.Boolean.mk_false ctx
      else if v = true_ident then Z3.Boolean.mk_true ctx
      else (
        let intsort = Z3.Arithmetic.Integer.mk_sort ctx in
        Z3.Expr.mk_const_s ctx (Ident.to_string v) intsort
      )
  | Expr_Lit (VInt i) -> Z3.Arithmetic.Integer.mk_numeral_s ctx (Z.to_string i)
  (* todo: the following lines involving DIV are not sound *)
  | Expr_TApply
      ( i,
        [],
        [ Expr_TApply (j, [], [ a; b ], _); c ],
        _
      )
    when Ident.equal i mul_int && Ident.equal j exact_div_int && b = c ->
      z3_of_expr ctx ufs a
  | Expr_TApply
      ( i,
        [],
        [ a; Expr_TApply (j, [], [ b; c ], _) ],
        _
      )
    when Ident.equal i mul_int && Ident.equal j exact_div_int && a = c ->
      z3_of_expr ctx ufs b
  | Expr_TApply
      ( i,
        [],
        [
          Expr_TApply (j, [], [ a1; b1 ], _);
          Expr_TApply (k, [], [ a2; b2 ], _);
        ],
        _
      )
    when Ident.equal i add_int && Ident.equal j exact_div_int && Ident.equal k exact_div_int && a1 = a2 && b1 = b2 && b1 = two ->
      z3_of_expr ctx ufs a1
  | Expr_TApply
      ( i,
        [],
        [ a; Expr_TApply (j, [], [ b; c ], _) ],
        _
      ) when Ident.equal i eq_int && Ident.equal j exact_div_int ->
      Z3.Boolean.mk_eq ctx
        (Z3.Arithmetic.mk_mul ctx
           [ z3_of_expr ctx ufs c; z3_of_expr ctx ufs a ])
        (z3_of_expr ctx ufs b)
  | Expr_TApply (i, [], [x], _) when Ident.equal i neg_int ->
      Z3.Arithmetic.mk_unary_minus ctx (z3_of_expr ctx ufs x)
  | Expr_TApply (i, [], xs, _) when Ident.equal i add_int ->
      Z3.Arithmetic.mk_add ctx (List.map (z3_of_expr ctx ufs) xs)
  | Expr_TApply (i, [], xs, _) when Ident.equal i sub_int ->
      Z3.Arithmetic.mk_sub ctx (List.map (z3_of_expr ctx ufs) xs)
  | Expr_TApply (i, [], xs, _) when Ident.equal i mul_int ->
      Z3.Arithmetic.mk_mul ctx (List.map (z3_of_expr ctx ufs) xs)
  | Expr_TApply (i, [], [a; b], _) when Ident.equal i shl_int ->
      let b' = Z3.Arithmetic.mk_power ctx (Z3.Arithmetic.Integer.mk_numeral_i ctx 2) (z3_of_expr ctx ufs b) in
      Z3.Arithmetic.mk_mul ctx [z3_of_expr ctx ufs a; b']
  | Expr_TApply (i, [], [a; b], _) when Ident.equal i shr_int ->
      let b' = Z3.Arithmetic.mk_power ctx (Z3.Arithmetic.Integer.mk_numeral_i ctx 2) (z3_of_expr ctx ufs b) in
      Z3.Arithmetic.mk_div ctx (z3_of_expr ctx ufs a) b'
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i exact_div_int ->
      Z3.Arithmetic.mk_div ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | Expr_TApply (i, [], [ a; b], _) when Ident.equal i min ->
      let a' = z3_of_expr ctx ufs a in
      let b' = z3_of_expr ctx ufs b in
      let c = Z3.Arithmetic.mk_le ctx a' b' in
      Z3.Boolean.mk_ite ctx c a' b'
  | Expr_TApply (i, [], [ a; b], _) when Ident.equal i max ->
      let a' = z3_of_expr ctx ufs a in
      let b' = z3_of_expr ctx ufs b in
      let c = Z3.Arithmetic.mk_le ctx a' b' in
      Z3.Boolean.mk_ite ctx c b' a'
  | Expr_TApply (i, [], [x], _) when Ident.equal i pow2_int ->
      Z3.Arithmetic.mk_power ctx (Z3.Arithmetic.Integer.mk_numeral_i ctx 2) (z3_of_expr ctx ufs x)
  | Expr_TApply (i, [], [a; b], _) when Ident.equal i pow_int_int ->
      Z3.Arithmetic.mk_power ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i eq_int ->
      Z3.Boolean.mk_eq ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i le_int ->
      Z3.Arithmetic.mk_le ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i lt_int ->
      Z3.Arithmetic.mk_lt ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i ge_int ->
      Z3.Arithmetic.mk_ge ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i gt_int ->
      Z3.Arithmetic.mk_gt ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i lazy_and_bool ->
    Z3.Boolean.mk_and ctx [z3_of_expr ctx ufs a; z3_of_expr ctx ufs b]
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i lazy_or_bool ->
    Z3.Boolean.mk_or ctx [z3_of_expr ctx ufs a; z3_of_expr ctx ufs b]
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i implies_bool ->
      Z3.Boolean.mk_implies ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | _ ->
      if verbose then
        Format.fprintf fmt
          "    Unable to translate %a - using as uninterpreted function\n"
          FMT.expr x;
      let intsort = Z3.Arithmetic.Integer.mk_sort ctx in
      ( match List.assoc_opt x !ufs with
      | None ->
          let uf = Z3.Expr.mk_fresh_const ctx "UNINTERPRETED" intsort in
          ufs := (x, uf) :: !ufs;
          uf
      | Some uf -> uf
      )
  )

let z3_ctx = Z3.mk_context []

let solver = Z3.Solver.mk_simple_solver z3_ctx

(** check that bs => cs where bs and cs are lists of boolean expressions *)
let check_constraints (bs : expr list) (cs : expr list) : bool =
  (* note that we rebuild the Z3 context each time.
   * It is possible to share them across all invocations to save
   * about 10% of execution time.
   *)
  let ufs = ref [] in
  (* uninterpreted function list *)
  let bs' = List.map (z3_of_expr z3_ctx ufs) bs in
  let cs' = List.map (z3_of_expr z3_ctx ufs) cs in
  let p =
    Z3.Boolean.mk_implies z3_ctx
      (Z3.Boolean.mk_and z3_ctx bs')
      (Z3.Boolean.mk_and z3_ctx cs')
  in
  if verbose then
    Format.fprintf fmt "      - Checking %s\n" (Z3.Expr.to_string p);
  Z3.Solver.push solver;
  Z3.Solver.add solver [ Z3.Boolean.mk_not z3_ctx p ];
  let q = Z3.Solver.check solver [] in
  Z3.Solver.pop solver 1;
  if verbose && q <> UNSATISFIABLE then begin
    Format.fprintf fmt "Failed property %s\n" (Z3.Expr.to_string p);
    ( match Z3.Solver.get_model solver with
    | Some model -> Format.fprintf fmt "        - Fails for model '%s'\n" (Z3.Model.to_string model)
    | None -> Format.fprintf fmt "        - Unable to extract counterexample"
    )
  end;
  q = UNSATISFIABLE

(****************************************************************)
(** {3 Checking subtyping}                                      *)
(****************************************************************)

let simplify_constraints (cs : constraint_range list) : constraint_range list =
  (* todo: remove constants, duplicates, etc. *)
  cs

let constraint_union (ocrs1 : constraint_range list option) (ocrs2 : constraint_range list option) : constraint_range list option =
  map2_option (fun crs1 crs2 -> simplify_constraints (crs1 @ crs2)) ocrs1 ocrs2

let check_equality
  (env : Env.t) (loc : Loc.t)
  (x : AST.expr) (y : AST.expr)
  : unit =
  let x' = const_fold_expr (simplify_expr x) in
  let y' = const_fold_expr (simplify_expr y) in

  (* As a performance optimisation, omit SMT calls that are trivially true *)
  if x' <> y' then begin
    let assumptions = Env.getConstraints env in
    let assumptions' = List.map (fun c -> const_fold_expr (simplify_expr c)) assumptions in
    let constraints = [mk_eq_int x' y'] in
    if not (check_constraints assumptions' constraints) then begin
      raise (DoesNotMatch (loc, "type width parameter", pp_expr x, pp_expr y))
    end
  end

(* a constraint range a..b is only legal if a <= b *)
let is_legal_constraint_range (cr : AST.constraint_range) : AST.expr option =
  ( match cr with
  | Constraint_Single _ -> None
  | Constraint_Range (lo, hi) -> Some (mk_le_int lo hi)
  )

let are_legal_constraint_ranges (crs : AST.constraint_range list) : AST.expr =
  mk_ands (List.filter_map is_legal_constraint_range crs)

let constraint_range_to_expr (v : AST.expr) (cr : AST.constraint_range) : AST.expr =
  ( match cr with
  | Constraint_Single e -> if v = e then asl_true else mk_eq_int v e
  | Constraint_Range (lo, hi) -> mk_and (mk_le_int lo v) (mk_le_int v hi)
  )

let constraint_ranges_to_expr (v : AST.expr) (crs : AST.constraint_range list) : AST.expr =
  mk_ors (List.map (constraint_range_to_expr v) crs)

let is_subrange (env : Env.t) (crs1 : constraint_range list) (crs2 : constraint_range list) : bool =
  let v = Expr_Var wildcard_ident in
  let chk = mk_implies
              (mk_and (are_legal_constraint_ranges crs1) (constraint_ranges_to_expr v crs1))
              (constraint_ranges_to_expr v crs2)  in
  let chk' = const_fold_expr (simplify_expr chk) in
  let assumptions = Env.getConstraints env in
  let assumptions' = List.map (fun c -> const_fold_expr (simplify_expr c)) assumptions in
  let r = check_constraints assumptions' [chk'] in
  if verbose && not r then begin
    Format.printf "Failed subrange check %a => %a\n"
      FMT.exprs assumptions
      FMT.expr chk
  end;
  r

let check_subrange_satisfies (env : Env.t) (loc : Loc.t) (ocrs1 : constraint_range list option) (ocrs2 : constraint_range list option) : unit =
  ( match (ocrs1, ocrs2) with
  | (_, None) -> ()
  | (None, Some crs2) ->
    let msg = Format.asprintf "`integer` is not a subrange of `%a`"
        FMT.constraints crs2
    in
    raise (TypeError (loc, msg))
  | (Some crs1, Some crs2) ->
    if not (is_subrange env crs1 crs2) then begin
      let msg = Format.asprintf "`%a` is not a subrange of `%a`"
          FMT.constraints crs1
          FMT.constraints crs2
      in
      raise (TypeError (loc, msg))
    end
  )

(** Check that ty1 subtype-satisfies ty2
    On failure, report an error.

    This differs from eq_structural and synthesize_type (below)
    in that it checks the dependent part of the type.
 *)
let rec check_subtype_satisfies (env : Env.t) (loc : Loc.t) (ty1 : AST.ty) (ty2 : AST.ty) : unit =
  let genv = Env.globals env in
  (* Substitute global constants in types *)
  let subst_consts = new substFunClass (GlobalEnv.getConstant genv) in
  let ty1' = Asl_visitor.visit_type subst_consts ty1 in
  let ty2' = Asl_visitor.visit_type subst_consts ty2 in
  ( match (derefType genv loc ty1', derefType genv loc ty2') with
  | Type_Integer ocrs1, Type_Integer ocrs2 ->
      if !enable_constraint_checks then check_subrange_satisfies env loc ocrs1 ocrs2
  | Type_Bits (e1, fs1), Type_Bits (e2, fs2)
    (* todo: check that register fields fs2 are a subset of fs1 *)
    -> check_equality env loc e1 e2
  | Type_Constructor (tc1, es1), Type_Constructor (tc2, es2) when tc1 = tc2 ->
      assert (List.length es1 = List.length es2);
      List.iter2 (check_equality env loc) es1 es2
  | Type_OfExpr e1, Type_OfExpr e2 ->
      raise (InternalError
        (loc, "check_subtype_satisfies: typeof", (fun fmt -> FMT.ty fmt ty1), __LOC__))
  | Type_Array (ixty1, elty1), Type_Array (ixty2, elty2) ->
      ( match (ixty1, ixty2) with
      | Index_Enum tc1, Index_Enum tc2 -> ()
      | Index_Int sz1, Index_Int sz2 -> check_equality env loc sz1 sz2
      | _ ->
        raise (DoesNotMatch (loc, "array index type", pp_ixtype ixty1, pp_ixtype ixty2))
      );
      check_subtype_satisfies env loc elty1 elty2
  | Type_Tuple tys1, Type_Tuple tys2 ->
      if List.length tys1 <> List.length tys2 then begin
        raise (DoesNotMatch (loc, "tuple length", pp_type ty2, pp_type ty1))
      end;
      List.iter2 (check_subtype_satisfies env loc) tys1 tys2
  | _ -> raise (DoesNotMatch (loc, "type", pp_type ty2, pp_type ty1))
  )


(** Calculate the least supertype of `ty1` and `ty2`
 *
 *  e.g. the least supertype of "integer \{0..3\}" and "integer \{8, 16\}"
 *  is "integer \{0..3, 8, 16\}"
 *  and the least supertype of "integer" and "integer \{8, 16\}" is "integer".
 *
 *  If no supertype exists, report an error.
 *)
let rec least_supertype (env : Env.t) (loc : Loc.t) (ty1 : AST.ty) (ty2 : AST.ty) : AST.ty =
  let genv = Env.globals env in
  (* Substitute global constants in types *)
  let subst_consts = new substFunClass (GlobalEnv.getConstant genv) in
  let ty1' = Asl_visitor.visit_type subst_consts ty1 in
  let ty2' = Asl_visitor.visit_type subst_consts ty2 in
  ( match (derefType genv loc ty1', derefType genv loc ty2') with
  | (Type_Integer ocrs1, Type_Integer ocrs2) -> Type_Integer (constraint_union ocrs1 ocrs2)
  | (Type_Bits (e1, _), Type_Bits (e2, _)) ->
      check_equality env loc e1 e2;
      ty1
  | (Type_Constructor (tc1, es1), Type_Constructor (tc2, es2)) when tc1 = tc2 ->
      assert (List.length es1 = List.length es2);
      List.iter2 (check_equality env loc) es1 es2;
      ty1
  | (Type_OfExpr e1, Type_OfExpr e2) ->
      raise (InternalError (loc, "least_supertype: typeof", (fun fmt -> FMT.ty fmt ty1), __LOC__))
  | (Type_Array (ixty1, elty1), Type_Array (ixty2, elty2)) ->
      ( match (ixty1, ixty2) with
      | (Index_Enum tc1, Index_Enum tc2) when tc1 == tc2 -> ()
      | (Index_Int sz1, Index_Int sz2) -> check_equality env loc sz1 sz2
      | _ ->
        raise (DoesNotMatch (loc, "array index type", pp_ixtype ixty1, pp_ixtype ixty2))
      );
      let elty' = least_supertype env loc elty1 elty2 in
      Type_Array (ixty1, elty')
  | Type_Tuple tys1, Type_Tuple tys2 ->
      if List.length tys1 <> List.length tys2 then begin
        raise (DoesNotMatch (loc, "tuple length", pp_type ty2, pp_type ty1))
      end;
      let tys' = List.map2 (least_supertype env loc) tys1 tys2 in
      Type_Tuple tys'
  | _ -> raise (DoesNotMatch (loc, "type", pp_type ty2, pp_type ty1))
  )

(****************************************************************)
(** {2 Inserting runtime checks}                                *)
(****************************************************************)

let check_vars = new Asl_utils.nameSupply "__check"

let add_check (loc : Loc.t) (asserts : check list ref) (x : AST.expr) : unit =
  asserts := (x, loc) :: !asserts

let mk_expr_safe_to_replicate (lets : binding list ref) (x : AST.expr) (ty : AST.ty) : AST.expr =
  if is_safe_to_replicate x then
    x
  else
    let v = check_vars#fresh in
    lets := (v, ty, x) :: !lets;
    Expr_Var v

let mk_zero_check (loc : Loc.t) (lets : binding list ref) (asserts : check list ref) (x : AST.expr) : AST.expr =
  if not !enable_runtime_checks then
      x
  else
      let x' = mk_expr_safe_to_replicate lets x type_integer in
      add_check loc asserts (mk_ne_int zero x');
      x'

let mk_exactdiv_check (loc : Loc.t) (lets : binding list ref) (asserts : check list ref) (x : AST.expr) (y : AST.expr) : (AST.expr * AST.expr) =
  if not !enable_runtime_checks then
      (x, y)
  else
      let x' = mk_expr_safe_to_replicate lets x type_integer in
      let y' = mk_expr_safe_to_replicate lets y type_integer in
      (* The exact div runtime check is disabled at present because it breaks too much code *)
      (* add_check loc asserts (mk_eq_int zero (mk_zrem_int x' y')); *)
      add_check loc asserts (mk_ne_int zero y');
      (x', y')

let mk_index_check (loc : Loc.t) (lets : binding list ref) (asserts : check list ref) (ixty : AST.ixtype) (ix : AST.expr) : AST.expr =
  if not !enable_runtime_checks then
      ix
  else
      ( match ixty with
      | Index_Enum tc -> ix
      | Index_Int size ->
          let size' = mk_expr_safe_to_replicate lets size type_integer in
          let ix'   = mk_expr_safe_to_replicate lets ix type_integer in
          add_check loc asserts (mk_le_int zero ix');
          add_check loc asserts (mk_lt_int ix' size');
          ix'
      )

let mk_slice_check (loc : Loc.t) (lets : binding list ref) (asserts : check list ref) (size : AST.expr) (s : AST.slice) : AST.slice =
  if not !enable_runtime_checks then
      s
  else
      ( match s with
      | Slice_Single ix ->
          let size' = mk_expr_safe_to_replicate lets size type_integer in
          let ix'   = mk_expr_safe_to_replicate lets ix type_integer in
          add_check loc asserts (mk_le_int zero ix');
          add_check loc asserts (mk_lt_int ix' size');
          Slice_Single ix'
      | Slice_HiLo (hi, lo) ->
          let size' = mk_expr_safe_to_replicate lets size type_integer in
          let hi'   = mk_expr_safe_to_replicate lets hi type_integer in
          let lo'   = mk_expr_safe_to_replicate lets lo type_integer in
          add_check loc asserts (mk_le_int zero lo');
          add_check loc asserts (mk_le_int lo' (mk_add_int hi' one));
          add_check loc asserts (mk_lt_int hi' size');
          Slice_HiLo (hi', lo')
      | Slice_LoWd (lo, wd) ->
          let size' = mk_expr_safe_to_replicate lets size type_integer in
          let lo'   = mk_expr_safe_to_replicate lets lo type_integer in
          let wd'   = mk_expr_safe_to_replicate lets wd type_integer in
          add_check loc asserts (mk_le_int zero lo');
          add_check loc asserts (mk_le_int zero wd');
          add_check loc asserts (mk_le_int (mk_add_int lo' wd') size');
          Slice_LoWd (lo', wd')
      | Slice_HiWd (hi, wd) ->
          let size' = mk_expr_safe_to_replicate lets size type_integer in
          let hi'   = mk_expr_safe_to_replicate lets hi type_integer in
          let wd'   = mk_expr_safe_to_replicate lets wd type_integer in
          add_check loc asserts (mk_lt_int hi' size');
          add_check loc asserts (mk_le_int zero wd');
          add_check loc asserts (mk_le_int wd' (mk_add_int hi' one));
          Slice_HiWd (hi', wd')
      | Slice_Element (ix, wd) ->
          let size' = mk_expr_safe_to_replicate lets size type_integer in
          let ix'   = mk_expr_safe_to_replicate lets ix type_integer in
          let wd'   = mk_expr_safe_to_replicate lets wd type_integer in
          add_check loc asserts (mk_le_int zero ix');
          add_check loc asserts (mk_le_int zero wd');
          add_check loc asserts (mk_le_int (mk_mul_int ix' wd') (mk_sub_int size' wd'));
          Slice_Element (ix, wd')
      )

let mk_constraint_check (loc : Loc.t) (lets : binding list ref) (asserts : check list ref) (crs : AST.constraint_range list) (x : AST.expr) : AST.expr =
  if not !enable_runtime_checks then
      x
  else
      let x' = mk_expr_safe_to_replicate lets x type_integer in
      add_check loc asserts (constraint_ranges_to_expr x' crs);
      x'

let mk_type_check (loc : Loc.t) (lets : binding list ref) (asserts : check list ref) (t : AST.ty) (x : AST.expr) : AST.expr =
  if not !enable_runtime_checks then
      x
  else
      ( match t with
      | Type_Integer (Some crs) -> mk_constraint_check loc lets asserts crs x
      | _ -> x
      )

(****************************************************************)
(** {2 Typechecking function (and operator) application}        *)
(*                                                              *)
(* This consists of                                             *)
(* - synthesizing type parameters                               *)
(*   (eg if "x : bits(8)", then, in a call "IsZero(x)", we      *)
(*   infer the parameter "8"                                    *)
(* - typechecking a function call                               *)
(* - disambiguating which function or operator is being called  *)
(*   (eg in the expression "x + y", we use the types of x and y *)
(*   to decide whether it is an integer addition, bit addition, *)
(*   etc.)                                                      *)
(****************************************************************)

(****************************************************************)
(** {3 Synthesis of type parameters (which checking subtyping}  *)
(****************************************************************)

(** Record bindings of type parameters to values from the types of
 * actual arguments.
 * (If there are multiple bindings, we will use the last one found
 * and detect any mismatches during the subsequent typecheck.
 *)
let synthesize_equality (s : (AST.expr option) Scope.t) (x : AST.expr) (y : AST.expr) : unit =
  ( match y with
  | Expr_Var v ->
      ( match Scope.get s v with
      | Some None -> Scope.set s v (Some x)
      | _ -> ()
      )
  | _ -> ()
  )

(** Synthesize type parameter values by matching
 *  an actual argument type `ty1` (e.g., "bits(32)")
 *  against the corresponding formal argument type `ty2`
 *  (e.g., "bits(N)" where `N` is a parameter of the function).
 *  The result is put in `s`.
 *
 *  This does not check that the types match so it is necessary
 *  to call `check_subtype_satisfies` after the type parameters
 *  have been determined.
 *)
let rec synthesize_type
  (env : Env.t) (loc : Loc.t) (s : AST.expr option Scope.t)
  (ty1 : AST.ty) (ty2 : AST.ty)
  : unit =
  let genv = Env.globals env in
  ( match (derefType genv loc ty1, derefType genv loc ty2) with
  | Type_Bits (e1, _), Type_Bits (e2, _) -> synthesize_equality s e1 e2
  | Type_Constructor (tc1, es1), Type_Constructor (tc2, es2) when tc1 = tc2 ->
      assert (List.length es1 = List.length es2);
      List.iter2 (synthesize_equality s) es1 es2
  | Type_Array (Index_Int n1, elty1), Type_Array (Index_Int n2, elty2) ->
      synthesize_equality s n1 n2;
      synthesize_type env loc s elty1 elty2
  | Type_Array (_, elty1), Type_Array (_, elty2) ->
      synthesize_type env loc s elty1 elty2
  | Type_Tuple tys1, Type_Tuple tys2 ->
      if List.length tys1 <> List.length tys2 then begin
        raise (DoesNotMatch (loc, "tuple length", pp_type ty2, pp_type ty1))
      end;
      List.iter2 (synthesize_type env loc s) tys1 tys2
  | _ -> ()
  )

(** Synthesize all the parameter values by matching actual argument types
 *  against the formal argument types.
 *)
let synthesize_parameters (env : Env.t) (loc : Loc.t)
    (fty : funtype) (es : AST.expr list) (tys : AST.ty list) : AST.expr Bindings.t =
  assert (List.length fty.atys = List.length es);
  let genv = Env.globals env in

  (* Synthesize parameter values using a combination of any explicit
   * parameter values and matching the types of the actual arguments
   * against the types of the formal arguments.
   * The parameter values are accumulated in `s`
   *)
  let s : (AST.expr option) Scope.t = Scope.empty () in
  List.iter (fun (p, _) -> Scope.set s p None) fty.params;
  iter3 (fun e ety (v, ty, od) ->
      if List.mem_assoc v fty.params then
        Scope.set s v (Some (subst_consts_expr genv e))
      else begin
        (* Substitute global constants in types *)
        let subst_consts = new substFunClass (GlobalEnv.getConstant genv) in
        let ety' = Asl_visitor.visit_type subst_consts ety in
        let ty' = Asl_visitor.visit_type subst_consts ty in
        synthesize_type env loc s ety' ty'
      end
    )
    es
    tys
    fty.atys;

  (* Extract values of the parameters from s *)
  let check_parameter (v, oe) =
    let e = from_option oe (fun _ ->
        raise (TypeError (loc, "unable to synthesize type parameter " ^ Ident.to_string v)))
    in
    (v, e)
  in
  Scope.bindings s
    |> List.map check_parameter
    |> mk_bindings

(****************************************************************)
(** {3 Constraints}                                             *)
(****************************************************************)

(** apply a unary constraint transformer to an optional constraint set *)
let lift1_constraints
    (f : constraint_range -> constraint_range)
    (oxs : constraint_range list option)
  : constraint_range list option
  =
  Option.map (fun xs -> simplify_constraints (List.map f xs)) oxs

(** apply a binary constraint transformer to two optional constraint sets *)
let lift2_constraints
    (f : constraint_range -> constraint_range -> constraint_range)
    (oxs : constraint_range list option)
    (oys : constraint_range list option)
  : constraint_range list option
  =
  ( match (oxs, oys) with
  | (Some xs, Some ys) -> Some (simplify_constraints (cross_combine f xs ys))
  | _ -> None
  )

let add_constraints (x : constraint_range) (y : constraint_range) : constraint_range =
  ( match (x, y) with
  | (Constraint_Single x, Constraint_Single y) -> Constraint_Single (mk_add_int x y)
  | (Constraint_Single x, Constraint_Range (yl,yh)) -> Constraint_Range (mk_add_int x yl, mk_add_int x yh)
  | (Constraint_Range (xl,xh), Constraint_Single y) -> Constraint_Range (mk_add_int xl y, mk_add_int xh y)
  | (Constraint_Range (xl,xh), Constraint_Range (yl,yh)) -> Constraint_Range (mk_add_int xl yl, mk_add_int xh yh)
  )

let sub_constraints (x : constraint_range) (y : constraint_range) : constraint_range =
  ( match (x, y) with
  | (Constraint_Single x, Constraint_Single y) -> Constraint_Single (mk_sub_int x y)
  | (Constraint_Single x, Constraint_Range (yl,yh)) -> Constraint_Range (mk_sub_int x yl, mk_sub_int x yh)
  | (Constraint_Range (xl,xh), Constraint_Single y) -> Constraint_Range (mk_sub_int xl y, mk_sub_int xh y)
  | (Constraint_Range (xl,xh), Constraint_Range (yl,yh)) -> Constraint_Range (mk_sub_int xl yh, mk_sub_int xh yl)
  )

let neg_constraints (x : constraint_range) : constraint_range =
  ( match x with
  | Constraint_Single x -> Constraint_Single (mk_neg_int x)
  | Constraint_Range (xl,xh) -> Constraint_Range (mk_neg_int xh, mk_neg_int xl)
  )

let mul_constraints (x : constraint_range) (y : constraint_range) : constraint_range =
  ( match (x, y) with
  | (Constraint_Single x, Constraint_Single y) -> Constraint_Single (mk_mul_int x y)
  | (Constraint_Single x, Constraint_Range (yl,yh)) ->
    let l = mk_min_int (mk_mul_int x yl) (mk_mul_int x yh) in
    let h = mk_max_int (mk_mul_int x yl) (mk_mul_int x yh) in
    Constraint_Range (l, h)
  | (Constraint_Range (xl, xh), Constraint_Single y) ->
    let l = mk_min_int (mk_mul_int y xl) (mk_mul_int y xh) in
    let h = mk_max_int (mk_mul_int y xl) (mk_mul_int y xh) in
    Constraint_Range (l, h)
  | (Constraint_Range (xl, xh), Constraint_Range (yl, yh)) ->
    let l = mk_min_int (mk_min_int (mk_mul_int yl xl) (mk_mul_int yl xh))
                       (mk_min_int (mk_mul_int yh xl) (mk_mul_int yh xh))
    in
    let h = mk_max_int (mk_max_int (mk_mul_int yl xl) (mk_mul_int yl xh))
                       (mk_max_int (mk_mul_int yh xl) (mk_mul_int yh xh))
    in
    Constraint_Range (l, h)
  )

let pow_constraints (x : constraint_range) (y : constraint_range) : constraint_range =
  ( match (x, y) with
  | (Constraint_Single x, Constraint_Single y) -> Constraint_Single (mk_pow_int_int x y)
  | (Constraint_Single x, Constraint_Range (yl,yh)) ->
    let l = mk_pow_int_int x yl in
    let h = mk_pow_int_int x yh in
    Constraint_Range (l, h)
  | (Constraint_Range (xl, xh), Constraint_Single y) ->
    let l = mk_pow_int_int xl y in
    let h = mk_pow_int_int xh y in
    Constraint_Range (l, h)
  | (Constraint_Range (xl, xh), Constraint_Range (yl, yh)) ->
    let l = mk_pow_int_int xl yl in
    let h = mk_pow_int_int xh yh in
    Constraint_Range (l, h)
  )

let exact_div_constraints (x : constraint_range) (y : constraint_range) : constraint_range =
  ( match (x, y) with
  | (Constraint_Single x, Constraint_Single y) -> Constraint_Single (mk_exact_div_int x y)
  | (Constraint_Single x, Constraint_Range (yl,yh)) ->
    let l = mk_exact_div_int x yh in
    let h = mk_exact_div_int x yl in
    Constraint_Range (l, h)
  | (Constraint_Range (xl, xh), Constraint_Single y) ->
    let l = mk_exact_div_int xl y in
    let h = mk_exact_div_int xh y in
    Constraint_Range (l, h)
  | (Constraint_Range (xl, xh), Constraint_Range (yl, yh)) ->
    let l = mk_exact_div_int xl yh in
    let h = mk_exact_div_int xh yl in
    Constraint_Range (l, h)
  )

let max_constraints (x : constraint_range) (y : constraint_range) : constraint_range =
  ( match (x, y) with
  | (Constraint_Single x, Constraint_Single y) -> Constraint_Single (mk_max_int x y)
  | (Constraint_Single x, Constraint_Range (yl,yh)) -> Constraint_Range (mk_max_int x yl, mk_max_int x yh)
  | (Constraint_Range (xl,xh), Constraint_Single y) -> Constraint_Range (mk_max_int xl y, mk_max_int xh y)
  | (Constraint_Range (xl,xh), Constraint_Range (yl,yh)) -> Constraint_Range (mk_max_int xl yl, mk_max_int xh yh)
  )

let min_constraints (x : constraint_range) (y : constraint_range) : constraint_range =
  ( match (x, y) with
  | (Constraint_Single x, Constraint_Single y) -> Constraint_Single (mk_min_int x y)
  | (Constraint_Single x, Constraint_Range (yl,yh)) -> Constraint_Range (mk_min_int x yl, mk_min_int x yh)
  | (Constraint_Range (xl,xh), Constraint_Single y) -> Constraint_Range (mk_min_int xl y, mk_min_int xh y)
  | (Constraint_Range (xl,xh), Constraint_Range (yl,yh)) -> Constraint_Range (mk_min_int xl yl, mk_min_int xh yh)
  )

let mk_unop (op : Ident.t) (tys : AST.expr list) (x : AST.expr) : AST.expr =
  Expr_TApply (op, tys, [x], NoThrow)

(** Construct "pow2_int(x)" *)
let mk_pow2_int (x : AST.expr) : AST.expr =
  ( match x with
  | Expr_Lit (VInt i) -> Asl_utils.mk_litbigint (Primops.prim_pow2_int i)
  | _ -> mk_unop pow2_int [] x
  )

(* Refine the result type of a calculation for known functions *)
let refine_type (fty : funtype) (tys : AST.ty list) : AST.ty option =
  ( match (fty.funname, tys) with
  | (i, [Type_Bits (n, _)]) when Ident.equal i cvt_bits_uint -> Some (Type_Integer (Some [Constraint_Range (zero, mk_sub_int (mk_pow2_int n) one)]))
  | (i, [Type_Bits (n, _)]) when Ident.equal i cvt_bits_sint -> Some (Type_Integer (Some [Constraint_Range (mk_neg_int (mk_pow2_int (mk_sub_int n one)), mk_sub_int (mk_pow2_int (mk_sub_int n one)) one)]))
  | (i, [Type_Integer c1; Type_Integer c2]) when Ident.equal i add_int -> Some (Type_Integer (lift2_constraints add_constraints c1 c2))
  | (i, [Type_Integer c1; Type_Integer c2]) when Ident.equal i sub_int -> Some (Type_Integer (lift2_constraints sub_constraints c1 c2))
  | (i, [Type_Integer c]) when Ident.equal i neg_int -> Some (Type_Integer (lift1_constraints neg_constraints c))
  | (i, [Type_Integer c1; Type_Integer c2]) when Ident.equal i mul_int -> Some (Type_Integer (lift2_constraints mul_constraints c1 c2))
  | (i, [Type_Integer c1; Type_Integer c2]) when Ident.equal i pow_int_int -> Some (Type_Integer (lift2_constraints pow_constraints c1 c2))
  | (i, [Type_Integer c1; Type_Integer c2]) when Ident.equal i exact_div_int -> Some (Type_Integer (lift2_constraints exact_div_constraints c1 c2))
  | (i, [Type_Integer c1; Type_Integer c2]) when Ident.equal i max -> Some (Type_Integer (lift2_constraints max_constraints c1 c2))
  | (i, [Type_Integer c1; Type_Integer c2]) when Ident.equal i min -> Some (Type_Integer (lift2_constraints min_constraints c1 c2))
  | _ -> None
  )

(****************************************************************)
(** {3 Instantiating/typechecking application of functions/operators} *)
(****************************************************************)

(** Instantiating a function type produces the following information *)
type fun_instance =
  { name       : Ident.t;
    parameters : AST.expr list;
    ovty       : AST.ty option; (* type of setter rhs *)
    rty        : AST.ty;
  }

(** Instantiate type of function *)
let instantiate_fun (env : Env.t) (loc : Loc.t)
    (fty : funtype) (es : AST.expr list) (tys : AST.ty list) : fun_instance =
  (* Format.printf "%a: Synthesizing parameters for call to %a\n" FMT.loc loc FMT.varname fty.funname; *)
  let bs = synthesize_parameters env loc fty es tys in

  (* Check each argument *)
  List.iter2 (fun actual_ty (v, aty, od) ->
    let aty' = subst_type bs aty in
    (* Format.printf "%a: Argument type %a -> %a\n" FMT.loc loc FMT.ty aty FMT.ty aty'; *)
    check_subtype_satisfies env loc actual_ty aty'
    )
    tys
    fty.atys;

  (* Construct result *)
  let parameters = List.map (fun (p, _) -> Bindings.find p bs) fty.params in
  let ovty = Option.map (subst_type bs) fty.ovty in
  let rty1 = subst_type bs fty.rty in
  (* Possible refined type based on constraints *)
  let rty2 = refine_type fty tys in
  let rty = Option.value rty2 ~default:rty1 in
  { name = fty.funname; parameters; ovty; rty }


(****************************************************************)
(** {3 Disambiguation of functions and operators}               *)
(*                                                              *)
(* Note that disambiguation of getter/setter functions is       *)
(* scattered through the LExpr/Expr-handling code. Search for   *)
(* calls to 'chooseFunction' to find that code                  *)
(****************************************************************)

(** structural match on two types - ignoring the dependent type part and constraints
 *
 *  This is called "type-clashing" in ASL 1.0
 *)
let rec eq_structural (env : GlobalEnv.t) (loc : Loc.t) (ty1 : AST.ty) (ty2 : AST.ty) : bool =
  match (derefType env loc ty1, derefType env loc ty2) with
  | Type_Integer _, Type_Integer _ -> true
  | Type_Bits (e1, _), Type_Bits (e2, _) -> true
  | Type_Constructor (c1, es1), Type_Constructor (c2, es2) -> c1 = c2
  | Type_OfExpr e1, Type_OfExpr e2 -> raise (InternalError
      (loc, "eq_structural: typeof", (fun fmt -> FMT.ty fmt ty1), __LOC__))
  | Type_Array (ixty1, elty1), Type_Array (ixty2, elty2) ->
      ( match (ixty1, ixty2) with
      | Index_Enum tc1, Index_Enum tc2 -> tc1 = tc2
      | Index_Int _, Index_Int _ -> true
      | _ -> false
      )
  | Type_Tuple tys1, Type_Tuple tys2 ->
      List.length tys1 = List.length tys2
      && List.for_all2 (eq_structural env loc) tys1 tys2
  | _ -> false

(** Generate error message when function disambiguation fails *)
let reportMatchChoices (loc : Loc.t) (what : string) (nm : string)
    (args : ((Ident.t option * AST.expr) * AST.ty) list)
    (funs : funtype list)
  : unit
  =
  let ppArg ((ov, e), ty) =
      ( match ov with
      | Some v -> Format.fprintf fmt "%a: %a" Ident.pp v FMT.ty ty
      | None   -> FMT.ty fmt ty
      )
  in
  FMT.loc fmt loc;
  FMT.colon fmt;
  FMTUtils.nbsp fmt;
  Format.fprintf fmt "While typechecking a call to '%s' with arguments of type@," nm;
  Format.fprintf fmt "  %s(" nm;
  FMTUtils.commasep fmt ppArg args;
  Format.fprintf fmt ")\n";
  ( match funs with
  | [] ->
      Format.fprintf fmt "No matching %s found@," what;
  | [f] ->
      Format.fprintf fmt "Type error in %s arguments for call to '%s'@," what nm;
      Format.fprintf fmt "  %a@," pp_funtype f
  | _ ->
      Format.fprintf fmt "Unable to decide which of the following definitions of '%s' to call@," nm;
      List.iter (Format.fprintf fmt "  %a@," pp_funtype) funs
  );
  FMTUtils.flush fmt

(** Match a list of function arguments against a function definition.
    This resolves all use of named arguments and default arguments but it
    does not typecheck the arguments.

    Matching proceeds by first building the 'matches' binding from argument
    names to any explicit arguments.

    Then the result is converted to a flat list of arguments and any default
    arguments are inserted.
 *)
let matchFunction (env : GlobalEnv.t) (loc : Loc.t)
    (args : ((Ident.t option * AST.expr) * AST.ty) list)
    (fty : funtype)
  : (AST.expr * AST.ty) list
  =
  let matches = ref Bindings.empty in (* accumulator for arguments *)
  let arg_names = List.map (fun (v,ty,e) -> v) fty.atys in
  let seen_named_arg = ref false in
  List.iteri (fun i arg ->
      ( match arg with
      | ((None, e), ty) ->
              if !seen_named_arg then begin
                  let msg = Format.asprintf "positional argument occurs after a named argument" in
                  raise (TypeError (loc, msg))
              end;
              if i >= List.length fty.atys then begin
                  let msg = Format.asprintf "too many arguments for function '%a' (%d expected)"
                               Ident.pp_untagged fty.funname
                               (List.length fty.atys)
                  in
                  raise (TypeError (loc, msg))
              end;
              let (arg_name, _, _) = List.nth fty.atys i in
              matches := Bindings.add arg_name (e, ty) !matches
      | ((Some v, e), ty) ->
              seen_named_arg := true;
              if Bindings.mem v !matches then begin
                  let msg = Format.asprintf "multiple bindings for argument '%a'" Ident.pp v in
                  raise (TypeError (loc, msg))
              end;
              if not (List.mem v arg_names) then begin
                  let msg = Format.asprintf "named argument '%a' does not match any argument of function '%a(%a)'"
                              Ident.pp v
                              Ident.pp_untagged fty.funname
                              FMT.varnames arg_names
                  in
                  raise (TypeError (loc, msg))
              end;
              matches := Bindings.add v (e, ty) !matches
      )
  ) args;

  (* Convert arguments to a flat list and insert default arguments *)
  let defaults = List.map (fun (v,ty,e) -> (v, (e, ty))) fty.atys in
  let args = List.map (fun v ->
      ( match Bindings.find_opt v !matches with
      | Some (e, ty) -> (e, ty)
      | None -> (* no argument provided - check for default argument *)
          ( match List.assoc_opt v defaults with
          | Some (Some e, ty) -> (e, ty)
          | _ ->
              let msg = Format.asprintf "missing argument '%a'"
                           Ident.pp v
              in
              raise (TypeError (loc, msg))
          )
      )
  ) arg_names
  in
  args


(** Disambiguate and typecheck application of a function to a list of arguments
 *  checking named arguments and applying default arguments as applicable.
 *)
let tc_apply (env : Env.t) (loc : Loc.t) (what : string)
    (f : Ident.t)
    (args : ((Ident.t option * AST.expr) * AST.ty) list)
  : (fun_instance * AST.expr list)
  =
  let genv = Env.globals env in
  let funs = GlobalEnv.getFuns genv f in
  let nm = Ident.to_string f in
  ( match funs with
  | [] ->
      raise (UnknownObject (loc, what, nm))
  | _ ->
      let isCompatible (fty, args) =
            List.for_all2 (eq_structural genv loc)
                          (List.map (fun (nm, ty, od) -> ty) fty.atys)
                          (List.map snd args)
      in
      let matches = List.map (fun f -> (f, matchFunction genv loc args f)) funs in
      let matches' = List.filter isCompatible matches in
      ( match matches' with
      | [] ->
          reportMatchChoices loc what nm args (List.map fst matches);
          raise (TypeError (loc, "function arguments"))
      | [(fty, args)] ->
          let (es, tys) = List.split args in
          (instantiate_fun env loc fty es tys, es)
      | _ ->
          reportMatchChoices loc what nm args (List.map fst matches);
          raise (Ambiguous (loc, what, nm))
      )
  )

(** Generate error message when function disambiguation fails *)
let reportChoices (loc : Loc.t) (what : string) (nm : string)
    (tys : AST.ty list) (funs : funtype list) : unit =
  FMT.loc fmt loc;
  FMT.colon fmt;
  FMTUtils.nbsp fmt;
  let error_message = match funs with
      | [] -> "Can't find matching " ^ what ^ " for " ^ nm
      | [f] -> "Type error in " ^ what ^ " arguments for " ^ nm
      | _ -> "Ambiguous choice for " ^ what ^ " " ^ nm
  in
  Format.pp_print_string fmt error_message;
  FMTUtils.cut fmt;
  FMTUtils.vbox fmt (fun _ ->
      Format.fprintf fmt "Function arguments with types (%a) do not match@," FMT.types tys;
      List.iter (Format.fprintf fmt "  %a@," pp_funtype) funs
    );
  FMTUtils.flush fmt

(** Check whether a list of function argument types is compatible with the
    type of a function.

    One function type is compatible with another if they have the same number
    of arguments and each argument has the same base type
 *)
let isCompatibleFunction (env : GlobalEnv.t) (loc : Loc.t) (isArr : bool) (tys : AST.ty list)
    (fty : funtype) : bool =
  let nargs = List.length tys in
  isArr = fty.isArray
  && List.length fty.atys = nargs
  && List.for_all2 (eq_structural env loc) (List.map (fun (nm, ty, od) -> ty) fty.atys) tys

(** Disambiguate a function name based on the number and type of arguments *)
let chooseFunction (env : GlobalEnv.t) (loc : Loc.t) (what : string)
    (nm : string) (isArr : bool) (tys : AST.ty list) (funs : funtype list) :
    funtype option =
  let funs' = List.filter (isCompatibleFunction env loc isArr tys) funs in
  match nub funs' with
  | [] -> None
  | [ r ] -> Some r
  | fs ->
      reportChoices loc what nm tys fs;
      raise (Ambiguous (loc, what, nm))

let check_duplicate_field_names (fx : 'a -> Ident.t) (fs : 'a list) (loc : Loc.t) =
  let fieldnames = ref IdentSet.empty in
  List.iter (fun f ->
      let f' = fx f in
      if IdentSet.mem f' !fieldnames then begin
        let msg = Format.asprintf "fieldname `%a` is declared multiple times"
            FMT.fieldname f'
        in
        raise (TypeError (loc, msg))
      end;
      fieldnames := IdentSet.add f' !fieldnames
    )
    fs

(** Disambiguate and typecheck application of a unary operator to argument *)
let tc_unop (env : Env.t) (loc : Loc.t) (op : unop)
    (x : AST.expr) (ty : AST.ty) : fun_instance =
  let genv = Env.globals env in
  let what = "unary operator" in
  let nm = pp_unop op in
  let tys = [ ty ] in
  let ops = GlobalEnv.getOperators1 genv loc op in
  match chooseFunction genv loc what nm false [ ty ] ops with
  | None ->
      reportChoices loc what nm tys ops;
      raise (UnknownObject (loc, what, nm))
  | Some fty ->
      instantiate_fun env loc fty [ x ] tys

(** Disambiguate and typecheck application of a binary operator to arguments *)
let tc_binop (env : Env.t) (loc : Loc.t) (op : binop)
    (x1 : AST.expr) (x2 : AST.expr) (ty1 : AST.ty) (ty2 : AST.ty) :
    fun_instance =
  let genv = Env.globals env in
  let what = "binary operator" in
  let nm = pp_binop op in
  let tys = [ ty1; ty2 ] in
  let ops = GlobalEnv.getOperators2 genv loc op in
  match chooseFunction genv loc what nm false tys ops with
  | None ->
      reportChoices loc "binary operator" nm tys ops;
      raise (UnknownObject (loc, what, nm))
  | Some fty ->
      instantiate_fun env loc fty [ x1; x2 ] tys

(****************************************************************)
(** {2 Typecheck expressions}                                   *)
(****************************************************************)

(** Lookup a variable in environment *)
let get_var (env : Env.t) (loc : Loc.t) (v : Ident.t) : var_info =
  from_option (Env.getVar env v) (fun _ -> raise (UnknownObject (loc, "variable", Ident.to_string v)))

(** check that we have exactly the fields required *)
let check_field_assignments (loc : Loc.t) (fs : (Ident.t * ty) list) (fas : (Ident.t * expr) list) : unit =
  let fields1 = List.map fst fs in
  let fields2 = List.map fst fas in
  let expected = IdentSet.of_list fields1 in
  let assigned = IdentSet.of_list fields2 in
  if not (IdentSet.equal assigned expected) then begin
    let missing = IdentSet.elements (IdentSet.diff expected assigned) in
    let extra = IdentSet.elements (IdentSet.diff assigned expected) in
    let msg = "record initializer is missing field[s] "
      ^ String.concat ", " (List.map Ident.to_string missing)
      ^ " and/or has extra field[s] "
      ^ String.concat ", " (List.map Ident.to_string extra)
    in
    raise (TypeError (loc, msg))
  end else if not (List.for_all2 Ident.equal fields1 fields2) then begin
    let msg = "record initializer must set fields in the same order as the type declaration."
      ^ "\nOrder of fields in type declaration: " ^ String.concat ", " (List.map Ident.to_string fields1)
      ^ "\nOrder of fields in record initializer: " ^ String.concat ", " (List.map Ident.to_string fields2)
    in
    raise (TypeError (loc, msg))
  end


(** Typecheck list of expressions *)
let rec tc_exprs (env : Env.t) (loc : Loc.t) (xs : AST.expr list)
    : (AST.expr * AST.ty) list =
  List.map (tc_expr env loc) xs

(** Typecheck expression and check that it is a subtype of ty *)
and check_expr (env : Env.t) (loc : Loc.t) (ty : AST.ty) (x : AST.expr) :
    AST.expr =
  let (x', ty') = tc_expr env loc x in
  if verbose then
    Format.fprintf fmt "        - Typechecking %a : %a\n" FMT.expr x' FMT.ty ty';
  check_subtype_satisfies env loc ty' ty;
  x'

and tc_args (env : Env.t) (loc : Loc.t) (xs : (Ident.t option * AST.expr) list) : ((Ident.t option * AST.expr) * AST.ty) list =
  List.map (tc_arg env loc) xs

and tc_arg (env : Env.t) (loc : Loc.t) (x : (Ident.t option * AST.expr)) : ((Ident.t option * AST.expr) * AST.ty) =
  let (nm, e) = x in
  let (e', ty) = tc_expr env loc e in
  ((nm, e'), ty)

(** Typecheck 'if c then expr' *)
and tc_e_elsif (env : Env.t) (loc : Loc.t) (x : AST.e_elsif) :
    AST.e_elsif * AST.ty =
  let (c, e) = x in
  let c' = check_expr env loc type_bool c in
  let e', ty = tc_expr env loc e in
  ((c', e'), ty)

(** Typecheck bitslice indices and insert runtime check *)
and tc_slice (env : Env.t) (loc : Loc.t) (x : AST.slice) :
    AST.slice * AST.ty =
  match x with
  | Slice_Single e ->
      let e', ty = tc_expr env loc e in
      (Slice_Single e', ty)
  | Slice_HiLo (hi, lo) ->
      let hi' = check_expr env loc type_integer hi in
      let lo' = check_expr env loc type_integer lo in
      (Slice_HiLo (hi', lo'), type_integer)
  | Slice_LoWd (lo, wd) ->
      let lo' = check_expr env loc type_integer lo in
      let wd' = check_expr env loc type_integer wd in
      (Slice_LoWd (lo', wd'), type_integer)
  | Slice_HiWd (hi, wd) ->
      let hi' = check_expr env loc type_integer hi in
      let wd' = check_expr env loc type_integer wd in
      (Slice_HiWd (hi', wd'), type_integer)
  | Slice_Element (lo, wd) ->
      let lo' = check_expr env loc type_integer lo in
      let wd' = check_expr env loc type_integer wd in
      (Slice_Element (lo', wd'), type_integer)

(** Typecheck pattern against type ty *)
and tc_pattern (env : Env.t) (loc : Loc.t) (ty : AST.ty) (x : AST.pattern) :
    AST.pattern =
  match x with
  | Pat_Lit (VInt _)  -> check_subtype_satisfies env loc ty type_integer; x
  | Pat_Lit (VIntN v)  -> check_subtype_satisfies env loc ty (type_sintN (mk_litint v.n)); x
  | Pat_Lit (VReal _) -> check_subtype_satisfies env loc ty type_real; x
  | Pat_Lit (VBits b) -> check_subtype_satisfies env loc ty (type_bits (mk_litint (Primops.prim_length_bits b))); x
  | Pat_Lit (VMask m) -> check_subtype_satisfies env loc ty (type_bits (mk_litint (Primops.prim_length_mask m))); x
  | Pat_Lit _ ->
      raise (InternalError
        (loc, "tc_pattern: lit", (fun fmt -> FMT.pattern fmt x), __LOC__))
  | Pat_Const c ->
      let i = get_var env loc c in
      if i.is_local || not i.is_constant then begin
        let msg = Format.asprintf "pattern match of `%a` should be a constant. (Variable was declared at `%a`.)"
                    FMT.varname i.name
                    FMT.loc i.loc
        in
        raise (TypeError (loc, msg))
      end;
      check_subtype_satisfies env loc ty i.ty;
      Pat_Const c
  | Pat_Wildcard -> Pat_Wildcard
  | Pat_Tuple ps ->
      let ps' =
        match ty with
        | Type_Tuple tys when List.length ps = List.length tys ->
            List.map2 (tc_pattern env loc) tys ps
        | _ -> raise (IsNotA (loc, "tuple of length ?", pp_type ty))
      in
      Pat_Tuple ps'
  | Pat_Set ps ->
      let ps' = List.map (tc_pattern env loc ty) ps in
      Pat_Set ps'
  | Pat_Single (Expr_Lit (VMask m as v)) ->
      (* todo: this is a workaround for bad IN sugar *)
      tc_pattern env loc ty (Pat_Lit v)
  | Pat_Single e ->
      let (e', ety) = tc_expr env loc e in
      ignore (least_supertype env loc ty ety);
      Pat_Single e'
  | Pat_Range (lo, hi) ->
      (* Must be integer because no other type supports <= operator *)
      let lo' = check_expr env loc type_integer lo in
      let hi' = check_expr env loc type_integer hi in
      check_subtype_satisfies env loc ty type_integer;
      Pat_Range (lo', hi')

(** Typecheck bitslice syntax
    This primarily consists of disambiguating between array indexing and bitslicing
    Note that this function is almost identical to tc_slice_lexpr
 *)
and tc_slice_expr (env : Env.t) (loc : Loc.t) (x : expr)
    (ss : (AST.slice * AST.ty) list) : AST.expr * AST.ty =
  if List.length ss = 0 then raise (TypeError (loc, "empty list of subscripts"));
  let ss' = List.map fst ss in
  let x', ty' = tc_expr env loc x in
  let wd = slices_width ss' in
  let ty = type_bits wd in
  match derefType (Env.globals env) loc ty' with
  | Type_Array (ixty, elty) -> (
      match ss with
      | [ (Slice_Single i, ity) ] ->
          check_subtype_satisfies env loc ity (ixtype_basetype ixty);
          let lets = ref [] in
          let asserts = ref [] in
          let i' = mk_index_check loc lets asserts ixty i in
          let x' =
              (Expr_Array (x', i'))
              |> mk_assert_exprs !asserts
              |> mk_let_exprs !lets
          in
          (x', elty)
      | _ -> raise (TypeError (loc, "multiple subscripts for array")))
  | Type_Bits (size, _) ->
      let lets = ref [] in
      let asserts = ref [] in
      let ss'' = List.map (mk_slice_check loc lets asserts size) ss' in
      let x'' =
          (Expr_Slices (ty', x', ss''))
          |> mk_assert_exprs !asserts
          |> mk_let_exprs !lets
      in
      (x'', ty)
  | Type_Integer _ ->
      (Expr_Slices (ty', x', ss'), ty)
  | _ -> raise (TypeError (loc, "slice of expr"))

(** Typecheck expression *)
and tc_expr (env : Env.t) (loc : Loc.t) (x : AST.expr) : AST.expr * AST.ty =
  ( match x with
  | Expr_If (els, e) ->
      let (els', eltys) = List.split (List.map (tc_e_elsif env loc) els) in
      let (e', ety) = tc_expr env loc e in
      let ty = List.fold_left (least_supertype env loc) ety eltys in
      (Expr_If (els', e'), ty)
  | Expr_Let (v, t, e, b) ->
      Env.nest (fun env' ->
          let t' = tc_type env' loc t in
          let e' = check_expr env' loc t' e in
          if t' = type_integer then begin
            Env.addConstraint env loc (mk_eq_int (Expr_Var v) e');
          end;
          Env.addLocalVar env' {name=v; loc; ty=t'; is_local=true; is_constant=true};
          let (b', bty') = tc_expr env' loc b in
          (Expr_Let (v, t', e', b'), bty')
        )
        env
  | Expr_Assert (e1, e2, loc) ->
      let e1' = check_expr env loc type_bool e1 in
      let (e2', ty') = tc_expr env loc e2 in
      (Expr_Assert (e1', e2', loc), ty')
  | Expr_Binop (x, Binop_Eq, Expr_Lit (VMask _ as m)) ->
      (* syntactic sugar *)
      tc_expr env loc (Expr_In (x, Pat_Lit m))
  | Expr_Binop (x, Binop_NtEq, Expr_Lit (VMask _ as m)) ->
      (* syntactic sugar *)
      tc_expr env loc (Expr_Unop (Unop_BoolNot, Expr_In (x, Pat_Lit m)))
  | Expr_Binop (x, op, y) ->
      let x', xty = tc_expr env loc x in
      let y', yty = tc_expr env loc y in
      let fty = tc_binop env loc op x' y' xty yty in
      let lets = ref [] in
      let asserts = ref [] in
      let r = if Ident.in_list fty.name [fdiv_int; frem_int; zdiv_int; zrem_int] then
              let y'' = mk_zero_check loc lets asserts y' in
              Expr_TApply (fty.name, fty.parameters, [x'; y''], NoThrow)
          else if Ident.equal fty.name Builtin_idents.exact_div_int then
              let (x'', y'') = mk_exactdiv_check loc lets asserts x' y' in
              Expr_TApply (fty.name, fty.parameters, [x''; y''], NoThrow)
          else
              Expr_TApply (fty.name, fty.parameters, [x'; y'], NoThrow)
      in
      let r' = r
              |> mk_assert_exprs !asserts
              |> mk_let_exprs !lets
      in
      (r', fty.rty)
  | Expr_Field (e, f) ->
      let e', ty = tc_expr env loc e in
      ( match typeFields (Env.globals env) loc ty with
      | FT_Record rfs -> (Expr_Field (e', f), get_recordfield loc rfs f)
      | FT_Register rfs ->
          let ss, ty' = get_regfield loc rfs f in
          (Expr_Slices (ty, e', ss), ty')
      )
  | Expr_Fields (e, fs) ->
      let e', ty = tc_expr env loc e in
      ( match typeFields (Env.globals env) loc ty with
      | FT_Record rfs ->
          let tys = List.map (get_recordfield loc rfs) fs in
          let ws = List.map (width_of_type (Env.globals env) loc) tys in
          let w = Xform_simplify_expr.mk_add_ints ws in
          (Expr_Fields (e', fs), type_bits w)
      | FT_Register rfs ->
          let ss, ty' = get_regfields loc rfs fs in
          (Expr_Slices (ty, e', ss), ty')
      )
  | Expr_Slices (_, e, ss) -> (
      let all_single =
        List.for_all (function Slice_Single _ -> true | _ -> false) ss
      in
      let ss' = List.map (tc_slice env loc) ss in

      (* Note that the order of the following check is critical:
       * First check for getter functions then check for arrays or bitvectors because
       * of conflicting names like SPSR and SPSR[] in the v8-A specification.
       *)

      (* variable slice or getter call? *)
      ( match e with
      | Expr_Var a -> (
          let tys = List.map (function _, ty -> ty) ss' in
          let getters =
            GlobalEnv.getFuns (Env.globals env) (Ident.add_suffix a ~suffix:"read")
          in
          let ogetters =
            chooseFunction (Env.globals env) loc "getter function"
              (Ident.to_string a) true tys getters
          in
          match ogetters with
          | Some fty when all_single ->
              let es =
                List.map
                  (function
                    | Slice_Single a, _ -> a
                    | _ -> raise (InternalError
                      (loc, "Expr_Slices", (fun fmt -> FMT.expr fmt x), __LOC__)))
                  ss'
              in
              let fty' = instantiate_fun env loc fty es tys in
              (Expr_TApply (fty'.name, fty'.parameters, es, NoThrow), fty'.rty)
          | _ -> tc_slice_expr env loc e ss')
      | _ -> tc_slice_expr env loc e ss'
      )
    )
  | Expr_WithChanges (_, e, cs) ->
      let lets = ref [] in
      let asserts = ref [] in
      let (e', ty) = tc_expr env loc e in
      let ty' = derefType (Env.globals env) loc ty in
      let cs' = List.map (fun (c, e2) ->
          let (c', cty) =
              ( match c with
              | Change_Field f ->
                  ( match typeFields (Env.globals env) loc ty' with
                  | FT_Record rfs ->
                      (Change_Field f, get_recordfield loc rfs f)
                  | FT_Register rfs ->
                      let (ss, sty) = get_regfield loc rfs f in
                      (Change_Slices ss, sty)
                  )
              | Change_Slices ss ->
                  ( match ty' with
                  | Type_Bits (n, _) ->
                      let ss' = List.map (tc_slice env loc) ss in
                      let ss'' = List.map (fun (s, _) -> mk_slice_check loc lets asserts n s) ss' in
                      let wd = slices_width ss'' in
                      let sty = type_bits wd in
                      (Change_Slices ss'', sty)
                  | _ -> raise (TypeError (loc, "slice changes"))
                  )
              )
          in
          let e2' = check_expr env loc cty e2 in
          (c', e2')
      ) cs
      in
      let r = Expr_WithChanges (ty', e', cs')
              |> mk_assert_exprs !asserts
              |> mk_let_exprs !lets
      in
      (r, ty')
  | Expr_Record (tc, args, fas) ->
      if not (GlobalEnv.isType (Env.globals env) tc) then
        raise (IsNotA (loc, "type constructor", Ident.to_string tc));
      let (ps, fs) =
        match GlobalEnv.getType (Env.globals env) tc with
        | Some (Type_Record (ps, fs)) -> (ps, fs)
        | Some (Type_Exception fs) -> ([], fs)
        | _ -> raise (IsNotA (loc, "record or exception type", Ident.to_string tc))
      in
      if List.length args <> List.length ps then
        raise (TypeError (loc, "wrong number of type parameters"));
      check_field_assignments loc fs fas;

      (* add values of type parameters to environment *)
      let args' = List.map (fun (nm, e) -> (nm, check_expr env loc type_integer e)) args in
      let es' = List.map snd args' in
      let s = mk_bindings (List.combine ps es') in

      (* typecheck each field of the record *)
      let fas' =
        List.map
          (fun (f, e) ->
            let fty = get_recordfield loc fs f in
            let fty' = subst_type s fty in
            let e' = check_expr env loc fty' e in
            (f, e'))
          fas
      in

      (Expr_Record (tc, args', fas'), Type_Constructor (tc, es'))
  | Expr_ArrayInit [] ->
      raise (InternalError (loc, "expr ArrayInit is empty", (fun fmt -> FMT.expr fmt x), __LOC__))
  | Expr_ArrayInit (e::es) ->
      let (e', ty) = tc_expr env loc e in
      let rty = ref ty in
      let es' = List.map
          (fun i ->
            let (i', ity) = tc_expr env loc i in
            rty := least_supertype env loc !rty ity;
            i')
          es
      in
      let n = List.length (e::es) in
      let ixty = Index_Int (mk_litint n) in
      (Expr_ArrayInit (e'::es'), Type_Array (ixty, !rty))
  | Expr_In (e, p) ->
      let (e', ety') = tc_expr env loc e in
      if verbose then
        Format.fprintf fmt "    - Typechecking %a IN ... : %a\n" FMT.expr e' FMT.ty ety';
      let p' = tc_pattern env loc ety' p in
      (Expr_In (e', p'), type_bool)
  | Expr_Var v ->
      ( match Env.getVar env v with
      | Some i -> (Expr_Var i.name, i.ty)
      | None ->
        ( match GlobalEnv.getType (Env.globals env) v with
        | Some (Type_Exception fs)
        | Some (Type_Record ([], fs)) ->
          if not (Utils.is_empty fs) then begin
            let msg = Format.asprintf "record/exception `%a` requires field values but none are supplied"
                FMT.varname v
            in
            raise (TypeError (loc, msg))
          end;
          (Expr_Record (v, [], []), Type_Constructor (v, []))
        | Some _ ->
          raise (IsNotA (loc, "record or exception type", Ident.to_string v));
        | None ->
          let getters =
            GlobalEnv.getFuns (Env.globals env) (Ident.add_suffix v ~suffix:"read")
          in
          match
            chooseFunction (Env.globals env) loc "getter function"
              (Ident.to_string v) false [] getters
          with
          | Some fty ->
              let fty' = instantiate_fun env loc fty [] [] in
              (Expr_TApply (fty'.name, fty'.parameters, [], NoThrow), fty'.rty)
          | None ->
              raise
                (UnknownObject
                   (loc, "variable or getter functions", Ident.to_string v))
        )
      )
  | Expr_UApply (f, args, throws) ->
      let args' = tc_args env loc args in
      let (fty, es) = tc_apply env loc "function" f args' in
      (Expr_TApply (fty.name, fty.parameters, es, throws), fty.rty)
  | Expr_Tuple es ->
      let es', tys = List.split (List.map (tc_expr env loc) es) in
      (Expr_Tuple es', Type_Tuple tys)
  | Expr_Concat (_, es) ->
      let es', tys = List.split (List.map (tc_expr env loc) es) in
      let ws = List.map (width_of_type (Env.globals env) loc) tys in
      let w = Xform_simplify_expr.mk_add_ints ws in
      (Expr_Concat (ws, es'), type_bits w)
  | Expr_Unop (op, e) ->
      let e', ety = tc_expr env loc e in
      (* Format.fprintf fmt "%a: unop %a : %a\n" FMT.loc loc FMT.expr e FMT.ty ety; *)
      let fty = tc_unop env loc op e ety in
      (Expr_TApply (fty.name, fty.parameters, [ e' ], NoThrow), fty.rty)
  | Expr_Unknown t ->
      let ty' = tc_type env loc t in
      (Expr_Unknown ty', ty')
  | Expr_Array (a, e) -> (
      let a', ty = tc_expr env loc a in
      match derefType (Env.globals env) loc ty with
      | Type_Array (ixty, elty) ->
          let e' = check_expr env loc (ixtype_basetype ixty) e in
          let lets = ref [] in
          let asserts = ref [] in
          let e'' = mk_index_check loc lets asserts ixty e' in
          let x' =
              (Expr_Array (a', e''))
              |> mk_assert_exprs !asserts
              |> mk_let_exprs !lets
          in
          (x', elty)
      | _ -> raise (TypeError (loc, "subscript of non-array")))
  | Expr_Lit (VInt _)    -> (x, Type_Integer (Some([Constraint_Single x])))
  | Expr_Lit (VIntN v)   -> (x, type_sintN (mk_litint v.n))
  | Expr_Lit (VReal _)   -> (x, type_real)
  | Expr_Lit (VBits b)   -> (x, type_bits (mk_litint (Primops.prim_length_bits b)))
  | Expr_Lit (VString _) -> (x, type_string)
  | Expr_Lit _ ->
      raise (InternalError
        (loc, "tc_expr: lit", (fun fmt -> FMT.expr fmt x), __LOC__))
  | Expr_AsConstraint (e, c) ->
      let e' = check_expr env loc type_integer e in
      let c' = tc_constraints env loc c in
      let ty = Type_Integer (Some c') in
      let lets = ref [] in
      let asserts = ref [] in
      let e'' = mk_constraint_check loc lets asserts c' e' in
      let x' =
             (Expr_AsConstraint (e'', c'))
             |> mk_assert_exprs !asserts
             |> mk_let_exprs !lets
      in
      (x', ty)
  | Expr_AsType (e, t) ->
      let e', ty = tc_expr env loc e in
      let t' = tc_type env loc t in
      (* todo: check that ty is structurally consistent with t' *)
      let lets = ref [] in
      let asserts = ref [] in
      let e'' = mk_type_check loc lets asserts (derefType (Env.globals env) loc t') e' in
      let x' =
             (Expr_AsType (e'', t'))
             |> mk_assert_exprs !asserts
             |> mk_let_exprs !lets
      in
      (x', t')
  | Expr_TApply _ ->
      raise (InternalError (loc, "Unexpected expression type in typechecker", (fun fmt -> FMT.expr fmt x), __LOC__))
  )


(** Typecheck list of types *)
and tc_types (env : Env.t) (loc : Loc.t) (xs : AST.ty list) : AST.ty list =
  List.map (tc_type env loc) xs

(** Typecheck type *)
and tc_type (env : Env.t) (loc : Loc.t) (x : AST.ty) : AST.ty =
  match x with
  | Type_Integer ocrs ->
      let ocrs' = Option.map (tc_constraints env loc) ocrs in
      Type_Integer ocrs'
  | Type_Bits (n, fs) ->
      let n' = check_expr env loc type_integer n in
      check_duplicate_field_names (fun (_, f) -> f) fs loc;
      let fs' =
        List.map
          (fun (ss, f) ->
            let ss' = List.map (fun s -> fst (tc_slice env loc s)) ss in
            (ss', f))
          fs
      in
      Type_Bits (n', fs')
  | Type_Constructor (tc, es) ->
      let es' = List.map (check_expr env loc type_integer) es in
      if (tc = Builtin_idents.sintN) || (GlobalEnv.isTycon (Env.globals env) tc) then (
        Type_Constructor (tc, es')
      ) else (
        raise (IsNotA (loc, "type constructor", Ident.to_string tc))
      )
  | Type_OfExpr e ->
      let (_, ty) = tc_expr env loc e in
      ty
  | Type_Array (Index_Enum tc, ety) ->
      if not (GlobalEnv.isEnum (Env.globals env) tc) then
        raise (IsNotA (loc, "enumeration type", Ident.to_string tc));
      let ety' = tc_type env loc ety in
      Type_Array (Index_Enum tc, ety')
  | Type_Array (Index_Int sz, ety) ->
      let sz' = check_expr env loc type_integer sz in
      let ety' = tc_type env loc ety in
      Type_Array (Index_Int sz', ety')
  | Type_Tuple tys ->
      let tys' = tc_types env loc tys in
      Type_Tuple tys'

(* check ty and check that ty1 is a subtype of ty. *)
and check_type (env : Env.t) (loc : Loc.t) (ty1 : ty) (ty : ty) : ty =
  let ty' = tc_type env loc ty in
  check_subtype_satisfies env loc ty1 ty';
  ty'

and tc_constraint (env : Env.t) (loc : Loc.t) (c : AST.constraint_range) :
    AST.constraint_range =
  match c with
  | Constraint_Single e ->
      let e' = check_expr env loc type_integer e in
      Constraint_Single e'
  | Constraint_Range (lo, hi) ->
      let lo' = check_expr env loc type_integer lo in
      let hi' = check_expr env loc type_integer hi in
      Constraint_Range (lo', hi')

and tc_constraints (env : Env.t) (loc : Loc.t) (cs : AST.constraint_range list)
    : AST.constraint_range list =
  List.map (tc_constraint env loc) cs

(****************************************************************)
(** {2 Typecheck L-expressions}                                 *)
(****************************************************************)

(** Typecheck bitslice syntax

    This primarily consists of disambiguating between array indexing and bitslicing
    Note that this function is almost identical to tc_slice_expr

    Insertion of runtime checking code may write additional bindings and assertions
    to 'lets' and 'asserts'.
 *)
let rec tc_slice_lexpr (env : Env.t) (loc : Loc.t)
    (lets : binding list ref) (asserts : check list ref)
    (x : lexpr) (ss : (AST.slice * AST.ty) list)
    : AST.lexpr * AST.ty =
  if List.length ss = 0 then raise (TypeError (loc, "empty list of subscripts"));
  let ss' = List.map fst ss in
  let x', ty' = tc_lexpr2 env loc lets asserts x in
  let ty = type_bits (slices_width ss') in
  let ty'' = derefType (Env.globals env) loc ty' in
  ( match ty'' with
  | Type_Array (ixty, elty) -> (
      match ss with
      | [ (Slice_Single i, ity) ] ->
          check_subtype_satisfies env loc ity (ixtype_basetype ixty);
          let i' = mk_index_check loc lets asserts ixty i in
          (LExpr_Array (x', i'), elty)
      | _ -> raise (TypeError (loc, "multiple subscripts for array")))
  | Type_Bits (n, _) ->
    let ss'' = List.map (mk_slice_check loc lets asserts n) ss' in
    (LExpr_Slices (ty'', x', ss''), ty)
  | Type_Integer _ ->
      (* There is an argument for making this operation illegal *)
      if false then
        Format.fprintf fmt "Warning: slice assignment of integer at %a\n"
          FMT.loc loc;
      (LExpr_Slices (ty'', x', ss'), ty)
  | _ -> raise (TypeError (loc, "slice of lexpr"))
  )

(** Typecheck left hand side of expression in context where
    type of right hand side is not yet known
    Insertion of runtime checking code may write additional bindings and assertions
    to 'lets' and 'asserts'.
 *)
and tc_lexpr2 (env : Env.t) (loc : Loc.t)
    (lets : binding list ref) (asserts : check list ref)
    (x : AST.lexpr)
    : AST.lexpr * AST.ty =
  match x with
  | LExpr_Wildcard -> raise (TypeError (loc, "wildcard in lexpr2"))
  | LExpr_Var v -> (
      match Env.getVar env v with
      | Some i ->
          if i.is_constant then begin
            let msg = Format.asprintf "assignment to immutable variable `%a` declared at `%a`"
                        FMT.varname i.name
                        FMT.loc i.loc
            in
            raise (TypeError (loc, msg))
          end;
          Env.markModified env v;
          (LExpr_Var i.name, i.ty)
      | None -> (
          let getters =
            GlobalEnv.getFuns (Env.globals env) (Ident.add_suffix v ~suffix:"read")
          in
          let setters =
            GlobalEnv.getSetterFun (Env.globals env) (Ident.add_suffix v ~suffix:"write")
          in
          let ogetter =
            chooseFunction (Env.globals env) loc "var getter function"
              (Ident.to_string v) false [] getters
          in
          match ogetter with
          | Some fty ->
              let gty =
                match
                  chooseFunction (Env.globals env) loc "var setter function"
                    (Ident.to_string v) false [] setters
                with
                | Some gty -> gty
                | None ->
                    raise
                      (UnknownObject (loc, "var setter function", Ident.to_string v))
              in
              let fty' = instantiate_fun env loc fty [] [] in
              let throws = NoThrow in (* todo: need to allow ? on var *)
              (LExpr_ReadWrite (fty'.name, gty.funname, fty'.parameters, [], throws), fty'.rty)
          | None -> raise (UnknownObject (loc, "variable", Ident.to_string v))))
  | LExpr_Field (l, f) -> (
      let (l', ty) = tc_lexpr2 env loc lets asserts l in
      let ty' = derefType (Env.globals env) loc ty in
      match typeFields (Env.globals env) loc ty' with
      | FT_Record rfs -> (LExpr_Field (l', f), get_recordfield loc rfs f)
      | FT_Register rfs ->
          let (ss, ssty) = get_regfield loc rfs f in
          (LExpr_Slices (ty', l', ss), ssty))
  | LExpr_Fields (l, fs) -> (
      let (l', ty) = tc_lexpr2 env loc lets asserts l in
      let ty' = derefType (Env.globals env) loc ty in
      match typeFields (Env.globals env) loc ty' with
      | FT_Record rfs ->
          let tys = List.map (get_recordfield loc rfs) fs in
          let ws = List.map (width_of_type (Env.globals env) loc) tys in
          let w = Xform_simplify_expr.mk_add_ints ws in
          (LExpr_Fields (l', fs), type_bits w)
      | FT_Register rfs ->
          let ss, ty' = get_regfields loc rfs fs in
          (LExpr_Slices (ty', l', ss), ty'))
  | LExpr_Slices (_, e, ss) -> (
      let all_single =
        List.for_all (function Slice_Single _ -> true | _ -> false) ss
      in
      let ss' = List.map (tc_slice env loc) ss in

      (* variable slice or setter call?
       * Start by testing for getter/setter pair
       * If that fails, test for an array variable or bitvector variable
       *)
      match e with
      | LExpr_Var a -> (
          let tys = List.map (function _, ty -> ty) ss' in
          let getters =
            GlobalEnv.getFuns (Env.globals env) (Ident.add_suffix a ~suffix:"read")
          in
          let setters =
            GlobalEnv.getSetterFun (Env.globals env) (Ident.add_suffix a ~suffix:"write")
          in
          let ogetters =
            chooseFunction (Env.globals env) loc "getter function"
              (Ident.to_string a) true tys getters
          in
          let osetters =
            chooseFunction (Env.globals env) loc "setter function"
              (Ident.to_string a) true tys setters
          in
          match (ogetters, osetters) with
          | Some fty, Some gty when all_single ->
              let es =
                List.map
                  (function
                    | Slice_Single a, _ -> a
                    | _ -> raise (InternalError
                      (loc, "Expr_Slices", (fun fmt -> FMT.lexpr fmt e), __LOC__)))
                  ss'
              in
              let fty' = instantiate_fun env loc fty es tys in
              let throws = NoThrow in (* todo : need to add throws to Var *)
              (LExpr_ReadWrite (fty'.name, gty.funname, fty'.parameters, es, throws), fty'.rty)
          | None, Some _ ->
              raise (UnknownObject (loc, "getter function", Ident.to_string a))
          | Some _, None ->
              raise (UnknownObject (loc, "setter function", Ident.to_string a))
          | _ -> tc_slice_lexpr env loc lets asserts e ss')
      | _ -> tc_slice_lexpr env loc lets asserts e ss')
  | LExpr_BitTuple (_, ls) ->
      let ls', tys = List.split (List.map (tc_lexpr2 env loc lets asserts) ls) in
      let ws = List.map (width_of_type (Env.globals env) loc) tys in
      let w = Xform_simplify_expr.mk_add_ints ws in
      (LExpr_BitTuple (ws, ls'), type_bits w)
  | LExpr_Tuple ls ->
      let ls', tys = List.split (List.map (tc_lexpr2 env loc lets asserts) ls) in
      (LExpr_Tuple ls', Type_Tuple tys)
  | LExpr_Array (a, e) -> (
      let a', ty = tc_lexpr2 env loc lets asserts a in
      match derefType (Env.globals env) loc ty with
      | Type_Array (ixty, elty) ->
          let e' = check_expr env loc (ixtype_basetype ixty) e in
          let e'' = mk_index_check loc lets asserts ixty e' in
          (LExpr_Array (a', e''), elty)
      | _ -> raise (TypeError (loc, "subscript of non-array")))
  | _ -> raise (InternalError (loc, "tc_lexpr2", (fun fmt -> FMT.lexpr fmt x), __LOC__))

(****************************************************************)
(** {2 Typecheck statements}                                    *)
(****************************************************************)

(** Typecheck left hand side of expression and check that rhs type 'ty' is compatible.
    Return set of variables assigned to in this expression
    Insertion of runtime checking code may write additional bindings and assertions
    to 'lets' and 'asserts'.
 *)
let rec tc_lexpr (env : Env.t) (loc : Loc.t)
    (lets : binding list ref) (asserts : check list ref)
    (ty : AST.ty) (x : AST.lexpr)
    : AST.lexpr =
  match x with
  | LExpr_Wildcard ->
      LExpr_Wildcard
  | LExpr_Var v when Ident.equal v  wildcard_ident ->
      (* treat '_' as wildcard token *)
      LExpr_Wildcard
  | LExpr_Var v -> (
      match Env.getVar env v with
      | Some i ->
          if i.is_constant then begin
            let msg = Format.asprintf "assignment to immutable variable `%a` declared at `%a`"
                        FMT.varname i.name
                        FMT.loc i.loc
            in
            raise (TypeError (loc, msg))
          end;
          check_subtype_satisfies env loc ty i.ty;
          Env.markModified env v;
          LExpr_Var v
      | None -> (
          let setters =
            GlobalEnv.getSetterFun (Env.globals env) (Ident.add_suffix v ~suffix:"write")
          in
          let osetter =
            chooseFunction (Env.globals env) loc "var setter function"
              (Ident.to_string v) false [] setters
          in
          match osetter with
          | Some gty ->
              let gty' = instantiate_fun env loc gty [] [] in
              let vty = from_option gty'.ovty (fun _ -> raise (InternalError
                (loc, "tc_lexpr LExpr_Var", (fun fmt -> FMT.lexpr fmt x), __LOC__))) in
              check_subtype_satisfies env loc ty vty;
              let throws = NoThrow in
              LExpr_Write (gty'.name, gty'.parameters, [], throws)
          | None ->
              raise (UnknownObject (loc, "variable", Ident.to_string v))
          )
      )
  | LExpr_Field (l, f) ->
      let (l', rty) = tc_lexpr2 env loc lets asserts l in
      let rty' = derefType (Env.globals env) loc rty in
      let (r, fty) =
        match typeFields (Env.globals env) loc rty' with
        | FT_Record rfs -> (LExpr_Field (l', f), get_recordfield loc rfs f)
        | FT_Register rfs ->
            let ss, ty' = get_regfield loc rfs f in
            (LExpr_Slices (rty', l', ss), ty')
      in
      check_subtype_satisfies env loc ty fty;
      r
  | LExpr_Fields (l, fs) ->
      let (l', lty) = tc_lexpr2 env loc lets asserts l in
      let lty' = derefType (Env.globals env) loc lty in
      let (r, ty') =
        ( match typeFields (Env.globals env) loc lty' with
        | FT_Record rfs ->
            let tys = List.map (get_recordfield loc rfs) fs in
            let ws = List.map (width_of_type (Env.globals env) loc) tys in
            let w = Xform_simplify_expr.mk_add_ints ws in
            (LExpr_Fields (l', fs), type_bits w)
        | FT_Register rfs ->
            let ss, ty' = get_regfields loc rfs fs in
            (LExpr_Slices (lty', l', ss), ty')
        )
      in
      check_subtype_satisfies env loc ty ty';
      r
  | LExpr_Slices (_, e, ss) ->
      let all_single =
        List.for_all (function Slice_Single _ -> true | _ -> false) ss
      in
      let ss' = List.map (tc_slice env loc) ss in

      (* variable slice or setter call?
       * Start by testing for getter/setter pair
       * If that fails, test for slice of a var-getter
       * If that fails, test for an array variable or bitvector variable
       *)
      let e', ty' =
        match e with
        | LExpr_Var a -> (
            let tys = List.map (function _, ty -> ty) ss' in
            let setters =
              GlobalEnv.getSetterFun (Env.globals env) (Ident.add_suffix a ~suffix:"write")
            in
            let osetters =
              chooseFunction (Env.globals env) loc "setter function"
                (Ident.to_string a) true tys setters
            in
            match osetters with
            | Some gty when all_single ->
                let es =
                  List.map
                    (function
                      | Slice_Single a, _ -> a
                      | _ -> raise (InternalError
                        (loc, "Expr_Slices1", (fun fmt -> FMT.lexpr fmt e), __LOC__)))
                    ss'
                in
                let gty' = instantiate_fun env loc gty es tys in
                let vty = from_option gty'.ovty (fun _ -> raise (InternalError
                  (loc, "tc_lexpr LExpr_Slices", (fun fmt -> FMT.lexpr fmt e), __LOC__))) in
                check_subtype_satisfies env loc ty vty;
                let throws = NoThrow in
                (LExpr_Write (gty'.name, gty'.parameters, es, throws), ty)
            | _ -> (
                let getters =
                  GlobalEnv.getFuns (Env.globals env) (Ident.add_suffix a ~suffix:"read")
                in
                let setters =
                  GlobalEnv.getSetterFun (Env.globals env) (Ident.add_suffix a ~suffix:"write")
                in
                let ogetter =
                  chooseFunction (Env.globals env) loc "var getter function"
                    (Ident.to_string a) false [] getters
                in
                let osetter =
                  chooseFunction (Env.globals env) loc "var setter function"
                    (Ident.to_string a) false [] setters
                in
                match (ogetter, osetter) with
                | Some fty, Some fty' ->
                    (* todo: calculate type correctly *)
                    let throws = NoThrow in
                    let wr = LExpr_ReadWrite (fty.funname, fty'.funname, [], [], throws) in
                    let rty' = derefType (Env.globals env) loc fty.rty in
                    ( match rty' with
                    | Type_Bits (n, _) ->
                        let ss'' = List.map (fun (s,_) -> mk_slice_check loc lets asserts n s) ss' in
                        let ty = type_bits (slices_width ss'') in
                        (LExpr_Slices (rty', wr, ss''), ty)
                    | _ -> raise (TypeError (loc, "slice of lexpr"))
                    )
                | None, Some _ ->
                    raise
                      (UnknownObject (loc, "var getter function", Ident.to_string a))
                | Some _, None ->
                    raise
                      (UnknownObject (loc, "var setter function", Ident.to_string a))
                | None, None -> tc_slice_lexpr env loc lets asserts e ss'))
        | _ -> tc_slice_lexpr env loc lets asserts e ss'
      in
      check_subtype_satisfies env loc ty ty';
      e'
  | LExpr_BitTuple (_, ls) ->
      let ls', tys = List.split (List.map (tc_lexpr2 env loc lets asserts) ls) in
      let ws = List.map (width_of_type (Env.globals env) loc) tys in
      let w = Xform_simplify_expr.mk_add_ints ws in
      check_subtype_satisfies env loc ty (type_bits w);
      LExpr_BitTuple (ws, ls')
  | LExpr_Tuple ls ->
      let ls' =
        match ty with
        | Type_Tuple tys when List.length ls = List.length tys ->
            List.map2 (tc_lexpr env loc lets asserts) tys ls
        | _ -> raise (IsNotA (loc, "tuple of length ?", pp_type ty))
      in
      LExpr_Tuple ls'
  | LExpr_Array (a, e) -> (
      let a', ty = tc_lexpr2 env loc lets asserts a in
      match derefType (Env.globals env) loc ty with
      | Type_Array (ixty, elty) ->
          let e', ety = tc_expr env loc e in
          check_subtype_satisfies env loc ety (ixtype_basetype ixty);
          let e'' = mk_index_check loc lets asserts ixty e' in
          LExpr_Array (a', e'')
      | _ -> raise (TypeError (loc, "subscript of non-array")))
  | _ -> raise (InternalError (loc, "tc_lexpr", (fun fmt -> FMT.lexpr fmt x), __LOC__))

let rec add_decl_item_vars (env : Env.t) (loc : Loc.t) (is_constant : bool) (x : AST.decl_item) : unit =
  match x with
  | DeclItem_Var (v, Some ty) ->
      Env.addLocalVar env {name=v; loc; ty; is_local=true; is_constant}
  | DeclItem_Tuple dis ->
      List.iter (add_decl_item_vars env loc is_constant) dis
  | DeclItem_BitTuple dis ->
      List.iter (fun (ov, ty) ->
        ( match ov with
        | Some v -> Env.addLocalVar env {name=v; loc; ty; is_local=true; is_constant}
        | _ -> ()
        ))
        dis
  | DeclItem_Wildcard oty ->
      ()
  | DeclItem_Var (v, None) ->
      raise (InternalError
        (loc, "visit_declitem", (fun fmt -> FMT.decl_item fmt x), __LOC__))

let tc_decl_bit (env : Env.t) (loc : Loc.t) (x : (Ident.t option * AST.ty)) : (Ident.t option * AST.ty) =
  let (ov, ty) = x in
  let ty' = tc_type env loc ty in
  ( match ty' with
  | Type_Bits _ -> (ov, ty')
  | _ -> raise (TypeError (loc, "bits type expected"))
  )

(** Convert the format string in a print command into a sequence of
 *  calls to primitive print functions
 *)
let tc_print (env : Env.t) (loc : Loc.t) (args : AST.expr list) : AST.stmt list =
  let error (msg : string) : exn =
      let msg = Format.asprintf "Error in Print format string: %s" msg in
      TypeError (loc, msg)
  in

  ( match args with
  | [(Expr_Lit (VString fmt))] ->
      let result = ref [] in
      let pending_string = Buffer.create 40 in
      let flush_buffer _ = if Buffer.length pending_string > 0 then begin
            let s = Expr_Lit (VString (Buffer.contents pending_string)) in
            let p = Stmt_TCall (print_str, [], [s], NoThrow, loc) in
            result := !result @ [p];
            Buffer.clear pending_string
          end
      in

      let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false in
      let is_alphanum = function '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false in

      let n = String.length fmt in
      let pos = ref 0 in

      let getc _ : char = String.get fmt !pos in
      let advance _ : unit = pos := !pos + 1 in
      let at_end _ : bool = !pos = n in

      let check (expected : char) : unit =
          if at_end () then begin
            raise (error "incomplete format string")
          end else begin
            let c = getc () in
            if c <> expected then begin
              let msg = Format.asprintf "expected '%c' in format string, got '%c'" expected c in
              raise (error msg)
            end;
            advance ()
          end
      in

      let ident _ : string option =
          let c = getc () in
          if not (is_alpha c) then (
            None
          ) else (
            let s = Buffer.create 16 in
            Buffer.add_char s c;
            advance ();
            while not (at_end ()) && is_alphanum (getc ()) do
                Buffer.add_char s (getc ());
            done;
            Some (Buffer.contents s)
          )
      in

      while not (at_end ()) do
        let c = getc () in
        if c = '{' then begin
          flush_buffer ();
          advance ();
          if at_end () then begin
            raise (error "premature end of format string")
          end else begin
            ( match ident () with
            | None -> raise (error "identifier expected")
            | Some nm ->
                check '}';
                let v = Ident.mk_ident nm in
                ( match Env.getVar env v with
                | None ->
                    let msg = Format.asprintf "unknown variable '%s'" nm in
                    raise (error msg)
                | Some info ->
                    let p = ( match info.ty with
                            | Type_Bits (n, _) ->
                                Stmt_TCall (print_bits_hex, [n], [Expr_Var v], NoThrow, loc)
                            | Type_Integer _ ->
                                Stmt_TCall (print_int_dec, [], [Expr_Var v], NoThrow, loc)
                            | Type_Constructor (tc, []) when Ident.equal tc boolean_ident ->
                                Stmt_TCall (print_boolean, [], [Expr_Var v], NoThrow, loc)
                            | Type_Constructor (tc, []) when Ident.equal tc string_ident ->
                                Stmt_TCall (print_str, [], [Expr_Var v], NoThrow, loc)
                            | _ ->
                               let msg = Format.asprintf "no print function defined for variable %a : %a"
                                   Ident.pp v
                                   FMT.ty info.ty
                               in
                               raise (error msg)
                            )
                    in
                    result := !result @ [p]
                )
            )
          end
        end else begin
          Buffer.add_char pending_string c;
          advance ()
        end
      done;
      flush_buffer ();

      !result
  | _ ->
      raise (error "Print must always be used with a literal format string like \"x = {x}\"")
  )

(* drop any integer constraints from a type *)
let drop_constraints (ty : AST.ty) : AST.ty =
  ( match ty with
  | Type_Integer _ -> Type_Integer None
  | _ -> ty
  )

(* typecheck a decl_item using the type `ity` of the initializer *)
let rec tc_decl_item (env : Env.t) (loc : Loc.t) (is_immutable : bool) (ity : AST.ty) (x : AST.decl_item) : AST.decl_item =
  match (ity, x) with
  | (ity, DeclItem_Var (v, None)) ->
    (* Mutable variable declarations do not inherit constraints from their
     * initializers because the whole point of a mutable variable is that it can be
     * assigned values different from their initializer.
     *)
    let ity' = if is_immutable then ity else drop_constraints ity in
    DeclItem_Var (v, Some ity')
  | (ity, DeclItem_Var (v, Some ty)) ->
      let ty' = check_type env loc ity ty in
      DeclItem_Var (v, Some ty')
  | (Type_Tuple itys, DeclItem_Tuple dis) when List.length dis = List.length itys ->
      let dis' = List.map2 (tc_decl_item env loc is_immutable) itys dis in
      DeclItem_Tuple dis'
  | (_, DeclItem_Tuple dis) ->
      let len = List.length dis in
      raise (IsNotA (loc, Format.asprintf "tuple of length %d" len, pp_type ity))
  | (Type_Bits (n, _), DeclItem_BitTuple dbs) ->
      let dbs' = List.map (tc_decl_bit env loc) dbs in
      let (vs', tys) = List.split dbs' in
      let ws = List.map (width_of_type (Env.globals env) loc) tys in
      let w = Xform_simplify_expr.mk_add_ints ws in
      let ty' = type_bits w in
      check_subtype_satisfies env loc ty' ity;
      DeclItem_BitTuple dbs'
  | (_, DeclItem_BitTuple _) ->
      raise (IsNotA (loc, "bitvector", pp_type ity))
  | (ity, DeclItem_Wildcard None) ->
      DeclItem_Wildcard (Some ity)
  | (ity, DeclItem_Wildcard (Some ty)) ->
      let ty' = check_type env loc ity ty in
      DeclItem_Wildcard (Some ty')

(** Typecheck list of statements *)
let rec tc_stmts (env : Env.t) (loc : Loc.t) (xs : AST.stmt list) :
    AST.stmt list =
  Env.nest
    (fun env' -> List.concat_map (tc_stmt env') xs)
    env

(** Typecheck 'if expr then stmt' *)
and tc_s_elsif (env : Env.t) (x : AST.s_elsif) : AST.s_elsif =
  let (c, s, loc) = x in
  let c' = check_expr env loc type_bool c in
  let s' = tc_stmts env loc s in
  (c', s', loc)

(** Typecheck case alternative *)
and tc_alt (env : Env.t) (ty : AST.ty) (x : AST.alt) : AST.alt =
  match x with
  | Alt_Alt (ps, oc, b, loc) ->
      let ps' = List.map (tc_pattern env loc ty) ps in
      let oc' = Option.map (fun c -> check_expr env loc type_bool c) oc in
      let b' = tc_stmts env loc b in
      Alt_Alt (ps', oc', b', loc)

(** Typecheck exception catcher 'when expr stmt' *)
and tc_catcher (env : Env.t) (loc : Loc.t) (x : AST.catcher) : AST.catcher =
  match x with
  | Catcher_Guarded (v, tc, b, loc) ->
      if not (GlobalEnv.isTycon (Env.globals env) tc) then begin
        raise (IsNotA (loc, "exception type", Ident.to_string tc))
      end;
      let b' = Env.nest
        (fun env' ->
          Env.addLocalVar env' {name=v; loc; ty=Type_Constructor (tc, []); is_local=true; is_constant=true};
          tc_stmts env' loc b)
        env
      in
      Catcher_Guarded (v, tc, b', loc)

(** typecheck statement
 *  This normally returns a single statement except for instructions
 *  where we insert a runtime check prior to the statement.
 *)
and tc_stmt (env : Env.t) (x : AST.stmt) : AST.stmt list =
  if verbose then Format.fprintf fmt "      - Typechecking statement %a\n" (FMT.stmt ~short:true) x;
  ( match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) ->
      let ty' = tc_type env loc ty in
      List.iter (fun v -> Env.addLocalVar env {name=v; loc; ty=ty'; is_local=true; is_constant=false}) vs;
      [Stmt_VarDeclsNoInit (vs, ty', loc)]
  | Stmt_VarDecl (is_constant, di, i, loc) ->
      let (i', ity) = tc_expr env loc i in
      let di' = tc_decl_item env loc is_constant ity di in
      add_decl_item_vars env loc is_constant di';
      if is_constant then begin
        (* add integer constants to type environment *)
        (match di' with
        | DeclItem_Var (v, Some ty) ->
            ( match ty with
            | Type_Integer _ -> Env.addConstraint env loc (mk_eq_int (Expr_Var v) i')
            | _ -> ()
            )
        | _ -> ());
        end;
      [Stmt_VarDecl (is_constant, di', i', loc)]
  | Stmt_Assign (l, r, loc) ->
      let (r', rty) = tc_expr env loc r in
      let lets = ref [] in
      let asserts = ref [] in
      let l' = tc_lexpr env loc lets asserts rty l in
      let lets' = Asl_utils.mk_assigns loc !lets in
      let asserts' = Asl_utils.mk_assert_stmts !asserts in
      if verbose then
        Format.fprintf fmt "    - Typechecking %a <- %a : %a\n"
          FMT.lexpr l' FMT.expr r' FMT.ty rty;
      (lets' @ asserts' @ [Stmt_Assign (l', r', loc)])
  | Stmt_UCall (f, args, throws, loc) ->
      let args' = tc_args env loc args in
      let (fty, es) = tc_apply env loc "procedure" f args' in
      check_subtype_satisfies env loc type_unit fty.rty;
      if Ident.equal fty.name print then (
        tc_print env loc es
      ) else (
        [Stmt_TCall (fty.name, fty.parameters, es, throws, loc)]
      )
  | Stmt_Return (e, loc) ->
      let rty = Env.getReturnType env in
      let e' = check_expr env loc rty e in
      [Stmt_Return (e', loc)]
  | Stmt_Assert (e, loc) ->
      let e' = check_expr env loc type_bool e in
      [Stmt_Assert (e', loc)]
  | Stmt_Throw (e, loc) ->
      let (e', ty') = tc_expr env loc e in
      (* todo: check that ty' is an exception type *)
      [Stmt_Throw (e', loc)]
  | Stmt_Block (ss, loc) ->
      let ss' = tc_stmts env loc ss in
      [Stmt_Block (ss', loc)]
  | Stmt_If (els, (e, el), loc) ->
      let els' = List.map (tc_s_elsif env) els in
      let e' = tc_stmts env el e in
      [Stmt_If (els', (e', el), loc)]
  | Stmt_Case (e, _, alts, odefault, loc) ->
      let (e', ty') = tc_expr env loc e in
      let alts' = List.map (tc_alt env ty') alts in
      let odefault' =
        Option.map (fun (b, bl) -> (tc_stmts env loc b, bl)) odefault
      in
      [Stmt_Case (e', Some ty', alts', odefault', loc)]
  | Stmt_For (v, ty, start, dir, stop, b, loc) ->
      let ty' = tc_type env loc ty in
      let start' = check_expr env loc ty start in
      let stop' = check_expr env loc ty stop in
      (* todo: we can calculate the range only if start and stop are immutable *)
      let ty'' =
        ( match (ty', dir) with
        | (Type_Integer(None), Direction_Up) -> Type_Integer (Some [Constraint_Range (start', stop')])
        | (Type_Integer(None), Direction_Down) -> Type_Integer (Some [Constraint_Range (stop', start')])
        | _ -> ty'
        )
      in
      let b' =
        Env.nest
          (fun env' ->
            Env.addLocalVar env' {name=v; loc; ty=ty''; is_local=true; is_constant=true};
            tc_stmts env' loc b)
          env
      in
    [Stmt_For (v, ty'', start', dir, stop', b', loc)]
  | Stmt_While (c, b, loc) ->
      let c' = check_expr env loc type_bool c in
      let b' = tc_stmts env loc b in
      [Stmt_While (c', b', loc)]
  | Stmt_Repeat (b, c, pos, loc) ->
      let b' = tc_stmts env loc b in
      let c' = check_expr env loc type_bool c in
      [Stmt_Repeat (b', c', pos, loc)]
  | Stmt_Try (tb, pos, catchers, odefault, loc) ->
      let tb' = tc_stmts env loc tb in
      let catchers' = List.map (tc_catcher env loc) catchers in
      let odefault' = Option.map (fun (b, bl) -> (tc_stmts env loc b, bl)) odefault in
      [Stmt_Try (tb', pos, catchers', odefault', loc)]
  | Stmt_TCall (_, _, _, _, loc) ->
      raise (InternalError (loc, "Unexpected stmt type in typechecker", (fun fmt -> FMT.stmt fmt x), __LOC__))
  )

(****************************************************************)
(** {2 Typecheck function definition}                           *)
(****************************************************************)

(** Typecheck function body (list of statements) *)
let tc_body = tc_stmts

(** Typecheck function parameter *)
let tc_parameter (env : Env.t) (loc : Loc.t)
    ((arg, oty) : Ident.t * AST.ty option) : Ident.t * AST.ty option =
  let ty' =
    ( match oty with
    | None -> Type_Integer (Some [Constraint_Single (Expr_Var arg)])
    | Some ty -> tc_type env loc ty
    )
  in
  Env.addLocalVar env {name=arg; loc; ty=ty'; is_local=true; is_constant=true};
  (* function parameters are implicitly non-negative because bitvectors can't have negative length *)
  Env.addConstraint env loc (mk_le_int zero (Expr_Var arg));
  ( match ty' with
  | Type_Integer (Some cs) -> Env.addConstraint env loc (constraint_ranges_to_expr (Expr_Var arg) cs)
  | _ -> ()
  );
  (arg, Some ty')

(** Typecheck function argument *)
let tc_argument
    (env : Env.t)
    (loc : Loc.t)
    (ps : (Ident.t * AST.ty option) list)
    ((arg, ty, od) : Ident.t * AST.ty * AST.expr option)
  : (Ident.t * AST.ty * AST.expr option)
  =
  let ty' = tc_type env loc ty in
  let od' = Option.map (check_expr env loc ty') od in
  if not (List.mem_assoc arg ps) then begin
    (* add to scope if it is not a parameter *)
    Env.addLocalVar env {name=arg; loc; ty=ty'; is_local=true; is_constant=true}
  end;
  (arg, ty', od')

(** Typecheck function setter argument
 *  (This is the same as tc_argument except there is no default arg)
 *)
let tc_setter
    (env : Env.t)
    (loc : Loc.t)
    (ps : (Ident.t * AST.ty option) list)
    ((arg, ty) : Ident.t * AST.ty)
  : Ident.t * AST.ty
  =
  let ty' = tc_type env loc ty in
  if not (List.mem_assoc arg ps) then begin
    (* add to scope if it is not a parameter *)
    Env.addLocalVar env {name=arg; loc; ty=ty'; is_local=true; is_constant=true}
  end;
  (arg, ty')

let mangle_setter_name (f : Ident.t) (fty : AST.function_type) : Ident.t =
  if fty.is_getter_setter then (
    if Option.is_none fty.setter_arg then
      Ident.add_suffix f ~suffix:"read"
    else
      Ident.add_suffix f ~suffix:"write"
  ) else (
    f
  )

(** Typecheck list of function arguments *)
(* todo: if this is a getter/setter check that if a setter function exists, it has a compatible type *)
let tc_funtype (env : Env.t) (loc : Loc.t) (fty : AST.function_type) : AST.function_type =
  let args = List.map (fun (nm, ty, od) -> (nm, ty)) fty.args @ Option.to_list fty.setter_arg in

  let globals = Env.globals env in
  let is_not_global (x : Ident.t) = Option.is_none (GlobalEnv.getGlobalVar globals x) in

  (* The implicit type parameters are based on the free variables of the function type. *)
  let argtys = List.map snd args in
  let argty_fvs = fv_types argtys |> IdentSet.filter is_not_global in
  let rty_fvs = fv_type fty.rty |> IdentSet.filter is_not_global in

  (* Type parameters in the return type cannot be synthesized directly
   * and must either be an explicit parameter of the function or
   * a free variable in one of the argument types.
   *)
  let arg_names = List.map fst args in
  let unsynthesizable = (IdentSet.diff rty_fvs argty_fvs)
                      |> IdentSet.filter (fun v -> not (List.mem v arg_names))
  in
  if not (IdentSet.is_empty unsynthesizable) then begin
    let msg = Format.asprintf "the width parameter(s) `%a` of the return type cannot be determined from the function arguments"
                FMT.varnames (IdentSet.elements unsynthesizable)
    in
    raise (TypeError (loc, msg))
  end;

  (* todo: check that all explicit params occur in arg type *)

  (* implicit parameters are sorted alphabetically for the sake of having a consistent order. *)
  let implicit_parameters =
    (IdentSet.union argty_fvs rty_fvs)
    |> IdentSet.filter (fun v -> not (List.mem_assoc v fty.parameters))
    |> Identset.to_sorted_list
  in

  (* calculate parameter type from any explicitly provided type and
   * any constraints on any matching argument type
   *)
  let merge_tys (v : Ident.t) (explicit_type : ty option) (arg_ty : ty option) : ty option =
    ( match (explicit_type, arg_ty) with
    | (Some ty, _) -> Some ty (* an explicit type on the parameter wins *)
    | (_, Some (Type_Integer (Some cs))) -> arg_ty (* explicit constraints on the arg are used if provided *)
    | _ -> None
      )
  in

  (* The type of any implicit or explicit parameters should match explicit argument if it exists *)
  let arg_type (v : Ident.t) : AST.ty option =
      List.find_opt (fun (nm, ty) -> Ident.equal v nm) args
      |> Option.map snd
  in
  let typed_explicit_parameters = List.map (fun (v, oty) -> (v, merge_tys v oty (arg_type v))) fty.parameters in
  let typed_implicit_parameters = List.map (fun v -> (v, arg_type v)) implicit_parameters in

  (* Having inferred all the implicit parameters, we can finally typecheck the function type *)
  let ps' = List.map (tc_parameter env loc) (typed_explicit_parameters @ typed_implicit_parameters) in
  let args' = List.map (tc_argument env loc ps') fty.args in
  let setter_arg' = Option.map (tc_setter env loc ps') fty.setter_arg in
  let rty' = tc_type env loc fty.rty in
  Env.setReturnType env rty';
  { fty with
    parameters=ps';
    args=args';
    setter_arg=setter_arg';
    rty=rty'
  }

(** Add function definition to environment
 *  Complaining if there is a previous definition with compatible type.
 *)
let addFunction (env : GlobalEnv.t) (loc : Loc.t) (qid : Ident.t) (fty : AST.function_type) : funtype =
  let argtys = List.map (fun (_, ty, _) -> ty) fty.args in
  let is_setter = Option.is_some fty.setter_arg in
  let funs = if is_setter then GlobalEnv.getSetterFun env qid else GlobalEnv.getFuns env qid in
  let num_funs = List.length funs in
  match List.filter (isCompatibleFunction env loc fty.use_array_syntax argtys) funs with
  | [] ->
      (* not defined yet *)
      (* ASL allows multiple functions to share the same name.
       * The typechecker disambiguates functions for the benefit of other parts of the
       * system by adding a unique tag to each ident.
       * We use the number of functions that already have that name as the tag.
       *)
      let tag = num_funs in
      let qid' = Ident.mk_fident_with_tag qid ~tag in
      let ovty = Option.map snd fty.setter_arg in
      let fty : funtype = { funname=qid'; loc; isArray=fty.use_array_syntax; params=fty.parameters; atys=fty.args; ovty; rty=fty.rty } in
      if is_setter then GlobalEnv.addSetterFuns env qid (fty :: funs) else GlobalEnv.addFuns env loc qid (fty :: funs);
      fty
  | [ fty ] ->
      if fty.loc <> loc then begin
        (* already defined *)
        let msg = Format.asprintf "function `%a` was previously defined at `%a`"
                    FMT.funname qid
                    FMT.loc fty.loc
        in
        raise (TypeError (loc, msg))
      end else begin
        fty
      end
  | ftys ->
      let msg = "multiple function definitions" in
      raise
        (InternalError (loc, msg, (fun fmt -> FMT.funname fmt qid), __LOC__))

(****************************************************************)
(** {2 Typecheck global declaration}                            *)
(****************************************************************)

(** Typecheck global declaration, extending environment as needed *)
let tc_declaration (env : GlobalEnv.t) (d : AST.declaration) :
    AST.declaration list =
  if verbose then Format.fprintf fmt "    - Typechecking declaration %a\n" (FMT.declaration ~short:true) d;
  ( match d with
  | Decl_BuiltinType (qid, loc) ->
      GlobalEnv.addType env loc qid (Type_Builtin qid);
      [ d ]
  | Decl_Forward (qid, loc) ->
      GlobalEnv.addType env loc qid Type_Forward;
      [ d ]
  | Decl_Record (qid, ps, fs, loc) ->
      let env' = Env.mkEnv env in
      List.iter (fun p -> Env.addLocalVar env' {name=p; loc; ty=type_integer; is_local=true; is_constant=true}) ps;

      check_duplicate_field_names (fun (f, _) -> f) fs loc;

      let fs' = List.map (fun (f, ty) -> (f, tc_type env' loc ty)) fs in
      GlobalEnv.addType env loc qid (Type_Record (ps, fs'));
      [ Decl_Record (qid, ps, fs', loc) ]
  | Decl_Exception (qid, fs, loc) ->
      let env' = Env.mkEnv env in

      check_duplicate_field_names (fun (f, _) -> f) fs loc;

      let fs' = List.map (fun (f, ty) -> (f, tc_type env' loc ty)) fs in
      GlobalEnv.addType env loc qid (Type_Exception fs');
      [ Decl_Exception (qid, fs', loc) ]
  | Decl_Typedef (qid, ps, ty, loc) ->
      (* todo: check for cyclic dependency *)
      let env' = Env.mkEnv env in
      List.iter (fun p -> Env.addLocalVar env' {name=p; loc; ty=type_integer; is_local=true; is_constant=true}) ps;
      let ty' = tc_type env' loc ty in
      GlobalEnv.addType env loc qid (Type_Abbreviation (ps, ty'));
      [ Decl_Typedef (qid, ps, ty', loc) ]
  | Decl_Enum (qid, es, loc) ->
      GlobalEnv.addType env loc qid (Type_Enumeration es);
      let ty = Type_Constructor (qid, []) in
      List.iter
        (fun e ->
           let v = { name=e; loc; ty; is_local=false; is_constant=true } in
           GlobalEnv.addGlobalVar env v)
        es;
      let fty = { parameters = [];
                  args = [ (Ident.mk_ident "x", ty, None); (Ident.mk_ident "y", ty, None) ];
                  setter_arg = None;
                  rty = type_bool;
                  is_getter_setter = false;
                  use_array_syntax = false;
                  throws=NoThrow
      } in
      let eq = addFunction env loc eq_enum fty in
      let ne = addFunction env loc ne_enum fty in
      GlobalEnv.addOperators2 env loc Binop_Eq [ eq ];
      GlobalEnv.addOperators2 env loc Binop_NtEq [ ne ];
      let deq = Decl_BuiltinFunction (eq.funname, fty, loc) in
      let dne = Decl_BuiltinFunction (ne.funname, fty, loc) in
      [ d; deq; dne ]
  | Decl_Var (qid, ty, loc) ->
      let ty' = tc_type (Env.mkEnv env) loc ty in
      let v = { name=qid; loc; ty=ty'; is_local=false; is_constant=false } in
      GlobalEnv.addGlobalVar env v;
      [ Decl_Var (qid, ty', loc) ]
  | Decl_Const (qid, ty_opt, i, loc) ->
      let locals = Env.mkEnv env in
      let (i', ity) = tc_expr locals loc i in
      let ty' = match ty_opt with
        | None -> ity
        | Some ty -> check_type locals loc ity ty
      in
      let v = { name=qid; loc; ty=ty'; is_local=false; is_constant=true } in
      GlobalEnv.addGlobalVar env v;
      GlobalEnv.addConstant env qid (simplify_expr i');
      [ Decl_Const (qid, Some ty', i', loc) ]
  | Decl_BuiltinFunction (qid, fty, loc) ->
      let locals = Env.mkEnv env in
      let fty' = tc_funtype locals loc fty in
      let qid' = (addFunction env loc qid fty').funname in
      [ Decl_BuiltinFunction (qid', fty', loc) ]
  | Decl_FunType (qid, fty, loc) ->
      let locals = Env.mkEnv env in
      let fty' = tc_funtype locals loc fty in
      let mangled = mangle_setter_name qid fty' in
      let qid' = (addFunction env loc mangled fty').funname in
      [ Decl_FunType (qid', fty', loc) ]
  | Decl_FunDefn (qid, fty, b, loc) ->
      let locals = Env.mkEnv env in
      let fty' = tc_funtype locals loc fty in
      let mangled = mangle_setter_name qid fty' in
      let qid' = (addFunction env loc mangled fty').funname in
      let b' = tc_body locals loc b in
      [ Decl_FunDefn (qid', fty', b', loc) ]
  | Decl_Operator1 (op, funs, loc) ->
      let funs' =
        List.concat
          (List.map
             (fun f ->
               let fs = GlobalEnv.getFuns env f in
               if fs = [] then
                 raise
                   (UnknownObject
                      (loc, "unary operator implementation", Ident.to_string f));
               fs)
             funs)
      in
      GlobalEnv.addOperators1 env loc op funs';
      [ Decl_Operator1 (op, List.map (fun f -> f.funname) funs', loc) ]
  | Decl_Operator2 (op, funs, loc) ->
      let funs' =
        List.concat
          (List.map
             (fun f ->
               let fs = GlobalEnv.getFuns env f in
               if fs = [] then
                 raise
                   (UnknownObject
                      (loc, "binary operator implementation", Ident.to_string f));
               fs)
             funs)
      in
      GlobalEnv.addOperators2 env loc op funs';
      [ Decl_Operator2 (op, List.map (fun f -> f.funname) funs', loc) ]
  | Decl_Config (qid, ty, i, loc) ->
      (* very similar to Decl_Const *)
      let locals = Env.mkEnv env in
      let ty' = tc_type locals loc ty in
      let i' = check_expr locals loc ty' i in
      let v = { name=qid; loc; ty=ty'; is_local=false; is_constant=true } in
      GlobalEnv.addGlobalVar env v;
      [ Decl_Config (qid, ty', i', loc) ]
  )

(** Generate function prototype declarations.

    This allows function declarations within a translation unit to be
    placed in any order.
 *)
let genPrototypes (ds : AST.declaration list) :
    AST.declaration list * AST.declaration list =
  let pre : AST.declaration list ref = ref [] in
  let post : AST.declaration list ref = ref [] in
  List.iter
    (fun d ->
      match d with
      | Decl_FunDefn (qid, fty, _, loc) ->
          post := d :: !post;
          pre := Decl_FunType (qid, fty, loc) :: !pre
      | _ -> pre := d :: !pre)
    ds;
  (List.rev !pre, List.rev !post)

(** Overall typechecking environment shared by all invocations of typechecker *)
let env0 = GlobalEnv.mkempty ()

(** Typecheck a list of declarations - main entrypoint into typechecker *)
let tc_declarations (env : GlobalEnv.t) ~(isPrelude : bool) ~(sort_decls : bool)
    (ds : AST.declaration list) : AST.declaration list option =
  if verbose then Format.fprintf fmt "  - Using Z3 %s\n" Z3.Version.to_string;
  (* Process declarations, starting by moving all function definitions to the
   * end of the list and replacing them with function prototypes.
   * As long as the type/var decls are all sorted correctly, this
   * is enough to handle functions that are used before being defined.
   *
   * Note that each declaration is evaluated in a separate local environment
   * but that they share the same global environment
   *)
  let (pre, post) = genPrototypes ds in
  let pre = if sort_decls then List.rev (Asl_utils.topological_sort pre) else pre in
  if verbose then Format.fprintf fmt "  - Typechecking %d declarations\n" (List.length pre);
  let error_count = ref 0 in
  let tc_decl env d =
    ( try
        tc_declaration env d
    with
    | UnknownObject _
    | DoesNotMatch _
    | IsNotA _
    | Ambiguous _
    | TypeError _
      as exn
    ->
      if !error_count < !max_errors then begin
        Error.print_exception exn;
        error_count := !error_count + 1;
        [d]
      end else begin
        raise exn
      end
    )
  in

  let pre' = List.map (tc_decl env) pre in
  let post' = List.map (tc_decl env) post in

  if !error_count != 0 then
    None
  else
    Some ((List.concat pre') @ (List.concat post'))

(****************************************************************
 * End
 ****************************************************************)
