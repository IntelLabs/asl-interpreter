(****************************************************************
 * ASL typechecker
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Type inference and checker for ASL language *)

module AST = Asl_ast
module Visitor = Asl_visitor
module FMT = Asl_fmt
module FMTUtils = Format_utils
open AST
open Utils
open Asl_utils
open Format

let verbose = false
let fmt = std_formatter

(****************************************************************)
(** {3 Exceptions thrown by typechecker}                        *)
(****************************************************************)

exception UnknownObject of (l * string * string)
exception DoesNotMatch of (l * string * string * string)
exception IsNotA of (l * string * string)
exception Ambiguous of (l * string * string)
exception TypeError of (l * string)
exception InternalError of string (* internal invariants have been broken *)

(****************************************************************)
(** {3 AST construction utilities}                              *)
(****************************************************************)

(* todo: given the function/procedure distinction, it is not clear
 * that we need type_unit
 *)
let type_unit = Type_Tuple []
let type_integer = Type_Integer None
let type_bool = Type_Constructor (Ident "boolean")
let type_real = Type_Constructor (Ident "real")
let type_string = Type_Constructor (Ident "string")
let type_bits (n : expr) = Type_Bits n
let type_exn = Type_Constructor (Ident "__Exception")
let type_bitsK (k : intLit) : AST.ty = type_bits (Expr_LitInt k)

(** Construct expression "eq_int(x, y)" *)
let mk_eq_int (x : AST.expr) (y : AST.expr) : AST.expr =
  Expr_TApply (FIdent ("eq_int", 0), [], [ x; y ])

(** Construct expression "add_int(x, y)" *)
let mk_add_int (x : AST.expr) (y : AST.expr) : AST.expr =
  Expr_TApply (FIdent ("add_int", 0), [], [ x; y ])

(** Construct expression "sub_int(x, y)" *)
let mk_sub_int (x : AST.expr) (y : AST.expr) : AST.expr =
  Expr_TApply (FIdent ("sub_int", 0), [], [ x; y ])

(** Construct expression "(0 + x1) + ... + xn" *)
let mk_add_ints (xs : AST.expr list) : AST.expr =
  List.fold_left mk_add_int (Expr_LitInt "0") xs

let mk_concat_ty (x : AST.ty) (y : AST.ty) : AST.ty =
  match (x, y) with
  | Type_Bits e1, Type_Bits e2 -> type_bits (mk_add_int e1 e2)
  | _ ->
      Printf.printf "Can't concatenate types %s and %s\n" (pp_type x)
        (pp_type y);
      raise (InternalError "mk_concat_ty")

let mk_concat_tys (xs : AST.ty list) : AST.ty =
  List.fold_left mk_concat_ty (type_bitsK "0") xs

let width_of_type (ty : AST.ty) : AST.expr =
  match ty with Type_Bits n -> n | _ -> raise (InternalError "width_of_type")

let one = Expr_LitInt "1"

let mk_bits_select (w : AST.expr) (n : AST.expr) (x : AST.expr) (lo : AST.expr)
    : AST.expr =
  Expr_TApply (FIdent ("asl_extract_bits", 0), [ w; n ], [ x; lo; w ])

let mk_int_select (w : AST.expr) (x : AST.expr) (lo : AST.expr) : AST.expr =
  Expr_TApply (FIdent ("asl_extract_int", 0), [ w ], [ x; lo; w ])

(* Lower bit slice expression *)
let mk_expr_slices (x : AST.expr) (ss : AST.slice list) (ty : AST.ty) : AST.expr
    =
  let n = width_of_type ty in
  match (x, ss) with
  (* don't lower the easy case *)
  | Expr_Var v, ss -> Expr_Slices (x, ss)
  (* lower all cases where x is not a variable *)
  | _, [ Slice_Single i ] -> mk_bits_select one n x i
  | _, [ Slice_HiLo (hi, lo) ] ->
      let w = mk_add_int (mk_sub_int hi lo) one in
      mk_bits_select w n x lo
  | _, [ Slice_LoWd (lo, wd) ] -> mk_bits_select wd n x lo
  (* todo: currently don't have a good story for lowering the multi-slice
     case *)
  | _ -> Expr_Slices (x, ss)

(* Lower int slice expression *)
let mk_expr_intslices (x : AST.expr) (ss : AST.slice list) : AST.expr =
  match (x, ss) with
  (* don't lower the easy case *)
  | Expr_Var v, ss -> Expr_Slices (x, ss)
  (* lower all cases where x is not a variable *)
  | _, [ Slice_Single i ] -> mk_int_select one x i
  | _, [ Slice_HiLo (hi, lo) ] ->
      let w = mk_add_int (mk_sub_int hi lo) one in
      mk_int_select w x lo
  | _, [ Slice_LoWd (lo, wd) ] -> mk_int_select wd x lo
  (* todo: currently don't have a good story for lowering the multi-slice
     case *)
  | _ -> Expr_Slices (x, ss)

let slice_width (x : AST.slice) : AST.expr =
  match x with
  | Slice_Single e -> Expr_LitInt "1"
  | Slice_HiLo (hi, lo) -> mk_add_int (mk_sub_int hi lo) (Expr_LitInt "1")
  | Slice_LoWd (lo, wd) -> wd

let slices_width (xs : AST.slice list) : AST.expr =
  mk_add_ints (List.map slice_width xs)

let ixtype_basetype (ty : AST.ixtype) : AST.ty =
  match ty with
  | Index_Enum tc -> Type_Constructor tc
  | Index_Range _ -> type_integer

(****************************************************************)
(** {3 Prettyprinting support}                                  *)
(****************************************************************)

(** Table of binary operators used for resugaring expressions when printing
    error messages.
 *)
let binop_table : AST.binop Bindings.t ref = ref Bindings.empty

let add_binop (op : binop) (x : ident) : unit =
  binop_table := Bindings.add x op !binop_table

(** Resugar expression *)
let ppp_expr (x : AST.expr) : AST.expr = resugar_expr !binop_table x

(** Very pretty print type (resugaring expressions) *)
let ppp_type (x : AST.ty) : AST.ty = resugar_type !binop_table x

(****************************************************************)
(** {2 Environment representing global and local objects}       *)
(****************************************************************)

type typedef =
  | Type_Builtin of ident
  | Type_Forward
  | Type_Record of (ident * ty) list
  | Type_Enumeration of ident list
  | Type_Abbreviation of ty

let pp_typedef (x : typedef) (fmt : formatter) : unit =
  match x with
  | Type_Builtin t ->
      FMT.kw_underscore_builtin fmt;
      FMTUtils.nbsp fmt;
      FMT.tycon fmt t
  | Type_Forward -> pp_print_string fmt "forward"
  | Type_Record fs ->
      FMT.kw_record fmt;
      FMTUtils.nbsp fmt;
      FMTUtils.braces fmt (fun _ ->
          FMTUtils.vbox fmt (fun _ ->
              FMTUtils.cutsep fmt
                (fun (f, ty) ->
                  FMT.fieldname fmt f;
                  FMTUtils.nbsp fmt;
                  FMT.coloncolon fmt;
                  FMTUtils.nbsp fmt;
                  FMT.ty fmt ty;
                  FMT.semicolon fmt)
                fs))
  | Type_Enumeration es ->
      FMT.kw_enumeration fmt;
      FMTUtils.nbsp fmt;
      FMTUtils.braces fmt (fun _ ->
          FMTUtils.vbox fmt (fun _ ->
              FMTUtils.commasep fmt (FMT.varname fmt) es))
  | Type_Abbreviation ty -> FMT.ty fmt ty

type funtype =
  { funname : AST.ident;
    isArray : bool;
    params  : (AST.ident * AST.ty option) list;
    atys    : (AST.ident * AST.ty) list;
    rty     : AST.ty;
  }

let pp_funtype (fty : funtype) : unit =
  FMT.varname fmt fty.funname;
  FMTUtils.nbsp fmt;
  FMT.coloncolon fmt;
  FMTUtils.nbsp fmt;
  FMTUtils.braces fmt (fun _ -> FMT.parameters fmt fty.params);
  if fty.isArray then FMTUtils.brackets fmt (fun _ -> FMT.formals fmt fty.atys)
  else FMTUtils.parens fmt (fun _ -> FMT.formals fmt fty.atys);
  FMTUtils.nbsp fmt;
  FMT.eq_gt fmt;
  FMTUtils.nbsp fmt;
  FMT.ty fmt fty.rty

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
  val addType : t -> AST.l -> AST.ident -> typedef -> unit
  val getType : t -> AST.ident -> typedef option
  val isType : t -> AST.ident -> bool
  val isTycon : t -> AST.ident -> bool
  val isEnum : t -> AST.ident -> bool
  val addFuns : t -> AST.l -> AST.ident -> funtype list -> unit
  val getFuns : t -> AST.ident -> funtype list
  val addSetterFuns : t -> AST.ident -> funtype list -> unit
  val getSetterFun : t -> AST.ident -> funtype list
  val addOperators1 : t -> AST.l -> AST.unop -> funtype list -> unit
  val getOperators1 : t -> AST.l -> AST.unop -> funtype list
  val addOperators2 : t -> AST.l -> AST.binop -> funtype list -> unit
  val getOperators2 : t -> AST.l -> AST.binop -> funtype list
  val addGlobalVar : t -> AST.l -> AST.ident -> AST.ty -> bool -> unit
  val getGlobalVar : t -> AST.ident -> AST.ty option
  val addConstant : t -> AST.ident -> AST.expr -> unit
  val getConstant : t -> AST.ident -> AST.expr option
end = struct
  type t = {
    mutable types : typedef Bindings.t;
    mutable functions : funtype list Bindings.t;
    mutable setters : funtype list Bindings.t;
    mutable operators1 : funtype list Operators1.t;
    mutable operators2 : funtype list Operators2.t;
    mutable globals : AST.ty Bindings.t;
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

  let addType (env : t) (loc : AST.l) (qid : AST.ident) (t : typedef) : unit =
    (* Format.fprintf fmt "New type %s at %s\n" qid (pp_loc loc); *)
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

  let getType (env : t) (qid : AST.ident) : typedef option =
    Bindings.find_opt qid env.types

  let isType (env : t) (qid : AST.ident) : bool = true (* todo *)
  let isTycon (env : t) (qid : AST.ident) : bool = true (* todo *)
  let isEnum (env : t) (qid : AST.ident) : bool = true (* todo *)

  let addFuns (env : t) (loc : AST.l) (qid : AST.ident) (ftys : funtype list) :
      unit =
    env.functions <- Bindings.add qid ftys env.functions

  let getFuns (env : t) (qid : AST.ident) : funtype list =
    match Bindings.find_opt qid env.functions with
    | None -> []
    | Some tys -> tys

  let addSetterFuns (env : t) (qid : AST.ident) (ftys : funtype list) : unit =
    env.setters <- Bindings.add qid ftys env.setters

  let getSetterFun (env : t) (qid : AST.ident) : funtype list =
    match Bindings.find_opt qid env.setters with None -> [] | Some tys -> tys

  let addOperators1 (env : t) (loc : AST.l) (op : AST.unop)
      (funs : funtype list) : unit =
    env.operators1 <-
      Operators1.update op
        (fun ov ->
          let old = Option.value ov ~default:[] in
          Some (List.append funs old))
        env.operators1

  let getOperators1 (env : t) (loc : AST.l) (op : AST.unop) : funtype list =
    Option.value (Operators1.find_opt op env.operators1) ~default:[]

  let addOperators2 (env : t) (loc : AST.l) (op : AST.binop)
      (funs : funtype list) : unit =
    List.iter (function fty -> add_binop op fty.funname) funs;
    env.operators2 <-
      Operators2.update op
        (fun ov ->
          let old = Option.value ov ~default:[] in
          Some (List.append funs old))
        env.operators2

  let getOperators2 (env : t) (loc : AST.l) (op : AST.binop) : funtype list =
    Option.value (Operators2.find_opt op env.operators2) ~default:[]

  let addGlobalVar (env : t) (loc : AST.l) (qid : AST.ident) (ty : AST.ty)
      (isConstant : bool) : unit =
    (* Format.fprintf fmt "New %s %s at %s\n" (if isConstant then "constant" else "variable") qid (pp_loc loc); *)
    env.globals <- Bindings.add qid ty env.globals

  let getGlobalVar (env : t) (v : AST.ident) : AST.ty option =
    (* Format.fprintf fmt "Looking for global variable %s\n" (pprint_ident v); *)
    Bindings.find_opt v env.globals

  let getConstant (env : t) (v : AST.ident) : AST.expr option =
    Bindings.find_opt v env.constants

  let addConstant (env : t) (v : AST.ident) (e : AST.expr) : unit =
    let e' = subst_fun_expr (getConstant env) e in
    env.constants <- Bindings.add v e' env.constants
end

let subst_consts_expr (env : GlobalEnv.t) (e : AST.expr) : AST.expr =
  subst_fun_expr (GlobalEnv.getConstant env) e

let subst_consts_type (env : GlobalEnv.t) (ty : AST.ty) : AST.ty =
  subst_fun_type (GlobalEnv.getConstant env) ty

let isConstant (env : GlobalEnv.t) (v : AST.ident) : bool =
  GlobalEnv.getConstant env v <> None

let removeConsts (env : GlobalEnv.t) (ids : IdentSet.t) : IdentSet.t =
  IdentSet.filter (fun v -> not (isConstant env v)) ids

(** dereference typedef *)
let rec derefType (env : GlobalEnv.t) (ty : AST.ty) : AST.ty =
  match ty with
  | Type_Constructor tc | Type_App (tc, _) -> (
      match GlobalEnv.getType env tc with
      (* todo: instantiate with type parameters? *)
      | Some (Type_Abbreviation ty') -> derefType env ty'
      | _ -> ty)
  | _ -> ty

(** compare index types *)
let cmp_ixtype (ty1 : AST.ixtype) (ty2 : AST.ixtype) : bool =
  match (ty1, ty2) with
  | Index_Enum tc1, Index_Enum tc2 -> tc1 = tc2
  | Index_Range _, Index_Range _ -> true
  | _ -> false

(* todo: does not handle register<->bits coercions *)

(** structural match on two types - ignoring the dependent type part and constraints *)
let rec cmp_type (env : GlobalEnv.t) (ty1 : AST.ty) (ty2 : AST.ty) : bool =
  match (derefType env ty1, derefType env ty2) with
  | Type_Constructor c1, Type_Constructor c2 -> c1 = c2
  | Type_Integer _, Type_Integer _ -> true
  | Type_Bits e1, Type_Bits e2 -> true
  | Type_App (c1, es1), Type_App (c2, es2) -> c1 = c2
  | Type_OfExpr e1, Type_OfExpr e2 -> raise (InternalError "cmp_type: typeof")
  (* todo: this is equating the types, not subtyping them *)
  | Type_Bits e1, Type_Register (w2, _) -> true
  | Type_Register (w1, _), Type_Bits e2 -> true
  | Type_Register (w1, _), Type_Register (w2, _) -> true
  | Type_Array (ixty1, elty1), Type_Array (ixty2, elty2) ->
      cmp_ixtype ixty1 ixty2 && cmp_type env elty1 elty2
  | Type_Tuple tys1, Type_Tuple tys2 ->
      List.length tys1 = List.length tys2
      && List.for_all2 (cmp_type env) tys1 tys2
  | _ -> false

(****************************************************************)
(** {3 Field typechecking support}                              *)
(****************************************************************)

(** Field accesses can be either record fields or fields of registers

    This type captures the information needed to typecheck either of these
    - a list of fieldname/type pairs for records
    - a list of fieldname/slice pairs for registers
 *)
type fieldtypes =
  | FT_Record of (ident * ty) list
  | FT_Register of (AST.slice list * ident) list

(** Get fieldtype information for a record/register type *)
let rec typeFields (env : GlobalEnv.t) (loc : AST.l) (x : ty) : fieldtypes =
  match derefType env x with
  | Type_Constructor tc | Type_App (tc, _) -> (
      match GlobalEnv.getType env tc with
      | Some (Type_Record fs) -> FT_Record fs
      | Some (Type_Abbreviation ty') -> typeFields env loc ty'
      | _ -> raise (IsNotA (loc, "record", pprint_ident tc)))
  | Type_Register (wd, fs) -> FT_Register fs
  | Type_OfExpr e ->
      raise
        (InternalError
           ("typeFields: Type_OfExpr "
           ^ to_string2 (Fun.flip FMT.expr (ppp_expr e))))
  | _ -> raise (IsNotA (loc, "record/register", pp_type x))

(** Get fieldtype information for a named field of a record *)
let get_recordfield (loc : AST.l) (rfs : (ident * ty) list) (f : ident) : AST.ty
    =
  match List.filter (fun (fnm, _) -> fnm = f) rfs with
  | [ (_, fty) ] -> fty
  | [] -> raise (UnknownObject (loc, "field", pprint_ident f))
  | fs -> raise (Ambiguous (loc, "field", pprint_ident f))

(** Get fieldtype information for a named field of a slice *)
let get_regfield_info (loc : AST.l) (rfs : (AST.slice list * ident) list)
    (f : ident) : AST.slice list =
  match List.filter (fun (_, fnm) -> fnm = f) rfs with
  | [ (ss, _) ] -> ss
  | [] -> raise (UnknownObject (loc, "field", pprint_ident f))
  | fs -> raise (Ambiguous (loc, "field", pprint_ident f))

(** Get named field of a register and calculate type *)
let get_regfield (loc : AST.l) (rfs : (AST.slice list * ident) list) (f : ident)
    : AST.slice list * AST.ty =
  let ss = get_regfield_info loc rfs f in
  (ss, type_bits (slices_width ss))

(** Get named fields of a register and calculate type of concatenating them *)
let get_regfields (loc : AST.l) (rfs : (AST.slice list * ident) list)
    (fs : ident list) : AST.slice list * AST.ty =
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
  val nest_with_bindings : (t -> 'a) -> t -> 'a * (AST.ident * AST.ty) list
  val addLocalVar : t -> AST.l -> AST.ident -> AST.ty -> unit
  val getVar : t -> AST.ident -> (AST.ident * AST.ty) option
  val markModified : t -> AST.ident -> unit
  val addConstraint : t -> AST.l -> AST.expr -> unit
  val getConstraints : t -> AST.expr list
  val setReturnType : t -> AST.ty -> unit
  val getReturnType : t -> AST.ty option
end = struct
  type t = {
    globals : GlobalEnv.t;
    mutable rty : AST.ty option;
    (* a stack of nested scopes representing the local type environment *)
    (* Invariant: the stack is never empty *)
    mutable locals : AST.ty Bindings.t list;
    mutable modified : IdentSet.t;
    (* constraints collected while typechecking current expression/assignment *)
    mutable constraints : AST.expr list;
  }

  let mkEnv (globalEnv : GlobalEnv.t) =
    {
      globals = globalEnv;
      rty = None;
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

  let nest_with_bindings (k : t -> 'a) (parent : t) :
      'a * (AST.ident * AST.ty) list =
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
    let locals = Bindings.bindings (List.hd child.locals) in
    (r, locals)

  let addLocalVar (env : t) (loc : AST.l) (v : AST.ident) (ty : AST.ty) : unit =
    if GlobalEnv.getConstant env.globals v <> None then
      raise
        (TypeError (loc, pprint_ident v ^ " already declared as global constant"));
    (* Format.fprintf fmt "New local var %s : %s at %s\n" (pprint_ident v) (ppp_type ty) (pp_loc loc); *)
    (match env.locals with
    | bs :: bss -> env.locals <- Bindings.add v ty bs :: bss
    | [] -> raise (InternalError "addLocalVar"));
    env.modified <- IdentSet.add v env.modified

  let getVar (env : t) (v : AST.ident) : (AST.ident * AST.ty) option =
    (* Format.fprintf fmt "Looking for variable %s\n" (pprint_ident v); *)
    let rec search (bss : AST.ty Bindings.t list) : AST.ty option =
      match bss with
      | bs :: bss' ->
          orelse_option (Bindings.find_opt v bs) (fun _ -> search bss')
      | [] ->
          GlobalEnv.getGlobalVar env.globals v
    in
    Option.map (fun ty -> (v, ty)) (search env.locals)

  let markModified (env : t) (v : AST.ident) : unit =
    env.modified <- IdentSet.add v env.modified

  let addConstraint (env : t) (loc : AST.l) (c : AST.expr) : unit =
    env.constraints <- c :: env.constraints

  let getConstraints (env : t) : AST.expr list = env.constraints
  let setReturnType (env : t) (ty : AST.ty) : unit = env.rty <- Some ty
  let getReturnType (env : t) : AST.ty option = env.rty
end

(****************************************************************)
(** {2 Unification}                                             *)
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

let rec simplify_expr (x : AST.expr) : AST.expr =
  let eval (x : AST.expr) : Z.t option =
    match x with Expr_LitInt x' -> Some (Z.of_string x') | _ -> None
  in
  let to_expr (x : Z.t) : AST.expr = Expr_LitInt (Z.to_string x) in

  match x with
  | Expr_TApply (f, tes, es) -> (
      let es' = List.map simplify_expr es in
      match (f, flatten_map_option eval es') with
      | FIdent ("add_int", _), Some [ a; b ] -> to_expr (Z.add a b)
      | FIdent ("sub_int", _), Some [ a; b ] -> to_expr (Z.sub a b)
      | FIdent ("mul_int", _), Some [ a; b ] -> to_expr (Z.mul a b)
      | _ -> Expr_TApply (f, tes, es'))
  | _ -> x

(** Perform simple constant folding of expressions within a type *)
let simplify_type (x : AST.ty) : AST.ty =
  let repl = new replaceExprClass (fun e -> Some (simplify_expr e)) in
  Asl_visitor.visit_type repl x

(****************************************************************)
(** {3 Z3 support code}                                         *)
(****************************************************************)

(** Convert ASL expression to Z3 expression.
    This only copes with a limited set of operations: ==, +, -, * and DIV.
    (It is possible that we will need to extend this list in the future but
    it is sufficient for the current ASL specifications.)

    The support for DIV is not sound - it is a hack needed to cope with
    the way ASL code is written and generally needs a side condition
    that the division is exact (no remainder).

    ufs is a mutable list of conversions used to handle subexpressions
    that cannot be translated.  We treat such subexpressions as
    uninterpreted functions and add them to the 'ufs' list so that
    we can reason that "F(x) == F(x)" without knowing "F".
 *)

let rec z3_of_expr (ctx : Z3.context) (ufs : (AST.expr * Z3.Expr.expr) list ref)
    (x : AST.expr) : Z3.Expr.expr =
  match x with
  | Expr_Var v ->
      let intsort = Z3.Arithmetic.Integer.mk_sort ctx in
      Z3.Expr.mk_const_s ctx (pprint_ident v) intsort
  | Expr_Parens y -> z3_of_expr ctx ufs y
  | Expr_LitInt i -> Z3.Arithmetic.Integer.mk_numeral_s ctx i
  (* todo: the following lines involving DIV are not sound *)
  | Expr_TApply
      ( FIdent ("mul_int", _),
        [],
        [ Expr_TApply (FIdent ("fdiv_int", _), [], [ a; b ]); c ] )
    when b = c ->
      z3_of_expr ctx ufs a
  | Expr_TApply
      ( FIdent ("mul_int", _),
        [],
        [ a; Expr_TApply (FIdent ("fdiv_int", _), [], [ b; c ]) ] )
    when a = c ->
      z3_of_expr ctx ufs b
  | Expr_TApply
      ( FIdent ("add_int", _),
        [],
        [
          Expr_TApply (FIdent ("fdiv_int", _), [], [ a1; b1 ]);
          Expr_TApply (FIdent ("fdiv_int", _), [], [ a2; b2 ]);
        ] )
    when a1 = a2 && b1 = b2 && b1 = Expr_LitInt "2" ->
      z3_of_expr ctx ufs a1
  | Expr_TApply
      ( FIdent ("eq_int", _),
        [],
        [ a; Expr_TApply (FIdent ("fdiv_int", _), [], [ b; c ]) ] ) ->
      Z3.Boolean.mk_eq ctx
        (Z3.Arithmetic.mk_mul ctx
           [ z3_of_expr ctx ufs c; z3_of_expr ctx ufs a ])
        (z3_of_expr ctx ufs b)
  | Expr_TApply (FIdent ("add_int", _), [], xs) ->
      Z3.Arithmetic.mk_add ctx (List.map (z3_of_expr ctx ufs) xs)
  | Expr_TApply (FIdent ("sub_int", _), [], xs) ->
      Z3.Arithmetic.mk_sub ctx (List.map (z3_of_expr ctx ufs) xs)
  | Expr_TApply (FIdent ("mul_int", _), [], xs) ->
      Z3.Arithmetic.mk_mul ctx (List.map (z3_of_expr ctx ufs) xs)
  | Expr_TApply (FIdent ("fdiv_int", _), [], [ a; b ]) ->
      Z3.Arithmetic.mk_div ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | Expr_TApply (FIdent ("eq_int", _), [], [ a; b ]) ->
      Z3.Boolean.mk_eq ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | _ -> (
      if verbose then
        Format.fprintf fmt
          "    Unable to translate %s - using as uninterpreted function\n"
          (pp_expr x);
      let intsort = Z3.Arithmetic.Integer.mk_sort ctx in
      match List.assoc_opt x !ufs with
      | None ->
          let uf = Z3.Expr.mk_fresh_const ctx "UNINTERPRETED" intsort in
          ufs := (x, uf) :: !ufs;
          uf
      | Some uf -> uf)

(** check that bs => cs *)
let check_constraints (bs : expr list) (cs : expr list) : bool =
  (* note that we rebuild the Z3 context each time.
   * It is possible to share them across all invocations to save
   * about 10% of execution time.
   *)
  let z3_ctx = Z3.mk_context [] in
  let solver = Z3.Solver.mk_simple_solver z3_ctx in
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
  Z3.Solver.add solver [ Z3.Boolean.mk_not z3_ctx p ];
  let q = Z3.Solver.check solver [] in
  if verbose && q = SATISFIABLE then
    Format.fprintf fmt "Failed property %s\n" (Z3.Expr.to_string p);
  q = UNSATISFIABLE

(****************************************************************)
(** {3 Unification support code}                                *)
(****************************************************************)

(** Unifier

    This class supports collecting all the constraints introduced while
    typechecking an expression, checking those constraints
    and synthesizing a solution.

    This is the most complex part of the entire typechecker.
    Most of that complexity is the result of having to support
    code like

        bits(64) x = ZeroExtend(R[i]);

    where the width of the ZeroExtend call is determined by
    the context that it occurs in.
 *)
class unifier (loc : AST.l) (assumptions : expr list) =
  object (self)
    (* unification results in bindings of the form "$i == $j".
     * We use a renaming structure to track equivalence classes
     * and to pick a canonical member of each equivalence class
     *)
    val mutable renamings = new equivalences
    val mutable bindings : AST.expr Bindings.t = Bindings.empty
    val mutable constraints : AST.expr list = []
    val mutable next = 0

    method fresh : ident =
      let v = genericTyvar next in
      ignore (renamings#canonicalize v);
      (* add v to rename table *)
      next <- next + 1;
      v

    method isFresh (x : ident) : bool = isGenericTyvar x

    method addEquality (x : AST.expr) (y : AST.expr) : unit =
      match (x, y) with
      | Expr_Var v, Expr_Var w when self#isFresh v && self#isFresh w ->
          renamings#merge v w
      | Expr_Var v, _ when self#isFresh v && not (Bindings.mem v bindings) ->
          bindings <- Bindings.add v y bindings
      | _, Expr_Var w when self#isFresh w && not (Bindings.mem w bindings) ->
          bindings <- Bindings.add w x bindings
      | _ -> constraints <- mk_eq_int x y :: constraints

    method addEqualities (xs : AST.expr list) (ys : AST.expr list) =
      if List.length xs = List.length ys then List.iter2 self#addEquality xs ys

    method checkConstraints : expr Bindings.t =
      (* Plan:
       * - Generate renaming that maps each fresh var to canonical
       *   representative of equivalence class.
       * - Collect all the bindings associated with each equivalence class.
       * - For each equivalence class, check that there is at least
       *   one closed binding for that equivalence class and
       *   add all the others as constraints.
       *   (A "closed binding" is a binding that does not contain any fresh
       *   variables - construct it by substituting other closed bindings
       *   into a binding.)
       * - If any equivalence class has no closed bindings, report an error.
       *)
      let rns = renamings#mapping in
      let classes = renamings#classes in

      (* map each canonical representative to set of bindings *)
      let binds =
        Bindings.map
          (fun vs ->
            flatmap_option
              (fun v -> Bindings.find_opt v bindings)
              (IdentSet.elements vs))
          classes
      in

      if verbose then
        FMTUtils.vbox fmt (fun _ ->
            Format.fprintf fmt "- Checking Constraints at %s\n" (pp_loc loc);
            FMTUtils.vbox fmt (fun _ ->
                Bindings.iter
                  (fun v e ->
                    Format.fprintf fmt "Old Bind: ";
                    FMT.varname fmt v;
                    FMT.delimiter fmt " -> ";
                    FMT.expr fmt (ppp_expr e);
                    FMTUtils.cut fmt)
                  bindings;
                Bindings.iter
                  (fun v es ->
                    List.iter
                      (fun e ->
                        Format.fprintf fmt "binds: ";
                        FMT.varname fmt v;
                        FMT.delimiter fmt " -> ";
                        FMT.expr fmt (ppp_expr e);
                        pp_print_space fmt ())
                      es)
                  binds);
            renamings#pp fmt "Renaming: ";
            Bindings.iter
              (fun v w ->
                Format.fprintf fmt "- renaming: ";
                FMT.varname fmt v;
                FMT.delimiter fmt " -> ";
                FMT.varname fmt w;
                FMTUtils.cut fmt)
              rns);

      let isClosed (x : expr) : bool =
        IdentSet.is_empty (IdentSet.filter self#isFresh (fv_expr x))
      in

      (* todo: memoize close_ident to improve performance - should probably
         profile first *)
      (* search for a closed binding for a variable x by testing whether any of
         the available bindings can be closed *)
      let rec close_ident (x : ident) : expr =
        let x' = renamings#canonicalize x in
        match
          Option.bind (Bindings.find_opt x' binds) (fun es ->
              first_option close_expr es)
        with
        | Some e -> e
        | None ->
            Format.fprintf fmt "Type Error at %s\n" (pp_loc loc);
            (* if verbose then begin
                   List.iter (fun v -> Format.fprintf fmt "  Related to: %s\n" (pprint_ident v)) (IdentSet.elements (Bindings.find x' classes));
                   List.iter (fun e -> Format.fprintf fmt "  Candidate: %s\n" (pp_expr e)) (Bindings.find x' binds);
                   renamings#pp "  Renaming: ";
                   Bindings.iter (fun v e -> Format.fprintf fmt "  Bind: %s -> %s\n" (pprint_ident v) (ppp_expr e)) bindings
               end; *)
            raise
              (TypeError
                 ( loc,
                   "Unable to infer value of type parameter " ^ pprint_ident x'
                 ))
      (* attempt to close an expression by replacing all fresh vars with a
         closed expression *)
      and close_expr (x : expr) : expr option =
        let subst =
          new substFunClass (fun x ->
              if self#isFresh x then Some (close_ident x) else None)
        in
        let x' = Asl_visitor.visit_expr subst x in
        if isClosed x' then Some x' else None
      in

      (* map of each canonical member to a closed expression *)
      let pre_closed = Bindings.mapi (fun k _ -> close_ident k) classes in

      (* extend map to all type variables *)
      let closed = Bindings.map (fun v -> Bindings.find v pre_closed) rns in

      if verbose then (
        Bindings.iter
          (fun v e ->
            Format.fprintf fmt "      PreClosed Bind: %s -> %s\n"
              (pprint_ident v)
              (pp_expr (ppp_expr e)))
          pre_closed;
        Bindings.iter
          (fun v e ->
            Format.fprintf fmt "      Closed Bind: %s -> %s\n" (pprint_ident v)
              (pp_expr (ppp_expr e)))
          closed);

      constraints <- List.map (subst_expr closed) constraints;

      (* turn all old bindings into constraints *)
      let new_constraints =
        List.map
          (fun (v, e) ->
            mk_eq_int (Bindings.find v closed) (subst_expr closed e))
          (Bindings.bindings bindings)
      in
      constraints <- new_constraints @ constraints;
      bindings <- closed;

      if verbose then (
        List.iter
          (fun c ->
            Format.fprintf fmt "      OldConstraint: %s\n"
              (pp_expr (ppp_expr c)))
          constraints;
        List.iter
          (fun c ->
            Format.fprintf fmt "      NewConstraint: %s\n"
              (pp_expr (ppp_expr c)))
          new_constraints;
        List.iter
          (fun c ->
            Format.fprintf fmt "      Constraint: %s\n" (pp_expr (ppp_expr c)))
          constraints);

      constraints <- List.map simplify_expr constraints;

      (* as a minor optimisation and also to declutter error messages, delete
         equalities that are obviously satisfied *)
      constraints <-
        List.filter
          (function
            | Expr_TApply (FIdent ("eq_int", _), [], [ x; y ]) -> x <> y
            | _ -> true)
          constraints;

      (* The optimisation of not invoking solver if there are no constraints
       * improves runtime by a factor of 6x
       *)
      if constraints <> [] && not (check_constraints assumptions constraints)
      then (
        Format.fprintf fmt "Type Error at %s\n" (pp_loc loc);
        if verbose then (
          renamings#pp fmt "      Renaming: ";
          Bindings.iter
            (fun v e ->
              Format.fprintf fmt "      Bind: %s -> %s\n" (pprint_ident v)
                (pp_expr (ppp_expr e)))
            bindings);
        List.iter
          (fun c ->
            Format.fprintf fmt "      Constraint: ";
            FMT.expr fmt (ppp_expr c);
            FMTUtils.cut fmt)
          constraints;
        flush stdout;
        raise (TypeError (loc, "Type mismatch")));

      bindings
  end

(** Create a fresh unifier, invoke a function that uses the unifier and check
    that the constraints are satisfied.
    Returns the synthesized bindings and result of function
 *)
let with_unify (env : Env.t) (loc : AST.l) (f : unifier -> 'a) :
    expr Bindings.t * 'a =
  let u = new unifier loc (Env.getConstraints env) in
  let r = f u in
  let bs = u#checkConstraints in
  (bs, r)

(****************************************************************)
(** {3 Type Unification}                                        *)
(****************************************************************)

(** Notes on how type inference works:

    - we use structural matching (ignoring the dependent type)
      to disambiguate each binop/unop/function/procedure call/getter/setter

    - as we construct each TApply node,
      - we insert fresh type variables $0, $1, ... for each of the type arguments
        (these are things we are going to solve for)
      - unification generates two kinds of constraints:
        1. bindings for type variables whenever unification requires "$i == e" or "e == $i"
           for some type variable $i
        2. constraints where there are multiple bindings for a single variable
        3. constraints on type variables whenever unification requires "e1 == e2"
           where e1 is not a variable

    - after scanning an entire assignment/expression, we check:
        1. do we have at least one binding for each variable?
        2. are the bindings consistent with the constraints?
      Note that we could potentially give better (more localized) type errors if
      we check for consistency as we go along and if we check that a variable
      is bound as soon as the result type could not possibly involve the variable.
      (e.g., when checking "(e1 == e2 && Q) || R", any type variables associated
      with the equality check do not impact the && or || because "boolean" does
      not have any type parameters.)

    Note that there is a choice of what type arguments to add to a function

        bits(N) ZeroExtend(bits(M) x, integer N)

    We can either:
    - add only the missing information "M"
      In effect, we are saying that missing type parameters are implicit parameters that are
      added by the type inference process and that the "type parameters" are basically just
      value expressions that are added by type inference.
    - add type arguments for both "M" and "N".
      In effect we are saying that type parameters are distinct from value parameters
      and we are in the strange situation that a function could have both a value
      parameter M and a type parameter N and they might be bound to different (but
      equivalent) arguments.
      This is what archex does.
 *)

(** Unify two index types *)
let unify_ixtype (u : unifier) (ty1 : AST.ixtype) (ty2 : AST.ixtype) : unit =
  match (ty1, ty2) with
  | Index_Enum tc1, Index_Enum tc2 -> ()
  | Index_Range (lo1, hi1), Index_Range (lo2, hi2) ->
      u#addEquality lo1 lo2;
      u#addEquality hi1 hi2
  | _ -> ()

(* todo: does not handle register<->bits coercions *)

(** Unify two types

    This performs a structural match on two types - ignoring the dependent type part
 *)
let rec unify_type (env : GlobalEnv.t) (u : unifier) (ty1 : AST.ty)
    (ty2 : AST.ty) : unit =
  (* Substitute global constants in types *)
  let subst_consts = new substFunClass (GlobalEnv.getConstant env) in
  let ty1' = Asl_visitor.visit_type subst_consts ty1 in
  let ty2' = Asl_visitor.visit_type subst_consts ty2 in
  match (derefType env ty1', derefType env ty2') with
  | Type_Constructor c1, Type_Constructor c2 -> ()
  | Type_Bits e1, Type_Bits e2 -> u#addEquality e1 e2
  | Type_App (c1, es1), Type_App (c2, es2) -> u#addEqualities es1 es2
  | Type_OfExpr e1, Type_OfExpr e2 -> raise (InternalError "unify_type: typeof")
  (* todo: this is equating the types, not subtyping them *)
  | Type_Bits e1, Type_Register (e2, _) -> u#addEquality e1 e2
  | Type_Register (e1, _), Type_Bits e2 -> u#addEquality e1 e2
  | Type_Register (e1, _), Type_Register (e2, _) -> u#addEquality e1 e2
  | Type_Array (ixty1, elty1), Type_Array (ixty2, elty2) ->
      unify_ixtype u ixty1 ixty2;
      unify_type env u elty1 elty2
  | Type_Tuple tys1, Type_Tuple tys2 -> List.iter2 (unify_type env u) tys1 tys2
  | _ -> ()

(** Apply substitutions to an expression *)
let unify_subst_e (s : expr Bindings.t) (x : AST.expr) : AST.expr =
  subst_expr s x

(** Apply substitutions to an L-expression *)
let unify_subst_le (s : expr Bindings.t) (x : AST.lexpr) : AST.lexpr =
  subst_lexpr s x

(** Apply substitutions to a type *)
let unify_subst_ty (s : expr Bindings.t) (x : AST.ty) : AST.ty = subst_type s x

(** Apply substitutions to a decl_item *)
let unify_subst_di (s : expr Bindings.t) (x : AST.decl_item) : AST.decl_item = subst_decl_item s x

(** Replace all type variables in function type with fresh variables *)
let mkfresh_funtype (u : unifier) (fty : funtype) : funtype =
  (* generate renamings for all type variables *)
  let rns = List.map (fun (tv, _) -> (tv, u#fresh)) fty.params in
  let s = mk_bindings (List.map (fun (v, w) -> (v, Expr_Var w)) rns) in

  let ps' =
    List.map
      (fun (a, oty) ->
        let ty = Option.get oty in
        let ty' = subst_type s ty in
        let a' = Option.value (List.assoc_opt a rns) ~default:a in
        (a', Some ty'))
      fty.params
  in
  let atys' =
    List.map
      (fun (a, ty) ->
        let ty' = subst_type s ty in
        let a' = Option.value (List.assoc_opt a rns) ~default:a in
        (a', ty'))
      fty.atys
  in
  let rty' = subst_type s fty.rty in
  { funname = fty.funname; isArray = fty.isArray; params = ps'; atys = atys'; rty = rty' }

(** Check that ty2 is a subtype of ty1: ty1 >= ty2 *)
let check_type (env : Env.t) (u : unifier) (loc : AST.l) (ty1 : AST.ty)
    (ty2 : AST.ty) : unit =
  if not (cmp_type (Env.globals env) ty1 ty2) then
    raise (DoesNotMatch (loc, "type", pp_type ty1, pp_type ty2))
  else unify_type (Env.globals env) u ty1 ty2

(* todo: make sure that this does not do subtyping *)

(** Check that ty1 is identical to ty2 *)
let check_type_exact (env : Env.t) (loc : AST.l) (ty1 : AST.ty) (ty2 : AST.ty) :
    unit =
  ignore (with_unify env loc (fun u -> check_type env u loc ty1 ty2))

(****************************************************************)
(** {2 Disambiguation of functions and operators}               *)
(****************************************************************)

(** Generate error message when function disambiguation fails *)
let reportChoices (loc : AST.l) (what : string) (nm : string)
    (tys : AST.ty list) (funs : funtype list) : unit =
  FMT.loc fmt loc;
  FMT.colon fmt;
  FMTUtils.nbsp fmt;
  if funs = [] then
    Format.pp_print_string fmt ("Can't find matching " ^ what ^ " for " ^ nm)
  else Format.pp_print_string fmt ("Ambiguous choice for " ^ what ^ " " ^ nm);
  FMTUtils.cut fmt;
  FMTUtils.vbox fmt (fun _ ->
      List.iter
        (fun ty ->
          Format.fprintf fmt "Arg : ";
          FMT.ty fmt ty;
          FMTUtils.cut fmt)
        tys;
      List.iter
        (fun fty ->
          FMT.funname fmt fty.funname;
          FMT.colon fmt;
          FMTUtils.nbsp fmt;
          FMTUtils.hlist fmt
            (fun _ -> FMTUtils.nbsp fmt)
            (fun (_, t) -> FMT.ty fmt t)
            fty.atys;
          FMT.delimiter fmt " -> ";
          FMT.ty fmt fty.rty)
        funs)

(** Check whether a list of function argument types is compatible with the
    type of a function.

    One function type is compatible with another if they have the same number
    of arguments and each argument has the same base type
 *)
let isCompatibleFunction (env : GlobalEnv.t) (isArr : bool) (tys : AST.ty list)
    (fty : funtype) : bool =
  let nargs = List.length tys in
  isArr = fty.isArray
  && List.length fty.atys = nargs
  && List.for_all2 (cmp_type env) (List.map snd fty.atys) tys

(** Disambiguate a function name based on the number and type of arguments *)
let chooseFunction (env : GlobalEnv.t) (loc : AST.l) (what : string)
    (nm : string) (isArr : bool) (tys : AST.ty list) (funs : funtype list) :
    funtype option =
  let funs' = List.filter (isCompatibleFunction env isArr tys) funs in
  match nub funs' with
  | [] -> None
  | [ r ] -> Some r
  | fs ->
      (* todo: it would probably be better to detect ambiguity when functions are
       * defined instead of waiting until they are called
       *)
      reportChoices loc what nm tys fs;
      raise (Ambiguous (loc, what, nm))

(** Instantiate type of function using unifier 'u' *)
let instantiate_fun (env : GlobalEnv.t) (u : unifier) (loc : AST.l)
    (fty : funtype) (es : AST.expr list) (tys : AST.ty list) :
    AST.ident * AST.expr list * AST.ty =
  let fty' = mkfresh_funtype u fty in

  (* Add bindings for every explicit type argument *)
  assert (List.length fty'.atys = List.length es);
  List.iter2
    (fun (v, _) e ->
      if List.mem_assoc v fty'.params then
        u#addEquality (Expr_Var v) (subst_consts_expr env e))
    fty'.atys es;

  (* unify argument types *)
  assert (List.length fty'.atys = List.length tys);
  List.iter2 (unify_type env u) (List.map snd fty'.atys) tys;

  let tes = List.map (fun (tv, _) -> Expr_Var tv) fty'.params in
  (fty'.funname, tes, fty'.rty)

(** Disambiguate and typecheck application of a function to a list of arguments *)
let tc_apply (env : GlobalEnv.t) (u : unifier) (loc : AST.l) (what : string)
    (f : AST.ident) (es : AST.expr list) (tys : AST.ty list) :
    AST.ident * AST.expr list * AST.ty =
  let funs = GlobalEnv.getFuns env f in
  let nm = pprint_ident f in
  match chooseFunction env loc "function" nm false tys funs with
  | None ->
      reportChoices loc what nm tys funs;
      raise (UnknownObject (loc, what, nm))
  | Some fty ->
      (* if verbose then Format.fprintf fmt "    - Found matching %s at %s for %s = %s\n" what (pp_loc loc) nm (Utils.to_string (pp_funtype fty)); *)
      instantiate_fun env u loc fty es tys

(** Disambiguate and typecheck application of a unary operator to argument *)
let tc_unop (env : GlobalEnv.t) (u : unifier) (loc : AST.l) (op : unop)
    (x : AST.expr) (ty : AST.ty) : AST.ident * AST.expr list * AST.ty =
  let what = "unary operator" in
  let nm = pp_unop op in
  let tys = [ ty ] in
  let ops = GlobalEnv.getOperators1 env loc op in
  match chooseFunction env loc what nm false [ ty ] ops with
  | None ->
      reportChoices loc what nm tys ops;
      raise (UnknownObject (loc, what, nm))
  | Some fty -> instantiate_fun env u loc fty [ x ] tys

(** Disambiguate and typecheck application of a binary operator to arguments *)
let tc_binop (env : GlobalEnv.t) (u : unifier) (loc : AST.l) (op : binop)
    (x1 : AST.expr) (x2 : AST.expr) (ty1 : AST.ty) (ty2 : AST.ty) :
    AST.ident * AST.expr list * AST.ty =
  let what = "binary operator" in
  let nm = pp_binop op in
  let tys = [ ty1; ty2 ] in
  let ops = GlobalEnv.getOperators2 env loc op in
  match chooseFunction env loc what nm false tys ops with
  | None ->
      reportChoices loc "binary operator" nm tys ops;
      raise (UnknownObject (loc, what, nm))
  | Some fty -> instantiate_fun env u loc fty [ x1; x2 ] tys

(****************************************************************)
(** {2 Typecheck expressions}                                   *)
(****************************************************************)

(** Lookup a variable in environment *)
let check_var (env : Env.t) (loc : AST.l) (v : AST.ident) : AST.ident * AST.ty =
  match Env.getVar env v with
  | None -> raise (UnknownObject (loc, "variable", pprint_ident v))
  | Some (v', ty') -> (v', ty')

(** Typecheck list of expressions *)
let rec tc_exprs (env : Env.t) (u : unifier) (loc : AST.l) (xs : AST.expr list)
    : (AST.expr * AST.ty) list =
  List.map (tc_expr env u loc) xs

(** Typecheck expression and check that it is a subtype of ty *)
and check_expr (env : Env.t) (loc : AST.l) (ty : AST.ty) (x : AST.expr) :
    AST.expr =
  let s, x' =
    with_unify env loc (fun u ->
        let x', ty' = tc_expr env u loc x in
        if verbose then
          Format.fprintf fmt "    - Typechecking %s : %s\n" (pp_expr x')
            (pp_type ty');
        check_type env u loc ty ty';
        x')
  in
  unify_subst_e s x'

(** Typecheck 'if c then expr' *)
and tc_e_elsif (env : Env.t) (u : unifier) (loc : AST.l) (x : AST.e_elsif) :
    AST.e_elsif * AST.ty =
  match x with
  | E_Elsif_Cond (c, e) ->
      let c' = check_expr env loc type_bool c in
      let e', ty = tc_expr env u loc e in
      (E_Elsif_Cond (c', e'), ty)

(** Typecheck bitslice indices *)
and tc_slice (env : Env.t) (u : unifier) (loc : AST.l) (x : AST.slice) :
    AST.slice * AST.ty =
  match x with
  | Slice_Single e ->
      let e', ty = tc_expr env u loc e in
      (Slice_Single e', ty)
  | Slice_HiLo (hi, lo) ->
      let hi' = check_expr env loc type_integer hi in
      let lo' = check_expr env loc type_integer lo in
      (Slice_HiLo (hi', lo'), type_integer)
  | Slice_LoWd (lo, wd) ->
      let lo' = check_expr env loc type_integer lo in
      let wd' = check_expr env loc type_integer wd in
      (Slice_LoWd (lo', wd'), type_integer)

(** Typecheck pattern against type ty *)
and tc_pattern (env : Env.t) (loc : AST.l) (ty : AST.ty) (x : AST.pattern) :
    AST.pattern =
  match x with
  | Pat_LitInt l ->
      check_type_exact env loc ty type_integer;
      Pat_LitInt l
  | Pat_LitHex l ->
      check_type_exact env loc ty type_integer;
      Pat_LitHex l
  | Pat_LitBits l ->
      check_type_exact env loc ty (type_bitsK (string_of_int (masklength l)));
      Pat_LitBits l
  | Pat_LitMask l ->
      check_type_exact env loc ty (type_bitsK (string_of_int (masklength l)));
      Pat_LitMask l
  | Pat_Const l ->
      let c, cty = check_var env loc l in
      (* todo: check it is a global constant *)
      check_type_exact env loc ty cty;
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
  | Pat_Single (Expr_LitMask m) ->
      (* todo: this is a workaround for bad IN sugar *)
      tc_pattern env loc ty (Pat_LitMask m)
  | Pat_Single e ->
      let e' = check_expr env loc ty e in
      Pat_Single e'
  | Pat_Range (lo, hi) ->
      let lo' = check_expr env loc ty lo in
      let hi' = check_expr env loc ty hi in
      (* Must be integer because no other type supports <= operator *)
      check_type_exact env loc ty type_integer;
      Pat_Range (lo', hi')

(** Typecheck bitslice syntax
    This primarily consists of disambiguating between array indexing and bitslicing
    Note that this function is almost identical to tc_slice_lexpr
 *)
and tc_slice_expr (env : Env.t) (u : unifier) (loc : AST.l) (x : expr)
    (ss : (AST.slice * AST.ty) list) : AST.expr * AST.ty =
  if List.length ss = 0 then raise (TypeError (loc, "empty list of subscripts"));
  let ss' = List.map fst ss in
  let x', ty = tc_expr env u loc x in
  match derefType (Env.globals env) ty with
  | Type_Array (ixty, elty) -> (
      match ss with
      | [ (Slice_Single i, ity) ] ->
          check_type env u loc (ixtype_basetype ixty) ity;
          (Expr_Array (x', i), elty)
      | _ -> raise (TypeError (loc, "multiple subscripts for array")))
  | Type_Bits n ->
      let ty = type_bits (slices_width ss') in
      (mk_expr_slices x' ss' ty, ty)
  | Type_Register (wd, _) ->
      let ty = type_bits (slices_width ss') in
      (mk_expr_slices x' ss' ty, ty)
  | Type_Integer _ ->
      (* todo: desugar into a call to slice_int? *)
      let ty = type_bits (slices_width ss') in
      (mk_expr_intslices x' ss', ty)
  | _ -> raise (TypeError (loc, "slice of expr"))

(** Typecheck expression *)
and tc_expr (env : Env.t) (u : unifier) (loc : AST.l) (x : AST.expr) :
    AST.expr * AST.ty =
  match x with
  | Expr_If (c, t, els, e) ->
      let c' = check_expr env loc type_bool c in
      let t', tty = tc_expr env u loc t in
      let els', eltys = List.split (List.map (tc_e_elsif env u loc) els) in
      let e', ety = tc_expr env u loc e in
      List.iter (fun elty -> check_type env u loc tty elty) eltys;
      check_type env u loc tty ety;
      (Expr_If (c', t', els', e'), tty)
  | Expr_Binop (x, Binop_Eq, Expr_LitMask y) ->
      (* syntactic sugar *)
      tc_expr env u loc (Expr_In (x, Pat_LitMask y))
  | Expr_Binop (x, Binop_NtEq, Expr_LitMask y) ->
      (* syntactic sugar *)
      tc_expr env u loc (Expr_Unop (Unop_BoolNot, Expr_In (x, Pat_LitMask y)))
  | Expr_Binop (x, op, y) ->
      let x', xty = tc_expr env u loc x in
      let y', yty = tc_expr env u loc y in
      let f, tes, ty = tc_binop (Env.globals env) u loc op x' y' xty yty in
      (Expr_TApply (f, tes, [ x'; y' ]), ty)
  | Expr_Field (e, f) -> (
      let e', ty = tc_expr env u loc e in
      match typeFields (Env.globals env) loc ty with
      | FT_Record rfs -> (Expr_Field (e', f), get_recordfield loc rfs f)
      | FT_Register rfs ->
          let ss, ty' = get_regfield loc rfs f in
          (mk_expr_slices e' ss ty', ty'))
  | Expr_Fields (e, fs) -> (
      let e', ty = tc_expr env u loc e in
      match typeFields (Env.globals env) loc ty with
      | FT_Record rfs ->
          let tys = List.map (get_recordfield loc rfs) fs in
          (Expr_Fields (e', fs), mk_concat_tys tys)
      | FT_Register rfs ->
          let ss, ty' = get_regfields loc rfs fs in
          (mk_expr_slices e' ss ty', ty'))
  | Expr_Slices (e, ss) -> (
      let all_single =
        List.for_all (function Slice_Single _ -> true | _ -> false) ss
      in
      let ss' = List.map (tc_slice env u loc) ss in

      (* Note that the order of the following check is critical:
       * First check for getter functions then check for arrays or bitvectors because
       * of conflicting names like SPSR and SPSR[] in the v8-A specification.
       *)

      (* variable slice or getter call? *)
      match e with
      | Expr_Var a -> (
          let tys = List.map (function _, ty -> ty) ss' in
          let getters =
            GlobalEnv.getFuns (Env.globals env) (addSuffix a "read")
          in
          let ogetters =
            chooseFunction (Env.globals env) loc "getter function"
              (pprint_ident a) true tys getters
          in
          match ogetters with
          | Some fty when all_single ->
              let es =
                List.map
                  (function
                    | Slice_Single a, _ -> a
                    | _ -> raise (InternalError "Expr_Slices"))
                  ss'
              in
              let f', tes', rty =
                instantiate_fun (Env.globals env) u loc fty es tys
              in
              (Expr_TApply (f', tes', es), rty)
          | _ -> tc_slice_expr env u loc e ss')
      | _ -> tc_slice_expr env u loc e ss')
  | Expr_RecordInit (tc, fas) ->
      if not (GlobalEnv.isType (Env.globals env) tc) then
        raise (IsNotA (loc, "type constructor", pprint_ident tc));
      let fs =
        match GlobalEnv.getType (Env.globals env) tc with
        | Some (Type_Record fs) -> fs
        | _ -> raise (IsNotA (loc, "record type", pprint_ident tc))
      in
      (* todo: check that fas has exactly one field assignment for every field
         in fs *)
      let fas' =
        List.map
          (fun (f, e) ->
            let ty = get_recordfield loc fs f in
            let e' = check_expr env loc ty e in
            (f, e'))
          fas
      in
      (Expr_RecordInit (tc, fas'), Type_Constructor tc)
  | Expr_In (e, p) ->
      let s, (e', ety') = with_unify env loc (fun u -> tc_expr env u loc e) in
      let e'' = unify_subst_e s e' in
      let ety'' = unify_subst_ty s ety' in
      if verbose then
        Format.fprintf fmt "    - Typechecking %s IN ... : %s\n" (pp_expr e')
          (pp_type ety');
      let p' = tc_pattern env loc ety'' p in
      (Expr_In (e'', p'), type_bool)
  | Expr_Var v -> (
      match Env.getVar env v with
      | Some (v', ty') -> (Expr_Var v', ty')
      | None -> (
          let getters =
            GlobalEnv.getFuns (Env.globals env) (addSuffix v "read")
          in
          match
            chooseFunction (Env.globals env) loc "getter function"
              (pprint_ident v) false [] getters
          with
          | Some fty ->
              let f', tes', rty =
                instantiate_fun (Env.globals env) u loc fty [] []
              in
              (Expr_TApply (f', tes', []), rty)
          | None ->
              raise
                (UnknownObject
                   (loc, "variable or getter functions", pprint_ident v))))
  | Expr_Parens e ->
      let e', ty = tc_expr env u loc e in
      (Expr_Parens e', ty)
  | Expr_TApply (f, tes, es) ->
      let es', tys = List.split (tc_exprs env u loc es) in
      let f', tes'', ty =
        tc_apply (Env.globals env) u loc "function" f es' tys
      in
      (Expr_TApply (f', tes'', es'), ty)
  | Expr_Tuple es ->
      let es', tys = List.split (List.map (tc_expr env u loc) es) in
      (Expr_Tuple es', Type_Tuple tys)
  | Expr_Concat (_, es) ->
      let es', tys = List.split (List.map (tc_expr env u loc) es) in
      let ws = List.map width_of_type tys in
      (Expr_Concat (ws, es'), mk_concat_tys tys)
  | Expr_Unop (op, e) ->
      let e', ety = tc_expr env u loc e in
      (* Format.fprintf fmt "%s: unop %s : %s\n" (pp_loc loc) (pp_expr e) (pp_type ety); *)
      let f, tes, ty = tc_unop (Env.globals env) u loc op e ety in
      (Expr_TApply (f, tes, [ e' ]), ty)
  | Expr_Unknown t ->
      let ty' = tc_type env loc t in
      (Expr_Unknown ty', ty')
  | Expr_ImpDef (os, t) ->
      let ty' = tc_type env loc t in
      (Expr_ImpDef (os, ty'), ty')
  | Expr_Array (a, e) -> (
      let a', ty = tc_expr env u loc a in
      match derefType (Env.globals env) ty with
      | Type_Array (ixty, elty) ->
          let e' = check_expr env loc (ixtype_basetype ixty) e in
          (Expr_Array (a', e'), elty)
      | _ -> raise (TypeError (loc, "subscript of non-array")))
  | Expr_LitInt i -> (Expr_LitInt i, type_integer)
  | Expr_LitHex i -> (Expr_LitHex i, type_integer)
  | Expr_LitReal r -> (Expr_LitReal r, type_real)
  | Expr_LitBits b -> (Expr_LitBits b, type_bitsK (string_of_int (masklength b)))
  | Expr_LitMask b ->
      (* todo: this case only exists because of the (bad) sugar of
       * writing "x == '0x'" instead of "x IN '0x'"
       *)
      raise (InternalError "tc_expr: litmask")
  | Expr_LitString s -> (Expr_LitString s, type_string)
  | Expr_AsConstraint (e, c) ->
      let e', ty = tc_expr env u loc e in
      let c' = tc_constraints env loc c in
      (* todo: check ty against c *)
      (* todo: refine ty using c *)
      (Expr_AsConstraint (e', c'), ty)
  | Expr_AsType (e, t) ->
      let e', ty = tc_expr env u loc e in
      let t' = tc_type env loc t in
      (* todo: check ty against t' *)
      (Expr_AsType (e', t'), t')

(** Typecheck list of types *)
and tc_types (env : Env.t) (loc : AST.l) (xs : AST.ty list) : AST.ty list =
  List.map (tc_type env loc) xs

(** Typecheck type *)
and tc_type (env : Env.t) (loc : AST.l) (x : AST.ty) : AST.ty =
  match x with
  | Type_Constructor tc -> (
      if not (GlobalEnv.isType (Env.globals env) tc) then
        raise (IsNotA (loc, "type constructor", pprint_ident tc));
      match GlobalEnv.getType (Env.globals env) tc with
      (* todo: instantiate with type parameters? *)
      | Some (Type_Abbreviation ty') -> derefType (Env.globals env) ty'
      | _ -> Type_Constructor tc)
  | Type_Integer ocrs ->
      let ocrs' = Option.map (tc_constraints env loc) ocrs in
      Type_Integer ocrs'
  | Type_Bits n ->
      let n' = check_expr env loc type_integer n in
      Type_Bits n'
  | Type_App (tc, es) ->
      if not (GlobalEnv.isTycon (Env.globals env) tc) then
        raise (IsNotA (loc, "type constructor", pprint_ident tc));
      let es' = List.map (check_expr env loc type_integer) es in
      Type_App (tc, es')
  | Type_OfExpr e ->
      let s, (_, ty) = with_unify env loc (fun u -> tc_expr env u loc e) in
      unify_subst_ty s ty
  | Type_Register (wd, fs) ->
      let fs' =
        List.map
          (fun (ss, f) ->
            let s, ss' =
              with_unify env loc (fun u ->
                  List.map (fun s -> fst (tc_slice env u loc s)) ss)
            in
            let ss'' = List.map (subst_slice s) ss' in
            (ss'', f))
          fs
      in
      Type_Register (wd, fs')
  | Type_Array (Index_Enum tc, ety) ->
      if not (GlobalEnv.isEnum (Env.globals env) tc) then
        raise (IsNotA (loc, "enumeration type", pprint_ident tc));
      let ety' = tc_type env loc ety in
      Type_Array (Index_Enum tc, ety')
  | Type_Array (Index_Range (lo, hi), ety) ->
      let lo' = check_expr env loc type_integer lo in
      let hi' = check_expr env loc type_integer hi in
      let ety' = tc_type env loc ety in
      Type_Array (Index_Range (lo', hi'), ety')
  | Type_Tuple tys ->
      let tys' = tc_types env loc tys in
      Type_Tuple tys'

and tc_constraint (env : Env.t) (loc : AST.l) (c : AST.constraint_range) :
    AST.constraint_range =
  match c with
  | Constraint_Single e ->
      let e' = check_expr env loc type_integer e in
      Constraint_Single e'
  | Constraint_Range (lo, hi) ->
      let lo' = check_expr env loc type_integer lo in
      let hi' = check_expr env loc type_integer hi in
      Constraint_Range (lo', hi')

and tc_constraints (env : Env.t) (loc : AST.l) (cs : AST.constraint_range list)
    : AST.constraint_range list =
  List.map (tc_constraint env loc) cs

(****************************************************************)
(** {2 Typecheck L-expressions}                                 *)
(****************************************************************)

(** Typecheck bitslice syntax

    This primarily consists of disambiguating between array indexing and bitslicing
    Note that this function is almost identical to tc_slice_expr
 *)
let rec tc_slice_lexpr (env : Env.t) (u : unifier) (loc : AST.l) (x : lexpr)
    (ss : (AST.slice * AST.ty) list) : AST.lexpr * AST.ty =
  if List.length ss = 0 then raise (TypeError (loc, "empty list of subscripts"));
  let ss' = List.map fst ss in
  let x', ty = tc_lexpr2 env u loc x in
  match derefType (Env.globals env) ty with
  | Type_Array (ixty, elty) -> (
      match ss with
      | [ (Slice_Single i, ity) ] ->
          check_type env u loc (ixtype_basetype ixty) ity;
          (LExpr_Array (x', i), elty)
      | _ -> raise (TypeError (loc, "multiple subscripts for array")))
  | Type_Bits n -> (LExpr_Slices (x', ss'), type_bits (slices_width ss'))
  | Type_Register (wd, _) ->
      (LExpr_Slices (x', ss'), type_bits (slices_width ss'))
  | Type_Integer _ ->
      (* There is an argument for making this operation illegal *)
      if false then
        Format.fprintf fmt "Warning: slice assignment of integer at %s\n"
          (pp_loc loc);
      (LExpr_Slices (x', ss'), type_bits (slices_width ss'))
  | _ -> raise (TypeError (loc, "slice of lexpr"))

(** Typecheck left hand side of expression in context where
    type of right hand side is not yet known
 *)
and tc_lexpr2 (env : Env.t) (u : unifier) (loc : AST.l) (x : AST.lexpr) :
    AST.lexpr * AST.ty =
  match x with
  | LExpr_Wildcard -> raise (TypeError (loc, "wildcard in lexpr2"))
  | LExpr_Var v -> (
      match Env.getVar env v with
      | Some (v', ty') ->
          Env.markModified env v;
          (LExpr_Var v', ty')
      | None -> (
          let getters =
            GlobalEnv.getFuns (Env.globals env) (addSuffix v "read")
          in
          let setters =
            GlobalEnv.getFuns (Env.globals env) (addSuffix v "write")
          in
          let ogetter =
            chooseFunction (Env.globals env) loc "var getter function"
              (pprint_ident v) false [] getters
          in
          match ogetter with
          | Some fty ->
              let gty =
                match
                  chooseFunction (Env.globals env) loc "var setter function"
                    (pprint_ident v) false [ fty.rty ] setters
                with
                | Some gty -> gty
                | None ->
                    raise
                      (UnknownObject (loc, "var setter function", pprint_ident v))
              in
              let f', tes', rty =
                instantiate_fun (Env.globals env) u loc fty [] []
              in
              (LExpr_ReadWrite (f', gty.funname, tes', []), rty)
          | None -> raise (UnknownObject (loc, "variable", pprint_ident v))))
  | LExpr_Field (l, f) -> (
      let l', ty = tc_lexpr2 env u loc l in
      match typeFields (Env.globals env) loc ty with
      | FT_Record rfs -> (LExpr_Field (l', f), get_recordfield loc rfs f)
      | FT_Register rfs ->
          let ss, ty' = get_regfield loc rfs f in
          (LExpr_Slices (l', ss), ty'))
  | LExpr_Fields (l, fs) -> (
      let l', ty = tc_lexpr2 env u loc l in
      match typeFields (Env.globals env) loc ty with
      | FT_Record rfs ->
          let tys = List.map (get_recordfield loc rfs) fs in
          (LExpr_Fields (l', fs), mk_concat_tys tys)
      | FT_Register rfs ->
          let ss, ty' = get_regfields loc rfs fs in
          (LExpr_Slices (l', ss), ty'))
  | LExpr_Slices (e, ss) -> (
      let all_single =
        List.for_all (function Slice_Single _ -> true | _ -> false) ss
      in
      let ss' = List.map (tc_slice env u loc) ss in

      (* variable slice or setter call?
       * Start by testing for getter/setter pair
       * If that fails, test for an array variable or bitvector variable
       *)
      match e with
      | LExpr_Var a -> (
          let tys = List.map (function _, ty -> ty) ss' in
          let getters =
            GlobalEnv.getFuns (Env.globals env) (addSuffix a "read")
          in
          let setters =
            GlobalEnv.getSetterFun (Env.globals env) (addSuffix a "set")
          in
          let ogetters =
            chooseFunction (Env.globals env) loc "getter function"
              (pprint_ident a) true tys getters
          in
          let osetters =
            chooseFunction (Env.globals env) loc "setter function"
              (pprint_ident a) true tys setters
          in
          match (ogetters, osetters) with
          | Some fty, Some gty when all_single ->
              let es =
                List.map
                  (function
                    | Slice_Single a, _ -> a
                    | _ -> raise (InternalError "Expr_Slices"))
                  ss'
              in
              let f', tes', rty =
                instantiate_fun (Env.globals env) u loc fty es tys
              in
              (LExpr_ReadWrite (f', gty.funname, tes', es), rty)
          | None, Some _ ->
              raise (UnknownObject (loc, "getter function", pprint_ident a))
          | Some _, None ->
              raise (UnknownObject (loc, "setter function", pprint_ident a))
          | _ -> tc_slice_lexpr env u loc e ss')
      | _ -> tc_slice_lexpr env u loc e ss')
  | LExpr_BitTuple ls ->
      let ls', tys = List.split (List.map (tc_lexpr2 env u loc) ls) in
      let ty = mk_concat_tys tys in
      (LExpr_BitTuple ls', ty)
  | LExpr_Tuple ls ->
      let ls', tys = List.split (List.map (tc_lexpr2 env u loc) ls) in
      (LExpr_Tuple ls', Type_Tuple tys)
  | LExpr_Array (a, e) -> (
      let a', ty = tc_lexpr2 env u loc a in
      match derefType (Env.globals env) ty with
      | Type_Array (ixty, elty) ->
          let e' = check_expr env loc (ixtype_basetype ixty) e in
          (LExpr_Array (a', e'), elty)
      | _ -> raise (TypeError (loc, "subscript of non-array")))
  | _ -> raise (InternalError "tc_lexpr2")

(****************************************************************)
(** {2 Typecheck statements}                                    *)
(****************************************************************)

(** Typecheck left hand side of expression and check that rhs type 'ty' is compatible.
    Return set of variables assigned to in this expression
 *)
let rec tc_lexpr (env : Env.t) (u : unifier) (loc : AST.l) (ty : AST.ty)
    (x : AST.lexpr) : AST.lexpr =
  match x with
  | LExpr_Wildcard ->
      LExpr_Wildcard
  | LExpr_Var v when v = Ident "_" ->
      (* treat '_' as wildcard token *)
      LExpr_Wildcard
  | LExpr_Var v -> (
      match Env.getVar env v with
      | Some (_, ty') ->
          check_type env u loc ty' ty;
          Env.markModified env v;
          LExpr_Var v
      | None -> (
          let setters =
            GlobalEnv.getFuns (Env.globals env) (addSuffix v "write")
          in
          let osetter =
            chooseFunction (Env.globals env) loc "var setter function"
              (pprint_ident v) false [ ty ] setters
          in
          match osetter with
          | Some gty ->
              let dummy_arg = Expr_LitInt "42" in
              (* the value and type of this are ignored *)
              let g', tes', rty =
                instantiate_fun (Env.globals env) u loc gty [ dummy_arg ] [ ty ]
              in
              LExpr_Write (gty.funname, tes', [])
          | None ->
              raise (UnknownObject (loc, "variable", pprint_ident v))
          )
      )
  | LExpr_Field (l, f) ->
      let l', rty = tc_lexpr2 env u loc l in
      let r, ty' =
        match typeFields (Env.globals env) loc rty with
        | FT_Record rfs -> (LExpr_Field (l', f), get_recordfield loc rfs f)
        | FT_Register rfs ->
            let ss, ty' = get_regfield loc rfs f in
            (LExpr_Slices (l', ss), ty')
      in
      check_type env u loc ty' ty;
      r
  | LExpr_Fields (l, fs) ->
      let l', lty = tc_lexpr2 env u loc l in
      let r, ty' =
        match typeFields (Env.globals env) loc lty with
        | FT_Record rfs ->
            let tys = List.map (get_recordfield loc rfs) fs in
            (LExpr_Fields (l', fs), mk_concat_tys tys)
        | FT_Register rfs ->
            let ss, ty' = get_regfields loc rfs fs in
            (LExpr_Slices (l', ss), ty')
      in
      check_type env u loc ty' ty;
      r
  | LExpr_Slices (e, ss) ->
      let all_single =
        List.for_all (function Slice_Single _ -> true | _ -> false) ss
      in
      let ss' = List.map (tc_slice env u loc) ss in

      (* variable slice or setter call?
       * Start by testing for getter/setter pair
       * If that fails, test for slice of a var-getter
       * If that fails, test for an array variable or bitvector variable
       *)
      let e', ty' =
        match e with
        | LExpr_Var a -> (
            let tys = ty :: List.map (function _, ty -> ty) ss' in
            let setters =
              GlobalEnv.getSetterFun (Env.globals env) (addSuffix a "set")
            in
            let osetters =
              chooseFunction (Env.globals env) loc "setter function"
                (pprint_ident a) true tys setters
            in
            match osetters with
            | Some gty when all_single ->
                let es =
                  List.map
                    (function
                      | Slice_Single a, _ -> a
                      | _ -> raise (InternalError "Expr_Slices1"))
                    ss'
                in
                let g', tes', _ =
                  instantiate_fun (Env.globals env) u loc gty es (ty :: tys)
                in
                (LExpr_Write (gty.funname, tes', es), ty)
            | _ -> (
                let getters =
                  GlobalEnv.getFuns (Env.globals env) (addSuffix a "read")
                in
                let setters =
                  GlobalEnv.getFuns (Env.globals env) (addSuffix a "write")
                in
                let vty = type_bitsK "0" in
                (* note that width is ignored *)
                let ogetter =
                  chooseFunction (Env.globals env) loc "var getter function"
                    (pprint_ident a) false [] getters
                in
                let osetter =
                  chooseFunction (Env.globals env) loc "var setter function"
                    (pprint_ident a) false [ vty ] setters
                in
                match (ogetter, osetter) with
                | Some fty, Some fty' ->
                    (* todo: calculate type correctly *)
                    let wr = LExpr_ReadWrite (fty.funname, fty'.funname, [], []) in
                    (* todo: check slices are integer *)
                    (* todo: check rty is bits(_) *)
                    let ss'' = List.map fst ss' in
                    (LExpr_Slices (wr, ss''), type_bits (slices_width ss''))
                | None, Some _ ->
                    raise
                      (UnknownObject (loc, "var getter function", pprint_ident a))
                | Some _, None ->
                    raise
                      (UnknownObject (loc, "var setter function", pprint_ident a))
                | None, None -> tc_slice_lexpr env u loc e ss'))
        | _ -> tc_slice_lexpr env u loc e ss'
      in
      check_type env u loc ty' ty;
      e'
  | LExpr_BitTuple ls ->
      let ls', tys = List.split (List.map (tc_lexpr2 env u loc) ls) in
      let ty' = mk_concat_tys tys in
      check_type env u loc ty' ty;
      LExpr_BitTuple ls'
  | LExpr_Tuple ls ->
      let ls' =
        match ty with
        | Type_Tuple tys when List.length ls = List.length tys ->
            List.map2 (tc_lexpr env u loc) tys ls
        | _ -> raise (IsNotA (loc, "tuple of length ?", pp_type ty))
      in
      LExpr_Tuple ls'
  | LExpr_Array (a, e) -> (
      let a', ty = tc_lexpr2 env u loc a in
      match derefType (Env.globals env) ty with
      | Type_Array (ixty, elty) ->
          let e', ety = tc_expr env u loc e in
          check_type env u loc (ixtype_basetype ixty) ety;
          LExpr_Array (a', e')
      | _ -> raise (TypeError (loc, "subscript of non-array")))
  | _ -> raise (InternalError "tc_lexpr")

let rec add_decl_item_vars (env : Env.t) (loc : AST.l) (x : AST.decl_item) : unit =
  match x with
  | DeclItem_Var (v, Some ty) ->
      Env.addLocalVar env loc v ty
  | DeclItem_Tuple dis ->
      List.iter (add_decl_item_vars env loc) dis
  | DeclItem_Wildcard oty ->
      ()
  | DeclItem_Var (v, None) ->
      raise (InternalError "visit_declitem")

(* typecheck a decl_item using the type `ity` of the initializer *)
let rec tc_decl_item (env : Env.t) (u : unifier) (loc : AST.l) (ity : AST.ty) (x : AST.decl_item) : AST.decl_item =
  match (ity, x) with
  | (ity, DeclItem_Var (v, None)) ->
    DeclItem_Var (v, Some ity)
  | (ity, DeclItem_Var (v, Some ty)) ->
      let ty' = tc_type env loc ty in
      check_type env u loc ty' ity;
      DeclItem_Var (v, Some ity)
  | (Type_Tuple itys, DeclItem_Tuple dis) when List.length dis = List.length itys ->
      let dis' = List.map2 (tc_decl_item env u loc) itys dis in
      DeclItem_Tuple dis'
  | (_, DeclItem_Tuple dis) ->
      raise (IsNotA (loc, "tuple of length ?", pp_type ity))
  | (ity, DeclItem_Wildcard None) ->
      DeclItem_Wildcard (Some ity)
  | (ity, DeclItem_Wildcard (Some ty)) ->
      let ty' = tc_type env loc ty in
      check_type env u loc ty' ity;
      DeclItem_Wildcard (Some ity)

(** Typecheck list of statements *)
let rec tc_stmts (env : Env.t) (loc : AST.l) (xs : AST.stmt list) :
    AST.stmt list =
  Env.nest
    (fun env' -> List.map (tc_stmt env') xs)
    env

(** Typecheck 'if expr then stmt' *)
and tc_s_elsif (env : Env.t) (x : AST.s_elsif) : AST.s_elsif =
  match x with
  | S_Elsif_Cond (c, s, loc) ->
      let c' = check_expr env loc type_bool c in
      let s' = tc_stmts env loc s in
      S_Elsif_Cond (c', s', loc)

(** Typecheck case alternative *)
and tc_alt (env : Env.t) (ty : AST.ty) (x : AST.alt) : AST.alt =
  match x with
  | Alt_Alt (ps, oc, b, loc) ->
      let ps' = List.map (tc_pattern env loc ty) ps in
      let oc' = Option.map (fun c -> check_expr env loc type_bool c) oc in
      let b' = tc_stmts env loc b in
      Alt_Alt (ps', oc', b', loc)

(** Typecheck exception catcher 'when expr stmt' *)
and tc_catcher (env : Env.t) (loc : AST.l) (x : AST.catcher) : AST.catcher =
  match x with
  | Catcher_Guarded (c, b, loc) ->
      let c' = check_expr env loc type_bool c in
      let b' = tc_stmts env loc b in
      Catcher_Guarded (c', b', loc)

(** typecheck statement *)
and tc_stmt (env : Env.t) (x : AST.stmt) : AST.stmt =
  match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) ->
      let ty' = tc_type env loc ty in
      List.iter (fun v -> Env.addLocalVar env loc v ty') vs;
      Stmt_VarDeclsNoInit (vs, ty', loc)
  | Stmt_VarDecl (di, i, loc) ->
      let s, (di', i') =
        with_unify env loc (fun u ->
            let i', ity = tc_expr env u loc i in
            let di' = tc_decl_item env u loc ity di in
            (di', i'))
      in
      let di'' = unify_subst_di s di' in
      let i'' = unify_subst_e s i' in
      add_decl_item_vars env loc di'';
      Stmt_VarDecl (di'', i'', loc)
  | Stmt_ConstDecl (di, i, loc) ->
      let s, (di', i') =
        with_unify env loc (fun u ->
            let i', ity = tc_expr env u loc i in
            let di' = tc_decl_item env u loc ity di in
            (di', i'))
      in
      let di'' = unify_subst_di s di' in
      let i'' = unify_subst_e s i' in
      add_decl_item_vars env loc di'';

      (* add integer constants to type environment *)
      (match di'' with
      | DeclItem_Var (v, Some ty) ->
          if ty = type_integer then
            Env.addConstraint env loc (mk_eq_int (Expr_Var v) i'')
      | _ -> ());

      Stmt_ConstDecl (di'', i'', loc)
  | Stmt_Assign (l, r, loc) ->
      let s, (r', rty, l') =
        with_unify env loc (fun u ->
            let r', rty = tc_expr env u loc r in
            let l' = tc_lexpr env u loc rty l in
            if verbose then
              Format.fprintf fmt "    - Typechecking %s <- %s : %s\n"
                (pp_lexpr l') (pp_expr r') (pp_type rty);
            (r', rty, l'))
      in
      let l'' = unify_subst_le s l' in
      let r'' = unify_subst_e s r' in
      Stmt_Assign (l'', r'', loc)
  | Stmt_TCall (f, tes, es, loc) ->
      let s, (f', tes'', es') =
        with_unify env loc (fun u ->
            let es', tys = List.split (tc_exprs env u loc es) in
            let f', tes'', ty =
              tc_apply (Env.globals env) u loc "procedure" f es' tys
            in
            check_type env u loc ty type_unit;
            (f', tes'', es'))
      in
      let es'' = List.map (unify_subst_e s) es' in
      let tes''' = List.map (unify_subst_e s) tes'' in
      Stmt_TCall (f', tes''', es'', loc)
  | Stmt_FunReturn (e, loc) ->
      let rty =
        match Env.getReturnType env with
        | Some ty -> ty
        | None -> raise (InternalError "Stmt_FunReturn")
      in
      let e' = check_expr env loc rty e in
      Stmt_FunReturn (e', loc)
  | Stmt_ProcReturn loc ->
      (match Env.getReturnType env with
      | None -> ()
      | Some (Type_Tuple []) -> ()
      | _ -> raise (InternalError "return type should be None"));
      Stmt_ProcReturn loc
  | Stmt_Assert (e, loc) ->
      let e' = check_expr env loc type_bool e in
      Stmt_Assert (e', loc)
  | Stmt_Throw (v, loc) ->
      let _ =
        with_unify env loc (fun u ->
            let v', ty = check_var env loc v in
            check_type env u loc type_exn ty)
      in
      Stmt_Throw (v, loc)
  | Stmt_Block (ss, loc) ->
      let ss' = tc_stmts env loc ss in
      Stmt_Block (ss', loc)
  | Stmt_If (c, t, els, (e, el), loc) ->
      let c' = check_expr env loc type_bool c in
      let t' = tc_stmts env loc t in
      let els' = List.map (tc_s_elsif env) els in
      let e' = tc_stmts env el e in
      Stmt_If (c', t', els', (e', el), loc)
  | Stmt_Case (e, alts, odefault, loc) ->
      let s, (e', ty') = with_unify env loc (fun u -> tc_expr env u loc e) in
      let e'' = unify_subst_e s e' in
      let ty'' = unify_subst_ty s ty' in
      let alts' = List.map (tc_alt env ty'') alts in
      let odefault' =
        Option.map (fun (b, bl) -> (tc_stmts env loc b, bl)) odefault
      in
      Stmt_Case (e'', alts', odefault', loc)
  | Stmt_For (v, start, dir, stop, b, loc) ->
      let start' = check_expr env loc type_integer start in
      let stop' = check_expr env loc type_integer stop in
      let b' =
        Env.nest
          (fun env' ->
            Env.addLocalVar env' loc v type_integer;
            tc_stmts env' loc b)
          env
      in
      Stmt_For (v, start', dir, stop', b', loc)
  | Stmt_While (c, b, loc) ->
      let c' = check_expr env loc type_bool c in
      let b' = tc_stmts env loc b in
      Stmt_While (c', b', loc)
  | Stmt_Repeat (b, c, pos, loc) ->
      let b' = tc_stmts env loc b in
      let c' = check_expr env loc type_bool c in
      Stmt_Repeat (b', c', pos, loc)
  | Stmt_Try (tb, ev, pos, catchers, odefault, loc) ->
      let tb' = tc_stmts env loc tb in
      Env.nest
        (fun env' ->
          Env.addLocalVar env' loc ev type_exn;
          let catchers' = List.map (tc_catcher env' loc) catchers in
          let odefault' =
            Option.map (fun (b, bl) -> (tc_stmts env loc b, bl)) odefault
          in
          Stmt_Try (tb', ev, pos, catchers', odefault', loc))
        env

(****************************************************************)
(** {2 Typecheck function definition}                           *)
(****************************************************************)

(** Typecheck function body (list of statements) *)
let tc_body (env : Env.t) (loc : AST.l) (xs : AST.stmt list) : AST.stmt list =
  tc_stmts env loc xs

(** Typecheck function parameter *)
let tc_parameter (env : Env.t) (loc : AST.l)
    ((arg, oty) : AST.ident * AST.ty option) : AST.ident * AST.ty option =
  let ty' =
    match oty with None -> type_integer | Some ty -> tc_type env loc ty
  in
  Env.addLocalVar env loc arg ty';
  (arg, Some ty')

(** Typecheck function argument *)
let tc_argument (env : Env.t) (loc : AST.l) ((arg, ty) : AST.ident * AST.ty) :
    AST.ident * AST.ty =
  let ty' = tc_type env loc ty in
  Env.addLocalVar env loc arg ty';
  (arg, ty')

(** Typecheck list of function arguments *)
let tc_arguments (env : Env.t) (loc : AST.l)
    (ps : (AST.ident * AST.ty option) list) (args : (AST.ident * AST.ty) list)
    (rty : AST.ty) :
    (AST.ident * AST.ty option) list * (AST.ident * AST.ty) list * AST.ty =
  let fvs =
    IdentSet.union (fv_args args) (fv_type rty)
    |> removeConsts (Env.globals env)
  in
  let ps =
    if Utils.is_null ps then
      (* If no parameter list was supplied, use freevars from arg/return type *)
      List.map (fun v -> (v, Some type_integer)) (Asl_utils.to_sorted_list fvs)
    else
      (* If parameter list was supplied, add any arguments if they are in
         freevar list *)
      let extra = List.filter (fun (v, ty) -> IdentSet.mem v fvs) args in
      let extra = List.map (fun (v, ty) -> (v, Some ty)) extra in
      List.append extra ps
  in
  let ps' = List.map (tc_parameter env loc) ps in
  let args' = List.map (tc_argument env loc) args in
  let rty' = tc_type env loc rty in
  Env.setReturnType env rty';
  (ps', args', rty')

(** Add function definition to environment *)
let addFunction (env : GlobalEnv.t) (loc : AST.l) (qid : AST.ident)
    (isArr : bool) (ps : (AST.ident * AST.ty option) list)
    (args : (AST.ident * AST.ty) list) (rty : AST.ty) : funtype =
  let argtys = List.map (fun (_, ty) -> ty) args in
  let funs = GlobalEnv.getFuns env qid in
  let num_funs = List.length funs in
  match List.filter (isCompatibleFunction env isArr argtys) funs with
  | [] ->
      (* not defined yet *)
      (* ASL allows multiple functions to share the same name.
       * The typechecker disambiguates functions for the benefit of other parts of the
       * system by adding a unique tag to each ident.
       * We use the number of functions that already have that name as the tag.
       *)
      let tag = num_funs in
      let qid' = addTag qid tag in
      let fty : funtype = { funname=qid'; isArray=isArr; params=ps; atys=args; rty=rty } in
      GlobalEnv.addFuns env loc qid (fty :: funs);
      fty
  | [ fty ] ->
      (* already defined *)
      fty
  | ftys ->
      (* internal error: multiple definitions *)
      failwith "addFunction"

let addSetterFunction (env : GlobalEnv.t) (loc : AST.l) (qid : AST.ident)
    (ps : (AST.ident * AST.ty option) list) (args : (AST.ident * AST.ty) list)
    (vty : AST.ty) : funtype =
  let argtys = List.map snd args in
  let funs = GlobalEnv.getSetterFun env qid in
  let num_funs = List.length funs in
  match List.filter (isCompatibleFunction env true argtys) funs with
  | [] ->
      (* not defined yet *)
      (* ASL allows multiple functions to share the same name.
       * The typechecker disambiguates functions for the benefit of other parts of the
       * system by adding a unique tag to each ident.
       * We use the number of functions that already have that name as the tag.
       *)
      let tag = num_funs in
      let qid' = addTag qid tag in
      let fty : funtype = { funname=qid'; isArray=true; params=ps; atys=args; rty=type_unit } in
      GlobalEnv.addSetterFuns env qid (fty :: funs);
      fty
  | [ fty ] ->
      (* already defined *)
      fty
  | ftys ->
      (* internal error: multiple definitions *)
      failwith "addFunction"

(****************************************************************)
(** {2 Typecheck global declaration}                            *)
(****************************************************************)

(** Typecheck global declaration, extending environment as needed *)
let tc_declaration (env : GlobalEnv.t) (d : AST.declaration) :
    AST.declaration list =
  match d with
  | Decl_BuiltinType (qid, loc) ->
      GlobalEnv.addType env loc qid (Type_Builtin qid);
      [ d ]
  | Decl_Forward (qid, loc) ->
      GlobalEnv.addType env loc qid Type_Forward;
      [ d ]
  | Decl_Record (qid, fs, loc) ->
      let env' = Env.mkEnv env in
      let fs' = List.map (fun (f, ty) -> (f, tc_type env' loc ty)) fs in
      GlobalEnv.addType env loc qid (Type_Record fs');
      [ Decl_Record (qid, fs', loc) ]
  | Decl_Typedef (qid, ty, loc) ->
      let ty' = tc_type (Env.mkEnv env) loc ty in
      GlobalEnv.addType env loc qid (Type_Abbreviation ty');
      [ Decl_Typedef (qid, ty', loc) ]
  | Decl_Enum (qid, es, loc) ->
      GlobalEnv.addType env loc qid (Type_Enumeration es);
      List.iter
        (fun e -> GlobalEnv.addGlobalVar env loc e (Type_Constructor qid) true)
        es;
      let ty = Type_Constructor qid in
      let cmp_args = [ (Ident "x", ty); (Ident "y", ty) ] in
      let eq =
        addFunction env loc (Ident "eq_enum") false [] cmp_args type_bool
      in
      let ne =
        addFunction env loc (Ident "ne_enum") false [] cmp_args type_bool
      in
      GlobalEnv.addOperators2 env loc Binop_Eq [ eq ];
      GlobalEnv.addOperators2 env loc Binop_NtEq [ ne ];
      let deq = Decl_BuiltinFunction (eq.funname, [], [], ty, loc) in
      let dne = Decl_BuiltinFunction (ne.funname, [], [], ty, loc) in
      [ d; deq; dne ]
  | Decl_Var (qid, ty, loc) ->
      let ty' = tc_type (Env.mkEnv env) loc ty in
      GlobalEnv.addGlobalVar env loc qid ty' false;
      [ Decl_Var (qid, ty', loc) ]
  | Decl_Const (qid, ty, i, loc) ->
      let ty' = tc_type (Env.mkEnv env) loc ty in
      let i' = check_expr (Env.mkEnv env) loc ty' i in
      GlobalEnv.addGlobalVar env loc qid ty' true;
      GlobalEnv.addConstant env qid (simplify_expr i');
      [ Decl_Const (qid, ty', i', loc) ]
  | Decl_BuiltinFunction (qid, ps, atys, rty, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys rty in
      let qid' = (addFunction env loc qid false ps' atys' rty').funname in
      [ Decl_BuiltinFunction (qid', ps', atys', rty', loc) ]
  | Decl_FunType (qid, ps, atys, rty, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys rty in
      let qid' = (addFunction env loc qid false ps' atys' rty').funname in
      [ Decl_FunType (qid', ps', atys', rty', loc) ]
  | Decl_FunDefn (qid, ps, atys, rty, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys rty in
      let qid' = (addFunction env loc qid false ps' atys' rty').funname in
      let b' = tc_body locals loc b in
      [ Decl_FunDefn (qid', ps', atys', rty', b', loc) ]
  | Decl_ProcType (qid, ps, atys, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys type_unit in
      let qid' = (addFunction env loc qid false ps' atys' rty').funname in
      [ Decl_ProcType (qid', ps', atys', loc) ]
  | Decl_ProcDefn (qid, ps, atys, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys type_unit in
      let qid' = (addFunction env loc qid false ps' atys' rty').funname in
      let b' = tc_body locals loc b in
      [ Decl_ProcDefn (qid', ps', atys', b', loc) ]
  | Decl_VarGetterType (qid, ps, rty, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps [] rty in
      (* todo: check that if a setter function exists, it has a compatible
         type *)
      let qid' =
        (addFunction env loc (addSuffix qid "read") false ps' atys' rty').funname
      in
      [ Decl_VarGetterType (qid', ps', rty', loc) ]
  | Decl_VarGetterDefn (qid, ps, rty, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps [] rty in
      (* todo: check that if a setter function exists, it has a compatible
         type *)
      let qid' =
        (addFunction env loc (addSuffix qid "read") false ps' atys' rty').funname
      in
      let b' = tc_body locals loc b in
      [ Decl_VarGetterDefn (qid', ps', rty', b', loc) ]
  | Decl_ArrayGetterType (qid, ps, atys, rty, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys rty in
      let qid' =
        (addFunction env loc (addSuffix qid "read") true ps' atys' rty').funname
      in
      (* todo: check that if a setter function exists, it has a compatible
         type *)
      [ Decl_ArrayGetterType (qid', ps', atys', rty', loc) ]
  | Decl_ArrayGetterDefn (qid, ps, atys, rty, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys rty in
      (* todo: check that if a setter function exists, it has a compatible
         type *)
      let qid' =
        (addFunction env loc (addSuffix qid "read") true ps' atys' rty').funname
      in
      let b' = tc_body locals loc b in
      [ Decl_ArrayGetterDefn (qid', ps', atys', rty', b', loc) ]
  | Decl_VarSetterType (qid, ps, v, ty, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps [ (v, ty) ] type_unit in
      let v', ty' = List.hd atys' in
      (* todo: check that if a getter function exists, it has a compatible type *)
      (* todo: this obscures the difference between "PC[]" and "PC" *)
      let qid' =
        (addFunction env loc (addSuffix qid "write") false ps' atys' rty').funname
      in
      [ Decl_VarSetterType (qid', ps', v, ty', loc) ]
  | Decl_VarSetterDefn (qid, ps, v, ty, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps [ (v, ty) ] type_unit in
      let v', ty' = List.hd atys' in
      (* todo: check that if a getter function exists, it has a compatible type *)
      (* todo: this obscures the difference between "PC[]" and "PC" *)
      let qid' =
        (addFunction env loc (addSuffix qid "write") false ps' atys' rty').funname
      in
      let b' = tc_body locals loc b in
      [ Decl_VarSetterDefn (qid', ps', v, ty', b', loc) ]
  | Decl_ArraySetterType (qid, ps, atys, v, ty, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', _ =
        tc_arguments locals loc ps ((v, ty) :: atys) type_unit
      in
      let v', ty' = List.hd atys' in
      (* todo: check that if a getter function exists, it has a compatible
         type *)
      let qid' =
        (addSetterFunction env loc (addSuffix qid "set") ps' atys' ty').funname
      in
      [ Decl_ArraySetterType (qid', ps', List.tl atys', v, ty', loc) ]
  | Decl_ArraySetterDefn (qid, ps, atys, v, ty, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', _ =
        tc_arguments locals loc ps ((v, ty) :: atys) type_unit
      in
      let v', ty' = List.hd atys' in
      (* todo: should I use name mangling or define an enumeration to select
       * which namespace to do lookup in?
       *)
      (* todo: check that if a getter function exists, it has a compatible
         type *)
      let qid' =
        (addSetterFunction env loc (addSuffix qid "set") ps' atys' ty').funname
      in
      let b' = tc_body locals loc b in
      [ Decl_ArraySetterDefn (qid', ps', List.tl atys', v, ty', b', loc) ]
  | Decl_Operator1 (op, funs, loc) ->
      let funs' =
        List.concat
          (List.map
             (fun f ->
               let fs = GlobalEnv.getFuns env f in
               if fs = [] then
                 raise
                   (UnknownObject
                      (loc, "unary operator implementation", pprint_ident f));
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
                      (loc, "binary operator implementation", pprint_ident f));
               fs)
             funs)
      in
      GlobalEnv.addOperators2 env loc op funs';
      [ Decl_Operator2 (op, List.map (fun f -> f.funname) funs', loc) ]
  | Decl_NewEventDefn (qid, ps, atys, loc) ->
      (* very similar to Decl_ProcType *)
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys type_unit in
      let qid' = (addFunction env loc qid false ps' atys' rty').funname in
      [ Decl_NewEventDefn (qid', ps', atys', loc) ]
  | Decl_EventClause (nm, b, loc) -> (
      match GlobalEnv.getFuns env nm with
      | [ fty ] ->
          let locals = Env.mkEnv env in
          let _ = tc_arguments locals loc fty.params fty.atys type_unit in
          let b' = tc_body locals loc b in
          [ Decl_EventClause (fty.funname, b', loc) ]
      | [] -> raise (UnknownObject (loc, "event", pprint_ident nm))
      | fs ->
          reportChoices loc "event" (pprint_ident nm) [] fs;
          raise (Ambiguous (loc, "event", pprint_ident nm)))
  | Decl_NewMapDefn (qid, ps, atys, rty, b, loc) ->
      (* very similar to Decl_FunDefn *)
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys rty in
      let qid' = (addFunction env loc qid false ps' atys' rty').funname in
      let b' = tc_body locals loc b in
      [ Decl_NewMapDefn (qid', ps', atys', rty', b', loc) ]
  | Decl_MapClause (nm, fs, oc, b, loc) -> (
      match GlobalEnv.getFuns env nm with
      | [ fty ] ->
          let locals = Env.mkEnv env in
          let _ = tc_arguments locals loc fty.params fty.atys fty.rty in
          let tc_mapfield (MapField_Field (id, pat)) =
            match Env.getVar locals id with
            | Some (_, ty) -> MapField_Field (id, tc_pattern locals loc ty pat)
            | None -> raise (UnknownObject (loc, "mapfield", pprint_ident id))
          in
          let fs' = List.map tc_mapfield fs in
          let oc' = Option.map (check_expr locals loc type_bool) oc in
          let b' = tc_stmts locals loc b in
          [ Decl_MapClause (fty.funname, fs', oc', b', loc) ]
      | [] -> raise (UnknownObject (loc, "map", pprint_ident nm))
      | fs ->
          reportChoices loc "map" (pprint_ident nm) [] fs;
          raise (Ambiguous (loc, "map", pprint_ident nm)))
  | Decl_Config (qid, ty, i, loc) ->
      (* very similar to Decl_Const *)
      let locals = Env.mkEnv env in
      let ty' = tc_type locals loc ty in
      let i' = check_expr locals loc ty' i in
      GlobalEnv.addGlobalVar env loc qid ty' true;
      [ Decl_Config (qid, ty', i', loc) ]

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
      | Decl_FunDefn (qid, ps, atys, rty, _, loc) ->
          post := d :: !post;
          pre := Decl_FunType (qid, ps, atys, rty, loc) :: !pre
      | Decl_ProcDefn (qid, ps, atys, _, loc) ->
          post := d :: !post;
          pre := Decl_ProcType (qid, ps, atys, loc) :: !pre
      | Decl_VarGetterDefn (qid, ps, rty, _, loc) ->
          post := d :: !post;
          pre := Decl_VarGetterType (qid, ps, rty, loc) :: !pre
      | Decl_ArrayGetterDefn (qid, ps, atys, rty, _, loc) ->
          post := d :: !post;
          pre := Decl_ArrayGetterType (qid, ps, atys, rty, loc) :: !pre
      | Decl_VarSetterDefn (qid, ps, v, ty, _, loc) ->
          post := d :: !post;
          pre := Decl_VarSetterType (qid, ps, v, ty, loc) :: !pre
      | Decl_ArraySetterDefn (qid, ps, atys, v, ty, _, loc) ->
          post := d :: !post;
          pre := Decl_ArraySetterType (qid, ps, atys, v, ty, loc) :: !pre
      | Decl_NewEventDefn (qid, ps, atys, loc) ->
          post := d :: !post;
          (* todo: replacing it with a function declaration is not
           * completely kosher *)
          pre := Decl_ProcType (qid, ps, atys, loc) :: !pre
      | Decl_NewMapDefn (qid, ps, atys, rty, b, loc) ->
          post := d :: !post;
          (* todo: replacing it with a function declaration is not
           * completely kosher *)
          pre := Decl_FunType (qid, ps, atys, rty, loc) :: !pre
      | Decl_EventClause (nm, b, loc) -> post := d :: !post
      | Decl_MapClause (nm, fs, oc, b, loc) -> post := d :: !post
      | _ -> pre := d :: !pre)
    ds;
  (List.rev !pre, List.rev !post)

(** Overall typechecking environment shared by all invocations of typechecker *)
let env0 = GlobalEnv.mkempty ()

(** Typecheck a list of declarations - main entrypoint into typechecker *)
let tc_declarations (env : GlobalEnv.t) (isPrelude : bool)
    (ds : AST.declaration list) : AST.declaration list =
  if verbose then Format.fprintf fmt "  - Using Z3 %s\n" Z3.Version.to_string;
  (* Process declarations, starting by moving all function definitions to the
   * end of the list and replacing them with function prototypes.
   * As long as the type/var decls are all sorted correctly, this
   * is enough to handle functions that are used before being defined.
   *
   * Note that each declaration is evaluated in a separate local environment
   * but that they share the same global environment
   *)
  let pre, post = if isPrelude then (ds, []) else genPrototypes ds in
  if verbose then
    Format.fprintf fmt "  - Typechecking %d phase 1 declarations\n"
      (List.length pre);
  let pre' = List.map (tc_declaration env) pre in
  let post' = List.map (tc_declaration env) post in
  (* if verbose then List.iter (fun ds -> List.iter (fun d -> Format.fprintf fmt "\nTypechecked %s\n" (Utils.to_string (PP.pp_declaration d))) ds) post'; *)
  if verbose then
    Format.fprintf fmt "  - Typechecking %d phase 2 declarations\n"
      (List.length post);
  List.append (List.concat pre') (List.concat post')

(****************************************************************
 * End
 ****************************************************************)
