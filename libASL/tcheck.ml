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
open Builtin_idents
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

(****************************************************************)
(** {3 AST construction utilities}                              *)
(****************************************************************)

let slice_width (x : AST.slice) : AST.expr =
  match x with
  | Slice_Single e -> one
  | Slice_HiLo (hi, lo) -> Xform_simplify_expr.mk_add_int (mk_sub_int hi lo) one
  | Slice_LoWd (lo, wd) -> wd

let slices_width (xs : AST.slice list) : AST.expr =
  mk_add_ints (List.map slice_width xs)

let ixtype_basetype (ty : AST.ixtype) : AST.ty =
  match ty with
  | Index_Enum tc -> Type_Constructor (tc, [])
  | Index_Int sz -> type_integer

(****************************************************************)
(** {3 Prettyprinting support}                                  *)
(****************************************************************)

(** Table of binary operators used for resugaring expressions when printing
    error messages.
 *)
let binop_table : AST.binop Bindings.t ref = ref Bindings.empty

let add_binop (op : binop) (x : Ident.t) : unit =
  binop_table := Bindings.add x op !binop_table

(** Resugar expression *)
let ppp_expr (x : AST.expr) : AST.expr = resugar_expr !binop_table x

(** Very pretty print type (resugaring expressions) *)
let ppp_type (x : AST.ty) : AST.ty = resugar_type !binop_table x

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
    loc         : AST.l;
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
    loc     : AST.l;
    isArray : bool;
    params  : (Ident.t * AST.ty option) list;
    atys    : (Ident.t * AST.ty) list;
    ovty    : AST.ty option; (* type of rhs in setter functions *)
    rty     : AST.ty;
  }

let pp_funtype (fmt : formatter) (fty : funtype) : unit =
  Format.fprintf fmt "%a%s : {%a}(%a)%a => %a @ %a"
    FMT.varname fty.funname
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
  val addType : t -> AST.l -> Ident.t -> typedef -> unit
  val getType : t -> Ident.t -> typedef option
  val isType : t -> Ident.t -> bool
  val isTycon : t -> Ident.t -> bool
  val isEnum : t -> Ident.t -> bool
  val addFuns : t -> AST.l -> Ident.t -> funtype list -> unit
  val getFuns : t -> Ident.t -> funtype list
  val addSetterFuns : t -> Ident.t -> funtype list -> unit
  val getSetterFun : t -> Ident.t -> funtype list
  val addOperators1 : t -> AST.l -> AST.unop -> funtype list -> unit
  val getOperators1 : t -> AST.l -> AST.unop -> funtype list
  val addOperators2 : t -> AST.l -> AST.binop -> funtype list -> unit
  val getOperators2 : t -> AST.l -> AST.binop -> funtype list
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

  let addType (env : t) (loc : AST.l) (qid : Ident.t) (t : typedef) : unit =
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

  let addFuns (env : t) (loc : AST.l) (qid : Ident.t) (ftys : funtype list) :
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
let expand_type (loc : AST.l) (ps : Ident.t list) (ty : AST.ty) (es : expr list) : AST.ty =
  if List.length ps <> List.length es then begin
    raise (TypeError (loc, "wrong number of type parameters"))
  end;
  let bs = mk_bindings (List.combine ps es) in
  subst_type bs ty

(** dereference typedef *)
let rec derefType (env : GlobalEnv.t) (loc : AST.l) (ty : AST.ty) : AST.ty =
  match ty with
  | Type_Constructor (tc, es) -> (
      match GlobalEnv.getType env tc with
      | Some (Type_Abbreviation (ps, ty')) ->
        let ty'' = expand_type loc ps ty' es in
        derefType env loc ty''
      | _ -> ty)
  | _ -> ty

let width_of_type (env : GlobalEnv.t) (loc : AST.l) (ty : AST.ty) : AST.expr =
  let ty' = derefType env loc ty in
  Utils.from_option (Asl_utils.width_of_type ty')
    (fun _ -> raise (InternalError ("width_of_type " ^ (pp_type ty'))))


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
let typeFields (env : GlobalEnv.t) (loc : AST.l) (x : ty) : fieldtypes =
  match derefType env loc x with
  | Type_Constructor (tc, es) -> (
      match GlobalEnv.getType env tc with
      | Some (Type_Record (ps, fs)) ->
        let fs' = List.map (fun (f, ty) -> (f, expand_type loc ps ty es)) fs in
        FT_Record fs'
      | Some (Type_Exception fs) ->
        FT_Record fs
      | _ -> raise (IsNotA (loc, "record or exception", Ident.pprint tc)))
  | Type_Register (wd, fs) -> FT_Register fs
  | Type_OfExpr e ->
      raise
        (InternalError ("typeFields: Type_OfExpr " ^ pp_expr e))
  | _ -> raise (IsNotA (loc, "record/register", pp_type x))

(** Get fieldtype information for a named field of a record *)
let get_recordfield (loc : AST.l) (rfs : (Ident.t * ty) list) (f : Ident.t) : AST.ty
    =
  match List.filter (fun (fnm, _) -> fnm = f) rfs with
  | [ (_, fty) ] -> fty
  | [] -> raise (UnknownObject (loc, "field", Ident.pprint f))
  | fs -> raise (Ambiguous (loc, "field", Ident.pprint f))

(** Get fieldtype information for a named field of a slice *)
let get_regfield_info (loc : AST.l) (rfs : (AST.slice list * Ident.t) list)
    (f : Ident.t) : AST.slice list =
  match List.filter (fun (_, fnm) -> fnm = f) rfs with
  | [ (ss, _) ] -> ss
  | [] -> raise (UnknownObject (loc, "field", Ident.pprint f))
  | fs -> raise (Ambiguous (loc, "field", Ident.pprint f))

(** Get named field of a register and calculate type *)
let get_regfield (loc : AST.l) (rfs : (AST.slice list * Ident.t) list) (f : Ident.t)
    : AST.slice list * AST.ty =
  let ss = get_regfield_info loc rfs f in
  (ss, type_bits (slices_width ss))

(** Get named fields of a register and calculate type of concatenating them *)
let get_regfields (loc : AST.l) (rfs : (AST.slice list * Ident.t) list)
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
    mutable locals : var_info Bindings.t list;
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
    | [] -> raise (InternalError "addLocalVar")
    );
    if not v.is_constant then env.modified <- IdentSet.add v.name env.modified

  let getVar (env : t) (v : Ident.t) : var_info option =
    (* Format.fprintf fmt "Looking for variable %a\n" FMT.varname v; *)
    search_var env.globals env.locals v

  let markModified (env : t) (v : Ident.t) : unit =
    env.modified <- IdentSet.add v env.modified

  let addConstraint (env : t) (loc : AST.l) (c : AST.expr) : unit =
    env.constraints <- c :: env.constraints

  let getConstraints (env : t) : AST.expr list = env.constraints
  let setReturnType (env : t) (ty : AST.ty) : unit = env.rty <- Some ty
  let getReturnType (env : t) : AST.ty option = env.rty
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

let rec simplify_expr (x : AST.expr) : AST.expr =
  let eval (x : AST.expr) : Z.t option =
    match x with Expr_LitInt x' -> Some (Z.of_string x') | _ -> None
  in
  let to_expr (x : Z.t) : AST.expr = Expr_LitInt (Z.to_string x) in

  match x with
  | Expr_TApply (f, tes, es, throws) -> (
      let es' = List.map simplify_expr es in
      match (f, flatten_map_option eval es') with
      | i, Some [ a ]    when Ident.equal i neg_int -> to_expr (Z.neg a)
      | i, Some [ a; b ] when Ident.equal i add_int -> to_expr (Z.add a b)
      | i, Some [ a; b ] when Ident.equal i sub_int -> to_expr (Z.sub a b)
      | i, Some [ a; b ] when Ident.equal i mul_int -> to_expr (Z.mul a b)
      | _ -> Expr_TApply (f, tes, es', throws))
  | _ -> x


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

let rec z3_of_expr (ctx : Z3.context) (ufs : (AST.expr * Z3.Expr.expr) list ref)
    (x : AST.expr) : Z3.Expr.expr =
  match x with
  | Expr_Var v ->
      let intsort = Z3.Arithmetic.Integer.mk_sort ctx in
      Z3.Expr.mk_const_s ctx (Ident.pprint v) intsort
  | Expr_Parens y -> z3_of_expr ctx ufs y
  | Expr_LitInt i -> Z3.Arithmetic.Integer.mk_numeral_s ctx i
  (* todo: the following lines involving DIV are not sound *)
  | Expr_TApply
      ( i,
        [],
        [ Expr_TApply (j, [], [ a; b ], _); c ],
        _
      )
    when Ident.equal i mul_int && Ident.equal j fdiv_int && b = c ->
      z3_of_expr ctx ufs a
  | Expr_TApply
      ( i,
        [],
        [ a; Expr_TApply (j, [], [ b; c ], _) ],
        _
      )
    when Ident.equal i mul_int && Ident.equal j fdiv_int && a = c ->
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
    when Ident.equal i add_int && Ident.equal j fdiv_int && Ident.equal k fdiv_int && a1 = a2 && b1 = b2 && b1 = two ->
      z3_of_expr ctx ufs a1
  | Expr_TApply
      ( i,
        [],
        [ a; Expr_TApply (j, [], [ b; c ], _) ],
        _
      ) when Ident.equal i eq_int && Ident.equal j fdiv_int ->
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
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i fdiv_int ->
      Z3.Arithmetic.mk_div ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | Expr_TApply (i, [], [ a; b ], _) when Ident.equal i eq_int ->
      Z3.Boolean.mk_eq ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
  | _ -> (
      if verbose then
        Format.fprintf fmt
          "    Unable to translate %a - using as uninterpreted function\n"
          FMT.expr x;
      let intsort = Z3.Arithmetic.Integer.mk_sort ctx in
      match List.assoc_opt x !ufs with
      | None ->
          let uf = Z3.Expr.mk_fresh_const ctx "UNINTERPRETED" intsort in
          ufs := (x, uf) :: !ufs;
          uf
      | Some uf -> uf)

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
  if verbose && q = SATISFIABLE then
    Format.fprintf fmt "Failed property %s\n" (Z3.Expr.to_string p);
  q = UNSATISFIABLE

(****************************************************************)
(** {3 Checking subtyping}                                      *)
(****************************************************************)

let check_equality
  (env : Env.t) (loc : AST.l)
  (x : AST.expr) (y : AST.expr)
  : unit =
  let x' = simplify_expr x in
  let y' = simplify_expr y in

  (* As a performance optimisation, omit SMT calls that are trivially true *)
  if x' <> y' then begin
    let assumptions = Env.getConstraints env in
    let constraints = [mk_eq_int x' y'] in
    if not (check_constraints assumptions constraints) then begin
      raise (DoesNotMatch (loc, "type width parameter", pp_expr x, pp_expr y))
    end
  end


(** Check that ty1 subtype-satisfies ty2
    On failure, report an error.

    This differs from eq_structural and synthesize_type (below)
    in that it checks the dependent part of the type.
 *)
let rec check_subtype_satisfies (env : Env.t) (loc : AST.l) (ty1 : AST.ty) (ty2 : AST.ty) : unit =
  let genv = Env.globals env in
  (* Substitute global constants in types *)
  let subst_consts = new substFunClass (GlobalEnv.getConstant genv) in
  let ty1' = Asl_visitor.visit_type subst_consts ty1 in
  let ty2' = Asl_visitor.visit_type subst_consts ty2 in
  ( match (derefType genv loc ty1', derefType genv loc ty2') with
  | Type_Integer _, Type_Integer _ -> ()
  | Type_Bits e1, Type_Bits e2 -> check_equality env loc e1 e2
  | Type_Bits e1, Type_Register (e2, _)
  | Type_Register (e1, _), Type_Bits e2
  | Type_Register (e1, _), Type_Register (e2, _)
    (* todo: check that register fields are a subset *)
    -> check_equality env loc e1 e2
  | Type_Constructor (tc1, es1), Type_Constructor (tc2, es2) when tc1 = tc2 ->
      assert (List.length es1 = List.length es2);
      List.iter2 (check_equality env loc) es1 es2
  | Type_OfExpr e1, Type_OfExpr e2 -> raise (InternalError "check_subtype_satisfies: typeof")
  | Type_Array (ixty1, elty1), Type_Array (ixty2, elty2) ->
      ( match (ixty1, ixty2) with
      | Index_Enum tc1, Index_Enum tc2 -> ()
      | Index_Int sz1, Index_Int sz2 -> check_equality env loc sz1 sz2
      | _ -> ()
      );
      check_subtype_satisfies env loc elty1 elty2
  | Type_Tuple tys1, Type_Tuple tys2 ->
      if List.length tys1 <> List.length tys2 then begin
        raise (DoesNotMatch (loc, "tuple length", pp_type ty2, pp_type ty1))
      end;
      List.iter2 (check_subtype_satisfies env loc) tys1 tys2
  | _ -> raise (DoesNotMatch (loc, "type", pp_type ty2, pp_type ty1))
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
  (env : Env.t) (loc : AST.l) (s : AST.expr option Scope.t)
  (ty1 : AST.ty) (ty2 : AST.ty)
  : unit =
  let genv = Env.globals env in
  ( match (derefType genv loc ty1, derefType genv loc ty2) with
  | Type_Bits e1, Type_Bits e2 -> synthesize_equality s e1 e2
  | Type_Constructor (tc1, es1), Type_Constructor (tc2, es2) when tc1 = tc2 ->
      assert (List.length es1 = List.length es2);
      List.iter2 (synthesize_equality s) es1 es2
  | Type_Bits e1, Type_Register (e2, _) -> synthesize_equality s e1 e2
  | Type_Register (e1, _), Type_Bits e2 -> synthesize_equality s e1 e2
  | Type_Register (e1, _), Type_Register (e2, _) -> synthesize_equality s e1 e2
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
let synthesize_parameters (env : Env.t) (loc : AST.l)
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
  iter3 (fun e ety (v, ty) ->
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
        raise (TypeError (loc, "unable to synthesize type parameter " ^ Ident.pprint v)))
    in
    (v, e)
  in
  Scope.bindings s
    |> List.map check_parameter
    |> mk_bindings


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
let instantiate_fun (env : Env.t) (loc : AST.l)
    (fty : funtype) (es : AST.expr list) (tys : AST.ty list) : fun_instance =
  (* Format.printf "%a: Synthesizing parameters for call to %a\n" FMT.loc loc FMT.varname fty.funname; *)
  let bs = synthesize_parameters env loc fty es tys in

  (* Check each argument *)
  List.iter2 (fun actual_ty (v, aty) ->
    let aty' = subst_type bs aty in
    (* Format.printf "%a: Argument type %a -> %a\n" FMT.loc loc FMT.ty aty FMT.ty aty'; *)
    check_subtype_satisfies env loc actual_ty aty'
    )
    tys
    fty.atys;

  (* Construct result *)
  let parameters = List.map (fun (p, _) -> Bindings.find p bs) fty.params in
  let ovty = Option.map (subst_type bs) fty.ovty in
  let rty = subst_type bs fty.rty in
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
let rec eq_structural (env : GlobalEnv.t) (loc : AST.l) (ty1 : AST.ty) (ty2 : AST.ty) : bool =
  match (derefType env loc ty1, derefType env loc ty2) with
  | Type_Integer _, Type_Integer _ -> true
  | Type_Bits e1, Type_Bits e2 -> true
  | Type_Constructor (c1, es1), Type_Constructor (c2, es2) -> c1 = c2
  | Type_OfExpr e1, Type_OfExpr e2 -> raise (InternalError "eq_structural: typeof")
  | Type_Bits e1, Type_Register (w2, _) -> true
  | Type_Register (w1, _), Type_Bits e2 -> true
  | Type_Register (w1, _), Type_Register (w2, _) -> true
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
let reportChoices (loc : AST.l) (what : string) (nm : string)
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
      List.iter (fun ty -> Format.fprintf fmt "  Arg : %a\n" FMT.ty ty) tys;
      List.iter (fun fty -> pp_funtype fmt fty; FMTUtils.cut fmt) funs
    );
  FMTUtils.flush fmt

(** Check whether a list of function argument types is compatible with the
    type of a function.

    One function type is compatible with another if they have the same number
    of arguments and each argument has the same base type
 *)
let isCompatibleFunction (env : GlobalEnv.t) (loc : AST.l) (isArr : bool) (tys : AST.ty list)
    (fty : funtype) : bool =
  let nargs = List.length tys in
  isArr = fty.isArray
  && List.length fty.atys = nargs
  && List.for_all2 (eq_structural env loc) (List.map snd fty.atys) tys

(** Disambiguate a function name based on the number and type of arguments *)
let chooseFunction (env : GlobalEnv.t) (loc : AST.l) (what : string)
    (nm : string) (isArr : bool) (tys : AST.ty list) (funs : funtype list) :
    funtype option =
  let funs' = List.filter (isCompatibleFunction env loc isArr tys) funs in
  match nub funs' with
  | [] -> None
  | [ r ] -> Some r
  | fs ->
      (* todo: it would probably be better to detect ambiguity when functions are
       * defined instead of waiting until they are called
       *)
      reportChoices loc what nm tys fs;
      raise (Ambiguous (loc, what, nm))

let check_duplicate_field_names (fx : 'a -> Ident.t) (fs : 'a list) (loc : AST.l) =
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

(** Disambiguate and typecheck application of a function to a list of arguments *)
let tc_apply (env : Env.t) (loc : AST.l) (what : string)
    (f : Ident.t) (es : AST.expr list) (tys : AST.ty list) : fun_instance =
  let genv = Env.globals env in
  let funs = GlobalEnv.getFuns genv f in
  let nm = Ident.pprint f in
  match funs with
  | [] ->
      raise (UnknownObject (loc, what, nm))
  | _ ->
      ( match chooseFunction genv loc what nm false tys funs with
      | None ->
          reportChoices loc what nm tys funs;
          raise (TypeError (loc, "function arguments"))
      | Some fty ->
          (* if verbose then Format.fprintf fmt "    - Found matching %s at %a for %s = %a\n" what FMT.loc loc nm pp_funtype fty; *)
          instantiate_fun env loc fty es tys
      )

(** Disambiguate and typecheck application of a unary operator to argument *)
let tc_unop (env : Env.t) (loc : AST.l) (op : unop)
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
let tc_binop (env : Env.t) (loc : AST.l) (op : binop)
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
let get_var (env : Env.t) (loc : AST.l) (v : Ident.t) : var_info =
  from_option (Env.getVar env v) (fun _ -> raise (UnknownObject (loc, "variable", Ident.pprint v)))

(** check that we have exactly the fields required *)
let check_field_assignments (loc : AST.l) (fs : (Ident.t * ty) list) (fas : (Ident.t * expr) list) : unit =
  let expected = IdentSet.of_list (List.map fst fs) in
  let assigned = IdentSet.of_list (List.map fst fas) in
  if not (IdentSet.equal assigned expected) then begin
    let missing = IdentSet.elements (IdentSet.diff expected assigned) in
    let extra = IdentSet.elements (IdentSet.diff assigned expected) in
    let msg = "record initializer is missing fields "
      ^ String.concat ", " (List.map Ident.pprint missing)
      ^ " and/or has extra fields "
      ^ String.concat ", " (List.map Ident.pprint extra)
    in
    raise (TypeError (loc, msg))
  end

(** Typecheck list of expressions *)
let rec tc_exprs (env : Env.t) (loc : AST.l) (xs : AST.expr list)
    : (AST.expr * AST.ty) list =
  List.map (tc_expr env loc) xs

(** Typecheck expression and check that it is a subtype of ty *)
and check_expr (env : Env.t) (loc : AST.l) (ty : AST.ty) (x : AST.expr) :
    AST.expr =
  let (x', ty') = tc_expr env loc x in
  if verbose then
    Format.fprintf fmt "    - Typechecking %a : %a\n" FMT.expr x' FMT.ty ty';
  check_subtype_satisfies env loc ty' ty;
  x'

(** Typecheck 'if c then expr' *)
and tc_e_elsif (env : Env.t) (loc : AST.l) (x : AST.e_elsif) :
    AST.e_elsif * AST.ty =
  match x with
  | E_Elsif_Cond (c, e) ->
      let c' = check_expr env loc type_bool c in
      let e', ty = tc_expr env loc e in
      (E_Elsif_Cond (c', e'), ty)

(** Typecheck bitslice indices *)
and tc_slice (env : Env.t) (loc : AST.l) (x : AST.slice) :
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

(** Typecheck pattern against type ty *)
and tc_pattern (env : Env.t) (loc : AST.l) (ty : AST.ty) (x : AST.pattern) :
    AST.pattern =
  match x with
  | Pat_LitInt l ->
      check_subtype_satisfies env loc ty type_integer;
      Pat_LitInt l
  | Pat_LitHex l ->
      check_subtype_satisfies env loc ty type_integer;
      Pat_LitHex l
  | Pat_LitBits l ->
      check_subtype_satisfies env loc ty (type_bits (masklength_expr l));
      Pat_LitBits l
  | Pat_LitMask l ->
      check_subtype_satisfies env loc ty (type_bits (masklength_expr l));
      Pat_LitMask l
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
      check_subtype_satisfies env loc ty type_integer;
      Pat_Range (lo', hi')

(** Typecheck bitslice syntax
    This primarily consists of disambiguating between array indexing and bitslicing
    Note that this function is almost identical to tc_slice_lexpr
 *)
and tc_slice_expr (env : Env.t) (loc : AST.l) (x : expr)
    (ss : (AST.slice * AST.ty) list) : AST.expr * AST.ty =
  if List.length ss = 0 then raise (TypeError (loc, "empty list of subscripts"));
  let ss' = List.map fst ss in
  let x', ty' = tc_expr env loc x in
  let ty = type_bits (slices_width ss') in
  match derefType (Env.globals env) loc ty' with
  | Type_Array (ixty, elty) -> (
      match ss with
      | [ (Slice_Single i, ity) ] ->
          check_subtype_satisfies env loc ity (ixtype_basetype ixty);
          (Expr_Array (x', i), elty)
      | _ -> raise (TypeError (loc, "multiple subscripts for array")))
  | Type_Bits _
  | Type_Register _
  | Type_Integer _ ->
      (Expr_Slices (ty', x', ss'), ty)
  | _ -> raise (TypeError (loc, "slice of expr"))

(** Typecheck expression *)
and tc_expr (env : Env.t) (loc : AST.l) (x : AST.expr) :
    AST.expr * AST.ty =
  match x with
  | Expr_If (c, t, els, e) ->
      let c' = check_expr env loc type_bool c in
      let t', tty = tc_expr env loc t in
      let els', eltys = List.split (List.map (tc_e_elsif env loc) els) in
      let e', ety = tc_expr env loc e in
      List.iter (fun elty -> check_subtype_satisfies env loc elty tty) eltys;
      check_subtype_satisfies env loc ety tty;
      (Expr_If (c', t', els', e'), tty)
  | Expr_Binop (x, Binop_Eq, Expr_LitMask y) ->
      (* syntactic sugar *)
      tc_expr env loc (Expr_In (x, Pat_LitMask y))
  | Expr_Binop (x, Binop_NtEq, Expr_LitMask y) ->
      (* syntactic sugar *)
      tc_expr env loc (Expr_Unop (Unop_BoolNot, Expr_In (x, Pat_LitMask y)))
  | Expr_Binop (x, op, y) ->
      let x', xty = tc_expr env loc x in
      let y', yty = tc_expr env loc y in
      let fty = tc_binop env loc op x' y' xty yty in
      (Expr_TApply (fty.name, fty.parameters, [ x'; y' ], false), fty.rty)
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
      match e with
      | Expr_Var a -> (
          let tys = List.map (function _, ty -> ty) ss' in
          let getters =
            GlobalEnv.getFuns (Env.globals env) (Ident.add_suffix a ~suffix:"read")
          in
          let ogetters =
            chooseFunction (Env.globals env) loc "getter function"
              (Ident.pprint a) true tys getters
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
              let fty' = instantiate_fun env loc fty es tys in
              (Expr_TApply (fty'.name, fty'.parameters, es, false), fty'.rty)
          | _ -> tc_slice_expr env loc e ss')
      | _ -> tc_slice_expr env loc e ss')
  | Expr_RecordInit (tc, es, fas) ->
      if not (GlobalEnv.isType (Env.globals env) tc) then
        raise (IsNotA (loc, "type constructor", Ident.pprint tc));
      let (ps, fs) =
        match GlobalEnv.getType (Env.globals env) tc with
        | Some (Type_Record (ps, fs)) -> (ps, fs)
        | Some (Type_Exception fs) -> ([], fs)
        | _ -> raise (IsNotA (loc, "record or exception type", Ident.pprint tc))
      in
      if List.length es <> List.length ps then
        raise (TypeError (loc, "wrong number of type parameters"));
      check_field_assignments loc fs fas;

      (* add values of type parameters to environment *)
      let es' = List.map (check_expr env loc type_integer) es in
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

      (Expr_RecordInit (tc, es', fas'), Type_Constructor (tc, es'))

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
          (Expr_RecordInit (v, [], []), Type_Constructor (v, []))
        | Some _ ->
          raise (IsNotA (loc, "record or exception type", Ident.pprint v));
        | None ->
          let getters =
            GlobalEnv.getFuns (Env.globals env) (Ident.add_suffix v ~suffix:"read")
          in
          match
            chooseFunction (Env.globals env) loc "getter function"
              (Ident.pprint v) false [] getters
          with
          | Some fty ->
              let fty' = instantiate_fun env loc fty [] [] in
              (Expr_TApply (fty'.name, fty'.parameters, [], false), fty'.rty)
          | None ->
              raise
                (UnknownObject
                   (loc, "variable or getter functions", Ident.pprint v))
        )
      )
  | Expr_Parens e ->
      let e', ty = tc_expr env loc e in
      (Expr_Parens e', ty)
  | Expr_TApply (f, tes, es, throws) ->
      let es', tys = List.split (tc_exprs env loc es) in
      let fty = tc_apply env loc "function" f es' tys in
      (Expr_TApply (fty.name, fty.parameters, es', throws), fty.rty)
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
      (Expr_TApply (fty.name, fty.parameters, [ e' ], false), fty.rty)
  | Expr_Unknown t ->
      let ty' = tc_type env loc t in
      (Expr_Unknown ty', ty')
  | Expr_ImpDef (os, t) ->
      let ty' = tc_type env loc t in
      (Expr_ImpDef (os, ty'), ty')
  | Expr_Array (a, e) -> (
      let a', ty = tc_expr env loc a in
      match derefType (Env.globals env) loc ty with
      | Type_Array (ixty, elty) ->
          let e' = check_expr env loc (ixtype_basetype ixty) e in
          (Expr_Array (a', e'), elty)
      | _ -> raise (TypeError (loc, "subscript of non-array")))
  | Expr_LitInt i -> (Expr_LitInt i, type_integer)
  | Expr_LitHex i -> (Expr_LitHex i, type_integer)
  | Expr_LitReal r -> (Expr_LitReal r, type_real)
  | Expr_LitBits b -> (Expr_LitBits b, type_bits (masklength_expr b))
  | Expr_LitMask b ->
      (* todo: this case only exists because of the (bad) sugar of
       * writing "x == '0x'" instead of "x IN '0x'"
       *)
      raise (InternalError "tc_expr: litmask")
  | Expr_LitString s -> (Expr_LitString s, type_string)
  | Expr_AsConstraint (e, c) ->
      let e', ty = tc_expr env loc e in
      let c' = tc_constraints env loc c in
      (* todo: check ty against c *)
      (* todo: refine ty using c *)
      (Expr_AsConstraint (e', c'), ty)
  | Expr_AsType (e, t) ->
      let e', ty = tc_expr env loc e in
      let t' = tc_type env loc t in
      (* todo: check ty against t' *)
      (Expr_AsType (e', t'), t')

(** Typecheck list of types *)
and tc_types (env : Env.t) (loc : AST.l) (xs : AST.ty list) : AST.ty list =
  List.map (tc_type env loc) xs

(** Typecheck type *)
and tc_type (env : Env.t) (loc : AST.l) (x : AST.ty) : AST.ty =
  match x with
  | Type_Integer ocrs ->
      let ocrs' = Option.map (tc_constraints env loc) ocrs in
      Type_Integer ocrs'
  | Type_Bits n ->
      let n' = check_expr env loc type_integer n in
      Type_Bits n'
  | Type_Constructor (tc, es) ->
      let es' = List.map (check_expr env loc type_integer) es in
      if not (GlobalEnv.isTycon (Env.globals env) tc) then begin
        raise (IsNotA (loc, "type constructor", Ident.pprint tc))
      end;
      derefType (Env.globals env) loc (Type_Constructor (tc, es'))
  | Type_OfExpr e ->
      let (_, ty) = tc_expr env loc e in
      ty
  | Type_Register (wd, fs) ->
      check_duplicate_field_names (fun (_, f) -> f) fs loc;

      let fs' =
        List.map
          (fun (ss, f) ->
            let ss' = List.map (fun s -> fst (tc_slice env loc s)) ss in
            (ss', f))
          fs
      in
      Type_Register (wd, fs')
  | Type_Array (Index_Enum tc, ety) ->
      if not (GlobalEnv.isEnum (Env.globals env) tc) then
        raise (IsNotA (loc, "enumeration type", Ident.pprint tc));
      let ety' = tc_type env loc ety in
      Type_Array (Index_Enum tc, ety')
  | Type_Array (Index_Int sz, ety) ->
      let sz' = check_expr env loc type_integer sz in
      let ety' = tc_type env loc ety in
      Type_Array (Index_Int sz', ety')
  | Type_Tuple tys ->
      let tys' = tc_types env loc tys in
      Type_Tuple tys'

and check_type (env : Env.t) (loc : l) (ty1 : ty) (ty : ty) : ty =
  let ty' = tc_type env loc ty in
  check_subtype_satisfies env loc ty' ty1;
  ty'

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
let rec tc_slice_lexpr (env : Env.t) (loc : AST.l) (x : lexpr)
    (ss : (AST.slice * AST.ty) list) : AST.lexpr * AST.ty =
  if List.length ss = 0 then raise (TypeError (loc, "empty list of subscripts"));
  let ss' = List.map fst ss in
  let x', ty' = tc_lexpr2 env loc x in
  let ty = type_bits (slices_width ss') in
  match derefType (Env.globals env) loc ty' with
  | Type_Array (ixty, elty) -> (
      match ss with
      | [ (Slice_Single i, ity) ] ->
          check_subtype_satisfies env loc ity (ixtype_basetype ixty);
          (LExpr_Array (x', i), elty)
      | _ -> raise (TypeError (loc, "multiple subscripts for array")))
  | Type_Bits n
  | Type_Register (n, _) ->
    (LExpr_Slices (ty', x', ss'), ty)
  | Type_Integer _ ->
      (* There is an argument for making this operation illegal *)
      if false then
        Format.fprintf fmt "Warning: slice assignment of integer at %a\n"
          FMT.loc loc;
      (LExpr_Slices (ty', x', ss'), ty)
  | _ -> raise (TypeError (loc, "slice of lexpr"))

(** Typecheck left hand side of expression in context where
    type of right hand side is not yet known
 *)
and tc_lexpr2 (env : Env.t) (loc : AST.l) (x : AST.lexpr) :
    AST.lexpr * AST.ty =
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
              (Ident.pprint v) false [] getters
          in
          match ogetter with
          | Some fty ->
              let gty =
                match
                  chooseFunction (Env.globals env) loc "var setter function"
                    (Ident.pprint v) false [] setters
                with
                | Some gty -> gty
                | None ->
                    raise
                      (UnknownObject (loc, "var setter function", Ident.pprint v))
              in
              let fty' = instantiate_fun env loc fty [] [] in
              let throws = false in (* todo: need to allow ? on var *)
              (LExpr_ReadWrite (fty'.name, gty.funname, fty'.parameters, [], throws), fty'.rty)
          | None -> raise (UnknownObject (loc, "variable", Ident.pprint v))))
  | LExpr_Field (l, f) -> (
      let l', ty = tc_lexpr2 env loc l in
      match typeFields (Env.globals env) loc ty with
      | FT_Record rfs -> (LExpr_Field (l', f), get_recordfield loc rfs f)
      | FT_Register rfs ->
          let ss, ty' = get_regfield loc rfs f in
          (LExpr_Slices (ty, l', ss), ty'))
  | LExpr_Fields (l, fs) -> (
      let l', ty = tc_lexpr2 env loc l in
      match typeFields (Env.globals env) loc ty with
      | FT_Record rfs ->
          let tys = List.map (get_recordfield loc rfs) fs in
          let ws = List.map (width_of_type (Env.globals env) loc) tys in
          let w = Xform_simplify_expr.mk_add_ints ws in
          (LExpr_Fields (l', fs), type_bits w)
      | FT_Register rfs ->
          let ss, ty' = get_regfields loc rfs fs in
          (LExpr_Slices (ty, l', ss), ty'))
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
            GlobalEnv.getSetterFun (Env.globals env) (Ident.add_suffix a ~suffix:"set")
          in
          let ogetters =
            chooseFunction (Env.globals env) loc "getter function"
              (Ident.pprint a) true tys getters
          in
          let osetters =
            chooseFunction (Env.globals env) loc "setter function"
              (Ident.pprint a) true tys setters
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
              let fty' = instantiate_fun env loc fty es tys in
              let throws = false in (* todo : need to add throws to Var *)
              (LExpr_ReadWrite (fty'.name, gty.funname, fty'.parameters, es, throws), fty'.rty)
          | None, Some _ ->
              raise (UnknownObject (loc, "getter function", Ident.pprint a))
          | Some _, None ->
              raise (UnknownObject (loc, "setter function", Ident.pprint a))
          | _ -> tc_slice_lexpr env loc e ss')
      | _ -> tc_slice_lexpr env loc e ss')
  | LExpr_BitTuple (_, ls) ->
      let ls', tys = List.split (List.map (tc_lexpr2 env loc) ls) in
      let ws = List.map (width_of_type (Env.globals env) loc) tys in
      let w = Xform_simplify_expr.mk_add_ints ws in
      (LExpr_BitTuple (ws, ls'), type_bits w)
  | LExpr_Tuple ls ->
      let ls', tys = List.split (List.map (tc_lexpr2 env loc) ls) in
      (LExpr_Tuple ls', Type_Tuple tys)
  | LExpr_Array (a, e) -> (
      let a', ty = tc_lexpr2 env loc a in
      match derefType (Env.globals env) loc ty with
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
let rec tc_lexpr (env : Env.t) (loc : AST.l) (ty : AST.ty) (x : AST.lexpr) : AST.lexpr =
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
              (Ident.pprint v) false [] setters
          in
          match osetter with
          | Some gty ->
              let gty' = instantiate_fun env loc gty [] [] in
              let vty = from_option gty'.ovty (fun _ -> raise (InternalError "tc_lexpr LExpr_Var")) in
              check_subtype_satisfies env loc ty vty;
              let throws = false in
              LExpr_Write (gty'.name, gty'.parameters, [], throws)
          | None ->
              raise (UnknownObject (loc, "variable", Ident.pprint v))
          )
      )
  | LExpr_Field (l, f) ->
      let l', rty = tc_lexpr2 env loc l in
      let r, fty =
        match typeFields (Env.globals env) loc rty with
        | FT_Record rfs -> (LExpr_Field (l', f), get_recordfield loc rfs f)
        | FT_Register rfs ->
            let ss, ty' = get_regfield loc rfs f in
            (LExpr_Slices (rty, l', ss), ty')
      in
      check_subtype_satisfies env loc ty fty;
      r
  | LExpr_Fields (l, fs) ->
      let l', lty = tc_lexpr2 env loc l in
      let r, ty' =
        match typeFields (Env.globals env) loc lty with
        | FT_Record rfs ->
            let tys = List.map (get_recordfield loc rfs) fs in
            let ws = List.map (width_of_type (Env.globals env) loc) tys in
            let w = Xform_simplify_expr.mk_add_ints ws in
            (LExpr_Fields (l', fs), type_bits w)
        | FT_Register rfs ->
            let ss, ty' = get_regfields loc rfs fs in
            (LExpr_Slices (lty, l', ss), ty')
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
              GlobalEnv.getSetterFun (Env.globals env) (Ident.add_suffix a ~suffix:"set")
            in
            let osetters =
              chooseFunction (Env.globals env) loc "setter function"
                (Ident.pprint a) true tys setters
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
                let gty' = instantiate_fun env loc gty es tys in
                let vty = from_option gty'.ovty (fun _ -> raise (InternalError "tc_lexpr LExpr_Slices")) in
                check_subtype_satisfies env loc ty vty;
                let throws = false in
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
                    (Ident.pprint a) false [] getters
                in
                let osetter =
                  chooseFunction (Env.globals env) loc "var setter function"
                    (Ident.pprint a) false [] setters
                in
                match (ogetter, osetter) with
                | Some fty, Some fty' ->
                    (* todo: calculate type correctly *)
                    let throws = false in
                    let wr = LExpr_ReadWrite (fty.funname, fty'.funname, [], [], throws) in
                    (* todo: check slices are integer *)
                    (* todo: check rty is bits(_) *)
                    let ss'' = List.map fst ss' in
                    (LExpr_Slices (ty, wr, ss''), type_bits (slices_width ss''))
                | None, Some _ ->
                    raise
                      (UnknownObject (loc, "var getter function", Ident.pprint a))
                | Some _, None ->
                    raise
                      (UnknownObject (loc, "var setter function", Ident.pprint a))
                | None, None -> tc_slice_lexpr env loc e ss'))
        | _ -> tc_slice_lexpr env loc e ss'
      in
      check_subtype_satisfies env loc ty ty';
      e'
  | LExpr_BitTuple (_, ls) ->
      let ls', tys = List.split (List.map (tc_lexpr2 env loc) ls) in
      let ws = List.map (width_of_type (Env.globals env) loc) tys in
      let w = Xform_simplify_expr.mk_add_ints ws in
      check_subtype_satisfies env loc ty (type_bits w);
      LExpr_BitTuple (ws, ls')
  | LExpr_Tuple ls ->
      let ls' =
        match ty with
        | Type_Tuple tys when List.length ls = List.length tys ->
            List.map2 (tc_lexpr env loc) tys ls
        | _ -> raise (IsNotA (loc, "tuple of length ?", pp_type ty))
      in
      LExpr_Tuple ls'
  | LExpr_Array (a, e) -> (
      let a', ty = tc_lexpr2 env loc a in
      match derefType (Env.globals env) loc ty with
      | Type_Array (ixty, elty) ->
          let e', ety = tc_expr env loc e in
          check_subtype_satisfies env loc ety (ixtype_basetype ixty);
          LExpr_Array (a', e')
      | _ -> raise (TypeError (loc, "subscript of non-array")))
  | _ -> raise (InternalError "tc_lexpr")

let rec add_decl_item_vars (env : Env.t) (loc : AST.l) (is_constant : bool) (x : AST.decl_item) : unit =
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
      raise (InternalError "visit_declitem")

let tc_decl_bit (env : Env.t) (loc : AST.l) (x : (Ident.t option * AST.ty)) : (Ident.t option * AST.ty) =
  let (ov, ty) = x in
  let ty' = tc_type env loc ty in
  ( match ty' with
  | Type_Bits _ -> (ov, ty')
  | _ -> raise (TypeError (loc, "bits type expected"))
  )

(* typecheck a decl_item using the type `ity` of the initializer *)
let rec tc_decl_item (env : Env.t) (loc : AST.l) (ity : AST.ty) (x : AST.decl_item) : AST.decl_item =
  match (ity, x) with
  | (ity, DeclItem_Var (v, None)) ->
    DeclItem_Var (v, Some ity)
  | (ity, DeclItem_Var (v, Some ty)) ->
      let ty' = check_type env loc ity ty in
      DeclItem_Var (v, Some ty')
  | (Type_Tuple itys, DeclItem_Tuple dis) when List.length dis = List.length itys ->
      let dis' = List.map2 (tc_decl_item env loc) itys dis in
      DeclItem_Tuple dis'
  | (_, DeclItem_Tuple dis) ->
      let len = List.length dis in
      raise (IsNotA (loc, Format.asprintf "tuple of length %d" len, pp_type ity))
  | (Type_Bits n, DeclItem_BitTuple dbs)
  | (Type_Register (n, _), DeclItem_BitTuple dbs) ->
      let dbs' = List.map (tc_decl_bit env loc) dbs in
      let (vs', tys) = List.split dbs' in
      let ws = List.map (width_of_type (Env.globals env) loc) tys in
      let w = Xform_simplify_expr.mk_add_ints ws in
      let ty' = type_bits w in
      check_subtype_satisfies env loc ity ty';
      DeclItem_BitTuple dbs'
  | (_, DeclItem_BitTuple _) ->
      raise (IsNotA (loc, "bitvector", pp_type ity))
  | (ity, DeclItem_Wildcard None) ->
      DeclItem_Wildcard (Some ity)
  | (ity, DeclItem_Wildcard (Some ty)) ->
      let ty' = check_type env loc ity ty in
      DeclItem_Wildcard (Some ty')

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
  | Catcher_Guarded (v, tc, b, loc) ->
      if not (GlobalEnv.isTycon (Env.globals env) tc) then begin
        raise (IsNotA (loc, "exception type", Ident.pprint tc))
      end;
      let b' = Env.nest
        (fun env' ->
          Env.addLocalVar env' {name=v; loc; ty=Type_Constructor (tc, []); is_local=true; is_constant=true};
          tc_stmts env' loc b)
        env
      in
      Catcher_Guarded (v, tc, b', loc)

(** typecheck statement *)
and tc_stmt (env : Env.t) (x : AST.stmt) : AST.stmt =
  match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) ->
      let ty' = tc_type env loc ty in
      List.iter (fun v -> Env.addLocalVar env {name=v; loc; ty=ty'; is_local=true; is_constant=false}) vs;
      Stmt_VarDeclsNoInit (vs, ty', loc)
  | Stmt_VarDecl (di, i, loc) ->
      let (i', ity) = tc_expr env loc i in
      let di' = tc_decl_item env loc ity di in
      add_decl_item_vars env loc false di';
      Stmt_VarDecl (di', i', loc)
  | Stmt_ConstDecl (di, i, loc) ->
      let (i', ity) = tc_expr env loc i in
      let di' = tc_decl_item env loc ity di in
      add_decl_item_vars env loc true di';

      (* add integer constants to type environment *)
      (match di' with
      | DeclItem_Var (v, Some ty) ->
          if ty = type_integer then
            Env.addConstraint env loc (mk_eq_int (Expr_Var v) i')
      | _ -> ());

      Stmt_ConstDecl (di', i', loc)
  | Stmt_Assign (l, r, loc) ->
      let (r', rty) = tc_expr env loc r in
      let l' = tc_lexpr env loc rty l in
      if verbose then
        Format.fprintf fmt "    - Typechecking %a <- %a : %a\n"
          FMT.lexpr l' FMT.expr r' FMT.ty rty;
      Stmt_Assign (l', r', loc)
  | Stmt_TCall (f, tes, es, throws, loc) ->
      let es', tys = List.split (tc_exprs env loc es) in
      let fty = tc_apply env loc "procedure" f es' tys in
      check_subtype_satisfies env loc type_unit fty.rty;
      Stmt_TCall (fty.name, fty.parameters, es', throws, loc)
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
  | Stmt_Throw (e, loc) ->
      let (e', ty') = tc_expr env loc e in
      (* todo: check that ty' is an exception type *)
      Stmt_Throw (e', loc)
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
      let (e', ty') = tc_expr env loc e in
      let alts' = List.map (tc_alt env ty') alts in
      let odefault' =
        Option.map (fun (b, bl) -> (tc_stmts env loc b, bl)) odefault
      in
      Stmt_Case (e', alts', odefault', loc)
  | Stmt_For (v, start, dir, stop, b, loc) ->
      let start' = check_expr env loc type_integer start in
      let stop' = check_expr env loc type_integer stop in
      let b' =
        Env.nest
          (fun env' ->
            Env.addLocalVar env' {name=v; loc; ty=type_integer; is_local=true; is_constant=true};
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
  | Stmt_Try (tb, pos, catchers, odefault, loc) ->
      let tb' = tc_stmts env loc tb in
      let catchers' = List.map (tc_catcher env loc) catchers in
      let odefault' = Option.map (fun (b, bl) -> (tc_stmts env loc b, bl)) odefault in
      Stmt_Try (tb', pos, catchers', odefault', loc)

(****************************************************************)
(** {2 Typecheck function definition}                           *)
(****************************************************************)

(** Typecheck function body (list of statements) *)
let tc_body = tc_stmts

(** Typecheck function parameter *)
let tc_parameter (env : Env.t) (loc : AST.l)
    ((arg, oty) : Ident.t * AST.ty option) : Ident.t * AST.ty option =
  let ty' =
    ( match oty with
    | None -> type_integer
    | Some ty -> tc_type env loc ty
    )
  in
  Env.addLocalVar env {name=arg; loc; ty=ty'; is_local=true; is_constant=true};
  (arg, Some ty')

(** Typecheck function argument *)
let tc_argument
    (env : Env.t)
    (loc : AST.l)
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

(** Typecheck list of function arguments *)
let tc_arguments
    (env : Env.t)
    (loc : AST.l)
    (ps : (Ident.t * AST.ty option) list)
    (args : (Ident.t * AST.ty) list)
    (rty : AST.ty)
    : (Ident.t * AST.ty option) list * (Ident.t * AST.ty) list * AST.ty
  =
  let globals = Env.globals env in
  let is_not_global (x : Ident.t) = Option.is_none (GlobalEnv.getGlobalVar globals x) in

  (* The implicit type parameters are based on the free variables of the function type. *)
  let argty_fvs = fv_args args |> IdentSet.filter is_not_global in
  let rty_fvs = fv_type rty |> IdentSet.filter is_not_global in

  (* Type parameters in the return type cannot be synthesized directly
   * and must either be an explicit parameter of the function or
   * a free variable in one of the argument types.
   *)
  let unsynthesizable = (IdentSet.diff rty_fvs argty_fvs)
                      |> IdentSet.filter (fun v -> not (List.mem_assoc v args))
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
    |> IdentSet.filter (fun v -> not (List.mem_assoc v ps))
    |> Asl_utils.to_sorted_list
  in

  (* The type of any implicit parameters should match explicit argument if it exists *)
  let typed_implicit_parameters = List.map (fun v -> (v, List.assoc_opt v args)) implicit_parameters in

  (* Having inferred all the implicit parameters, we can finally typecheck the function type *)
  let ps' = List.map (tc_parameter env loc) (ps @ typed_implicit_parameters) in
  let args' = List.map (tc_argument env loc ps') args in
  let rty' = tc_type env loc rty in
  Env.setReturnType env rty';
  (ps', args', rty')

(** Add function definition to environment *)
let addFunction (env : GlobalEnv.t) (loc : AST.l) (qid : Ident.t)
    (isArr : bool) (ps : (Ident.t * AST.ty option) list)
    (args : (Ident.t * AST.ty) list) (rty : AST.ty) : funtype =
  let argtys = List.map (fun (_, ty) -> ty) args in
  let funs = GlobalEnv.getFuns env qid in
  let num_funs = List.length funs in
  match List.filter (isCompatibleFunction env loc isArr argtys) funs with
  | [] ->
      (* not defined yet *)
      (* ASL allows multiple functions to share the same name.
       * The typechecker disambiguates functions for the benefit of other parts of the
       * system by adding a unique tag to each ident.
       * We use the number of functions that already have that name as the tag.
       *)
      let tag = num_funs in
      let qid' = Ident.mk_fident_with_tag qid ~tag in
      let fty : funtype = { funname=qid'; loc; isArray=isArr; params=ps; atys=args; ovty=None; rty=rty } in
      GlobalEnv.addFuns env loc qid (fty :: funs);
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
      (* internal error: multiple definitions *)
      failwith "addFunction"

let addSetterFunction (env : GlobalEnv.t) (loc : AST.l) (qid : Ident.t)
    (isArr : bool) (ps : (Ident.t * AST.ty option) list)
    (args : (Ident.t * AST.ty) list) (vty : AST.ty) : funtype =
  let argtys = List.map snd args in
  let funs = GlobalEnv.getSetterFun env qid in
  let num_funs = List.length funs in
  match List.filter (isCompatibleFunction env loc isArr argtys) funs with
  | [] ->
      (* not defined yet *)
      (* ASL allows multiple functions to share the same name.
       * The typechecker disambiguates functions for the benefit of other parts of the
       * system by adding a unique tag to each ident.
       * We use the number of functions that already have that name as the tag.
       *)
      let tag = num_funs in
      let qid' = Ident.mk_fident_with_tag qid ~tag in
      let fty : funtype = { funname=qid'; loc; isArray=isArr; params=ps; atys=args; ovty=Some vty; rty=type_unit } in
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
      let cmp_args = [ (Ident.mk_ident "x", ty); (Ident.mk_ident "y", ty) ] in
      let eq =
        addFunction env loc eq_enum false [] cmp_args type_bool
      in
      let ne =
        addFunction env loc ne_enum false [] cmp_args type_bool
      in
      GlobalEnv.addOperators2 env loc Binop_Eq [ eq ];
      GlobalEnv.addOperators2 env loc Binop_NtEq [ ne ];
      let deq = Decl_BuiltinFunction (eq.funname, [], [], ty, loc) in
      let dne = Decl_BuiltinFunction (ne.funname, [], [], ty, loc) in
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
      (* todo: check that if a setter function exists, it has a compatible type *)
      let fty = addFunction env loc (Ident.add_suffix qid ~suffix:"read") false ps' atys' rty' in
      [ Decl_VarGetterType (fty.funname, ps', rty', loc) ]
  | Decl_VarGetterDefn (qid, ps, rty, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps [] rty in
      (* todo: check that if a setter function exists, it has a compatible type *)
      let fty = addFunction env loc (Ident.add_suffix qid ~suffix:"read") false ps' atys' rty' in
      let b' = tc_body locals loc b in
      [ Decl_VarGetterDefn (fty.funname, ps', rty', b', loc) ]
  | Decl_ArrayGetterType (qid, ps, atys, rty, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys rty in
      let fty = addFunction env loc (Ident.add_suffix qid ~suffix:"read") true ps' atys' rty' in
      (* todo: check that if a setter function exists, it has a compatible type *)
      [ Decl_ArrayGetterType (fty.funname, ps', atys', rty', loc) ]
  | Decl_ArrayGetterDefn (qid, ps, atys, rty, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps atys rty in
      (* todo: check that if a setter function exists, it has a compatible type *)
      let fty = addFunction env loc (Ident.add_suffix qid ~suffix:"read") true ps' atys' rty' in
      let b' = tc_body locals loc b in
      [ Decl_ArrayGetterDefn (fty.funname, ps', atys', rty', b', loc) ]
  | Decl_VarSetterType (qid, ps, v, ty, loc) ->
      let locals = Env.mkEnv env in
      let (ps', atys', _) = tc_arguments locals loc ps [ (v, ty) ] type_unit in
      let v', ty' = Utils.last atys' in
      (* todo: check that if a getter function exists, it has a compatible type *)
      let fty = addSetterFunction env loc (Ident.add_suffix qid ~suffix:"write") false ps' [] ty' in
      [ Decl_VarSetterType (fty.funname, ps', v, ty', loc) ]
  | Decl_VarSetterDefn (qid, ps, v, ty, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', rty' = tc_arguments locals loc ps [ (v, ty) ] type_unit in
      let (v', ty') = Utils.last atys' in
      (* todo: check that if a getter function exists, it has a compatible type *)
      let fty = addSetterFunction env loc (Ident.add_suffix qid ~suffix:"write") false ps' [] ty' in
      let b' = tc_body locals loc b in
      [ Decl_VarSetterDefn (fty.funname, ps', v, ty', b', loc) ]
  | Decl_ArraySetterType (qid, ps, atys, v, ty, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', _ = tc_arguments locals loc ps (atys @ [(v, ty)]) type_unit in
      let atys'' = Utils.init atys' in
      let v', ty' = Utils.last atys' in
      (* todo: check that if a getter function exists, it has a compatible type *)
      let fty = addSetterFunction env loc (Ident.add_suffix qid ~suffix:"set") true ps' atys'' ty' in
      [ Decl_ArraySetterType (fty.funname, ps', atys'', v, ty', loc) ]
  | Decl_ArraySetterDefn (qid, ps, atys, v, ty, b, loc) ->
      let locals = Env.mkEnv env in
      let ps', atys', _ = tc_arguments locals loc ps (atys @ [(v, ty)]) type_unit in
      let atys'' = Utils.init atys' in
      let v', ty' = Utils.last atys' in
      (* todo: should I use name mangling or define an enumeration to select
       * which namespace to do lookup in?
       *)
      (* todo: check that if a getter function exists, it has a compatible
         type *)
      let fty = addSetterFunction env loc (Ident.add_suffix qid ~suffix:"set") true ps' atys'' ty' in
      let b' = tc_body locals loc b in
      [ Decl_ArraySetterDefn (fty.funname, ps', atys'', v, ty', b', loc) ]
  | Decl_Operator1 (op, funs, loc) ->
      let funs' =
        List.concat
          (List.map
             (fun f ->
               let fs = GlobalEnv.getFuns env f in
               if fs = [] then
                 raise
                   (UnknownObject
                      (loc, "unary operator implementation", Ident.pprint f));
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
                      (loc, "binary operator implementation", Ident.pprint f));
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
  (* if verbose then List.iter (fun ds -> List.iter (fun d -> Format.fprintf fmt "\nTypechecked %a\n" FMT.declaration d) ds) post'; *)
  if verbose then
    Format.fprintf fmt "  - Typechecking %d phase 2 declarations\n"
      (List.length post);
  List.append (List.concat pre') (List.concat post')

(****************************************************************
 * End
 ****************************************************************)
