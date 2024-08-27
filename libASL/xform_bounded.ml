(****************************************************************
 * ASL integer bounds transform
 *
 * Transforms any bounded integer expression to use a bounded
 * representation (i.e., the '__sint(N)' type).
 *
 * Specifically,
 *
 * - For any constrained integer type "T" (e.g., "integer {0..31}"),
 *   we define a corresponding bounded integer type "B(T)" that can
 *   represent all the values in its domain.
 *   For example, for "{0..31}" we use "__sint(6)" which can represent
 *   {-32 .. 31}.
 *
 * - Any expression/variable that has a constrained integer type "T"
 *   is transformed to have the bounded type "B(T)".
 *
 *   In particular,
 *
 *   * Declarations of globals (variables, configuration variables, constants),
 *     local variables and function arguments are changed to
 *     type "B(T)".
 *     Note that function parameters and type parameters are not transformed.
 *
 *   * All integer constants are replaced by bounded constants.
 *
 *   * Functions that return constrained integers are transformed
 *     to return bounded integers.
 *
 *   * Fields of records
 *     todo: check this
 *
 * - Any user-defined function that has a constrained integer type in its
 *   arguments or return type is transformed to use bounded argument/result types.
 *
 * - Calls to selected primitive operations are transformed to
 *   calls to bounded integer primitive operations when all necessary
 *   arguments are constrained.
 *
 * - When an expression "x" with a bounded type "__sintN(N)"
 *   is used in a context where an unconstrained integer argument
 *   is required it is transformed to "asl_cvt_sintN_int{N}(x)".
 *
 *   In particular,
 *
 *   * Arguments of functions with unconstrained integer type
 *   * Array indexes
 *   * Bitslice indexes/widths
 *   * Function parameters
 *   * Return statements where a function has unconstrained return type
 *
 *   Todo: maybe the code generator should optimize use of
 *   "asl_cvt_sintN_int{N}(x)" in array indexes and bitslice indexes/widths?
 *
 * - When a bounded integer value 'x' (introduced by the transformations above)
 *   of type '__sintN(M)' is used in a context that needs a bounded integer
 *   type '__sintN(N)', then
 *
 *   - if M==N: use 'x'
 *   - if M<N: use `asl_resize_sintN(x)'
 *   - if M>N: report an internal error
 *
 * - Integer constraints are not transformed because they do not influence
 *   the generated code.
 *
 * Note that this transformation does not introduce any bitwidth
 * polymorphism so it can be run after the monomorphization transform.
 * And, although constant propagation could be run after this transform,
 * it is better not to since constant propagation would convert literal
 * sintN constants back to integer constants.
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

[@@@warning "-26-32-39"]

module AST = Asl_ast
open Asl_utils
open Identset
open Builtin_idents
open Primops
open Utils

type bounds = Z.t * Z.t

let union_bounds (r1 : bounds) (r2 : bounds) : bounds =
  let (lo1, hi1) = r1 in
  let (lo2, hi2) = r2 in
  (Z.min lo1 lo2, Z.max hi1 hi2)

type range = bounds option

let union_range (r1 : range) (r2 : range) : range =
  Option.bind r1 (fun b1 ->
  Option.bind r2 (fun b2 ->
  Some (union_bounds b1 b2)))

(* union of multiple ranges *)
let union_ranges (rs : range list) : range =
  ( match rs with
  | [] -> None
  | (r :: rs) -> List.fold_left union_range r rs
  )

let range_of_value (v : Value.value) : range =
  ( match v with
  | VInt i -> Some (i, i)
  | _ -> None
  )

let range_of_expr (x : AST.expr) : range =
  ( match x with
  | Expr_Lit v -> range_of_value v
  | _ -> None
  )

let range_of_constraint (x : AST.constraint_range) : range =
  ( match x with
  | Constraint_Single y -> range_of_expr y
  | Constraint_Range (lo, hi) -> union_range (range_of_expr lo) (range_of_expr hi)
  )

let range_of_type (x : AST.ty) : range =
  ( match x with
  | Type_Integer (Some crs) -> union_ranges (List.map range_of_constraint crs)
  | _ -> None
  )

type range_env = bounds Bindings.t

let empty_range_env = Bindings.empty

(* ceiling (log2 x) *)
let size_of_uint (x : Z.t) : int =
  let r = ref 0 in
  let b = ref Z.one in
  (* invariant: b = 2^r *)
  while Z.gt x !b do
    r := !r + 1;
    b := Z.add !b !b
  done;
  (* 2^(r-1) < x <= 2^r *)
  !r

let size_of_sint (x : Z.t) : int =
  if Z.lt x Z.zero then
    1 + size_of_uint (Z.neg x)
  else
    1 + size_of_uint (Z.add x Z.one)

let expr_of_int (x : int) : AST.expr = Expr_Lit (VInt (Z.of_int x))

let expr_of_sintN (n : int) (v : Z.t) : AST.expr = AST.Expr_Lit (VIntN (Primops.mksintN n v))

let int_of_bounds (b : bounds) : int =
  let (lo, hi) = b in
  let lo_n = size_of_sint lo in
  let hi_n = size_of_sint hi in
  Int.max lo_n hi_n

let type_of_bounds (b : bounds) : AST.ty = type_sintN (expr_of_int (int_of_bounds b))

let type_of_range (r : range) : AST.ty =
  ( match r with
  | None -> type_integer
  | Some b -> type_of_bounds b
  )

(* If x is a constrained integer, return suitable __sint(N).
 * Otherwise, return x
 *)
let xform_type (x : AST.ty) : AST.ty =
  ( match range_of_type x with
  | None -> x
  | Some b -> type_of_bounds b
  )

let xform_function_type (x : AST.function_type) : AST.function_type =
  assert (Option.is_none x.setter_arg);
  assert (not x.use_array_syntax);
  assert (not x.is_getter_setter);
  {
      parameters = x.parameters;
      args = List.map (fun (v, ty) -> (v, xform_type ty)) x.args;
      setter_arg = None;
      rty = Option.map xform_type x.rty;
      use_array_syntax = x.use_array_syntax;
      is_getter_setter = x.is_getter_setter;
      throws = x.throws;
  }

(* todo: move to asl_utils *)
let resize_sintN (m : int) (n : int) (x : AST.expr) : AST.expr =
  if m = n then
    x
  else (
    (* note: there is no requirement that m <= n *)
    let m' = expr_of_int m in
    let n' = expr_of_int n in
    Expr_TApply (resize_sintN, [m'; n'], [x; n'], NoThrow)
  )

let cvt_int_sintN (n : int) (x : AST.expr) : AST.expr =
  let n' = expr_of_int n in
  Expr_TApply (cvt_int_sintN, [n'], [x; n'], NoThrow)

let cvt_sintN_int (n : int) (x : AST.expr) : AST.expr =
  Expr_TApply (cvt_sintN_int, [expr_of_int n], [x], NoThrow)

(* Transform a possibly bounded expression to integer *)
let pack (x : (AST.expr * range)) : AST.expr =
  ( match x with
  | (e, None) -> e
  | (e, Some b) -> cvt_sintN_int (int_of_bounds b) e
  )

(* Convert a possibly bounded expression to required type *)
let cvt_to_range (x : (AST.expr * range)) (rr : range) : AST.expr =
  let (e, rx) = x in
  ( match (rx, rr) with
  | (None, None) -> e
  | (Some b, None) -> cvt_sintN_int (int_of_bounds b) e
  | (None, Some b) -> cvt_int_sintN (int_of_bounds b) e
  | (Some bx, Some br) -> resize_sintN (int_of_bounds bx) (int_of_bounds br) e
  )

(* Convert a possibly bounded expression to required type *)
let cvt_to_type (x : (AST.expr * range)) (ty : AST.ty) : AST.expr = cvt_to_range x (range_of_type ty)

(* Bounds of a 1-input contravariant function *)
let bounds_of_cofun1 (f : Z.t -> Z.t) (r1 : bounds) : bounds =
  let (lo1, hi1) = r1 in
  (f hi1, f lo1)

(* Bounds of a 2-input function that is contravariant in 2nd argument *)
let bounds_of_cofun2 (f : Z.t -> Z.t -> Z.t) (r1 : bounds) (r2 : bounds) : bounds =
  let (lo1, hi1) = r1 in
  let (lo2, hi2) = r2 in
  (f lo1 hi2, f hi1 lo2)

(* Bounds of a 2-input covariant function *)
let bounds_of_fun1 (f : Z.t -> Z.t) (r1 : bounds) : bounds =
  let (lo1, hi1) = r1 in
  (f lo1, f hi1)

(* Bounds of a 2-input covariant function *)
let bounds_of_fun2 (f : Z.t -> Z.t -> Z.t) (r1 : bounds) (r2 : bounds) : bounds =
  let (lo1, hi1) = r1 in
  let (lo2, hi2) = r2 in
  (f lo1 lo2, f hi1 hi2)

let range_of_cofun1 (f : Z.t -> Z.t) (r1 : range) : range =
    Option.map (bounds_of_cofun1 f) r1
let range_of_cofun2 (f : Z.t -> Z.t -> Z.t) (r1 : range) (r2 : range) : range =
    map2_option (bounds_of_cofun2 f) r1 r2
let range_of_fun1 (f : Z.t -> Z.t) (r1 : range) : range =
    Option.map (bounds_of_fun1 f) r1
let range_of_fun2 (f : Z.t -> Z.t -> Z.t) (r1 : range) (r2 : range) : range =
    map2_option (bounds_of_fun2 f) r1 r2

let mk_unop (op : range -> range) (f : Ident.t) (x1 : (AST.expr * range)) : (AST.expr * range) option =
  let (e1, r1) = x1 in
  let rr = op r1 in
  let r = union_range r1 rr in
  Option.map (fun b ->
    let n = int_of_bounds b in
    (AST.Expr_TApply (f, [expr_of_int n], [cvt_to_range x1 r], NoThrow), r)
    (* todo: this is slightly awkward: the type is __sintN(n) which might not match type_of_range(rr)
       For example neg_int {0..9} : {-9..0}
       This breaks the simple pattern that result type matches result range. Should we resize? *)
  ) r

let mk_binop (op : range -> range -> range) (f : Ident.t) (x1 : (AST.expr * range)) (x2 : (AST.expr * range)) : (AST.expr * range) option =
  let (e1, r1) = x1 in
  let (e2, r2) = x2 in
  let rr = op r1 r2 in
  let r = union_range (union_range r1 r2) r2 in
  Option.map (fun b ->
    let n = int_of_bounds b in
    (AST.Expr_TApply (f, [expr_of_int n], [cvt_to_range x1 r; cvt_to_range x2 r], NoThrow), r)
    (* todo: this is slightly awkward: the type is __sintN(n) which might not match type_of_range(rr)
       For example sub_int {10..20} {10} : {0..10}
       This breaks the simple pattern that result type matches result range. Should we resize? *)
  ) r

let mk_cmp (f : Ident.t) (x1 : (AST.expr * range)) (x2 : (AST.expr * range)) : (AST.expr * range) option =
  let (e1, r1) = x1 in
  let (e2, r2) = x2 in
  let r = union_range r1 r2 in
  Option.map (fun b ->
    let n = int_of_bounds b in
    (AST.Expr_TApply (f, [expr_of_int n], [cvt_to_range x1 r; cvt_to_range x2 r], NoThrow), None)
  ) r

let primop (f : Ident.t) (ftype : AST.function_type) (ps : AST.expr list) (args : (AST.expr * range) list) : (AST.expr * range) option =
  ( match (ps, args) with
  | ([], [x1; x2]) when Ident.equal f add_int -> mk_binop (range_of_fun2 prim_add_int) add_sintN x1 x2
  | ([], [x1])     when Ident.equal f neg_int -> mk_unop (range_of_cofun1 prim_neg_int) neg_sintN x1
  | ([], [x1; x2]) when Ident.equal f sub_int -> mk_binop (range_of_cofun2 prim_sub_int) sub_sintN x1 x2
  | ([], [x1; x2]) when Ident.equal f eq_int  -> mk_cmp eq_sintN x1 x2
  | ([], [x1; x2]) when Ident.equal f ne_int  -> mk_cmp ne_sintN x1 x2
  | ([], [x1; x2]) when Ident.equal f lt_int  -> mk_cmp lt_sintN x1 x2
  | ([], [x1; x2]) when Ident.equal f le_int  -> mk_cmp le_sintN x1 x2
  | ([], [x1; x2]) when Ident.equal f ge_int  -> mk_cmp ge_sintN x1 x2
  | ([], [x1; x2]) when Ident.equal f gt_int  -> mk_cmp gt_sintN x1 x2
  | _ -> None
  )

class boundedClass = object (self)
  inherit Asl_visitor.nopAslVisitor

  (* track the *original* function type for each function *)
  val mutable fun_env : AST.function_type Bindings.t = Bindings.empty;

  method add_funtype (f : Ident.t) (fty : AST.function_type) : unit =
    fun_env <- Bindings.add f fty fun_env

  method get_funtype (f : Ident.t) : AST.function_type =
    ( match Bindings.find_opt f fun_env with
    | Some fty -> fty
    | None -> failwith (Ident.name f)
    )

  (* global environment *)
  val mutable genv : range_env = empty_range_env;

  method add_grange (v : Ident.t) (r : range) : unit =
    Option.iter (fun b -> genv <- Bindings.add v b genv) r

  (* local environment *)
  val mutable lenv : range_env = empty_range_env;

  method add_lrange (v : Ident.t) (r : range) : unit =
    ( match r with
    | Some b -> lenv <- Bindings.add v b lenv
    | None -> ()
    )

  method sub_lrange (v : Ident.t) : unit =
    lenv <- Bindings.remove v lenv

  method get_range (v : Ident.t) : range =
    orelse_option (Bindings.find_opt v lenv) (fun _ -> Bindings.find_opt v genv)

  method expr (x : AST.expr) : (AST.expr * range) =
    ( match x with
    | Expr_Lit (VInt c) -> (expr_of_sintN (size_of_sint c) c, Some (c, c))
    | Expr_Lit (VIntN c) -> (x, Some (c.v, c.v))
    | Expr_Var v -> (x, self#get_range v)
    | Expr_Let (v, ty, e1, e2) ->
        let (e1', r1) = self#expr e1 in
        let ty' = Option.fold ~none:ty ~some:type_of_bounds r1 in
        self#add_lrange v r1;
        let (e2', r2) = self#expr e2 in
        self#sub_lrange v;
        (Expr_Let (v, ty', e1', e2'), r2)
    | Expr_TApply (f, ps, args, can_throw) ->
        let fty = self#get_funtype f in
        let fty' = xform_function_type fty in
        let args' = List.map self#expr args in
        ( match primop f fty' ps args' with
        | Some (e, r) -> (e, r)
        | None ->
            let arg_tys = List.map snd fty.args in
            let args'' = List.map2 cvt_to_type args' arg_tys in
            let x' = AST.Expr_TApply (f, ps, args'', can_throw) in
            let r = Option.bind fty.rty range_of_type in
            (x', r)
        )
    | _ -> (x, None)
    )

  (* Do not transform constraints *)
  method! vconstraint x = SkipChildren

  method! vexpr x =
    ( match x with
    | _ -> ChangeTo (pack (self#expr x))
    )

  val mutable return_type : AST.ty option = None

  method! vdeclitem x =
    ( match x with
    | DeclItem_Var (v, Some ty) ->
        let r = range_of_type ty in
        let ty' = xform_type ty in
        self#add_lrange v r;
        ChangeTo (DeclItem_Var (v, Some ty'))
    | _ ->
        DoChildren
    )

  method! vstmt x =
    ( match x with
    | Stmt_VarDeclsNoInit (vs, ty, loc) ->
        let r = range_of_type ty in
        let ty' = xform_type ty in
        List.iter (fun v -> self#add_lrange v r) vs;
        ChangeTo [Stmt_VarDeclsNoInit (vs, ty', loc)]

    | Stmt_TCall (f, ps, args, can_throw, loc) ->
        let fty = self#get_funtype f in
        let fty' = xform_function_type fty in
        let args' = List.map self#expr args in
        let arg_tys = List.map snd fty.args in
        let args'' = List.map2 cvt_to_type args' arg_tys in
        let s' = AST.Stmt_TCall (f, ps, args'', can_throw, loc) in
        ChangeTo [s']

    | Stmt_FunReturn (e, loc) ->
        let rty = Option.get return_type in
        let e' = cvt_to_type (self#expr e) rty in
        ChangeTo [Stmt_FunReturn (e', loc)]
    | _ -> DoChildren
    )

  method! vdecl (d : AST.declaration) : AST.declaration Visitor.visitAction =
    let xform_var (v : Ident.t) (ty : AST.ty) (e : AST.expr) : (AST.expr * AST.ty) =
      let r = Bindings.find_opt v genv in
      let e' = cvt_to_range (self#expr e) r in
      let ty' = Option.fold ~none:ty ~some:type_of_bounds r in
      (e', ty')
    in

    ( match d with
    | Decl_Const (v, Some ty, e, loc) ->
        let (e', ty') = xform_var v ty e in
        ChangeTo (AST.Decl_Const (v, Some ty', e', loc))
    | Decl_Config (v, ty, e, loc) ->
        let (e', ty') = xform_var v ty e in
        ChangeTo (AST.Decl_Config (v, ty', e', loc))
    | Decl_Var (v, ty, loc) ->
        let r = Bindings.find_opt v genv in
        let ty' = Option.fold ~none:ty ~some:type_of_bounds r in
        ChangeTo (AST.Decl_Var (v, ty', loc))

    | Decl_FunType (f, fty, loc) ->
        let fty' = self#get_funtype f in
        let fty'' = xform_function_type fty in
        ChangeTo (Decl_FunType (f, fty'', loc))

    | Decl_FunDefn (f, fty, b, loc) ->
        let fty' = self#get_funtype f in
        let fty'' = xform_function_type fty in
        lenv <- empty_range_env;
        List.iter (fun (arg, ty) -> self#add_lrange arg (range_of_type ty)) fty'.args;
        return_type <- fty'.rty;
        let b' = Asl_visitor.visit_stmts (self :> Asl_visitor.aslVisitor) b in
        ChangeTo (Decl_FunDefn (f, fty'', b', loc))

    | _ -> DoChildren
    )

  method vdecls (ds : AST.declaration list) : AST.declaration list =
    let decls : AST.declaration list ref = ref [] in
    let add (x : AST.declaration) : unit = decls := x :: !decls in

    (* first add transformed types of globals to environment *)
    List.iter (fun d ->
      ( match d with
      | AST.Decl_Const (v, Some ty, _, _) -> self#add_grange v (range_of_type ty)
      | Decl_Config (v, ty, e, loc) -> self#add_grange v (range_of_type ty)
      | Decl_Var (v, ty, loc) -> self#add_grange v (range_of_type ty)

      | Decl_BuiltinFunction (f, fty, loc) -> self#add_funtype f fty
      | Decl_FunType (f, fty, loc) -> self#add_funtype f fty
      | Decl_FunDefn (f, fty, _, loc) -> self#add_funtype f fty
      | _ -> ()
      ))
      ds;

    (* second, tranform each declaration *)
    Visitor.mapNoCopy (Asl_visitor.visit_decl (self :> Asl_visitor.aslVisitor)) ds
end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let xform = new boundedClass in
  xform#vdecls ds

(****************************************************************
 * Command: :xform_bounded
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  let options = [] in
  Commands.registerCommand "xform_bounded" options [] [] "Transform bitslice operations" cmd

(****************************************************************
 * End
 ****************************************************************)
