(****************************************************************
 * ASL to C backend
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C backend *)

module AST = Asl_ast
module FMT = Asl_fmt
module PP = Format
module V = Value
open Runtime
open Asl_utils
open Format_utils
open Identset
open Builtin_idents
open Utils

module type RuntimeLib = Runtime.RuntimeLib
let runtime = ref (module Runtime_fallback.Runtime : RuntimeLib)

let runtimes = ["ac"; "c23"; "fallback"]

let is_cxx = ref false

let set_runtime (rt : string) : unit =
  runtime := if rt = "ac" then (module Runtime_ac.Runtime : RuntimeLib)
             else if rt = "c23" then (module Runtime_c23.Runtime : RuntimeLib)
             else (module Runtime_fallback.Runtime : RuntimeLib);
  is_cxx := (rt = "ac")

let include_line_info : bool ref = ref false
let new_ffi : bool ref = ref false

let commasep (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  PP.pp_print_list
    ~pp_sep:(fun fmt' _ -> PP.pp_print_string fmt' ", ")
    pp
    fmt
    xs

let cutsep (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  PP.pp_print_list
    pp
    fmt
    xs

(** List of all the reserved words in C *)
let reserved_c = [
  "auto";
  "break";
  "case";
  "char";
  "const";
  "continue";
  "default";
  "do";
  "double";
  "else";
  "enum";
  "extern";
  "float";
  "for";
  "goto";
  "if";
  "int";
  "long";
  "register";
  "return";
  "short";
  "signed";
  "sizeof";
  "static";
  "struct";
  "switch";
  "typedef";
  "union";
  "unsigned";
  "void";
  "volatile";
  "while"
]

(** List of all the reserved words in C++ *)
let reserved_cpp = [
  "alignas";
  "alignof";
  "and";
  "and_eq";
  "asm";
  "auto";
  "bitand";
  "bitor";
  "bool";
  "break";
  "case";
  "catch";
  "char";
  "char8_t";
  "char16_t";
  "char32_t";
  "class";
  "compl";
  "concept";
  "consteval";
  "constexpr";
  "constinit";
  "const_cast";
  "continue";
  "co_await";
  "co_return";
  "co_yield";
  "decltype";
  "default";
  "delete";
  "do";
  "double";
  "dynamic_cast";
  "else";
  "enum";
  "explicit";
  "export";
  "extern";
  "false";
  "float";
  "for";
  "friend";
  "goto";
  "if";
  "inline";
  "int";
  "long";
  "mutable";
  "namespace";
  "new";
  "noexcept";
  "not";
  "not_eq";
  "nullptr";
  "operator";
  "or";
  "or_eq";
  "private";
  "protected";
  "public";
  "register";
  "reinterpret_cast";
  "requires";
  "return";
  "short";
  "signed";
  "sizeof";
  "static";
  "static_assert";
  "static_cast";
  "struct";
  "switch";
  "template";
  "this";
  "thread_local";
  "throw";
  "true";
  "try";
  "typedef";
  "typeid";
  "typename";
  "union";
  "unsigned";
  "using";
  "virtual";
  "void";
  "volatile";
  "wchar_t";
  "xor";
  "xor_eq"
]

let reserved_idents =
  reserved_c @ reserved_cpp
  |> Ident.mk_idents
  |> IdentSet.of_list

let delimiter (fmt : PP.formatter) (s : string) : unit =
  PP.pp_print_string fmt s

let ident_str (fmt : PP.formatter) (x : string) : unit =
  PP.pp_print_string fmt x

let ident (fmt : PP.formatter) (x : Ident.t) : unit =
  (* Rename any identifiers that match C/C++ reserved words *)
  if IdentSet.mem x reserved_idents
  then ident_str fmt ("__asl_" ^ Ident.name_with_tag x)
  else ident_str fmt (Ident.name_with_tag x)

(* C delimiters *)

let semicolon (fmt : PP.formatter) : unit = PP.pp_print_string fmt ";"

(* Support for packing thread-local state (e.g., registers) into a struct that is then
 * accessed via the named pointer.
 *
 * todo: at present, all global variable are treated as thread-local.
 * In the future shared state such as memory should be stored separately.
 *)
let opt_thread_local_pointer : string option ref = ref None
let thread_local_variables : Ident.t list ref = ref []

let pointer (fmt : PP.formatter) (x : Ident.t) : unit =
  ( match !opt_thread_local_pointer with
  | Some ptr when List.mem x !thread_local_variables -> PP.fprintf fmt "%s->" ptr
  | _ -> ()
  )

(* list of all exception tycons - used to decide whether to insert a tag in
 * Expr_RecordInit
 *)
let exception_tcs : Ident.t list ref = ref []

(** Supply of goto labels for exception implementation *)
let catch_labels = new Asl_utils.nameSupply "catch"

module Catcher = struct
  type t = { mutable label : Ident.t option }

  let create (_ : unit) : t = { label = None }
  let get_label (x : t) : Ident.t = Option.get x.label
  let is_active (x : t) : bool = Option.is_some x.label
  let activate (x : t) : unit = x.label <- Some catch_labels#fresh
end

let catcher_stack : Catcher.t list ref = ref []

let current_catch_label (_ : unit) : Ident.t =
  match !catcher_stack with
  | c :: _ ->
      if not (Catcher.is_active c) then Catcher.activate c;
      Catcher.get_label c
  | [] ->
      raise
        (InternalError (Unknown, "No topmost catcher", (fun _ -> ()), __LOC__))

let with_catch_label (f : Catcher.t -> unit) : unit =
  let prev = !catcher_stack in
  let catcher = Catcher.create () in
  catcher_stack := catcher :: prev;
  f catcher;
  catcher_stack := prev

let rethrow_stmt (fmt : PP.formatter) : unit =
  PP.fprintf fmt "if (ASL_exception._exc.ASL_tag != ASL_no_exception) goto %a;"
    ident (current_catch_label ())

let rethrow_expr (fmt : PP.formatter) (f : unit -> unit) : unit =
  PP.fprintf fmt "({ __auto_type __r = ";
  f ();
  PP.fprintf fmt "; ";
  rethrow_stmt fmt;
  PP.fprintf fmt " __r; })"

let const_expr (loc : Loc.t) (x : AST.expr) : V.value =
  ( match x with
  | Expr_Lit v -> v
  | _ ->
      let msg = Format.asprintf "const_expr: not literal constant '%a'" FMT.expr x in
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, msg, pp))
  )

let const_int_expr (loc : Loc.t) (x : AST.expr) : int =
  let v = const_expr loc x in
  ( match v with
  | VInt i -> Z.to_int i
  | _ ->
      let msg = Format.asprintf "const_int_expr: integer expected '%a'" V.pp_value v in
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, msg, pp))
  )

let const_int_exprs (loc : Loc.t) (xs : AST.expr list) : int list =
  List.map (const_int_expr loc) xs

let valueLit (loc : Loc.t) (fmt : PP.formatter) (x : Value.value) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match x with
  | VInt v    -> Runtime.int_literal fmt v
  | VIntN v   -> Runtime.sintN_literal fmt v
  | VBits v   -> Runtime.bits_literal fmt v
  | VString v -> PP.pp_print_string fmt ("\"" ^ String.escaped v ^ "\"")
  | _ -> raise (InternalError (loc, "valueLit", (fun fmt -> Value.pp_value fmt x), __LOC__))
  )

let rec apply (loc : Loc.t) (fmt : PP.formatter) (f : unit -> unit) (args : AST.expr list) : unit =
  f ();
  parens fmt (fun _ -> exprs loc fmt args)

and unop (loc : Loc.t) (fmt : PP.formatter) (op : string) (x : AST.expr) : unit =
  PP.fprintf fmt "(%s %a)"
    op
    (expr loc) x

and binop (loc : Loc.t) (fmt : PP.formatter) (op : string) (x : AST.expr) (y : AST.expr) : unit =
  PP.fprintf fmt "(%a %s %a)"
    (expr loc) x
    op
    (expr loc) y

and mk_expr (loc : Loc.t) : AST.expr -> rt_expr = Runtime.mk_rt_expr (expr loc)

and funcall (loc : Loc.t) (fmt : PP.formatter) (f : Ident.t) (tes : AST.expr list) (args : AST.expr list) =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match (const_int_exprs loc tes, args) with

  (* Boolean builtin functions *)
  | ([], [x;y]) when Ident.equal f eq_bool      -> binop loc fmt "==" x y
  | ([], [x;y]) when Ident.equal f equiv_bool   -> binop loc fmt "==" x y
  | ([], [x;y]) when Ident.equal f ne_bool      -> binop loc fmt "!=" x y
  | ([], [x;y]) when Ident.equal f and_bool     -> binop loc fmt "&&" x y
  | ([], [x;y]) when Ident.equal f or_bool      -> binop loc fmt "||" x y
  | ([], [x])   when Ident.equal f not_bool     -> unop loc fmt "!" x
  | ([], [x;y]) when Ident.equal f implies_bool -> cond loc fmt x y Asl_utils.asl_true

  (* Enumeration builtin functions *)
  (* The reason we need direct checks against eq_enum and ne_enum is because
     the identifier eq_enum with tag 0 doesn't have a root. And we need this
     function identifier in the xform_case transform right now *)
  | ([], [x;y]) when Ident.equal f eq_enum -> binop loc fmt "==" x y
  | ([], [x;y]) when Ident.equal f ne_enum -> binop loc fmt "!=" x y
  | ([], [x;y]) when Ident.root_equal f ~root:eq_enum -> binop loc fmt "==" x y
  | ([], [x;y]) when Ident.root_equal f ~root:ne_enum -> binop loc fmt "!=" x y

  (* Integer builtin functions *)
  | ([], [x;y]) when Ident.equal f add_int -> Runtime.add_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x])   when Ident.equal f neg_int ->
      ( match x with
      | Expr_Lit (VInt l) -> Runtime.int_literal fmt (Z.neg l)
      | _ -> Runtime.neg_int fmt (mk_expr loc x)
      )
  | ([], [x;y]) when Ident.equal f sub_int -> Runtime.sub_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f shl_int -> Runtime.shl_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f shr_int -> Runtime.shr_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f mul_int -> Runtime.mul_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f zdiv_int -> Runtime.zdiv_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f zrem_int -> Runtime.zrem_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f exact_div_int -> Runtime.exact_div_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f fdiv_int -> Runtime.fdiv_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f frem_int -> Runtime.frem_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f eq_int -> Runtime.eq_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f ne_int -> Runtime.ne_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f ge_int -> Runtime.ge_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f gt_int -> Runtime.gt_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f le_int -> Runtime.le_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f lt_int -> Runtime.lt_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f align_int -> Runtime.align_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x])   when Ident.equal f is_pow2_int -> Runtime.is_pow2_int fmt (mk_expr loc x)
  | ([], [x;y]) when Ident.equal f mod_pow2_int -> Runtime.mod_pow2_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x])   when Ident.equal f pow2_int -> Runtime.pow2_int fmt (mk_expr loc x)
  | ([], [x])   when Ident.equal f print_int_dec -> Runtime.print_int_dec fmt (mk_expr loc x)
  | ([], [x])   when Ident.equal f print_int_hex -> Runtime.print_int_hex fmt (mk_expr loc x)

  (* Bounded integer builtin functions *)
  | ([n], [x;y]) when Ident.equal f add_sintN -> Runtime.add_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x])   when Ident.equal f neg_sintN -> Runtime.neg_sintN fmt n (mk_expr loc x)
  | ([n], [x;y]) when Ident.equal f sub_sintN -> Runtime.sub_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f shl_sintN -> Runtime.shl_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f shr_sintN -> Runtime.shr_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f mul_sintN -> Runtime.mul_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f zdiv_sintN -> Runtime.zdiv_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f zrem_sintN -> Runtime.zrem_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f exact_div_sintN -> Runtime.exact_div_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f fdiv_sintN -> Runtime.fdiv_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f frem_sintN -> Runtime.frem_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f eq_sintN -> Runtime.eq_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f ne_sintN -> Runtime.ne_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f ge_sintN -> Runtime.ge_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f gt_sintN -> Runtime.gt_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f le_sintN -> Runtime.le_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f lt_sintN -> Runtime.lt_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f align_sintN -> Runtime.align_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x])   when Ident.equal f is_pow2_sintN -> Runtime.is_pow2_sintN fmt n (mk_expr loc x)
  | ([n], [x;y]) when Ident.equal f mod_pow2_sintN -> Runtime.mod_pow2_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x])   when Ident.equal f pow2_sintN -> Runtime.pow2_sintN fmt n (mk_expr loc x)
  | ([n], [x])   when Ident.equal f cvt_bits_ssintN -> Runtime.cvt_bits_ssintN fmt n (mk_expr loc x)
  | ([n], [x])   when Ident.equal f cvt_bits_usintN -> Runtime.cvt_bits_usintN fmt n (mk_expr loc x)
  | ([m;n],[x;_])  when Ident.equal f cvt_sintN_bits -> Runtime.cvt_sintN_bits fmt m n (mk_expr loc x)
  | ([m;n], [x;_]) when Ident.equal f resize_sintN -> Runtime.resize_sintN fmt m n (mk_expr loc x)
  | ([n],[x])      when Ident.equal f cvt_sintN_int -> Runtime.cvt_sintN_int fmt n (mk_expr loc x)
  | ([n],[x;_])    when Ident.equal f cvt_int_sintN -> Runtime.cvt_int_sintN fmt n (mk_expr loc x)
  | ([n], [x])   when Ident.equal f print_sintN_dec -> Runtime.print_sintN_dec fmt n (mk_expr loc x)
  | ([n], [x])   when Ident.equal f print_sintN_hex -> Runtime.print_sintN_hex fmt n (mk_expr loc x)

  (* Real builtin functions *)
  | (_, _) when Ident.in_list f [add_real;
    cvt_int_real;
    divide_real;
    eq_real ;
    ge_real;
    gt_real;
    le_real;
    lt_real;
    mul_real;
    ne_real ;
    neg_real;
    pow2_real;
    round_down_real;
    round_tozero_real;
    round_up_real ;
    sqrt_real;
    sub_real] ->
      let pp fmt = FMT.funname fmt f in
      raise (Error.Unimplemented (loc, "real builtin function", pp))

  (* Bitvector builtin functions *)
  | ([n],   [x;y]) when Ident.equal f eq_bits -> Runtime.eq_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f ne_bits -> Runtime.ne_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f add_bits -> Runtime.add_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f sub_bits -> Runtime.sub_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f mul_bits -> Runtime.mul_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f and_bits -> Runtime.and_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f or_bits -> Runtime.or_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f eor_bits -> Runtime.eor_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x])   when Ident.equal f not_bits -> Runtime.not_bits fmt n (mk_expr loc x)
  | ([n],   [x;y]) when Ident.equal f lsl_bits -> Runtime.lsl_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f lsr_bits -> Runtime.lsr_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f asr_bits -> Runtime.asr_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x])   when Ident.equal f cvt_bits_sint -> Runtime.cvt_bits_sint fmt n (mk_expr loc x)
  | ([n],   [x])   when Ident.equal f cvt_bits_uint -> Runtime.cvt_bits_uint fmt n (mk_expr loc x)
  | ([n],   [x;_]) when Ident.equal f cvt_int_bits -> Runtime.cvt_int_bits fmt n (mk_expr loc x)
  | ([n],   [x;y]) when Ident.equal f mk_mask -> Runtime.mk_mask fmt n (mk_expr loc x)
  | ([n],   [x])   when Ident.equal f zeros_bits -> Runtime.zeros_bits fmt n
  | ([n],   [x])   when Ident.equal f ones_bits -> Runtime.ones_bits fmt n
  | ([m;n], [x;y]) when Ident.equal f append_bits -> Runtime.append_bits fmt m n (mk_expr loc x) (mk_expr loc y)
  | ([m;n], [x;y]) when Ident.equal f replicate_bits -> Runtime.replicate_bits fmt m n (mk_expr loc x) (mk_expr loc y)
  | ([m;n], [x;_]) when Ident.equal f zero_extend_bits -> Runtime.zero_extend_bits fmt m n (mk_expr loc x)
  | ([m;n], [x;_]) when Ident.equal f sign_extend_bits -> Runtime.sign_extend_bits fmt m n (mk_expr loc x)
  | ([n],   [x])   when Ident.equal f print_bits_hex -> Runtime.print_bits_hex fmt n (mk_expr loc x)

  | _ when Ident.in_list f
      [ frem_bits_int;
        in_mask;
        notin_mask
      ] ->
      let pp fmt = FMT.funname fmt f in
      raise (Error.Unimplemented (loc, "bitvector builtin function", pp))

  (* String builtin functions *)
  | _ when Ident.in_list f [
        append_str_str;
        cvt_bits_str;
        cvt_bool_str;
        cvt_int_decstr;
        cvt_int_hexstr;
        cvt_real_str
      ] ->
      PP.fprintf fmt "\"\""
  | _ when Ident.in_list f [ eq_str; ne_str ] ->
      let pp fmt = FMT.funname fmt f in
      raise (Error.Unimplemented (loc, "string builtin function", pp))

  (* RAM builtin functions *)
  | ([a], [_;ram;v]) when Ident.equal f ram_init -> Runtime.ram_init fmt a (mk_expr loc ram) (mk_expr loc v)
  | ([n;a], [_;_;ram;addr]) when Ident.equal f ram_read -> Runtime.ram_read fmt a n (mk_expr loc ram) (mk_expr loc addr)
  | ([n;a], [_;_;ram;addr;v]) when Ident.equal f ram_write -> Runtime.ram_write fmt a n (mk_expr loc ram) (mk_expr loc addr) (mk_expr loc v)

  (* Printing builtin functions *)
  | ([], [x]) when Ident.equal f print_char -> Runtime.print_char fmt (mk_expr loc x)
  | ([], [x]) when Ident.equal f print_str -> Runtime.print_str fmt (mk_expr loc x)

  (* File builtin functions *)
  | _ when Ident.in_list f [ asl_file_getc; asl_file_open; asl_file_write ] ->
      let pp fmt = FMT.funname fmt f in
      raise (Error.Unimplemented (loc, "file builtin function", pp))

  (* Helper functions with ASL_ prefix *)
  | _ when String.starts_with ~prefix:"ASL_" (Ident.name f) ->
      apply loc fmt (fun _ -> ident_str fmt (Ident.name f)) args

  (* User defined function *)
  | _ -> apply loc fmt (fun _ -> ident fmt f) args
  )

and cond_cont (loc : Loc.t) (fmt : PP.formatter) (c : AST.expr) (x : AST.expr)
    (y : unit -> unit) : unit =
  parens fmt (fun _ ->
      expr loc fmt c;
      nbsp fmt;
      delimiter fmt "?";
      nbsp fmt;
      expr loc fmt x;
      nbsp fmt;
      delimiter fmt ":";
      nbsp fmt;
      y ())

and cond (loc : Loc.t) (fmt : PP.formatter) (c : AST.expr) (x : AST.expr) (y : AST.expr) :
    unit =
  cond_cont loc fmt c x (fun _ -> expr loc fmt y)

and conds (loc : Loc.t) (fmt : PP.formatter) (cts : (AST.expr * AST.expr) list) (e : AST.expr) : unit =
  ( match cts with
  | [] -> expr loc fmt e
  | (c, t) :: cts' -> cond_cont loc fmt c t (fun _ -> conds loc fmt cts' e)
  )

and expr (loc : Loc.t) (fmt : PP.formatter) (x : AST.expr) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match x with
  | Expr_Concat _ ->
      raise (InternalError (loc, "Expr_Concat not expected", (fun fmt -> FMT.expr fmt x), __LOC__))
  | Expr_Field (e, f) ->
      PP.fprintf fmt "%a.%a"
        (expr loc) e
        ident f
  | Expr_If (c, t, els, e) ->
      let els1 = List.map (function AST.E_Elsif_Cond (c, e) -> (c, e)) els in
      conds loc fmt ((c, t) :: els1) e
  | Expr_Let (v, t, e, b) ->
      PP.fprintf fmt "({ const ";
      varty loc fmt v t;
      PP.fprintf fmt " = %a; %a; })"
        (expr loc) e
        (expr loc) b
  | Expr_Lit v -> valueLit loc fmt v
  | Expr_RecordInit (tc, [], fas) ->
      if List.mem tc !exception_tcs then begin
        PP.fprintf fmt "(ASL_exception_t){ ._%a={ .ASL_tag = tag_%a, " ident tc ident tc;
        commasep
          (fun fmt' (f, e) -> PP.fprintf fmt' ".%a = %a" ident f (expr loc) e)
          fmt
          fas;
        PP.fprintf fmt " }}"
      end else begin
        PP.fprintf fmt "(%a){ " ident tc;
        commasep
          (fun fmt' (f, e) -> PP.fprintf fmt' ".%a = %a" ident f (expr loc) e)
          fmt
          fas;
        PP.fprintf fmt " }"
      end
  | Expr_Slices (Type_Bits (n,_), e, [Slice_LoWd (lo, wd)]) ->
      let module Runtime = (val (!runtime) : RuntimeLib) in
      Runtime.get_slice fmt (const_int_expr loc n) (const_int_expr loc wd) (mk_expr loc e) (mk_expr loc lo)
  | Expr_Slices (Type_Integer _, e, [Slice_LoWd (lo, wd)]) when lo = Asl_utils.zero ->
      let module Runtime = (val (!runtime) : RuntimeLib) in
      Runtime.cvt_int_bits fmt (const_int_expr loc wd) (mk_expr loc e)
  | Expr_TApply (f, tes, es, throws) ->
      if throws <> NoThrow then
        rethrow_expr fmt (fun _ -> funcall loc fmt f tes es)
      else
        funcall loc fmt f tes es
  | Expr_Var v ->
      if Ident.equal v true_ident then ident_str fmt "true"
      else if Ident.equal v false_ident then ident_str fmt "false"
      else begin
        pointer fmt v;
        ident fmt v
      end
  | Expr_Array (a, i) ->
      PP.fprintf fmt "%a[%a]"
        (expr loc) a
        (index_expr loc) i
  | Expr_In (e, Pat_Lit (VMask m)) ->
      Runtime.in_bits fmt m.n (mk_expr loc e) m
  | Expr_AsConstraint (e, _)
  | Expr_AsType (e, _) ->
      expr loc fmt e
  | Expr_ArrayInit _
  | Expr_Binop _
  | Expr_Fields _
  | Expr_In _
  | Expr_RecordInit _
  | Expr_Slices _
  | Expr_Tuple _
  | Expr_Unknown _
  | Expr_Unop _ ->
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, "expression", pp))
  )

and exprs (loc : Loc.t) (fmt : PP.formatter) (es : AST.expr list) : unit =
  commasep (expr loc) fmt es

(* The same as expr except that it guarantees that the result is a legal type
 * to use as a C/C++ array index.
 *)
and index_expr (loc : Loc.t) (fmt : PP.formatter) (x : AST.expr) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  Runtime.ffi_integer_to_c_sint64 fmt (mk_expr loc x)

and varty (loc : Loc.t) (fmt : PP.formatter) (v : Ident.t) (x : AST.ty) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match x with
  | Type_Bits (n, _) ->
    Runtime.ty_bits fmt (const_int_expr loc n);
    nbsp fmt;
    ident fmt v
  | Type_Constructor (tc, []) ->
      ( match tc with
      | i when Ident.equal i boolean_ident ->
        PP.fprintf fmt "bool %a"
          ident v
      | i when Ident.equal i string_ident ->
        PP.fprintf fmt "const char *%a"
          ident v
      | _ ->
        PP.fprintf fmt "%a %a"
          ident tc
          ident v
      )
  | Type_Constructor (i, [n]) when Ident.equal i Builtin_idents.sintN ->
    Runtime.ty_sintN fmt (const_int_expr loc n);
    nbsp fmt;
    ident fmt v
  | Type_Constructor (i, [_]) when Ident.equal i Builtin_idents.ram ->
    Runtime.ty_ram fmt;
    nbsp fmt;
    ident fmt v
  | Type_Integer _ ->
    Runtime.ty_int fmt;
    nbsp fmt;
    ident fmt v
  | Type_Array (Index_Enum tc, ety) ->
    varty loc fmt v ety;
    PP.fprintf fmt "[%a]" ident tc
  | Type_Array (Index_Int (Expr_Lit (VInt sz)), ety) ->
    varty loc fmt v ety;
    PP.fprintf fmt "[%s]" (Z.format "%d" sz)
  | Type_Constructor (_, _)
  | Type_OfExpr _
  | Type_Tuple _
  | Type_Array _ ->
      let pp fmt = FMT.ty fmt x in
      raise (Error.Unimplemented (loc, "type", pp))
  )

and varoty (loc : Loc.t) (fmt : PP.formatter) (v : Ident.t) (ot : AST.ty option) : unit =
  match ot with
  | None -> raise (InternalError (loc, "expected identifier to have a type", (fun fmt -> FMT.varname fmt v), __LOC__))
  | Some t -> varty loc fmt v t

let pattern (loc : Loc.t) (fmt : PP.formatter) (x : AST.pattern) : unit =
  ( match x with
  | Pat_Lit (VInt c) ->
    if not (Z.fits_int64 c) then begin
      let pp fmt = FMT.pattern fmt x in
      raise (Error.Unimplemented (loc, "large (> 64 bit) integer pattern", pp))
    end;
    PP.fprintf fmt "%sLL" (Z.format "%d" c)
  | Pat_Lit (VBits c) ->
    if c.n > 64 then begin
      let pp fmt = FMT.pattern fmt x in
      raise (Error.Unimplemented (loc, "large (> 64 bit) bitvector pattern", pp))
    end;
    PP.fprintf fmt "0x%sULL" (Z.format "%x" c.v)
  | Pat_Lit v -> valueLit loc fmt v
  | Pat_Const v ->
      if Ident.equal v true_ident then ident_str fmt "true"
      else if Ident.equal v false_ident then ident_str fmt "false"
      else ident fmt v
  | Pat_Range _ | Pat_Set _ | Pat_Single _
  | Pat_Tuple _ | Pat_Wildcard ->
      let pp fmt = FMT.pattern fmt x in
      raise (Error.Unimplemented (loc, "pattern", pp))
  )

let rec lexpr (loc : Loc.t) (fmt : PP.formatter) (x : AST.lexpr) : unit =
  ( match x with
  | LExpr_Var v ->
    pointer fmt v;
    ident fmt v
  | LExpr_Array (a, i) ->
    PP.fprintf fmt "%a[%a]"
      (lexpr loc) a
      (index_expr loc) i
  | LExpr_Field (l, f) ->
    PP.fprintf fmt "%a.%a"
      (lexpr loc) l
      ident f
  | LExpr_Wildcard
  | LExpr_BitTuple _
  | LExpr_Fields _
  | LExpr_ReadWrite _
  | LExpr_Slices _
  | LExpr_Tuple _
  | LExpr_Write _ ->
      let pp fmt = FMT.lexpr fmt x in
      raise (Error.Unimplemented (loc, "l-expression", pp))
  )

let mk_lexpr (loc : Loc.t) (l : AST.lexpr) : rt_expr = fun fmt -> lexpr loc fmt l

let lslice (loc : Loc.t) (fmt : PP.formatter) (t : AST.ty) (l : AST.lexpr) (r : AST.expr) (s : AST.slice) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match (t, s) with
  | (Type_Bits (n, _), Slice_LoWd (lo, wd)) ->
      Runtime.set_slice fmt (const_int_expr loc n) (const_int_expr loc wd) (mk_lexpr loc l) (mk_expr loc lo) (mk_expr loc r)
  | _ -> raise (InternalError (loc, "Only Slice_LoWd is supported", (fun fmt -> FMT.lexpr fmt l), __LOC__))
  )

let lexpr_assign (loc : Loc.t) (fmt : PP.formatter) (x : AST.lexpr) (r : AST.expr) : unit =
  ( match x with
  | LExpr_Slices (t, l, ss) ->
      List.iter (lslice loc fmt t l r) ss;
  | LExpr_Wildcard ->
      PP.fprintf fmt "(void)%a;"
        (expr loc) r
  | _ ->
      PP.fprintf fmt "%a = %a;@,"
        (lexpr loc) x
        (expr loc) r
  )

let rec declitem (loc : Loc.t) (fmt : PP.formatter) (x : AST.decl_item) =
  ( match x with
  | DeclItem_Var (v, Some t) ->
      varty loc fmt v t;
      PP.fprintf fmt ";@,"
  | DeclItem_Tuple dis ->
      cutsep (declitem loc) fmt dis;
      cut fmt
  | DeclItem_BitTuple dbs ->
      let pp fmt = FMT.decl_item fmt x in
      raise (Error.Unimplemented (loc, "declitem: bittuple", pp))
  | DeclItem_Var (v, None) ->
      let pp fmt = FMT.varname fmt v in
      raise (Error.Unimplemented (loc, "decl: type of variable unknown", pp))
  | DeclItem_Wildcard _ -> ()
  )

let decl (fmt : PP.formatter) (x : AST.stmt) : unit =
  ( match x with
  | Stmt_VarDeclsNoInit (vs, (Type_Constructor (i, [ _ ]) as t), loc)
    when Ident.equal i Builtin_idents.ram ->
      List.iter (fun v ->
          varty loc fmt v t;
          PP.fprintf fmt " __attribute__((cleanup(ASL_ram_free)));@,"
      )
      vs
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      List.iter (fun v ->
          varty loc fmt v t;
          PP.fprintf fmt ";@,"
      )
      vs
  | Stmt_VarDecl (di, i, loc) | Stmt_ConstDecl (di, i, loc) -> declitem loc fmt di
  | _ -> ()
  )

let direction (x : AST.direction) (up : 'a) (down : 'a) : 'a =
  ( match x with
  | Direction_Up -> up
  | Direction_Down -> down
  )

let stmt_line_info (fmt : PP.formatter) (x : AST.stmt) : unit =
  if !include_line_info then
    ( match Asl_utils.stmt_loc x with
    | Range (p, _) ->
        let fname = p.Lexing.pos_fname in
        let line = p.Lexing.pos_lnum in
        PP.fprintf fmt "#line %d \"%s\"@," line fname
    | _ -> ()
    )

let rec stmt (fmt : PP.formatter) (x : AST.stmt) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  stmt_line_info fmt x;
  ( match x with
  | Stmt_Assert (e, loc) ->
      PP.fprintf fmt "ASL_assert(\"%s\", \"%s\", %a);"
        (String.escaped (Loc.to_string loc))
        (String.escaped (Utils.to_string2 (Fun.flip FMT.expr e)))
        (expr loc) e
  | Stmt_Assign (l, r, loc) ->
      lexpr_assign loc fmt l r
  | Stmt_Block (ss, _) ->
      PP.fprintf fmt "{%a@,}" indented_block ss
  | Stmt_Case (e, Some ty, alts, ob, loc) ->
      vbox fmt (fun _ ->
          PP.fprintf fmt "switch (";
          ( match ty with
          | Type_Integer _
          -> Runtime.ffi_integer_to_c_sint64 fmt (mk_expr loc e)
          | Type_Bits (n,_)
          -> Runtime.ffi_bits_to_c_uint64 fmt (const_int_expr loc n) (mk_expr loc e)
          | _
          -> expr loc fmt e
          );
          PP.fprintf fmt ") {@,";
          indented fmt (fun _ ->
              map fmt
                (fun (AST.Alt_Alt (ps, oc, ss, loc)) ->
                  if Option.is_some oc then
                    raise (Error.Unimplemented (loc, "pattern_guard", fun fmt -> ()));
                  List.iter (PP.fprintf fmt "case %a:@," (pattern loc)) ps;
                  Format.fprintf fmt "{%a@,    break;@,}@," indented_block ss)
                alts;

              PP.fprintf fmt "default: {";
              ( match ob with
              | Some (b, bl) ->
                  indented_block fmt b;
              | None ->
                  indented fmt (fun _ ->
                    Format.fprintf fmt "ASL_error_unmatched_case(\"%s\");@,"
                      (String.escaped (Loc.to_string loc))
                  )
              );
              PP.fprintf fmt "@,}"
            );
          PP.fprintf fmt "@,}"
      )
  | Stmt_VarDecl (DeclItem_Var (v, _), i, loc)
  | Stmt_ConstDecl (DeclItem_Var (v, _), i, loc) ->
      Format.fprintf fmt "%a = %a;"
        ident v
        (expr loc) i
  | Stmt_VarDecl (DeclItem_Wildcard _, i, loc)
  | Stmt_ConstDecl (DeclItem_Wildcard _, i, loc) ->
      PP.fprintf fmt "(void)%a;"
        (expr loc) i
  | Stmt_For (v, f, dir, t, b, loc) ->
      PP.fprintf fmt  "for (%a %a = %a; %a %s %a; %s%a) {%a@,}"
          (fun fmt _ -> Runtime.ty_int fmt) ()
          ident v
          (expr loc) f

          ident v
          (direction dir "<=" ">=")
          (expr loc) t

          (direction dir "++" "--")
          ident v
          indented_block b
  | Stmt_FunReturn (e, loc) ->
      PP.fprintf fmt "return %a;" (expr loc) e
  | Stmt_If (c, t, els, (e, el), loc) ->
      vbox fmt (fun _ ->
          PP.fprintf fmt "if (%a) {%a@,}"
            (expr loc) c
            indented_block t;
          map fmt
            (fun (AST.S_Elsif_Cond (c, s, loc)) ->
              PP.fprintf fmt " else if (%a) {%a@,}"
                (expr loc) c
                indented_block s)
            els;
          if e <> [] then begin
            PP.fprintf fmt " else {%a@,}"
              indented_block e
          end)
  | Stmt_While (c, b, loc) ->
      PP.fprintf fmt "while (%a) {%a@,}"
        (expr loc) c
        indented_block b
  | Stmt_Repeat (b, c, pos, loc) ->
      PP.fprintf fmt "do {%a@,} while (!(%a));"
        indented_block b
        (expr loc) c
  | Stmt_ProcReturn loc ->
      PP.fprintf fmt "return;"
  | Stmt_TCall (f, tes, args, throws, loc) ->
      funcall loc fmt f tes args;
      semicolon fmt;
      if throws <> NoThrow then rethrow_stmt fmt
  | Stmt_VarDeclsNoInit (vs, Type_Constructor (i, [ _ ]), loc)
    when Ident.equal i Builtin_idents.ram ->
      cutsep (fun fmt' -> PP.fprintf fmt' "%a = ASL_ram_alloc();" ident) fmt vs
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      (* handled by decl *)
      ()
  | Stmt_Throw (e, loc) ->
      PP.fprintf fmt "ASL_exception = %a;@," (expr loc) e;
      PP.fprintf fmt "goto %a;" ident (current_catch_label ())
  | Stmt_Try (tb, pos, catchers, odefault, loc) ->
      with_catch_label (fun catcher ->
        PP.fprintf fmt "{%a@,}" indented_block tb;
        if Catcher.is_active catcher then
          PP.fprintf fmt "@,%a:" ident (Catcher.get_label catcher);
        cut fmt
      );
      PP.fprintf fmt "if (ASL_exception._exc.ASL_tag == ASL_no_exception) {@,";
      List.iter (function AST.Catcher_Guarded (v, tc, b, loc) ->
          PP.fprintf fmt "} else if (ASL_exception._exc.ASL_tag == tag_%a) {" ident tc;
          indented fmt (fun _ ->
            PP.fprintf fmt "%a %a = ASL_exception._%a;@,"
              ident tc
              ident v
              ident tc;
            PP.fprintf fmt "ASL_exception._exc.ASL_tag = ASL_no_exception;@,";
            PP.fprintf fmt "{%a@,}" indented_block b
            );
          cut fmt
        )
        catchers;
      PP.fprintf fmt "} else {";
      indented fmt (fun _ ->
        ( match odefault with
        | None -> PP.fprintf fmt "goto %a;@," ident (current_catch_label ())
        | Some (s, _) ->
            PP.fprintf fmt "ASL_exception._exc.ASL_tag = ASL_no_exception;@,";
            PP.fprintf fmt "{%a@,}" indented_block s
        ));
      PP.fprintf fmt "@,}"
  | Stmt_VarDecl _
  | Stmt_ConstDecl _
  | Stmt_Case _
  ->
    let pp fmt = FMT.stmt fmt x in
    raise (Error.Unimplemented (Loc.Unknown, "statement", pp))
  )

and indented_block (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if xs <> [] then begin
    indented fmt (fun _ ->
      map fmt (decl fmt) xs;
      cutsep stmt fmt xs)
  end

let formal (loc : Loc.t) (fmt : PP.formatter) (x : Ident.t * AST.ty) : unit =
  let v, t = x in
  varty loc fmt v t

let function_header (loc : Loc.t) (fmt : PP.formatter) (f : Ident.t) (fty : AST.function_type) : unit =
  PP.pp_print_option
    ~none:(fun _ _ -> PP.fprintf fmt "void "; ident fmt f)
    (fun _ t -> varty loc fmt f t) fmt fty.rty;
  parens fmt (fun _ -> commasep (formal loc) fmt fty.args)

let function_body (loc : Loc.t) (fmt : PP.formatter) (b : AST.stmt list) (orty : AST.ty option) : unit =
  with_catch_label (fun catcher ->
    braces fmt
      (fun _ ->
         indented_block fmt b;
         cut fmt;

         if Catcher.is_active catcher then (
           PP.fprintf fmt "%a:" ident (Catcher.get_label catcher);
           (* When throwing an exception, we need to return a value with
            * the correct type. This value will never be used so the easy way
            * to create it is to declare a variable, not initialize it and
            * return the variables. This will make the C compiler emit a warning.
            * *)
           indented fmt (fun _ ->
             match orty with
             | None -> PP.fprintf fmt "return;"
             | Some rty ->
               braces fmt (fun _ ->
                 indented fmt (fun _ ->
                   let v = asl_fake_return_value in
                   varty loc fmt v rty;
                   PP.fprintf fmt ";@,return %a;" ident v
                 );
                 cut fmt
               )
             );
           cut fmt
         )
      )
    )

let pp_field (loc : Loc.t) (fmt : PP.formatter) (f : (Ident.t * AST.ty)) : unit =
  let (fname, t) = f in
  varty loc fmt fname t;
  semicolon fmt

let declaration (fmt : PP.formatter) ?(is_extern : bool option) (x : AST.declaration) : unit =
  let is_extern_val = Option.value is_extern ~default:false in
  vbox fmt (fun _ ->
      match x with
      | Decl_BuiltinType (tc, loc) ->
          ( match tc with
          | _ when Ident.in_list tc [
              real_ident;
              string_ident;
              mask_ident;
              ram
            ] -> ()
          | _ ->
              let pp fmt = FMT.tycon fmt tc in
              raise (Error.Unimplemented (Loc.Unknown, "builtin type", pp))
          )
      | Decl_Const (v, oty, e, loc) ->
          PP.fprintf fmt "const ";
          varoty loc fmt v oty;
          if not is_extern_val then (
            PP.fprintf fmt " = ";
            ( match e with
            | Expr_ArrayInit es -> PP.fprintf fmt "{ %a }" (exprs loc) es
            | _ -> expr loc fmt e
            )
          );
          PP.fprintf fmt ";@,@,"
      | Decl_Config (v, ty, i, loc) ->
          varty loc fmt v ty;
          if not is_extern_val then (
            PP.fprintf fmt " = ";
            expr loc fmt i
          );
          PP.fprintf fmt ";@,@,"
      | Decl_Enum (tc, es, loc) ->
          if Ident.equal tc boolean_ident then (
              (* is in C99 stdbool.h *)
          ) else (
            PP.fprintf fmt "typedef enum {%a} %a;@,@,"
              (commasep ident) es
              ident tc;
          )
      | Decl_FunDefn (f, fty, b, loc) ->
          function_header loc fmt f fty;
          nbsp fmt;
          function_body loc fmt b fty.rty;
          PP.fprintf fmt "@,@,"
      | Decl_FunType (f, fty, loc) ->
          function_header loc fmt f fty;
          PP.fprintf fmt ";@,@,"
      | Decl_Record (tc, [], fs, loc) ->
          PP.fprintf fmt "typedef struct {";
          indented fmt (fun _ -> cutsep (pp_field loc) fmt fs);
          PP.fprintf fmt "@,} %a;@,@," ident tc
      | Decl_Typedef (tc, [], t, loc) ->
          PP.fprintf fmt "typedef ";
          varty loc fmt tc t;
          PP.fprintf fmt ";@,@,"
      | Decl_Var (v, ty, loc) ->
          (match ty with
          | Type_Constructor (i, [ _ ])
            when Ident.equal i Builtin_idents.ram && not is_extern_val ->
              let ram fmt v = PP.fprintf fmt "ASL_%a" ident v in
              PP.fprintf fmt "struct ASL_ram %a = (struct ASL_ram){ 0 };@," ram v;
              varty loc fmt v ty;
              PP.fprintf fmt " = &%a;@,@," ram v
          | _ ->
              varty loc fmt v ty;
              PP.fprintf fmt ";@,@,"
          );
      | Decl_BuiltinFunction (f, fty, loc) ->
          ()
      | _ ->
          let pp fmt = FMT.declaration fmt x in
          raise (Error.Unimplemented (Loc.Unknown, "declaration", pp))
      )

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ -> map fmt (declaration fmt) xs)

let extern_declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ ->
      map fmt
        (fun x ->
          PP.fprintf fmt "extern @?";
          declaration fmt ~is_extern:true x)
        xs)

let exceptions (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ ->
    let excs = List.filter_map
        ( function
        | AST.Decl_Exception (tc, fs, loc) -> Some (tc, fs, loc)
        | _ -> None
        )
        xs
    in
    exception_tcs := List.map (fun (tc, _, _) -> tc) excs;
    PP.fprintf fmt "typedef enum ASL_exception_tag { ASL_no_exception, %a } ASL_exception_tag_t;@,@,"
      (commasep (fun fmt' (tc, _, _) -> PP.fprintf fmt' "tag_%a" ident tc)) excs;
    List.iter (fun (tc, fs, loc) ->
        PP.fprintf fmt "typedef struct {@,";
        PP.fprintf fmt "    ASL_exception_tag_t ASL_tag;@,";
        indented fmt (fun _ -> cutsep (pp_field loc) fmt fs);
        PP.fprintf fmt "} %a;@,@," ident tc
      )
      excs;
    PP.fprintf fmt "typedef union {\n    %s\n%a\n} ASL_exception_t;@,"
      "struct { ASL_exception_tag_t ASL_tag; } _exc;"
      (PP.pp_print_list (fun fmt (tc, _, _) ->
          PP.fprintf fmt "    %a _%a;"
            ident tc
            ident tc)
      )
      excs;
    PP.fprintf fmt "@,extern ASL_exception_t ASL_exception;@,";
  )

let exceptions_init (fmt : PP.formatter) : unit =
  vbox fmt (fun _ ->
    PP.fprintf fmt "ASL_exception_t ASL_exception =@,";
    PP.fprintf fmt "    (ASL_exception_t){ ._exc = { .ASL_tag = ASL_no_exception } };@,"
  )

(****************************************************************
 * Generating files
 *
 * Generate separate files containing
 * - types, constants and function prototypes
 * - variable definitions
 * - function definitions
 ****************************************************************)

let type_decls (xs : AST.declaration list) : AST.declaration list =
  let mk_type_decl (x : AST.declaration) : AST.declaration option =
    ( match x with
    | Decl_Enum _
    | Decl_Record _
    | Decl_Typedef _
    | Decl_FunType _
      -> Some x

    (* Add Fun/Proc-Type declarations for any functions that have been created
     * after typechecking such as functions created during monomorphization.
     * Since the typechecker creates Fun/Proc-Type declarations for all functions
     * in the original spec, this will result in duplicate function prototypes
     * for many functions.
     *)
    | Decl_FunDefn (f, fty, b, loc) -> Some (Decl_FunType (f, fty, loc))

    | Decl_Const _
    | Decl_Exception _
    | Decl_Var _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_Operator1 _
    | Decl_Operator2 _
    | Decl_Config _
      -> None
    )
  in
  List.filter_map mk_type_decl xs

let var_decls (xs : AST.declaration list) : AST.declaration list =
  let is_var_decl (x : AST.declaration) : bool =
    ( match x with
    | Decl_Const _
    | Decl_Config _
    | Decl_Var _
      -> true

    | Decl_Enum _
    | Decl_Record _
    | Decl_Exception _
    | Decl_Typedef _
    | Decl_FunType _
    | Decl_FunDefn _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_Operator1 _
    | Decl_Operator2 _
      -> false
    )
  in
  List.filter is_var_decl xs

let fun_decls (xs : AST.declaration list) : AST.declaration list =
  let is_fun_decl (x : AST.declaration) : bool =
    ( match x with
    | Decl_FunDefn _
      -> true

    | Decl_Var _
    | Decl_Const _
    | Decl_Enum _
    | Decl_Record _
    | Decl_Exception _
    | Decl_Typedef _
    | Decl_FunType _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_Operator1 _
    | Decl_Operator2 _
    | Decl_Config _
      -> false
    )
  in
  List.filter is_fun_decl xs

(****************************************************************
 * Foreign Function Interface (FFI)
 ****************************************************************)

let ffi_ty (loc : Loc.t) (fmt : PP.formatter) (x : AST.ty) : unit =
    ( match x with
    | Type_Bits(Expr_Lit (VInt n), _) when List.mem (Z.to_int n) [8; 16; 32; 64] ->
        Format.fprintf fmt "uint%d_t" (Z.to_int n)
    | Type_Integer _ ->
        Format.fprintf fmt "int"
    | Type_Constructor (tc, _) ->
        ident fmt tc
    | _ ->
        let msg = Format.asprintf "Type '%a' cannot be used in functions that are imported or exported between ASL and C"
          FMT.ty x
        in
        raise (Error.TypeError (loc, msg))
    )

let ffi_rty (loc : Loc.t) (fmt : PP.formatter) (ox : AST.ty option) : unit =
    ( match ox with
    | Some x -> ffi_ty loc fmt x
    | None -> Format.fprintf fmt "void"
    )

let ffi_fun_header (loc : Loc.t) (fmt : PP.formatter) (function_name : string) (x : AST.function_type) : unit =
    if not (Utils.is_empty x.parameters)
      || Option.is_some x.setter_arg
      || x.use_array_syntax
      || x.is_getter_setter
      || x.throws != NoThrow
    then begin
        let msg = Format.asprintf "Type '%a' cannot be used in functions that are imported or exported between ASL and C"
          FMT.function_type x
        in
        raise (Error.TypeError (loc, msg))
    end;
    Format.fprintf fmt "%a %s("
      (ffi_rty loc) x.rty
      function_name;
    commasep (fun fmt (arg, ty) ->
      Format.fprintf fmt "%a %a"
        (ffi_ty loc) ty
        ident arg)
      fmt
      x.args;
    Format.fprintf fmt ")"

(* convert runtime value to corresponding C value based on type *)
let mk_ffi_to_C (loc : Loc.t) (fmt : PP.formatter) (t : AST.ty) (x : rt_expr) : unit =
    let module Runtime = (val (!runtime) : RuntimeLib) in
    ( match t with
    | Type_Integer _ -> Runtime.ffi_integer_to_c_int fmt x
    | _ ->
        let msg = Format.asprintf "Type '%a' cannot be used in functions that are imported or exported between ASL and C"
          FMT.ty t
        in
        raise (Error.TypeError (loc, msg))
    )

let mk_ffi_proto (fmt : PP.formatter) ((function_name, fty, loc) : (string * AST.function_type * Loc.t)) : unit =
    ffi_fun_header loc fmt function_name fty;
    Format.fprintf fmt ";@,"

let mk_ffi_defn (fmt : PP.formatter) ((function_name, fty, loc) : (string * AST.function_type * Loc.t)) : unit =
    ffi_fun_header loc fmt function_name fty;
    Format.fprintf fmt "{@,";
    Format.fprintf fmt "    ";
    let call (fmt : PP.formatter) : unit =
      Format.fprintf fmt "%a(%a)"
        ident (Ident.mk_fident function_name)
        (commasep (fun fmt (arg, ty) -> ident fmt arg)) fty.args
    in
    ( match fty.rty with
    | Some rty ->
        Format.fprintf fmt "return ";
        mk_ffi_to_C loc fmt rty call;
        Format.fprintf fmt ";@,"
    | None ->
        call fmt
    );
    Format.fprintf fmt "}@,"

let mk_ffi_exports (decls : AST.declaration list) (function_names : string list) :
    (PP.formatter -> unit) * (PP.formatter -> unit) =
  let map = Asl_utils.decls_map_of decls in

  let missing : string list ref = ref [] in
  let infos = List.filter_map (fun function_name ->
      let fn_name = Ident.mk_fident function_name in
      ( match Bindings.find_opt fn_name map with
      | Some (Decl_FunType (_, fty, loc) :: _) -> Some (function_name, fty, loc)
      | Some (Decl_FunDefn (_, fty, _, loc) :: _) -> Some (function_name, fty, loc)
      | _ ->
          missing := function_name :: !missing;
          None
      )
    ) function_names
  in
  if not (Utils.is_empty !missing) then begin
      Format.eprintf "Error: unable to find exported functions\n";
      List.iter (Format.eprintf "  '%s'\n") !missing;
      exit 1
  end;
  let mk_protos fmt = cutsep mk_ffi_proto fmt infos in
  let mk_defns fmt = cutsep mk_ffi_defn fmt infos in
  (mk_protos, mk_defns)

(****************************************************************
 * File writing support
 ****************************************************************)

let get_rt_header (_ : unit) : string list =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  Runtime.file_header

let emit_c_header (dirname : string) (basename : string) (f : PP.formatter -> unit) : unit =
  let header_suffix = if !is_cxx then ".hpp" else ".h" in
  let basename = basename ^ header_suffix in
  let filename = Filename.concat dirname basename in
  let macro =
    String.uppercase_ascii basename
    |> String.map (fun c -> if List.mem c [ '.'; '/'; '-' ] then '_' else c)
  in
  Utils.to_file filename (fun fmt ->
      PP.fprintf fmt "#ifndef %s@." macro;
      PP.fprintf fmt "#define %s@,@." macro;

      List.iter (PP.fprintf fmt "%s\n") (get_rt_header ());

      if not !is_cxx then begin
        PP.fprintf fmt "#ifdef __cplusplus@.";
        PP.fprintf fmt "extern \"C\" {@.";
        PP.fprintf fmt "#endif@,@.";
      end;

      f fmt;

      if not !is_cxx then begin
        PP.fprintf fmt "#ifdef __cplusplus@.";
        PP.fprintf fmt "}@.";
        PP.fprintf fmt "#endif@,@.";
      end;

      PP.fprintf fmt "#endif  // %s@." macro
  )

let emit_c_source (filename : string) ?(index : int option) (includes : string list)
  (f : PP.formatter -> unit) : unit
  =
  let suffix = function None -> "" | Some i -> "_" ^ string_of_int i in
  let code_suffix = if !is_cxx then ".cpp" else ".c" in
  let filename = filename ^ suffix index ^ code_suffix in
  Utils.to_file filename (fun fmt ->
      List.iter (PP.fprintf fmt "%s\n") (get_rt_header ());
      List.iter (PP.fprintf fmt "#include \"%s\"\n") includes;
      f fmt
  )

let generate_files (num_c_files : int) (dirname : string) (basename : string)
    (ffi_prototypes : PP.formatter -> unit)
    (ffi_definitions : PP.formatter -> unit)
    (ds : AST.declaration list) : unit =
  let basename_t = basename ^ "_types" in
  emit_c_header dirname basename_t (fun fmt ->
      type_decls ds |> Asl_utils.topological_sort |> List.rev |> declarations fmt;
      Format.fprintf fmt "@,";
      ffi_prototypes fmt;
      Format.fprintf fmt "@,"
  );
  let basename_e = basename ^ "_exceptions" in
  emit_c_header dirname basename_e (fun fmt ->
      exceptions fmt ds
  );
  let basename_v = basename ^ "_vars" in
  emit_c_header dirname basename_v (fun fmt ->
      extern_declarations fmt (var_decls ds)
  );

  let header_suffix = if !is_cxx then ".hpp" else ".h" in
  let gen_h_filenames =
    List.map (fun s -> s ^ header_suffix) [ basename_t; basename_e; basename_v ]
  in

  let filename_e = Filename.concat dirname basename_e in
  emit_c_source filename_e gen_h_filenames exceptions_init;

  let filename_v = Filename.concat dirname basename_v in
  emit_c_source filename_v gen_h_filenames (fun fmt -> declarations fmt (var_decls ds));

  let ds = fun_decls ds in
  let filename_f = Filename.concat dirname (basename ^ "_funs") in
  let emit_funs ?(index : int option) (ds : AST.declaration list) : unit =
    emit_c_source filename_f ?index gen_h_filenames (fun fmt -> declarations fmt ds)
  in
  if num_c_files = 1 then begin
    emit_c_source filename_f gen_h_filenames (fun fmt ->
      declarations fmt ds;
      Format.fprintf fmt "@,";
      ffi_definitions fmt
    )
  end else begin
    let threshold = List.length ds / num_c_files in
    let rec emit_funs_by_chunk (i : int) (acc : AST.declaration list) = function
      (* last chunk *)
      | l when i = num_c_files ->
          emit_c_source filename_f ~index:i gen_h_filenames (fun fmt ->
            declarations fmt (List.rev acc @ l);
            Format.fprintf fmt "@,";
            ffi_definitions fmt
          )
      | h :: t when List.length acc < threshold ->
          emit_funs_by_chunk i (h :: acc) t
      | h :: t ->
          emit_c_source filename_f ~index:i gen_h_filenames (fun fmt -> declarations fmt (List.rev acc));
          emit_funs_by_chunk (i + 1) [ h ] t
      | [] -> emit_funs ~index:i (List.rev acc)
    in
    emit_funs_by_chunk 1 [] ds
  end

(****************************************************************
 * Command: :generate_c
 ****************************************************************)

let _ =
  let opt_dirname = ref "" in
  let opt_num_c_files = ref 1 in
  let opt_basename = ref "asl2c" in

  let add_thread_local_variables (group : string) : unit =
    let names = Configuration.get_strings group in
    thread_local_variables := !thread_local_variables @ (Ident.mk_idents names)
  in

  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let decls = !Commands.declarations in
    let exports =
      if !new_ffi then
        Configuration.get_strings "exports"
      else
        (* the new FFI doesn't change code if it thinks there are no exports *)
        []
    in
    let (ffi_protos, ffi_defns) = mk_ffi_exports decls exports in
    generate_files !opt_num_c_files !opt_dirname !opt_basename ffi_protos ffi_defns decls;
    true
  in

  let flags = Arg.align [
        ("--output-dir",   Arg.Set_string opt_dirname,         "<dirname>    Directory for output files");
        ("--basename",     Arg.Set_string opt_basename,        "<basename>   Basename of output files");
        ("--num-c-files",  Arg.Set_int opt_num_c_files,        "<num>        Number of .c files created (default: 1)");
        ("--runtime",      Arg.Symbol (runtimes, set_runtime), "fallback|c23 Select runtime system");
        ("--new-ffi",      Arg.Set   new_ffi,                  " Use new FFI");
        ("--no-new-ffi",   Arg.Clear new_ffi,                  " Do not use new FFI");
        ("--line-info",    Arg.Set include_line_info,          " Insert line number information");
        ("--no-line-info", Arg.Clear include_line_info,        " Do not insert line number information");
        ("--thread-local-pointer", Arg.String (fun s -> opt_thread_local_pointer := Some s), "<varname> Access all thread-local variables through named pointer");
        ("--thread-local", Arg.String add_thread_local_variables, "<config name> Configuration file group of thread local variable names");
      ]
  in
  Commands.registerCommand "generate_c_new" flags [] [] "Generate C (new)" cmd

(****************************************************************
 * End
 ****************************************************************)