(****************************************************************
 * ASL to C backend
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C backend *)

module AST = Asl_ast
module FMTAST = Asl_fmt
module PP = Format
module V = Value
open Format_utils
open Utils

exception Unimplemented of (AST.l * string * (Format.formatter -> unit))

let drop_spaces (x : string) : string = Value.drop_chars x ' '
let drop_underscores (x : string) : string = Value.drop_chars x '_'

(* list of all exception tycons - used to decide whether to insert a tag in
 * Expr_RecordInit
 *)
let exception_tcs : AST.ident list ref = ref []

(** supply of goto labels for exception implementation *)
let catch_labels = new Asl_utils.nameSupply "catch"

let catch_stack : AST.ident list ref = ref []

let with_catch_label (f : AST.ident -> 'a) : 'a =
  let prev = !catch_stack in
  let catch_label = catch_labels#fresh in
  catch_stack := catch_label :: prev;
  let r = f catch_label in
  catch_stack := prev;
  r

let current_catcher (_ : unit) : AST.ident = List.hd !catch_stack

(** List of all the reserved words in C *)
let reserved = [
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

(** Rename any identifiers that match C reserved words *)
let mangle (s : string) : string =
  if List.mem s reserved then
    "xx"^s
  else
    s

let delimiter (fmt : PP.formatter) (s : string) : unit =
  PP.pp_print_string fmt s

let keyword (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt s
let asl_keyword (fmt : PP.formatter) (s : string) : unit = keyword fmt ("ASL_" ^ s)
let constant (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt s

let ident_str (fmt : PP.formatter) (x : string) : unit =
  PP.pp_print_string fmt x

let ident (fmt : PP.formatter) (x : AST.ident) : unit =
  match x with
  | Ident s -> ident_str fmt (mangle s)
  | FIdent (s, t) -> ident_str fmt (s ^ "_" ^ string_of_int t)

let tycon (fmt : PP.formatter) (x : AST.ident) : unit = ident fmt x
let funname (fmt : PP.formatter) (x : AST.ident) : unit = ident fmt x
let varname (fmt : PP.formatter) (x : AST.ident) : unit = ident fmt x
let fieldname (fmt : PP.formatter) (x : AST.ident) : unit = ident fmt x
let varnames (fmt : PP.formatter) (xs : AST.ident list) : unit =
  commasep fmt (varname fmt) xs

(* C delimiters *)

let amp                 (fmt : PP.formatter) : unit = delimiter fmt "&"
let amp_amp             (fmt : PP.formatter) : unit = delimiter fmt "&&"
let amp_eq              (fmt : PP.formatter) : unit = delimiter fmt "&="
let bang                (fmt : PP.formatter) : unit = delimiter fmt "!"
let bang_eq             (fmt : PP.formatter) : unit = delimiter fmt "!="
let bar                 (fmt : PP.formatter) : unit = delimiter fmt "|"
let bar_bar             (fmt : PP.formatter) : unit = delimiter fmt "||"
let bar_eq              (fmt : PP.formatter) : unit = delimiter fmt "|="
let caret               (fmt : PP.formatter) : unit = delimiter fmt "^"
let caret_eq            (fmt : PP.formatter) : unit = delimiter fmt "^="
let colon               (fmt : PP.formatter) : unit = delimiter fmt ":"
let dot                 (fmt : PP.formatter) : unit = delimiter fmt "."
let dot_dot_dot         (fmt : PP.formatter) : unit = delimiter fmt "..."
let dquote              (fmt : PP.formatter) : unit = delimiter fmt "\""
let eq                  (fmt : PP.formatter) : unit = delimiter fmt "="
let eq_eq               (fmt : PP.formatter) : unit = delimiter fmt "=="
let gt                  (fmt : PP.formatter) : unit = delimiter fmt ">"
let gt_eq               (fmt : PP.formatter) : unit = delimiter fmt ">="
let gt_gt               (fmt : PP.formatter) : unit = delimiter fmt ">>"
let gt_gt_eq            (fmt : PP.formatter) : unit = delimiter fmt ">>="
let hash                (fmt : PP.formatter) : unit = delimiter fmt "#"
let hash_hash           (fmt : PP.formatter) : unit = delimiter fmt "##"
let lt                  (fmt : PP.formatter) : unit = delimiter fmt "<"
let lt_eq               (fmt : PP.formatter) : unit = delimiter fmt "<="
let lt_lt               (fmt : PP.formatter) : unit = delimiter fmt "<<"
let lt_lt_eq            (fmt : PP.formatter) : unit = delimiter fmt "<<="
let minus               (fmt : PP.formatter) : unit = delimiter fmt "-"
let minus_eq            (fmt : PP.formatter) : unit = delimiter fmt "-="
let minus_minus         (fmt : PP.formatter) : unit = delimiter fmt "--"
let minus_qt            (fmt : PP.formatter) : unit = delimiter fmt "->"
let percent             (fmt : PP.formatter) : unit = delimiter fmt "%"
let percent_eq          (fmt : PP.formatter) : unit = delimiter fmt "%="
let plus                (fmt : PP.formatter) : unit = delimiter fmt "+"
let plus_eq             (fmt : PP.formatter) : unit = delimiter fmt "+="
let plus_plus           (fmt : PP.formatter) : unit = delimiter fmt "++"
let qmark               (fmt : PP.formatter) : unit = delimiter fmt "?"
let semicolon           (fmt : PP.formatter) : unit = delimiter fmt ";"
let slash               (fmt : PP.formatter) : unit = delimiter fmt "/"
let slash_eq            (fmt : PP.formatter) : unit = delimiter fmt "/="
let star                (fmt : PP.formatter) : unit = delimiter fmt "*"
let star_eq             (fmt : PP.formatter) : unit = delimiter fmt "*="
let tilde               (fmt : PP.formatter) : unit = delimiter fmt "~"

(* C keywords *)

let kw_auto (fmt : PP.formatter) : unit = keyword fmt "auto"
let kw_break (fmt : PP.formatter) : unit = keyword fmt "break"
let kw_case (fmt : PP.formatter) : unit = keyword fmt "case"
let kw_char (fmt : PP.formatter) : unit = keyword fmt "char"
let kw_const (fmt : PP.formatter) : unit = keyword fmt "const"
let kw_continue (fmt : PP.formatter) : unit = keyword fmt "continue"
let kw_default (fmt : PP.formatter) : unit = keyword fmt "default"
let kw_do (fmt : PP.formatter) : unit = keyword fmt "do"
let kw_double (fmt : PP.formatter) : unit = keyword fmt "double"
let kw_else (fmt : PP.formatter) : unit = keyword fmt "else"
let kw_enum (fmt : PP.formatter) : unit = keyword fmt "enum"
let kw_extern (fmt : PP.formatter) : unit = keyword fmt "extern"
let kw_float (fmt : PP.formatter) : unit = keyword fmt "float"
let kw_for (fmt : PP.formatter) : unit = keyword fmt "for"
let kw_goto (fmt : PP.formatter) : unit = keyword fmt "goto"
let kw_if (fmt : PP.formatter) : unit = keyword fmt "if"
let kw_inline (fmt : PP.formatter) : unit = keyword fmt "inline"
let kw_int (fmt : PP.formatter) : unit = keyword fmt "int"
let kw_long (fmt : PP.formatter) : unit = keyword fmt "long"
let kw_register (fmt : PP.formatter) : unit = keyword fmt "register"
let kw_restrict (fmt : PP.formatter) : unit = keyword fmt "restrict"
let kw_return (fmt : PP.formatter) : unit = keyword fmt "return"
let kw_short (fmt : PP.formatter) : unit = keyword fmt "short"
let kw_signed (fmt : PP.formatter) : unit = keyword fmt "signed"
let kw_sizeof (fmt : PP.formatter) : unit = keyword fmt "sizeof"
let kw_static (fmt : PP.formatter) : unit = keyword fmt "static"
let kw_struct (fmt : PP.formatter) : unit = keyword fmt "struct"
let kw_switch (fmt : PP.formatter) : unit = keyword fmt "switch"
let kw_typedef (fmt : PP.formatter) : unit = keyword fmt "typedef"
let kw_union (fmt : PP.formatter) : unit = keyword fmt "union"
let kw_unsigned (fmt : PP.formatter) : unit = keyword fmt "unsigned"
let kw_void (fmt : PP.formatter) : unit = keyword fmt "void"
let kw_volatile (fmt : PP.formatter) : unit = keyword fmt "volatile"
let kw_while (fmt : PP.formatter) : unit = keyword fmt "while"

(* C pseudo-keywords *)

let kw_bool (fmt : PP.formatter) : unit = keyword fmt "bool"
let kw_false (fmt : PP.formatter) : unit = keyword fmt "false"
let kw_int16 (fmt : PP.formatter) : unit = keyword fmt "int16_t"
let kw_int32 (fmt : PP.formatter) : unit = keyword fmt "int32_t"
let kw_int64 (fmt : PP.formatter) : unit = keyword fmt "int64_t"
let kw_int8 (fmt : PP.formatter) : unit = keyword fmt "int8_t"
let kw_true (fmt : PP.formatter) : unit = keyword fmt "true"
let kw_uint16 (fmt : PP.formatter) : unit = keyword fmt "uint16_t"
let kw_uint32 (fmt : PP.formatter) : unit = keyword fmt "uint32_t"
let kw_uint64 (fmt : PP.formatter) : unit = keyword fmt "uint64_t"
let kw_uint8 (fmt : PP.formatter) : unit = keyword fmt "uint8_t"

(* C types defined elsewhere *)
let ty_ram (fmt : PP.formatter) : unit = asl_keyword fmt "ram_t"

(* C functions defined elsewhere *)
let fn_slice_lowd (fmt : PP.formatter) : unit = asl_keyword fmt "slice_lowd"
let fn_slice_lowd_w (fmt : PP.formatter) : unit = asl_keyword fmt "slice_lowd_w"
let fn_error_unmatched_case (fmt : PP.formatter) : unit = asl_keyword fmt "error_unmatched_case"
let fn_assert (fmt : PP.formatter) : unit = asl_keyword fmt "assert"

let fn_extern (fmt : PP.formatter) (x : AST.ident) : unit =
  asl_keyword fmt (AST.name_of_FIdent x)

let round_up_to_pow2 (x : int) : int =
  let x = Z.log2up (Z.of_int x) in
  Z.to_int (Z.shift_left Z.one x)

let intLit (fmt : PP.formatter) (x : AST.intLit) : unit = constant fmt x
let hexLit (fmt : PP.formatter) (x : AST.hexLit) : unit = constant fmt ("0x" ^ x)

let bitsLit (fmt : PP.formatter) (x : AST.bitsLit) : unit =
  let (x : string) = drop_spaces x in
  let len = String.length x in
  let bit_to_hex (s : string) : string =
    Z.format "%#x" (Z.of_string_base 2 s) ^ "ULL"
  in

  if len <= 64 then constant fmt (bit_to_hex x)
  else
    let len_ext = round_up_to_pow2 len in
    let x_ext = String.make (len_ext - len) '0' ^ x in
    let num_limbs = len_ext / 64 in
    let limbs =
      List.init num_limbs (fun i ->
          let pos = i * 64 in
          bit_to_hex (String.sub x_ext pos 64))
    in
    asl_keyword fmt "bits";
    parens fmt (fun _ ->
        commasep fmt (constant fmt) (string_of_int len_ext :: limbs))

let strLit (fmt : PP.formatter) (x : string) : unit =
  constant fmt ("\"" ^ String.escaped x ^ "\"")

let const_expr (loc : AST.l) (x : AST.expr) : V.value =
  match x with
  | Expr_LitBits b -> V.from_bitsLit b
  | Expr_LitHex i -> V.from_hexLit i
  | Expr_LitInt i -> V.from_intLit i
  | Expr_LitMask b -> V.from_maskLit b
  | Expr_LitReal r -> V.from_realLit r
  | Expr_LitString s -> V.from_stringLit s
  | _ ->
      raise
        (Unimplemented
           ( loc,
             "const_expr: not literal constant '" ^ Asl_utils.pp_expr x ^ "'",
             fun fmt -> FMTAST.expr fmt x ))

let const_int_expr (loc : AST.l) (x : AST.expr) : int =
  let v = const_expr loc x in
  match v with
  | VInt i -> Z.to_int i
  | _ ->
      raise
        (Unimplemented
           ( loc,
             "const_int_expr: integer expected '" ^ V.string_of_value v ^ "'",
             fun fmt -> FMTAST.expr fmt x ))

let c_int_width (width : int) : int =
  if width > 8 then round_up_to_pow2 width else 8

let c_int_width_64up (width : int) : int =
  if width > 64 then round_up_to_pow2 width else 64

let c_int_width_64up_expr (loc : AST.l) (width : AST.expr) : AST.expr =
  Asl_utils.mk_litint (c_int_width_64up (const_int_expr loc width))

let bits (fmt : PP.formatter) (width : int) : unit =
  asl_keyword fmt ("bits" ^ string_of_int (c_int_width_64up width) ^ "_t")

(* Similar to width_of_type except that it also handles integers and new
   types *)
let size_of_type (ty : AST.ty) : AST.expr option =
  match ty with
  (* TODO For now assume the new type takes 64 bits *)
  | Type_Constructor _
  (* TODO implement integer range analysis to determine the correct type width.
     For now assume 64 bits. *)
  | Type_Integer _ -> Some (Asl_utils.mk_litint 64)
  | _ -> Asl_utils.width_of_type ty

let rethrow_stmt (fmt : PP.formatter) : unit =
  Format.fprintf fmt "if (ASL_exception._exc.ASL_tag != ASL_no_exception) goto %a;"
    varname (current_catcher ())

let rethrow_expr (fmt : PP.formatter) (f : unit -> unit) : unit =
  Format.fprintf fmt "({ __auto_type __r = ";
  f ();
  Format.fprintf fmt "; ";
  rethrow_stmt fmt;
  Format.fprintf fmt " __r; })"

let rec varty (loc : AST.l) (fmt : PP.formatter) (v : AST.ident) (x : AST.ty) : unit =
  ( match x with
  | Type_Bits n
  | Type_Register (n, _) ->
    bits fmt (const_int_expr loc n);
    nbsp fmt;
    varname fmt v
  | Type_Constructor (tc, []) ->
      ( match tc with
      | Ident "boolean" ->
        kw_bool fmt;
        nbsp fmt;
        varname fmt v
      | Ident "string" ->
        kw_const fmt;
        nbsp fmt;
        kw_char fmt;
        nbsp fmt;
        star fmt;
        varname fmt v
      | _ ->
        tycon fmt tc;
        nbsp fmt;
        varname fmt v
      )
  | Type_Constructor (Ident "__RAM", [_]) ->
    ty_ram fmt;
    nbsp fmt;
    varname fmt v
  (* TODO implement integer range analysis to determine the correct type width.
   * For now use int64. *)
  | Type_Integer _ ->
    kw_int64 fmt;
    nbsp fmt;
    varname fmt v
  | Type_Array (Index_Enum tc, ety) ->
    varty loc fmt v ety;
    brackets fmt (fun _ -> tycon fmt tc)
  | Type_Array (Index_Int sz, ety) ->
    varty loc fmt v ety;
    brackets fmt (fun _ -> expr loc fmt sz)
  | Type_Constructor (_, _)
  | Type_OfExpr _
  | Type_Tuple _ ->
      raise (Unimplemented (loc, "type", fun fmt -> FMTAST.ty fmt x))
  )

and apply (loc : AST.l) (fmt : PP.formatter) (f : unit -> unit) (args : AST.expr list) :
    unit =
  f ();
  parens fmt (fun _ -> exprs loc fmt args)

and make_cast (fmt : PP.formatter) (t : unit -> unit) (x : unit -> unit)
    : unit =
  parens fmt t;
  x ()

and make_binop (fmt : PP.formatter) (op : unit -> unit) (x : unit -> unit)
    (y : unit -> unit) : unit =
  parens fmt (fun _ ->
      x ();
      nbsp fmt;
      op ();
      nbsp fmt;
      y ())

and make_unop (fmt : PP.formatter) (op : unit -> unit) (x : unit -> unit) : unit
    =
  parens fmt (fun _ ->
      op ();
      x ())

and pow2_int (loc : AST.l) (fmt : PP.formatter) (x : AST.expr) : unit =
  make_binop fmt
    (fun _ -> lt_lt fmt)
    (fun _ ->
      (* TODO determine correct type width. For now use uint64. *)
      make_cast fmt (fun _ -> kw_uint64 fmt) (fun _ -> intLit fmt "1"))
    (fun _ -> expr loc fmt x)

(* Calculate mask with x ones *)
and mask_int (loc : AST.l) (fmt : PP.formatter) (x : AST.expr) : unit =
  make_binop fmt
    (fun _ -> minus fmt)
    (fun _ -> pow2_int loc fmt x)
    (fun _ -> intLit fmt "1")

and binop (loc : AST.l) (fmt : PP.formatter) (op : string) (args : AST.expr list) : unit =
  match args with
  | [ x; y ] ->
      make_binop fmt
        (fun _ -> delimiter fmt op)
        (fun _ -> expr loc fmt x)
        (fun _ -> expr loc fmt y)
  | _ -> raise (Unimplemented (loc, "binop: " ^ op, fun fmt -> ()))

and unop (loc : AST.l) (fmt : PP.formatter) (op : string) (args : AST.expr list) : unit =
  match args with
  | [ x ] -> make_unop fmt (fun _ -> delimiter fmt op) (fun _ -> expr loc fmt x)
  | _ -> raise (Unimplemented (loc, "unop: " ^ op, fun fmt -> ()))

and cond_cont (loc : AST.l) (fmt : PP.formatter) (c : AST.expr) (x : AST.expr)
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

and cond (loc : AST.l) (fmt : PP.formatter) (c : AST.expr) (x : AST.expr) (y : AST.expr) :
    unit =
  cond_cont loc fmt c x (fun _ -> expr loc fmt y)

and conds (loc : AST.l) (fmt : PP.formatter) (cts : (AST.expr * AST.expr) list) (e : AST.expr)
    : unit =
  match cts with
  | [] -> expr loc fmt e
  | (c, t) :: cts' -> cond_cont loc fmt c t (fun _ -> conds loc fmt cts' e)

and funcall (loc : AST.l) (fmt : PP.formatter) (f : AST.ident) (tes : AST.expr list)
    (args : AST.expr list) (loc : AST.l) =
  match (f, args) with
  (* Boolean builtin functions *)
  | FIdent ("and_bool", _), _ -> binop loc fmt "&&" args
  | FIdent ("eq_bool", _), _ | FIdent ("equiv_bool", _), _ ->
      binop loc fmt "==" args
  | FIdent ("implies_bool", _), [ x; y ] ->
      cond loc fmt x y Asl_utils.asl_true
  | FIdent ("ne_bool", _), _ -> binop loc fmt "!=" args
  | FIdent ("not_bool", _), _ -> unop loc fmt "!" args
  | FIdent ("or_bool", _), _ -> binop loc fmt "||" args
  (* Enumeration builtin functions *)
  | FIdent ("eq_enum", _), _ -> binop loc fmt "==" args
  | FIdent ("ne_enum", _), _ -> binop loc fmt "!=" args
  (* Integer builtin functions *)
  | FIdent ("add_int", _), _ -> binop loc fmt "+" args
  | FIdent ("align_int", _), [ x; y ] ->
      make_binop fmt
        (fun _ -> amp fmt)
        (fun _ -> expr loc fmt x)
        (fun _ -> make_unop fmt (fun _ -> tilde fmt) (fun _ -> mask_int loc fmt y))
  | FIdent ("eq_int", _), _ -> binop loc fmt "==" args
  | FIdent ("fdiv_int", _), _
  | FIdent ("frem_int", _), _ -> apply loc fmt (fun _ -> fn_extern fmt f) args
  | FIdent ("ge_int", _), _ -> binop loc fmt ">=" args
  | FIdent ("gt_int", _), _ -> binop loc fmt ">" args
  | FIdent ("is_pow2_int", _), _ -> apply loc fmt (fun _ -> fn_extern fmt f) args
  | FIdent ("le_int", _), _ -> binop loc fmt "<=" args
  | FIdent ("lt_int", _), _ -> binop loc fmt "<" args
  | FIdent ("mod_pow2_int", _), [ x; y ] ->
      make_binop fmt
        (fun _ -> amp fmt)
        (fun _ -> expr loc fmt x)
        (fun _ -> mask_int loc fmt y)
  | FIdent ("mul_int", _), _ -> binop loc fmt "*" args
  | FIdent ("ne_int", _), _ -> binop loc fmt "!=" args
  | FIdent ("neg_int", _), _ -> unop loc fmt "-" args
  | FIdent ("pow2_int", _), [ x ] -> pow2_int loc fmt x
  | FIdent ("shl_int", _), _ -> binop loc fmt "<<" args
  | FIdent ("shr_int", _), _ -> binop loc fmt ">>" args
  | FIdent ("sub_int", _), _ -> binop loc fmt "-" args
  | FIdent ("zdiv_int", _), _ -> binop loc fmt "/" args
  | FIdent ("zrem_int", _), _ -> binop loc fmt "%" args
  (* Real builtin functions *)
  | FIdent ("add_real", _), _
  | FIdent ("cvt_int_real", _), _
  | FIdent ("divide_real", _), _
  | FIdent ("eq_real", _), _
  | FIdent ("ge_real", _), _
  | FIdent ("gt_real", _), _
  | FIdent ("le_real", _), _
  | FIdent ("lt_real", _), _
  | FIdent ("mul_real", _), _
  | FIdent ("ne_real", _), _
  | FIdent ("neg_real", _), _
  | FIdent ("pow2_real", _), _
  | FIdent ("round_down_real", _), _
  | FIdent ("round_tozero_real", _), _
  | FIdent ("round_up_real", _), _
  | FIdent ("sqrt_real", _), _
  | FIdent ("sub_real", _), _ ->
      raise
        (Unimplemented
           ( loc,
             "real builtin function",
             fun fmt -> FMTAST.funname fmt f ))
  (* Bitvector builtin functions *)
  | FIdent ("add_bits", _), _
  | FIdent ("and_bits", _), _
  | FIdent ("asr_bits", _), _
  | FIdent ("cvt_bits_sint", _), _
  | FIdent ("cvt_bits_uint", _), _ ->
      let n = List.hd tes in
      apply loc fmt (fun _ -> fn_extern fmt f) ((c_int_width_64up_expr loc n) :: n :: args)
  | FIdent ("append_bits", _), [ x; y ] ->
      let m, n =
        match tes with
        | [ m; n ] -> (m, n)
        | _ -> failwith "wrong number of type parameters"
      in
      let nm = Asl_utils.mk_litint (const_int_expr loc n + const_int_expr loc m) in
      apply loc fmt
        (fun _ -> fn_extern fmt f)
        [
          c_int_width_64up_expr loc nm;
          m;
          n;
          Asl_utils.mk_zero_extend_bits nm m x;
          Asl_utils.mk_zero_extend_bits nm n y;
        ]
  | FIdent ("cvt_int_bits", _), [ x; n ] ->
      apply loc fmt (fun _ -> fn_extern fmt f) ((c_int_width_64up_expr loc n) :: n :: [ x ])
  | FIdent ("eor_bits", _), _
  | FIdent ("eq_bits", _), _ ->
      let n = List.hd tes in
      apply loc fmt (fun _ -> fn_extern fmt f) ((c_int_width_64up_expr loc n) :: n :: args)
  | FIdent ("mk_mask", _), [ w; _ ] ->
      let n = List.hd tes in
      apply loc fmt (fun _ -> fn_extern fmt f) [(c_int_width_64up_expr loc n); w]
  | FIdent ("frem_bits_int", _), _
  | FIdent ("in_mask", _), _
  | FIdent ("notin_mask", _), _ ->
      raise
        (Unimplemented
           (loc, "bitvector builtin function", fun fmt -> FMTAST.funname fmt f))
  | FIdent ("lsl_bits", _), _
  | FIdent ("lsr_bits", _), _
  | FIdent ("mul_bits", _), _
  | FIdent ("ne_bits", _), _
  | FIdent ("not_bits", _), _ ->
      let n = List.hd tes in
      apply loc fmt (fun _ -> fn_extern fmt f) ((c_int_width_64up_expr loc n) :: n :: args)
  | FIdent ("ones_bits", _), _ ->
      let n = List.hd tes in
      apply loc fmt (fun _ -> fn_extern fmt f) ((c_int_width_64up_expr loc n) :: args)
  | FIdent ("or_bits", _), _ ->
      let n = List.hd tes in
      apply loc fmt (fun _ -> fn_extern fmt f) ((c_int_width_64up_expr loc n) :: n :: args)
  | FIdent ("replicate_bits", _), [ x; n ] ->
      let m = List.nth tes 1 in
      let nm = Asl_utils.mk_litint (const_int_expr loc n * const_int_expr loc m) in
      apply loc fmt
        (fun _ -> fn_extern fmt f)
        [ c_int_width_64up_expr loc nm; m; Asl_utils.mk_zero_extend_bits nm m x; n ]
  | FIdent ("sub_bits", _), _ ->
      let n = List.hd tes in
      apply loc fmt (fun _ -> fn_extern fmt f) ((c_int_width_64up_expr loc n) :: n :: args)
  | FIdent ("zeros_bits", _), _ ->
      let n = List.hd tes in
      apply loc fmt (fun _ -> fn_extern fmt f) ((c_int_width_64up_expr loc n) :: args)
  | FIdent ("zero_extend_bits", _), _ ->
      let n, m =
        match tes with
        | [ n; m ] -> (n, m)
        | _ -> failwith "wrong number of type parameters"
      in
      apply loc fmt (fun _ -> fn_extern fmt f)
        (c_int_width_64up_expr loc m :: c_int_width_64up_expr loc n :: m :: args)
  (* String builtin functions *)
  | FIdent ("append_str_str", _), [x; y] -> expr loc fmt x (* not perfect but better than nothing *)
  | FIdent ("cvt_bits_str", _), _
  | FIdent ("cvt_bool_str", _), _
  | FIdent ("cvt_int_decstr", _), _
  | FIdent ("cvt_int_hexstr", _), _
  | FIdent ("cvt_real_str", _), _ ->
      dquote fmt;
      dquote fmt
  | FIdent ("eq_str", _), _
  | FIdent ("ne_str", _), _ ->
      raise
        (Unimplemented
           (loc, "string builtin function", fun fmt -> FMTAST.funname fmt f))
  | FIdent ("print_bits", _), _ ->
      raise
        (Unimplemented
           (loc, "print_bits builtin function", fun fmt -> FMTAST.funname fmt f))
  | FIdent ("print_char", _), _
  | FIdent ("print_str", _), _ -> apply loc fmt (fun _ -> fn_extern fmt f) args
  (* RAM builtin functions *)
  | FIdent ("ram_init", _), [ a; n; ram; i ]
  | FIdent ("ram_read", _), [ a; n; ram; i ] ->
      fn_extern fmt f;
      parens fmt (fun _ ->
          commasep fmt
            (fun f -> f ())
            [
              (fun _ -> expr loc fmt a);
              (fun _ -> expr loc fmt n);
              (fun _ ->
                make_unop fmt (fun _ -> amp fmt) (fun _ -> expr loc fmt ram));
              (fun _ -> expr loc fmt i);
            ])
  | FIdent ("ram_write", _), [ a; n; ram; i; x ] ->
      fn_extern fmt f;
      parens fmt (fun _ ->
          commasep fmt
            (fun f -> f ())
            [
              (fun _ -> expr loc fmt a);
              (fun _ -> expr loc fmt n);
              (fun _ ->
                make_unop fmt (fun _ -> amp fmt) (fun _ -> expr loc fmt ram));
              (fun _ -> expr loc fmt i);
              (fun _ -> expr loc fmt x);
            ])
  | _ -> apply loc fmt (fun _ -> funname fmt f) args

and slice (loc : AST.l) (fmt : PP.formatter) (t : AST.ty) (e : AST.expr)
    (s : AST.slice) : unit =
  let ew = c_int_width_64up_expr loc (Option.get (size_of_type t)) in
  match s with
  | Slice_LoWd (lo, wd) ->
      let wdw = c_int_width_64up_expr loc wd in
      apply loc fmt (fun _ -> fn_slice_lowd fmt) [ ew; wdw; e; lo; wd ]
  | Slice_Single _ ->
      raise (InternalError (__LOC__ ^ ": Slice_Single not expected"))
  | Slice_HiLo _ ->
      raise (InternalError (__LOC__ ^ ": Slice_HiLo not expected"))

and lslice (loc : AST.l) (fmt : PP.formatter) (v : AST.expr) (e : AST.expr)
    (s : AST.slice) : unit =
  (* e is evaluated twice below, which could result in performance impact *)
  expr loc fmt e;
  nbsp fmt;
  eq fmt;
  nbsp fmt;
  match s with
  | Slice_LoWd (lo, wd) ->
      apply loc fmt (fun _ -> fn_slice_lowd_w fmt) [ v; e; lo; wd ]
  | Slice_Single _ ->
      raise (InternalError (__LOC__ ^ ": Slice_Single not expected"))
  | Slice_HiLo _ ->
      raise (InternalError (__LOC__ ^ ": Slice_HiLo not expected"))

and expr (loc : AST.l) (fmt : PP.formatter) (x : AST.expr) : unit =
  match x with
  | Expr_Concat (ws, es) ->
      assert (List.length ws == List.length es);
      let _, shifts =
        List.fold_right
          (fun w (acc, shifts) -> (acc + const_int_expr loc w, acc :: shifts))
          ws (0, [])
      in
      sepby fmt
        (fun _ ->
          nbsp fmt;
          bar fmt;
          nbsp fmt)
        (fun (sh, e) ->
          make_binop fmt
            (fun _ -> lt_lt fmt)
            (fun _ -> expr loc fmt e)
            (fun _ -> intLit fmt (string_of_int sh)))
        (List.combine shifts es)
  | Expr_Field (e, f) ->
      expr loc fmt e;
      dot fmt;
      fieldname fmt f
  | Expr_If (c, t, els, e) ->
      let els1 = List.map (function AST.E_Elsif_Cond (c, e) -> (c, e)) els in
      conds loc fmt ((c, t) :: els1) e
  | Expr_LitBits l -> bitsLit fmt l
  | Expr_LitHex l -> hexLit fmt l
  | Expr_LitInt l -> intLit fmt l
  | Expr_LitString l -> strLit fmt l
  | Expr_Parens e -> expr loc fmt e
  | Expr_RecordInit (tc, [], fas) ->
      if List.mem tc !exception_tcs then begin
        Format.fprintf fmt "(ASL_exception_t){ ._%a={ .ASL_tag = tag_%a, " tycon tc tycon tc;
        commasep fmt
          (fun (f, e) -> Format.fprintf fmt ".%a = %a" varname f (expr loc) e)
          fas;
        Format.fprintf fmt " }}"
      end else begin
        Format.fprintf fmt "(%a){ " tycon tc;
        commasep fmt
          (fun (f, e) -> Format.fprintf fmt ".%a = %a" varname f (expr loc) e)
          fas;
        Format.fprintf fmt " }"
      end
  | Expr_Slices (t, e, [ s ]) -> slice loc fmt t e s
  | Expr_TApply (f, tes, es, throws) ->
      if throws then
        rethrow_expr fmt (fun _ -> funcall loc fmt f tes es loc)
      else
        funcall loc fmt f tes es loc
  | Expr_Var v -> (
      match v with
      | Ident "TRUE" -> kw_true fmt
      | Ident "FALSE" -> kw_false fmt
      | _ -> varname fmt v)
  | Expr_Array (a, i) ->
      expr loc fmt a;
      brackets fmt (fun _ -> expr loc fmt i)
  | Expr_In (e, Pat_LitMask x) ->
      let x' = drop_spaces x in
      let v = String.map (function 'x' -> '0' | c -> c) x' in
      let m = String.map (function 'x' -> '0' | c -> '1') x' in
      let v = Z.format "%#x" (Z.of_string_base 2 v) in
      let m = Z.format "%#x" (Z.of_string_base 2 m) in
      parens fmt (fun _ ->
          parens fmt (fun _ ->
              expr loc fmt e;
              nbsp fmt;
              amp fmt;
              nbsp fmt;
              constant fmt m
            );
          nbsp fmt;
          eq_eq fmt;
          nbsp fmt;
          constant fmt v
        )
  | Expr_AsConstraint _
  | Expr_AsType _
  | Expr_Binop _
  | Expr_Fields _
  | Expr_ImpDef _
  | Expr_In _
  | Expr_LitMask _
  | Expr_LitReal _
  | Expr_RecordInit _
  | Expr_Slices _
  | Expr_Tuple _
  | Expr_Unknown _
  | Expr_Unop _ ->
      raise
        (Unimplemented (loc, "expression", fun fmt -> FMTAST.expr fmt x))

and exprs (loc : AST.l) (fmt : PP.formatter) (es : AST.expr list) : unit =
  commasep fmt (expr loc fmt) es

let pattern (loc : AST.l) (fmt : PP.formatter) (x : AST.pattern) : unit =
  match x with
  | Pat_LitBits l -> bitsLit fmt l
  | Pat_LitHex l -> hexLit fmt l
  | Pat_LitInt l -> intLit fmt l
  | Pat_Const v ->
      ( match v with
      | Ident "TRUE" -> kw_true fmt
      | Ident "FALSE" -> kw_false fmt
      | _ -> varname fmt v
      )
  | Pat_LitMask _ | Pat_Range _ | Pat_Set _ | Pat_Single _
  | Pat_Tuple _ | Pat_Wildcard ->
      raise
        (Unimplemented (loc, "pattern", fun fmt -> FMTAST.pattern fmt x))

let assign (loc : AST.l) (fmt : PP.formatter) (l : unit -> unit) (r : AST.expr) : unit =
  l ();
  nbsp fmt;
  eq fmt;
  nbsp fmt;
  expr loc fmt r;
  semicolon fmt

let rec lexpr (loc : AST.l) (fmt : PP.formatter) (x : AST.lexpr) : unit =
  match x with
  | LExpr_Var v -> varname fmt v
  | LExpr_Array (a, i) ->
    lexpr loc fmt a;
    brackets fmt (fun _ -> expr loc fmt i)
  | LExpr_Field (l, f) ->
    lexpr loc fmt l;
    dot fmt;
    varname fmt f
  | LExpr_Wildcard
  | LExpr_BitTuple _
  | LExpr_Fields _
  | LExpr_ReadWrite _
  | LExpr_Slices _
  | LExpr_Tuple _
  | LExpr_Write _ ->
      raise
        (Unimplemented
           (loc, "l-expression", fun fmt -> FMTAST.lexpr fmt x))

let rec lexpr_to_expr (loc : AST.l) (x : AST.lexpr) : AST.expr =
  ( match x with
  | LExpr_Var v -> Expr_Var v
  | LExpr_Field (l, f) -> Expr_Field (lexpr_to_expr loc l, f)
  | LExpr_Array (l, e) -> Expr_Array (lexpr_to_expr loc l, e)
  | _ ->
      raise
        (Unimplemented
           (loc, "lexpr_to_expr", fun fmt -> FMTAST.lexpr fmt x))
  )

let lexpr_assign (loc : AST.l) (fmt : PP.formatter) (x : AST.lexpr) (r : AST.expr) : unit =
  match x with
  | LExpr_Slices (_, l, [ s ]) ->
      lslice loc fmt r (lexpr_to_expr loc l) s;
      semicolon fmt
  | LExpr_Wildcard ->
      make_cast fmt (fun _ -> kw_void fmt) (fun _ -> expr loc fmt r);
      semicolon fmt
  | _ -> assign loc fmt (fun _ -> lexpr loc fmt x) r

let rec declitem (loc : AST.l) (fmt : PP.formatter) (x : AST.decl_item) =
  match x with
  | DeclItem_Var (v, Some t) ->
      varty loc fmt v t;
      semicolon fmt;
      cut fmt
  | DeclItem_Tuple dis ->
      cutsep fmt (declitem loc fmt) dis;
      cut fmt
  | DeclItem_Var (v, None) ->
      raise
        (Unimplemented
           ( loc,
             "decl: type of variable unknown",
             fun fmt -> FMTAST.varname fmt v ))
  | DeclItem_Wildcard _ -> ()

let decl (fmt : PP.formatter) (x : AST.stmt) : unit =
  match x with
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      List.iter (fun v ->
          varty loc fmt v t;
          semicolon fmt;
          cut fmt
      )
      vs
  | Stmt_VarDecl (di, i, loc) | Stmt_ConstDecl (di, i, loc) -> declitem loc fmt di
  | _ -> ()

let direction (x : AST.direction) (up : unit -> unit) (down : unit -> unit) :
    unit =
  match x with Direction_Up -> up () | Direction_Down -> down ()

let rec stmt (fmt : PP.formatter) (x : AST.stmt) : unit =
  match x with
  | Stmt_Assert (e, loc) ->
      let expr_string = AST.Expr_LitString (Utils.to_string2 (Fun.flip FMTAST.expr e)) in
      let loc_string = AST.Expr_LitString (AST.pp_loc loc) in
      apply loc fmt (fun _ -> fn_assert fmt) [loc_string; expr_string; e];
      semicolon fmt
  | Stmt_Assign (l, r, loc) -> lexpr_assign loc fmt l r
  | Stmt_Block (ss, _) -> brace_enclosed_block fmt ss
  | Stmt_Case (e, alts, ob, loc) ->
      vbox fmt (fun _ ->
          kw_switch fmt;
          nbsp fmt;
          parens fmt (fun _ -> expr loc fmt e);
          nbsp fmt;
          braces fmt (fun _ ->
              indented fmt (fun _ ->
                  cutsep fmt
                    (fun (AST.Alt_Alt (ps, oc, ss, loc)) ->
                      if Option.is_some oc then
                        raise
                          (Unimplemented (loc, "pattern_guard", fun fmt -> ()));
                      List.iter (Format.fprintf fmt "case %a:@," (pattern loc)) ps;
                      braces fmt (fun _ ->
                          indented_block fmt ss;
                          indented fmt (fun _ ->
                              kw_break fmt;
                              semicolon fmt);
                          cut fmt))
                    alts;
                  cut fmt;

                  kw_default fmt;
                  colon fmt;
                  nbsp fmt;
                  braces fmt (fun _ ->
                    ( match ob with
                    | Some (b, bl) ->
                        indented_block fmt b;
                        indented fmt (fun _ ->
                          kw_break fmt;
                          semicolon fmt
                          )
                    | None ->
                        indented fmt (fun _ ->
                          let loc_string = AST.Expr_LitString (AST.pp_loc loc) in
                          apply loc fmt (fun _ -> fn_error_unmatched_case fmt) [loc_string];
                          semicolon fmt
                        );
                    );
                    cut fmt
                    )
                );
              cut fmt)
      )
  | Stmt_VarDecl (DeclItem_Var (v, _), i, loc)
  | Stmt_ConstDecl (DeclItem_Var (v, _), i, loc) ->
      assign loc fmt (fun _ -> varname fmt v) i
  | Stmt_VarDecl (DeclItem_Wildcard _, i, loc)
  | Stmt_ConstDecl (DeclItem_Wildcard _, i, loc) ->
      make_cast fmt (fun _ -> kw_void fmt) (fun _ -> expr loc fmt i);
      semicolon fmt
  | Stmt_For (v, f, dir, t, b, loc) ->
      kw_for fmt;
      nbsp fmt;
      parens fmt (fun _ ->
          kw_int64 fmt;
          nbsp fmt;
          varname fmt v;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          expr loc fmt f;
          semicolon fmt;
          nbsp fmt;
          varname fmt v;
          nbsp fmt;
          direction dir (fun _ -> lt_eq fmt) (fun _ -> gt_eq fmt);
          nbsp fmt;
          expr loc fmt t;
          semicolon fmt;
          nbsp fmt;
          direction dir (fun _ -> plus_plus fmt) (fun _ -> minus_minus fmt);
          varname fmt v);
      nbsp fmt;
      brace_enclosed_block fmt b
  | Stmt_FunReturn (e, loc) ->
      kw_return fmt;
      nbsp fmt;
      expr loc fmt e;
      semicolon fmt
  | Stmt_If (c, t, els, (e, el), loc) ->
      vbox fmt (fun _ ->
          kw_if fmt;
          nbsp fmt;
          parens fmt (fun _ -> expr loc fmt c);
          nbsp fmt;
          brace_enclosed_block fmt t;
          map fmt
            (fun (AST.S_Elsif_Cond (c, s, loc)) ->
              nbsp fmt;
              kw_else fmt;
              nbsp fmt;
              kw_if fmt;
              nbsp fmt;
              parens fmt (fun _ -> expr loc fmt c);
              nbsp fmt;
              brace_enclosed_block fmt s)
            els;
          if e <> [] then (
            nbsp fmt;
            kw_else fmt;
            nbsp fmt;
            brace_enclosed_block fmt e))
  | Stmt_While (c, b, loc) ->
      kw_while fmt;
      nbsp fmt;
      parens fmt (fun _ -> expr loc fmt c);
      nbsp fmt;
      brace_enclosed_block fmt b
  | Stmt_Repeat (b, c, pos, loc) ->
      kw_do fmt;
      nbsp fmt;
      brace_enclosed_block fmt b;
      nbsp fmt;
      kw_while fmt;
      nbsp fmt;
      parens fmt (fun _ -> expr loc fmt (Asl_utils.mk_not c));
      semicolon fmt
  | Stmt_ProcReturn loc ->
      kw_return fmt;
      semicolon fmt
  | Stmt_TCall (f, tes, args, throws, loc) ->
      funcall loc fmt f tes args loc;
      semicolon fmt;
      if (throws) then rethrow_stmt fmt
  | Stmt_VarDeclsNoInit (vs, Type_Constructor (Ident "__RAM", [_]), loc) ->
      cutsep fmt
        (fun v ->
          varname fmt v;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          parens fmt (fun _ -> ty_ram fmt);
          braces fmt (fun _ ->
              nbsp fmt;
              intLit fmt "0";
              nbsp fmt);
          semicolon fmt)
        vs
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      (* handled by decl *)
      ()
  | Stmt_Throw (e, loc) ->
      Format.fprintf fmt "ASL_exception = %a;@," (expr loc) e;
      Format.fprintf fmt "goto %a;" varname (current_catcher ())
  | Stmt_Try (tb, pos, catchers, odefault, loc) ->
      with_catch_label (fun catch_label ->
        brace_enclosed_block fmt tb;
        Format.fprintf fmt "@,%a:@," varname catch_label;
        );
      Format.fprintf fmt "if (ASL_exception._exc.ASL_tag == ASL_no_exception) {@,";
      List.iter (function AST.Catcher_Guarded (v, tc, b, loc) ->
          Format.fprintf fmt "} else if (ASL_exception._exc.ASL_tag == tag_%a) {" tycon tc;
          indented fmt (fun _ ->
            Format.fprintf fmt "%a %a = ASL_exception._%a;@,"
              tycon tc
              varname v
              tycon tc;
            brace_enclosed_block fmt tb
            );
          cut fmt
        )
        catchers;
      Format.fprintf fmt "} else {";
      indented fmt (fun _ ->
        ( match odefault with
        | None -> Format.fprintf fmt "goto %a;@," varname (current_catcher ())
        | Some (s, _) -> brace_enclosed_block fmt s
        ));
      Format.fprintf fmt "@,}"
  | Stmt_VarDecl _
  | Stmt_ConstDecl _
    ->
      raise
        (Unimplemented (AST.Unknown, "statement", fun fmt -> FMTAST.stmt fmt x))

and indented_block (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if xs <> [] then
    indented fmt (fun _ ->
        map fmt (decl fmt) xs;
        cutsep fmt (stmt fmt) xs)

and brace_enclosed_block (fmt : PP.formatter) (b : AST.stmt list) =
  braces fmt (fun _ ->
      indented_block fmt b;
      cut fmt)

let formal (loc : AST.l) (fmt : PP.formatter) (x : AST.ident * AST.ty) : unit =
  let v, t = x in
  varty loc fmt v t

let formals (loc : AST.l) (fmt : PP.formatter) (xs : (AST.ident * AST.ty) list) : unit =
  commasep fmt (formal loc fmt) xs

let function_header (loc : AST.l) (fmt : PP.formatter) (ot : AST.ty option) (f : AST.ident)
    (args : unit -> unit) : unit =
  PP.pp_print_option
    ~none:(fun _ _ -> kw_void fmt; nbsp fmt; varname fmt f)
    (fun _ t -> varty loc fmt f t) fmt ot;
  parens fmt args

let function_body (fmt : PP.formatter) (b : AST.stmt list) (orty : AST.ty option) : unit =
  with_catch_label (fun catch_label ->
    braces fmt
      (fun _ ->
         indented_block fmt b;
         cut fmt;
         Format.fprintf fmt "%a:" varname catch_label;
         (* When throwing an exception, we need to return a value with
          * the correct type. This value will never be used so the easy way
          * to create it is to declare a variable, not initialize it and
          * return the variables. This will make the C compiler emit a warning.
          * *)
         indented fmt (fun _ ->
           ( match orty  with
           | None -> Format.fprintf fmt "return;"
           | Some rty ->
             braces fmt (fun _ ->
               indented fmt (fun _ ->
                 let v = AST.Ident "ASL_fake_return_value" in
                 varty AST.Unknown fmt v rty;
                 Format.fprintf fmt ";@,return %a;" varname v
               )
             )
           ));
         cut fmt
      )
    )

let typedef (fmt : PP.formatter) (pp : unit -> unit) : unit =
  kw_typedef fmt;
  nbsp fmt;
  pp ();
  semicolon fmt;
  cut fmt

let declaration (fmt : PP.formatter) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      match x with
      | Decl_BuiltinType (tc, loc) -> (
          match tc with
          | Ident "real"
          | Ident "string"
          | Ident "__mask"
          | Ident "__Exception"
          | Ident "__RAM" -> ()
          | _ ->
              raise
                (Unimplemented
                   (AST.Unknown, "builtin type", fun fmt -> FMTAST.tycon fmt tc))
          )
      | Decl_Const (v, ty, e, loc) ->
          kw_const fmt;
          nbsp fmt;
          varty loc fmt v ty;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          expr loc fmt e;
          semicolon fmt;
          cut fmt;
          cut fmt
      | Decl_Enum (tc, es, loc) ->
          if tc = Ident "boolean" then (* is in C99 stdbool.h *) ()
          else (
            typedef fmt (fun _ ->
                kw_enum fmt;
                nbsp fmt;
                braces fmt (fun _ -> commasep fmt (varname fmt) es);
                nbsp fmt;
                tycon fmt tc
              );
            cut fmt)
      | Decl_FunDefn (f, ps, args, t, b, loc) ->
          function_header loc fmt (Some t) f (fun _ -> formals loc fmt args);
          nbsp fmt;
          function_body fmt b (Some t);
          cut fmt
      | Decl_FunType (f, ps, args, t, loc) ->
          function_header loc fmt (Some t) f (fun _ -> formals loc fmt args);
          semicolon fmt;
          cut fmt;
          cut fmt
      | Decl_ProcDefn (f, ps, args, b, loc) ->
          function_header loc fmt None f (fun _ -> formals loc fmt args);
          nbsp fmt;
          function_body fmt b None;
          cut fmt
      | Decl_ProcType (f, ps, args, loc) ->
          function_header loc fmt None f (fun _ -> formals loc fmt args);
          semicolon fmt;
          cut fmt;
          cut fmt
      | Decl_Record (tc, [], fs, loc) ->
          typedef fmt (fun _ ->
              kw_struct fmt;
              nbsp fmt;
              braces fmt (fun _ ->
                  indented fmt (fun _ ->
                      cutsep fmt
                        (fun (f, t) ->
                          varty loc fmt f t;
                          semicolon fmt)
                        fs);
                  cut fmt);
              nbsp fmt;
              tycon fmt tc);
          cut fmt
      | Decl_Typedef (tc, [], t, loc) ->
          typedef fmt (fun _ -> varty loc fmt tc t);
          cut fmt
      | Decl_Var (v, ty, loc) ->
          varty loc fmt v ty;
          semicolon fmt;
          cut fmt;
          cut fmt
      | Decl_BuiltinFunction (f, ps, args, t, loc) -> ()
      | _ ->
          raise
            (Unimplemented
               (AST.Unknown, "declaration", fun fmt -> FMTAST.declaration fmt x)))

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ -> map fmt (declaration fmt) xs)

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
    Format.fprintf fmt "enum ASL_exception_tag { ASL_no_exception, %a };@,@,"
      (Fun.flip commasep (fun (tc, fs, _) -> Format.fprintf fmt "tag_%a" tycon tc)) excs;
    List.iter (fun (tc, fs, loc) ->
      typedef fmt (fun _ ->
          kw_struct fmt;
          nbsp fmt;
          braces fmt (fun _ ->
              indented fmt (fun _ ->
                  Format.fprintf fmt "enum ASL_exception_tag ASL_tag;@,";
                  cutsep fmt
                    (fun (f, t) ->
                      varty loc fmt f t;
                      semicolon fmt)
                    fs);
              cut fmt);
          nbsp fmt;
          tycon fmt tc);
      cut fmt)
      excs;
    Format.fprintf fmt "@,typedef union {\n    %s\n%a\n} ASL_exception_t;@,"
      "struct { enum ASL_exception_tag ASL_tag; } _exc;"
      (Format.pp_print_list (fun fmt (tc, _, _) -> Format.fprintf fmt "    %a _%a;"
                                tycon tc
                                tycon tc))
      excs;
    Format.fprintf fmt "ASL_exception_t ASL_exception = (ASL_exception_t){ ._exc={.ASL_tag = ASL_no_exception} };@,"
    )

(****************************************************************
 * End
 ****************************************************************)
