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

exception Unimplemented of (AST.l * string * (Format.formatter -> unit))

let mangle (s : string) : string =
  (* TODO this should detect whether s is a reserved name in C and rename to
     avoid conflict *)
  s

let delimiter (fmt : PP.formatter) (s : string) : unit =
  PP.pp_print_string fmt s

let keyword (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt s
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

let kw_assert (fmt : PP.formatter) : unit = keyword fmt "assert"
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

(* C functions defined elsewhere *)
let fn_cvt_bits_sint (fmt : PP.formatter) : unit = keyword fmt "ASL_cvt_bits_sint"
let fn_is_pow2_int (fmt : PP.formatter) : unit = keyword fmt "ASL_is_pow2"
let fn_replicate_bits (fmt : PP.formatter) : unit = keyword fmt "ASL_replicate_bits"
let fn_slice_lowd (fmt : PP.formatter) : unit = keyword fmt "ASL_slice_lowd"
let fn_slice_hilo (fmt : PP.formatter) : unit = keyword fmt "ASL_slice_hilo"
let fn_slice_lowd_w (fmt : PP.formatter) : unit = keyword fmt "ASL_slice_lowd_w"
let fn_slice_hilo_w (fmt : PP.formatter) : unit = keyword fmt "ASL_slice_hilo_w"

let intLit (fmt : PP.formatter) (x : AST.intLit) : unit = constant fmt x
let hexLit (fmt : PP.formatter) (x : AST.hexLit) : unit = constant fmt ("0x" ^ x)

let bitsLit (fmt : PP.formatter) (x : AST.bitsLit) : unit =
  let bit_string = Value.drop_chars x ' ' in
  let integer = Z.of_string_base 2 bit_string in
  constant fmt ("0x" ^ Z.format "%x" integer)

let strLit (fmt : PP.formatter) (x : string) : unit =
  constant fmt ("\"" ^ x ^ "\"")

let const_expr (x : AST.expr) : V.value =
  match x with
  | Expr_LitBits b -> V.from_bitsLit b
  | Expr_LitHex i -> V.from_hexLit i
  | Expr_LitInt i -> V.from_intLit i
  | Expr_LitMask b -> V.from_maskLit b
  | Expr_LitReal r -> V.from_realLit r
  | Expr_LitString s -> V.from_stringLit s
  | _ ->
      failwith ("const_expr: not literal constant '" ^ Asl_utils.pp_expr x ^ "'")

let const_int_expr (x : AST.expr) : int =
  match const_expr x with
  | VInt i -> Z.to_int i
  | _ -> failwith "const_int_expr: integer expected"

let uint (fmt : PP.formatter) (width : int) : unit =
  if width <= 8 then kw_uint8 fmt
  else if width <= 16 then kw_uint16 fmt
  else if width <= 32 then kw_uint32 fmt
  else if width <= 64 then kw_uint64 fmt
  else
    raise
      (Unimplemented
         (AST.Unknown, "uint width: " ^ string_of_int width, fun fmt -> ()))

let sint (fmt : PP.formatter) (width : int) : unit =
  if width <= 8 then kw_int8 fmt
  else if width <= 16 then kw_int16 fmt
  else if width <= 32 then kw_int32 fmt
  else if width <= 64 then kw_int64 fmt
  else
    raise
      (Unimplemented
         (AST.Unknown, "sint width: " ^ string_of_int width, fun fmt -> ()))

let ty (fmt : PP.formatter) (x : AST.ty) : unit =
  match x with
  | Type_Bits n -> uint fmt (const_int_expr n)
  | Type_Constructor tc -> (
      match tc with
      | Ident "boolean" -> kw_bool fmt
      | Ident "string" ->
          kw_char fmt;
          nbsp fmt;
          star fmt
      | _ -> tycon fmt tc)
  (* TODO implement integer range analysis to determine the correct type width.
   * For now use int64. *)
  | Type_Integer _ -> kw_int64 fmt
  | Type_Register (n, _) -> uint fmt (const_int_expr n)
  | Type_App (_, _)
  | Type_Array (_, _)
  | Type_OfExpr _
  | Type_Tuple _ ->
      raise (Unimplemented (AST.Unknown, "type", fun fmt -> FMTAST.ty fmt x))

let rec apply (fmt : PP.formatter) (f : unit -> unit) (args : AST.expr list) :
    unit =
  f ();
  parens fmt (fun _ -> exprs fmt args)

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

and pow2_int (fmt : PP.formatter) (x : AST.expr) : unit =
  make_binop fmt
    (fun _ -> lt_lt fmt)
    (fun _ ->
      (* TODO determine correct type width. For now use uint64. *)
      make_cast fmt (fun _ -> kw_uint64 fmt) (fun _ -> intLit fmt "1"))
    (fun _ -> expr fmt x)

(* Calculate mask with x ones *)
and mask_int (fmt : PP.formatter) (x : AST.expr) : unit =
  make_binop fmt
    (fun _ -> minus fmt)
    (fun _ -> pow2_int fmt x)
    (fun _ -> intLit fmt "1")

and binop (fmt : PP.formatter) (op : string) (args : AST.expr list) : unit =
  match args with
  | [ x; y ] ->
      make_binop fmt
        (fun _ -> delimiter fmt op)
        (fun _ -> expr fmt x)
        (fun _ -> expr fmt y)
  | _ -> raise (Unimplemented (AST.Unknown, "binop: " ^ op, fun fmt -> ()))

and unop (fmt : PP.formatter) (op : string) (args : AST.expr list) : unit =
  match args with
  | [ x ] -> make_unop fmt (fun _ -> delimiter fmt op) (fun _ -> expr fmt x)
  | _ -> raise (Unimplemented (AST.Unknown, "unop: " ^ op, fun fmt -> ()))

and cond_cont (fmt : PP.formatter) (c : AST.expr) (x : AST.expr)
    (y : unit -> unit) : unit =
  parens fmt (fun _ ->
      expr fmt c;
      nbsp fmt;
      delimiter fmt "?";
      nbsp fmt;
      expr fmt x;
      nbsp fmt;
      delimiter fmt ":";
      nbsp fmt;
      y ())

and cond (fmt : PP.formatter) (c : AST.expr) (x : AST.expr) (y : AST.expr) :
    unit =
  cond_cont fmt c x (fun _ -> expr fmt y)

and conds (fmt : PP.formatter) (cts : (AST.expr * AST.expr) list) (e : AST.expr)
    : unit =
  match cts with
  | [] -> expr fmt e
  | (c, t) :: cts' -> cond_cont fmt c t (fun _ -> conds fmt cts' e)

and funcall (fmt : PP.formatter) (f : AST.ident) (tes : AST.expr list)
    (args : AST.expr list) (loc : AST.l) =
  match (f, args) with
  (* Boolean builtin functions *)
  | FIdent ("and_bool", _), _ -> binop fmt "&&" args
  | FIdent ("eq_bool", _), _ | FIdent ("equiv_bool", _), _ ->
      binop fmt "==" args
  | FIdent ("implies_bool", _), [ x; y ] ->
      cond fmt x y (AST.Expr_Var (Ident "TRUE"))
  | FIdent ("ne_bool", _), _ -> binop fmt "!=" args
  | FIdent ("not_bool", _), _ -> unop fmt "!" args
  | FIdent ("or_bool", _), _ -> binop fmt "||" args
  (* Integer builtin functions *)
  | FIdent ("add_int", _), _ -> binop fmt "+" args
  | FIdent ("align_int", _), [ x; y ] ->
      make_binop fmt
        (fun _ -> amp fmt)
        (fun _ -> expr fmt x)
        (fun _ -> make_unop fmt (fun _ -> tilde fmt) (fun _ -> mask_int fmt y))
  | FIdent ("eq_int", _), _ -> binop fmt "==" args
  | FIdent ("ge_int", _), _ -> binop fmt ">=" args
  | FIdent ("gt_int", _), _ -> binop fmt ">" args
  | FIdent ("is_pow2_int", _), _ -> apply fmt (fun _ -> fn_is_pow2_int fmt) args
  | FIdent ("le_int", _), _ -> binop fmt "<=" args
  | FIdent ("lt_int", _), _ -> binop fmt "<" args
  | FIdent ("mod_pow2_int", _), [ x; y ] ->
      make_binop fmt
        (fun _ -> amp fmt)
        (fun _ -> expr fmt x)
        (fun _ -> mask_int fmt y)
  | FIdent ("mul_int", _), _ -> binop fmt "*" args
  | FIdent ("ne_int", _), _ -> binop fmt "!=" args
  | FIdent ("neg_int", _), _ -> unop fmt "-" args
  | FIdent ("pow2_int", _), [ x ] -> pow2_int fmt x
  | FIdent ("shl_int", _), _ -> binop fmt "<<" args
  | FIdent ("shr_int", _), _ -> binop fmt ">>" args
  | FIdent ("sub_int", _), _ -> binop fmt "-" args
  | FIdent ("zdiv_int", _), _ -> binop fmt "/" args
  | FIdent ("zrem_int", _), _ -> binop fmt "%" args
  | FIdent ("fdiv_int", _), _ | FIdent ("frem_int", _), _ ->
      raise
        (Unimplemented
           ( AST.Unknown,
             "integer builtin function",
             fun fmt -> FMTAST.funname fmt f ))
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
           ( AST.Unknown,
             "real builtin function",
             fun fmt -> FMTAST.funname fmt f ))
  (* Bitvector builtin functions *)
  | FIdent ("add_bits", _), _ -> binop fmt "+" args
  | FIdent ("and_bits", _), _ -> binop fmt "&" args
  | FIdent ("cvt_bits_sint", _), [ x ] -> (
      let x_width = const_int_expr (List.hd tes) in
      match x_width with
      | 64 | 32 | 16 | 8 ->
          make_cast fmt (fun _ -> sint fmt x_width) (fun _ -> expr fmt x)
      | _ -> apply fmt (fun _ -> fn_cvt_bits_sint fmt) (args @ [ List.hd tes ]))
  | FIdent ("cvt_bits_uint", _), [ x ] ->
      (* TODO determine correct type width. For now use uint64. *)
      make_cast fmt (fun _ -> kw_uint64 fmt) (fun _ -> expr fmt x)
  | FIdent ("cvt_int_bits", _), [ x; n ] ->
      make_binop fmt
        (fun _ -> amp fmt)
        (fun _ -> expr fmt x)
        (fun _ -> mask_int fmt n)
  | FIdent ("eor_bits", _), _ -> binop fmt "^" args
  | FIdent ("eq_bits", _), _ -> binop fmt "==" args
  | FIdent ("frem_bits_int", _), _
  | FIdent ("in_mask", _), _
  | FIdent ("notin_mask", _), _ ->
      raise
        (Unimplemented
           (AST.Unknown, "bitvector builtin function", fun fmt -> FMTAST.funname fmt f))
  | FIdent ("mul_bits", _), _ -> binop fmt "*" args
  | FIdent ("ne_bits", _), _ -> binop fmt "!=" args
  | FIdent ("not_bits", _), _ -> unop fmt "~" args
  | FIdent ("ones_bits", _), [] ->
      let x = List.hd tes in
      bitsLit fmt (String.make (const_int_expr x) '1')
  | FIdent ("or_bits", _), _ -> binop fmt "|" args
  | FIdent ("replicate_bits", _), _ ->
      let x_width = List.nth tes 1 in
      apply fmt (fun _ -> fn_replicate_bits fmt) (args @ [x_width])
  | FIdent ("sub_bits", _), _ -> binop fmt "-" args
  | FIdent ("zeros_bits", _), [] ->
      let x = List.hd tes in
      bitsLit fmt (String.make (const_int_expr x) '0')
  (* String builtin functions *)
  | FIdent ("append_str_str", _), _
  | FIdent ("cvt_bits_str", _), _
  | FIdent ("cvt_bool_str", _), _
  | FIdent ("cvt_int_decstr", _), _
  | FIdent ("cvt_int_hexstr", _), _
  | FIdent ("cvt_real_str", _), _
  | FIdent ("eq_str", _), _
  | FIdent ("ne_str", _), _ ->
      raise
        (Unimplemented
           (AST.Unknown, "string builtin function", fun fmt -> FMTAST.funname fmt f))
  | FIdent ("print_bits", _), _
  | FIdent ("print_char", _), [ _ ]
  | FIdent ("print_str", _), _ ->
      raise
        (Unimplemented
           (AST.Unknown, "print builtin function", fun fmt -> FMTAST.funname fmt f))
  | _ -> apply fmt (fun _ -> funname fmt f) args

and slice (fmt : PP.formatter) (e : AST.expr) (s : AST.slice) : unit =
  match s with
  | Slice_HiLo (hi, lo) -> apply fmt (fun _ -> fn_slice_hilo fmt) [ e; hi; lo ]
  | Slice_LoWd (lo, wd) -> apply fmt (fun _ -> fn_slice_lowd fmt) [ e; lo; wd ]
  | Slice_Single lo ->
      apply fmt (fun _ -> fn_slice_lowd fmt) [ e; lo; Expr_LitInt "1" ]

and lslice (fmt : PP.formatter) (v : AST.expr) (e : AST.expr) (s : AST.slice) :
    unit =
  match s with
  | Slice_HiLo (hi, lo) ->
      apply fmt (fun _ -> fn_slice_hilo_w fmt) [ v; e; hi; lo ]
  | Slice_LoWd (lo, wd) ->
      apply fmt (fun _ -> fn_slice_lowd_w fmt) [ v; e; lo; wd ]
  | Slice_Single lo ->
      apply fmt (fun _ -> fn_slice_lowd_w fmt) [ v; e; lo; Expr_LitInt "1" ]

and expr (fmt : PP.formatter) (x : AST.expr) : unit =
  match x with
  | Expr_Concat (ws, es) ->
      let _, shifts =
        List.fold_right
          (fun w (acc, shifts) -> (acc + const_int_expr w, acc :: shifts))
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
            (fun _ -> expr fmt e)
            (fun _ -> intLit fmt (string_of_int sh)))
        (List.combine shifts es)
  | Expr_Field (e, f) ->
      expr fmt e;
      dot fmt;
      fieldname fmt f
  | Expr_If (c, t, els, e) ->
      let els1 = List.map (function AST.E_Elsif_Cond (c, e) -> (c, e)) els in
      conds fmt ((c, t) :: els1) e
  | Expr_LitBits l -> bitsLit fmt l
  | Expr_LitHex l -> hexLit fmt l
  | Expr_LitInt l -> intLit fmt l
  | Expr_LitString l -> strLit fmt l
  | Expr_Parens e -> expr fmt e
  | Expr_RecordInit (tc, fas) ->
      parens fmt (fun _ -> tycon fmt tc);
      braces fmt (fun _ ->
          nbsp fmt;
          commasep fmt
            (fun (f, e) ->
              dot fmt;
              varname fmt f;
              nbsp fmt;
              eq fmt;
              nbsp fmt;
              expr fmt e)
            fas;
          nbsp fmt)
  | Expr_Slices (e, [ s ]) -> slice fmt e s
  | Expr_TApply (f, tes, es) -> funcall fmt f tes es AST.Unknown
  | Expr_Var v -> (
      match v with
      | Ident "TRUE" -> kw_true fmt
      | Ident "FALSE" -> kw_false fmt
      | _ -> varname fmt v)
  | Expr_Array _
  | Expr_AsConstraint _
  | Expr_AsType _
  | Expr_Binop _
  | Expr_Fields _
  | Expr_ImpDef _
  | Expr_In _
  | Expr_LitMask _
  | Expr_LitReal _
  | Expr_Slices _
  | Expr_Tuple _
  | Expr_Unknown _
  | Expr_Unop _ ->
      raise
        (Unimplemented (AST.Unknown, "expression", fun fmt -> FMTAST.expr fmt x))

and exprs (fmt : PP.formatter) (es : AST.expr list) : unit =
  commasep fmt (expr fmt) es

let pattern (fmt : PP.formatter) (x : AST.pattern) : unit =
  match x with
  | Pat_LitBits l -> bitsLit fmt l
  | Pat_LitHex l -> hexLit fmt l
  | Pat_LitInt l -> intLit fmt l
  | Pat_Const _ | Pat_LitMask _ | Pat_Range _ | Pat_Set _ | Pat_Single _
  | Pat_Tuple _ | Pat_Wildcard ->
      raise
        (Unimplemented (AST.Unknown, "pattern", fun fmt -> FMTAST.pattern fmt x))

let patterns (fmt : PP.formatter) (ps : AST.pattern list) : unit =
  match ps with
  | [ p ] -> pattern fmt p
  | _ ->
      raise
        (Unimplemented
           (AST.Unknown, "patterns", fun fmt -> FMTAST.patterns fmt ps))

let assign (fmt : PP.formatter) (l : unit -> unit) (r : AST.expr) : unit =
  l ();
  nbsp fmt;
  eq fmt;
  nbsp fmt;
  expr fmt r;
  semicolon fmt

let lexpr (fmt : PP.formatter) (x : AST.lexpr) (r : AST.expr) : unit =
  match x with
  | LExpr_Slices (LExpr_Var v, [ s ]) ->
      lslice fmt r (Expr_Var v) s;
      semicolon fmt
  | LExpr_Var v -> assign fmt (fun _ -> varname fmt v) r
  | LExpr_Wildcard ->
      make_cast fmt (fun _ -> kw_void fmt) (fun _ -> expr fmt r);
      semicolon fmt
  | LExpr_Array _
  | LExpr_BitTuple _
  | LExpr_Field _
  | LExpr_Fields _
  | LExpr_ReadWrite _
  | LExpr_Slices _
  | LExpr_Tuple _
  | LExpr_Write _ ->
      raise
        (Unimplemented
           (AST.Unknown, "l-expression", fun fmt -> FMTAST.lexpr fmt x))

let varty (fmt : PP.formatter) (v : AST.ident) (t : AST.ty) : unit =
  match t with
  | _ ->
      ty fmt t;
      nbsp fmt;
      varname fmt v

let rec declitem (fmt : PP.formatter) (x : AST.decl_item) =
  match x with
  | DeclItem_Var (v, Some t) ->
      varty fmt v t;
      semicolon fmt
  | DeclItem_Tuple dis -> cutsep fmt (declitem fmt) dis
  | DeclItem_Var (v, None) ->
      raise
        (Unimplemented
           ( AST.Unknown,
             "decl: type of variable unknown",
             fun fmt -> FMTAST.varname fmt v ))
  | DeclItem_Wildcard _ ->
      raise
        (Unimplemented (AST.Unknown, "decl: use of wildcard", fun fmt -> ()))

let decl (fmt : PP.formatter) (x : AST.stmt) : unit =
  match x with
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      ty fmt t;
      nbsp fmt;
      varnames fmt vs;
      semicolon fmt;
      cut fmt
  | Stmt_VarDecl (di, i, loc)
  | Stmt_ConstDecl (di, i, loc) ->
      declitem fmt di;
      cut fmt
  | _ -> ()

let direction (x : AST.direction) (up : unit -> unit) (down : unit -> unit) :
    unit =
  match x with Direction_Up -> up () | Direction_Down -> down ()

let rec stmt (fmt : PP.formatter) (x : AST.stmt) : unit =
  match x with
  | Stmt_Assert (e, loc) ->
      kw_assert fmt;
      parens fmt (fun _ -> expr fmt e);
      semicolon fmt
  | Stmt_Assign (l, r, loc) -> lexpr fmt l r
  | Stmt_Block (ss, _) ->
      braces fmt (fun _ -> indented_block fmt ss; cut fmt)
  | Stmt_Case (e, alts, ob, loc) ->
      vbox fmt (fun _ ->
          kw_switch fmt;
          nbsp fmt;
          parens fmt (fun _ -> expr fmt e);
          nbsp fmt;
          braces fmt (fun _ ->
              indented fmt (fun _ ->
                  cutsep fmt
                    (fun (AST.Alt_Alt (ps, oc, ss, loc)) ->
                      kw_case fmt;
                      nbsp fmt;
                      patterns fmt ps;
                      if Option.is_some oc then
                        raise
                          (Unimplemented (loc, "pattern_guard", fun fmt -> ()));
                      colon fmt;
                      nbsp fmt;
                      braces fmt (fun _ ->
                          indented_block fmt ss;
                          indented fmt (fun _ ->
                              kw_break fmt;
                              semicolon fmt);
                          cut fmt))
                    alts;
                  PP.pp_print_option
                    (fun _ (b, bl) ->
                      cut fmt;
                      kw_default fmt;
                      colon fmt;
                      nbsp fmt;
                      braces fmt (fun _ ->
                          indented_block fmt b;
                          indented fmt (fun _ ->
                              kw_break fmt;
                              semicolon fmt);
                          cut fmt))
                    fmt ob);
              cut fmt))
  | Stmt_VarDecl (DeclItem_Var (v, _), i, loc)
  | Stmt_ConstDecl (DeclItem_Var (v, _), i, loc) ->
      varname fmt v;
      nbsp fmt;
      eq fmt;
      nbsp fmt;
      expr fmt i;
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
          expr fmt f;
          semicolon fmt;
          nbsp fmt;
          varname fmt v;
          nbsp fmt;
          direction dir (fun _ -> lt_eq fmt) (fun _ -> gt_eq fmt);
          nbsp fmt;
          expr fmt t;
          semicolon fmt;
          nbsp fmt;
          direction dir (fun _ -> plus_plus fmt) (fun _ -> minus_minus fmt);
          varname fmt v);
      nbsp fmt;
      braces fmt (fun _ ->
          indented_block fmt b;
          cut fmt)
  | Stmt_FunReturn (e, loc) ->
      kw_return fmt;
      nbsp fmt;
      expr fmt e;
      semicolon fmt
  | Stmt_If (c, t, els, (e, el), loc) ->
      vbox fmt (fun _ ->
          kw_if fmt;
          nbsp fmt;
          parens fmt (fun _ -> expr fmt c);
          nbsp fmt;
          braces fmt (fun _ ->
              indented_block fmt t;
              cut fmt);
          map fmt
            (fun (AST.S_Elsif_Cond (c, s, loc)) ->
              nbsp fmt;
              kw_else fmt;
              nbsp fmt;
              kw_if fmt;
              nbsp fmt;
              parens fmt (fun _ -> expr fmt c);
              nbsp fmt;
              braces fmt (fun _ ->
                  indented_block fmt s;
                  cut fmt))
            els;
          if e <> [] then (
            nbsp fmt;
            kw_else fmt;
            nbsp fmt;
            braces fmt (fun _ ->
                indented_block fmt e;
                cut fmt)))
  | Stmt_ProcReturn loc ->
      kw_return fmt;
      semicolon fmt
  | Stmt_TCall (f, tes, args, loc) ->
      funcall fmt f tes args loc;
      semicolon fmt
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      (* handled by decl *)
      ()
  | Stmt_VarDecl _
  | Stmt_ConstDecl _
  | Stmt_Repeat _
  | Stmt_Throw _
  | Stmt_Try _
  | Stmt_While _ ->
      raise
        (Unimplemented (AST.Unknown, "statement", fun fmt -> FMTAST.stmt fmt x))

and indented_block (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if xs <> [] then
    indented fmt (fun _ ->
        map fmt (decl fmt) xs;
        cutsep fmt (stmt fmt) xs)

let formal (fmt : PP.formatter) (x : AST.ident * AST.ty) : unit =
  let v, t = x in
  varty fmt v t

let formals (fmt : PP.formatter) (xs : (AST.ident * AST.ty) list) : unit =
  commasep fmt (formal fmt) xs

let function_header (fmt : PP.formatter) (ot : AST.ty option) (f : AST.ident)
    (args : unit -> unit) : unit =
  PP.pp_print_option ~none:(fun _ _ -> kw_void fmt) (fun _ t -> ty fmt t) fmt ot;
  nbsp fmt;
  funname fmt f;
  parens fmt args

let function_body (fmt : PP.formatter) (b : AST.stmt list) : unit =
  braces fmt (fun _ ->
      indented_block fmt b;
      cut fmt);
  cut fmt

let typedef (fmt : PP.formatter) (tc : AST.ident) (pp : unit -> unit) : unit =
  kw_typedef fmt;
  nbsp fmt;
  pp ();
  nbsp fmt;
  tycon fmt tc;
  semicolon fmt;
  cut fmt

let declaration (fmt : PP.formatter) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      match x with
      | Decl_Enum (tc, es, loc) ->
          if tc = Ident "boolean" then (* is in C99 stdbool.h *) ()
          else (
            typedef fmt tc (fun _ ->
                kw_enum fmt;
                nbsp fmt;
                braces fmt (fun _ -> commasep fmt (varname fmt) es));
            cut fmt)
      | Decl_FunDefn (f, ps, args, t, b, loc) ->
          function_header fmt (Some t) f (fun _ -> formals fmt args);
          nbsp fmt;
          function_body fmt b;
          cut fmt
      | Decl_FunType (f, ps, args, t, loc) ->
          function_header fmt (Some t) f (fun _ -> formals fmt args);
          semicolon fmt;
          cut fmt;
          cut fmt
      | Decl_ProcDefn (f, ps, args, b, loc) ->
          function_header fmt None f (fun _ -> formals fmt args);
          nbsp fmt;
          function_body fmt b;
          cut fmt
      | Decl_ProcType (f, ps, args, loc) ->
          function_header fmt None f (fun _ -> formals fmt args);
          semicolon fmt;
          cut fmt;
          cut fmt
      | Decl_Record (tc, fs, loc) ->
          typedef fmt tc (fun _ ->
              kw_struct fmt;
              nbsp fmt;
              braces fmt (fun _ ->
                  indented fmt (fun _ ->
                      cutsep fmt
                        (fun (f, t) ->
                          varty fmt f t;
                          semicolon fmt)
                        fs);
                  cut fmt));
          cut fmt
      | Decl_Typedef (tc, t, loc) ->
          typedef fmt tc (fun _ -> ty fmt t);
          cut fmt
      | Decl_Var (v, ty, loc) ->
          varty fmt v ty;
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

(****************************************************************
 * End
 ****************************************************************)
