(****************************************************************
 * ASL to Verilog backend
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to Verilog backend *)

module AST = Asl_ast
module FMTAST = Asl_fmt
module PP = Format
module V = Value
open Builtin_idents
open Format_utils
open Utils

let drop_spaces (x : string) : string = Value.drop_chars x ' '
let drop_underscores (x : string) : string = Value.drop_chars x '_'

let mangle (s : string) : string =
  (* detect whether s is a reserved name in System Verilog and rename to avoid conflict *)
  if s = "bit" then "_bit"
  else s

let comment (fmt : PP.formatter) (x : string) : unit =
  PP.pp_print_string fmt ("// " ^ x);
  cut fmt

let delimiter (fmt : PP.formatter) (s : string) : unit =
  PP.pp_print_string fmt s

let keyword (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt s
let constant (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt s

let ident_str (fmt : PP.formatter) (x : string) : unit =
  PP.pp_print_string fmt x

let ident (fmt : PP.formatter) (x : Ident.t) : unit =
  ident_str fmt (mangle (Ident.name_with_tag x))

let tycon (fmt : PP.formatter) (x : Ident.t) : unit =
  if Ident.equal x boolean_ident
  then ident_str fmt "asl_boolean"
  else ident fmt x

let funname (fmt : PP.formatter) (x : Ident.t) : unit = ident fmt x
let varname (fmt : PP.formatter) (x : Ident.t) : unit = ident fmt x
let fieldname (fmt : PP.formatter) (x : Ident.t) : unit = ident fmt x
let varnames (fmt : PP.formatter) (xs : Ident.t list) : unit =
  commasep fmt (varname fmt) xs

(* integer representation is a signed value of some width
 * this width should be large enough that data does not overflow
 *)
let int_width = ref 66

(* Include `line directives in generated code *)
let include_line_info : bool ref = ref false

(* list of all exception tycons - used to decide whether to insert a tag in
 * Expr_RecordInit
 *)
let exception_tcs : Ident.t list ref = ref []

(** Current function that we are generating code for.
 *  This is used when throwing exceptions.
 *)
let current_function : (Ident.t * AST.ty option) option ref = ref None

(* Verilog delimiters *)

let amp_amp (fmt : PP.formatter) : unit = delimiter fmt "&&"
let bang (fmt : PP.formatter) : unit = delimiter fmt "!"
let bang_eq (fmt : PP.formatter) : unit = delimiter fmt "!="
let bar_bar (fmt : PP.formatter) : unit = delimiter fmt "||"
let caret (fmt : PP.formatter) : unit = delimiter fmt "^"
let colon (fmt : PP.formatter) : unit = delimiter fmt ":"
let dot (fmt : PP.formatter) : unit = delimiter fmt "."
let dot_dot (fmt : PP.formatter) : unit = delimiter fmt ".."
let eq (fmt : PP.formatter) : unit = delimiter fmt "="
let eq_eq (fmt : PP.formatter) : unit = delimiter fmt "=="
let eq_gt (fmt : PP.formatter) : unit = delimiter fmt "=>"
let gt (fmt : PP.formatter) : unit = delimiter fmt ">"
let gt_eq (fmt : PP.formatter) : unit = delimiter fmt ">="
let gt_gt (fmt : PP.formatter) : unit = delimiter fmt ">>"
let lt (fmt : PP.formatter) : unit = delimiter fmt "<"
let lt_eq (fmt : PP.formatter) : unit = delimiter fmt "<="
let lt_lt (fmt : PP.formatter) : unit = delimiter fmt "<<"
let minus (fmt : PP.formatter) : unit = delimiter fmt "-"
let plus (fmt : PP.formatter) : unit = delimiter fmt "+"
let plus_colon (fmt : PP.formatter) : unit = delimiter fmt "+:"
let plus_plus (fmt : PP.formatter) : unit = delimiter fmt "++"
let rbrace (fmt : PP.formatter) : unit = delimiter fmt "}"
let rbrack (fmt : PP.formatter) : unit = delimiter fmt "]"
let rparen (fmt : PP.formatter) : unit = delimiter fmt ")"
let semicolon (fmt : PP.formatter) : unit = delimiter fmt ";"
let slash (fmt : PP.formatter) : unit = delimiter fmt "/"
let star (fmt : PP.formatter) : unit = delimiter fmt "*"

(* Verilog keywords *)

let kw_array (fmt : PP.formatter) : unit = keyword fmt "array"
let kw_assert (fmt : PP.formatter) : unit = keyword fmt "assert"
let kw_assume (fmt : PP.formatter) : unit = keyword fmt "assume"
let kw_automatic (fmt : PP.formatter) : unit = keyword fmt "automatic"
let kw_begin (fmt : PP.formatter) : unit = keyword fmt "begin"
let kw_bit (fmt : PP.formatter) : unit = keyword fmt "bit"
let kw_break (fmt : PP.formatter) : unit = keyword fmt "break"
let kw_case (fmt : PP.formatter) : unit = keyword fmt "case"
let kw_casez (fmt : PP.formatter) : unit = keyword fmt "casez"
let kw_const (fmt : PP.formatter) : unit = keyword fmt "const"
let kw_constant (fmt : PP.formatter) : unit = keyword fmt "constant"
let kw_continue (fmt : PP.formatter) : unit = keyword fmt "continue"
let kw_default (fmt : PP.formatter) : unit = keyword fmt "default"
let kw_do (fmt : PP.formatter) : unit = keyword fmt "do"
let kw_else (fmt : PP.formatter) : unit = keyword fmt "else"
let kw_end (fmt : PP.formatter) : unit = keyword fmt "end"
let kw_endcase (fmt : PP.formatter) : unit = keyword fmt "endcase"
let kw_endfunction (fmt : PP.formatter) : unit = keyword fmt "endfunction"
let kw_enum (fmt : PP.formatter) : unit = keyword fmt "enum"
let kw_function (fmt : PP.formatter) : unit = keyword fmt "function"
let kw_if (fmt : PP.formatter) : unit = keyword fmt "if"
let kw_iff (fmt : PP.formatter) : unit = keyword fmt "iff"
let kw_implies (fmt : PP.formatter) : unit = keyword fmt "implies"
let kw_integer (fmt : PP.formatter) : unit = keyword fmt "asl_integer"
let kw_input (fmt : PP.formatter) : unit = keyword fmt "input"
let kw_inout (fmt : PP.formatter) : unit = keyword fmt "inout"
let kw_inside (fmt : PP.formatter) : unit = keyword fmt "inside"
let kw_int (fmt : PP.formatter) : unit = keyword fmt "int"
let kw_let (fmt : PP.formatter) : unit = keyword fmt "let"
let kw_logic (fmt : PP.formatter) : unit = keyword fmt "logic"
let kw_packed (fmt : PP.formatter) : unit = keyword fmt "packed"
let kw_priority (fmt : PP.formatter) : unit = keyword fmt "priority"
let kw_return (fmt : PP.formatter) : unit = keyword fmt "return"
let kw_signed (fmt : PP.formatter) : unit = keyword fmt "signed"
let kw_static (fmt : PP.formatter) : unit = keyword fmt "static"
let kw_string (fmt : PP.formatter) : unit = keyword fmt "string"
let kw_struct (fmt : PP.formatter) : unit = keyword fmt "struct"
let kw_type (fmt : PP.formatter) : unit = keyword fmt "type"
let kw_typedef (fmt : PP.formatter) : unit = keyword fmt "typedef"
let kw_typeof (fmt : PP.formatter) : unit = keyword fmt "typeof"
let kw_union (fmt : PP.formatter) : unit = keyword fmt "union"
let kw_unsigned (fmt : PP.formatter) : unit = keyword fmt "unsigned"
let kw_var (fmt : PP.formatter) : unit = keyword fmt "var"
let kw_void (fmt : PP.formatter) : unit = keyword fmt "void"
let kw_with (fmt : PP.formatter) : unit = keyword fmt "with"

(* pseudo-keywords *)
let kw_false = "1'b0"
let kw_true = "1'b1"

(* Verilog system functions *)

let fn_writeb (fmt : PP.formatter) : unit = keyword fmt "$writeb"
let fn_writed (fmt : PP.formatter) : unit = keyword fmt "$write"
let fn_writeh (fmt : PP.formatter) : unit = keyword fmt "$writeh"
let fn_writes (fmt : PP.formatter) : unit = keyword fmt "$write"
let fn_typeof (fmt : PP.formatter) : unit = keyword fmt "$typeof"
let fn_onehot (fmt : PP.formatter) : unit = keyword fmt "$onehot"

let intLit (fmt : PP.formatter) (x : AST.intLit) : unit =
  constant fmt (string_of_int !int_width ^ "'sd" ^ drop_underscores x)

let hexLit (fmt : PP.formatter) (x : AST.hexLit) : unit =
  constant fmt (string_of_int !int_width ^ "'sh" ^ drop_underscores x)

let bitsLit (fmt : PP.formatter) (x : AST.bitsLit) : unit =
  let bits = drop_spaces x in
  (* todo: better to just change spaces to _ *)
  let width = String.length bits in
  constant fmt (string_of_int width ^ "'b" ^ bits)

let maskLit (fmt : PP.formatter) (x : AST.maskLit) : unit =
  let bits = drop_spaces x in
  (* todo: better to just change spaces to _ *)
  let bits = String.map (fun c -> if c = 'x' then '?' else c) bits in
  let width = String.length bits in
  constant fmt (string_of_int width ^ "'b" ^ bits)

let realLit (fmt : PP.formatter) (x : AST.realLit) : unit = constant fmt x

let strLit (fmt : PP.formatter) (x : string) : unit =
  constant fmt ("\"" ^ String.escaped x ^ "\"")

let valueLit (loc : AST.l) (fmt : PP.formatter) (x : V.value) : unit =
  match x with
  | VBool b -> constant fmt (if b then kw_true else kw_false)
  | VEnum (e, _) -> ident fmt e
  | VInt i -> intLit fmt (Z.to_string i)
  | VBits b ->
      let s = Z.format "%0b" b.v in
      let pad = String.make (b.n - String.length s) '0' in
      bitsLit fmt (pad ^ s)
  | VString s -> strLit fmt s
  | _ ->
      raise (Error.Unimplemented (loc, "constant value type", fun fmt -> ()))

let ones (fmt : PP.formatter) (x : int) : unit =
  braces fmt (fun _ ->
      intLit fmt (string_of_int x);
      braces fmt (fun _ -> bitsLit fmt "1"))

let rethrow_stmt (loc : AST.l) (fmt : PP.formatter) : unit =
  ( match !current_function with
  | None ->
      raise (Error.Unimplemented (loc, "exception rethrow outside a function", fun fmt -> ()))
  | Some (f, None) ->
      PP.fprintf fmt
        "if (ASL_exception._exc.ASL_tag != ASL_no_exception) return;"
  | Some (f, oty) ->
      PP.fprintf fmt
        "if (ASL_exception._exc.ASL_tag != ASL_no_exception) return %a;"
        varname f
  )

let rec varty (loc : AST.l) (fmt : PP.formatter) (v : Ident.t) (x : AST.ty) : unit =
  ( match x with
  | Type_Bits n
  | Type_Register (n, _) ->
    kw_bit fmt;
    brackets fmt (fun _ ->
      constant fmt (string_of_int (const_int_expr loc n - 1));
      colon fmt;
      intLit fmt "0");
    nbsp fmt;
    varname fmt v
  | Type_Constructor (tc, []) ->
    tycon fmt tc;
    nbsp fmt;
    varname fmt v
  | Type_Constructor (i, [a]) when Ident.equal i Builtin_idents.ram ->
    PP.fprintf fmt "byte %a[%d]"
      varname v
      (Int.shift_left 1 (const_int_expr loc a))
  (* TODO implement integer range analysis to determine the correct type width. *)
  | Type_Integer _ ->
    kw_integer fmt;
    nbsp fmt;
    varname fmt v
  | Type_Array (Index_Enum tc, ety) ->
    varty loc fmt v ety;
    brackets fmt (fun _ -> tycon fmt tc)
  | Type_Array (Index_Int sz, ety) ->
    varty loc fmt v ety;
    brackets fmt (fun _ -> expr loc fmt sz)
  | Type_OfExpr e ->
      fn_typeof fmt;
      parens fmt (fun _ -> expr loc fmt e)
  | Type_Constructor (_, _)
  | Type_Tuple _ ->
      raise (Error.Unimplemented (loc, "type", fun fmt -> FMTAST.ty fmt x))
  )

(* todo: indices need to be reduced to ceil(log2(width(operand))) *)
and slice (loc : AST.l) (fmt : PP.formatter) (x : AST.slice) : unit =
  match x with
  | Slice_LoWd (lo, wd) ->
      expr loc fmt lo;
      nbsp fmt;
      plus_colon fmt;
      nbsp fmt;
      expr loc fmt wd
  | Slice_Single _ ->
      raise (InternalError (loc, "Slice_Single not expected", (fun _ -> ()), __LOC__))
  | Slice_HiLo _ ->
      raise (InternalError (loc, "Slice_HiLo not expected", (fun _ -> ()), __LOC__))
  | Slice_Element _ ->
      raise (InternalError (loc, "Slice_Element not expected", (fun _ -> ()), __LOC__))

and ixtype (loc : AST.l) (fmt : PP.formatter) (x : AST.ixtype) : unit =
  match x with
  | Index_Enum tc -> tycon fmt tc
  | Index_Int sz -> expr loc fmt sz

and unknown (loc : AST.l) (fmt : PP.formatter) (t : AST.ty) : unit =
  match t with
  | Type_Bits k ->
      expr loc fmt k;
      constant fmt "'X"
  | _ ->
      raise (Error.Unimplemented (loc, "UNKNOWN", fun fmt -> FMTAST.ty fmt t))

and const_expr (loc : AST.l) (x : AST.expr) : V.value =
  match x with
  | Expr_LitInt i -> V.from_intLit i
  | Expr_LitHex i -> V.from_hexLit i
  | Expr_LitReal r -> V.from_realLit r
  | Expr_LitBits b -> V.from_bitsLit b
  | Expr_LitMask b -> V.from_maskLit b
  | Expr_LitString s -> V.from_stringLit s
  | _ ->
    raise (Error.Unimplemented
            (loc,
             "const_expr: not literal constant '" ^ Asl_utils.pp_expr x ^ "'",
             fun fmt -> FMTAST.expr fmt x ))

and const_int_expr (loc : AST.l) (x : AST.expr) : int =
  match const_expr loc x with
  | VInt i -> Z.to_int i
  | _ ->
    raise (Error.Unimplemented
            (loc,
             "const_int_expr: not literal constant '" ^ Asl_utils.pp_expr x ^ "'",
             fun fmt -> FMTAST.expr fmt x ))

and apply (loc : AST.l) (fmt : PP.formatter) (f : unit -> unit) (args : AST.expr list) : unit
    =
  f ();
  parens fmt (fun _ -> exprs loc fmt args)

and cast (fmt : PP.formatter) (f : string) (x : unit -> unit) : unit =
  delimiter fmt (f ^ "'");
  parens fmt x

and zero_extend_bits (loc : AST.l) (fmt : PP.formatter) (target_width : int) (x : AST.expr) :
    unit =
  cast fmt (string_of_int target_width) (fun _ -> expr loc fmt x)

and sign_extend_bits (loc : AST.l) (fmt : PP.formatter) (target_width : int) (x : AST.expr) :
    unit =
  cast fmt (string_of_int target_width) (fun _ ->
      cast fmt "signed" (fun _ -> expr loc fmt x))

(** calculate mask of the form '11111100000' with (intwidth-x) ones and x zeros *)
and notmask_int (loc : AST.l) (fmt : PP.formatter) (x : AST.expr) : unit =
  parens fmt (fun _ ->
      ones fmt !int_width;
      nbsp fmt;
      delimiter fmt "<<";
      nbsp fmt;
      expr loc fmt x)

and binop (loc : AST.l) (fmt : PP.formatter) (op : string) (args : AST.expr list) : unit =
  match args with
  | [ x; y ] ->
      parens fmt (fun _ ->
          expr loc fmt x;
          nbsp fmt;
          delimiter fmt op;
          nbsp fmt;
          expr loc fmt y)
  | _ -> raise (Error.Unimplemented (loc, "binop: " ^ op, fun fmt -> ()))

and unop (loc : AST.l) (fmt : PP.formatter) (op : string) (args : AST.expr list) : unit =
  match args with
  | [ x ] ->
      parens fmt (fun _ ->
          delimiter fmt op;
          nbsp fmt;
          expr loc fmt x)
  | _ -> raise (Error.Unimplemented (loc, "unop: " ^ op, fun fmt -> ()))

and cond_cont (loc : AST.l) (fmt : PP.formatter) (c : AST.expr) (x : AST.expr)
    (y : unit -> unit) : unit =
  (* TODO: confirm that SV only evaluates either x or y, not both *)
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
  (* TODO: confirm that SV only evaluates either x or y, not both *)
  cond_cont loc fmt c x (fun _ -> expr loc fmt y)

and conds (loc : AST.l) (fmt : PP.formatter) (cts : (AST.expr * AST.expr) list) (e : AST.expr)
    : unit =
  match cts with
  | [] -> expr loc fmt e
  | (c, t) :: cts' -> cond_cont loc fmt c t (fun _ -> conds loc fmt cts' e)

and funcall (fmt : PP.formatter) (f : Ident.t) (tes : AST.expr list)
    (args : AST.expr list) (loc : AST.l) =
  match (f, args) with
  (* Boolean builtin functions *)
  | i, _ when Ident.equal i eq_bool -> binop loc fmt "==" args
  | i, _ when Ident.equal i ne_bool -> binop loc fmt "!=" args
  | i, _ when Ident.equal i not_bool -> unop loc fmt "!" args
  | i, [ x; y ] when Ident.equal i and_bool ->
      cond loc fmt x y Asl_utils.asl_false
  | i, [ x; y ] when Ident.equal i or_bool ->
      cond loc fmt x Asl_utils.asl_true y
  | i, _ when Ident.equal i equiv_bool -> binop loc fmt "==" args
  | i, [ x; y ] when Ident.equal i implies_bool ->
      cond loc fmt x y Asl_utils.asl_true
  (* Enumeration builtin functions *)
  (* The reason we need direct checks against eq_enum and ne_enum is because
     the identifier eq_enum with tag 0 doesn't have a root. And we need this
     function identifier in the xform_case transform right now *)
  | i, _ when Ident.equal i eq_enum -> binop loc fmt "==" args
  | i, _ when Ident.equal i ne_enum -> binop loc fmt "!=" args
  | i, _ when Ident.root_equal i ~root:eq_enum -> binop loc fmt "==" args
  | i, _ when Ident.root_equal i ~root:ne_enum -> binop loc fmt "!=" args
  (* Integer builtin functions *)
  | i, _ when Ident.equal i eq_int -> binop loc fmt "==" args
  | i, _ when Ident.equal i ne_int -> binop loc fmt "!=" args
  | i, _ when Ident.equal i gt_int -> binop loc fmt ">" args
  | i, _ when Ident.equal i ge_int -> binop loc fmt ">=" args
  | i, _ when Ident.equal i le_int -> binop loc fmt "<=" args
  | i, _ when Ident.equal i lt_int -> binop loc fmt "<" args
  | i, _ when Ident.equal i is_pow2_int ->
      apply loc fmt (fun _ -> fn_onehot fmt) args
  | i, _ when Ident.equal i add_int -> binop loc fmt "+" args
  | i, _ when Ident.equal i neg_int -> unop loc fmt "-" args
  | i, _ when Ident.equal i sub_int -> binop loc fmt "-" args
  | i, _ when Ident.equal i shl_int -> binop loc fmt "<<" args
  | i, _ when Ident.equal i shr_int -> binop loc fmt ">>" args
  | i, _ when Ident.equal i mul_int -> binop loc fmt "*" args
  | i, _ when Ident.equal i zdiv_int ->
      binop loc fmt "/" args (* todo: check behaviour on -ve values *)
  | i, _ when Ident.equal i zrem_int ->
      binop loc fmt "%" args (* todo: check behaviour on -ve values *)
  (* todo: I am fairly sure that these are incorrect
     | (FIdent ("fdiv_int", _), _) -> binop loc fmt "/" args
     | (FIdent ("frem_int", _), _) -> binop loc fmt "%" args
  *)
  | i, [ x; y ] when Ident.equal i mod_pow2_int ->
      parens fmt (fun _ ->
          expr loc fmt x;
          nbsp fmt;
          delimiter fmt "&";
          nbsp fmt;
          delimiter fmt "~";
          notmask_int loc fmt y)
  | i, [ x; y ] when Ident.equal i align_int ->
      parens fmt (fun _ ->
          expr loc fmt x;
          nbsp fmt;
          delimiter fmt "&";
          nbsp fmt;
          notmask_int loc fmt y)
  | i, [ x ] when Ident.equal i pow2_int ->
      parens fmt (fun _ ->
          intLit fmt "1";
          nbsp fmt;
          delimiter fmt "<<";
          nbsp fmt;
          expr loc fmt x)
  | i, [ x; y ] when Ident.equal i pow_int_int ->
      PP.fprintf fmt "(%a ** %a)"
        (expr loc) x
        (expr loc) y
  (* Real builtin functions *)
  | _ when Ident.in_list f
        [ add_real
        ; cvt_int_real
        ; divide_real
        ; eq_real
        ; ge_real
        ; gt_real
        ; le_real
        ; lt_real
        ; mul_real
        ; ne_real
        ; neg_real
        ; pow2_real
        ; round_down_real
        ; round_tozero_real
        ; round_up_real
        ; sqrt_real
        ; sub_real
        ] ->
      raise
        (Error.Unimplemented
           ( loc,
             "real builtin function",
             fun fmt -> FMTAST.funname fmt f ))
  | i, [ x; n ] when Ident.equal i cvt_int_bits ->
      expr loc fmt x;
      brackets fmt (fun _ ->
          intLit fmt "0";
          plus_colon fmt;
          expr loc fmt n)
  | i, [ x ] when Ident.equal i cvt_bits_sint ->
      sign_extend_bits loc fmt !int_width x
  | i, [ x ] when Ident.equal i cvt_bits_uint ->
      zero_extend_bits loc fmt !int_width x
  | i, _ when Ident.equal i in_mask -> binop loc fmt "=?=" args
  | i, _ when Ident.equal i notin_mask ->
      parens fmt (fun _ ->
          delimiter fmt "!";
          binop loc fmt "=?=" args)
  | i, _ when Ident.equal i eq_bits -> binop loc fmt "==" args
  | i, _ when Ident.equal i ne_bits -> binop loc fmt "!=" args
  | i, _ when Ident.equal i add_bits -> binop loc fmt "+" args
  | i, _ when Ident.equal i sub_bits -> binop loc fmt "-" args
  | i, _ when Ident.equal i mul_bits -> binop loc fmt "*" args
  | i, _ when Ident.equal i frem_bits_int ->
      binop loc fmt "/" args (* todo: check behaviour on -ve values *)
  | i, _ when Ident.equal i and_bits -> binop loc fmt "&" args
  | i, _ when Ident.equal i or_bits -> binop loc fmt "|" args
  | i, _ when Ident.equal i eor_bits -> binop loc fmt "^" args
  | i, _ when Ident.equal i not_bits -> unop loc fmt "~" args
  | i, [_] when Ident.equal i zeros_bits ->
      let x = List.hd tes in
      bitsLit fmt (String.make (const_int_expr loc x) '0')
  | i, [_] when Ident.equal i ones_bits ->
      let x = List.hd tes in
      bitsLit fmt (String.make (const_int_expr loc x) '1')
  | i, _ when Ident.equal i lsl_bits -> binop loc fmt "<<" args
  | i, _ when Ident.equal i lsr_bits -> binop loc fmt ">>" args
  | i, _ when Ident.equal i asr_bits -> binop loc fmt ">>>" args
  | i, [ w; _ ] when Ident.equal i mk_mask ->
      let n = List.hd tes in
      PP.fprintf fmt "(";
      bitsLit fmt (String.make (const_int_expr loc n) '1');
      PP.fprintf fmt " >> (%a - %a))"
        (expr loc) n
        (expr loc) w
  | i, [ x; y ] when Ident.equal i replicate_bits ->
      braces fmt (fun _ ->
          valueLit loc fmt (const_expr loc y);
          braces fmt (fun _ -> expr loc fmt x))
  | i, [ x; y ] when Ident.equal i append_bits ->
      PP.fprintf fmt "{ %a, %a }"
        (expr loc) x
        (expr loc) y
  | i, [ x; n ] when Ident.equal i Builtin_idents.zero_extend_bits ->
      zero_extend_bits loc fmt (const_int_expr loc n) x
  | _ when Ident.in_list f
        [ append_str_str
        ; cvt_bits_str
        ; cvt_bool_str
        ; cvt_int_decstr
        ; cvt_int_hexstr
        ; cvt_real_str
        ; eq_str
        ; ne_str
        ] ->
      raise
        (Error.Unimplemented
           (loc, "string builtin function", fun fmt -> FMTAST.funname fmt f))
  | i, _ when Ident.equal i print_str ->
      apply loc fmt (fun _ -> fn_writes fmt) args
  | i, _ when Ident.equal i print_int_dec ->
      apply loc fmt (fun _ -> fn_writed fmt) args
  | i, _ when Ident.equal i print_int_hex ->
      apply loc fmt (fun _ -> fn_writeh fmt) args
  | i, _ when Ident.equal i print_bits_hex ->
      apply loc fmt (fun _ -> fn_writeh fmt) args
  | i, [ c ] when Ident.equal i print_char ->
      fn_writes fmt;
      parens fmt (fun _ ->
          constant fmt "\"%c\"";
          comma fmt;
          expr loc fmt c)
  | i, [ a; n; ram; v ] when Ident.equal i ram_init -> ()
  | i, [ a; n; ram; addr ] when Ident.equal i ram_read ->
    PP.fprintf fmt "(%a[%a])"
      (expr loc) ram
      (expr loc) addr
  | i, [ a; n; ram; addr; v ] when Ident.equal i ram_write ->
    PP.fprintf fmt "%a[%a] = %a"
      (expr loc) ram
      (expr loc) addr
      (expr loc) v
  | _ -> apply loc fmt (fun _ -> funname fmt f) args

and expr (loc : AST.l) (fmt : PP.formatter) (x : AST.expr) : unit =
  match x with
  | Expr_If (c, t, els, e) ->
      let els1 = List.map (function AST.E_Elsif_Cond (c, e) -> (c, e)) els in
      conds loc fmt ((c, t) :: els1) e
  | Expr_Field (e, f) ->
      expr loc fmt e;
      dot fmt;
      fieldname fmt f
  (* two special cases of slices are supported *)
  | Expr_Slices (_, e, [ s ]) ->
      expr loc fmt e;
      brackets fmt (fun _ -> slice loc fmt s)
  | Expr_Slices (_, (Expr_Var v as e), ss) ->
      braces fmt (fun _ ->
          commasep fmt
            (fun s ->
              expr loc fmt e;
              brackets fmt (fun _ -> slice loc fmt s))
            ss)
  | Expr_Var v -> (
      match v with
      | v when Ident.equal v true_ident -> constant fmt kw_true
      | v when Ident.equal v false_ident -> constant fmt kw_false
      | _ -> varname fmt v)
  | Expr_Parens e -> expr loc fmt e
  | Expr_TApply (f, tes, es, false) -> funcall fmt f tes es loc
  | Expr_Concat (_, es) -> braces fmt (fun _ -> exprs loc fmt es)
  | Expr_Unknown t -> unknown loc fmt t
  | Expr_LitInt l -> intLit fmt l
  | Expr_LitHex l -> hexLit fmt l
  | Expr_LitReal l -> realLit fmt l
  | Expr_LitBits l -> bitsLit fmt l
  | Expr_LitMask l -> maskLit fmt l
  | Expr_LitString l -> strLit fmt l
  | Expr_Array (a, i) ->
      expr loc fmt a;
      brackets fmt (fun _ -> expr loc fmt i)
  | Expr_RecordInit (tc, [], fas) ->
      if List.mem tc !exception_tcs then begin
        PP.fprintf fmt "'{ _%a: '{ ASL_tag: tag_%a" tycon tc tycon tc;
        List.iter (fun (f, e) -> PP.fprintf fmt ", %a: %a" varname f (expr loc) e) fas;
        PP.fprintf fmt " }}"
      end else begin
        delimiter fmt "'";
        braces fmt (fun _ ->
          commasep fmt
            (fun (f, e) ->
              varname fmt f;
              colon fmt;
              nbsp fmt;
              expr loc fmt e)
            fas)
      end
  | Expr_ArrayInit es ->
      PP.fprintf fmt "{ %a }"
        (exprs loc) es
  | Expr_In (e, Pat_LitMask x) ->
      PP.fprintf fmt "(%a ==? %a)"
        (expr loc) e
        bitsLit x
  | Expr_AsConstraint (e, _)
  | Expr_AsType (e, _) ->
      expr loc fmt e
  (* unimplemented *)
  | Expr_Slices _ | Expr_In _ | Expr_Binop _ | Expr_Unop _
  | Expr_RecordInit _ | Expr_ImpDef (_, _)
  | Expr_TApply _
  | Expr_Let _
  | Expr_Tuple _ | Expr_Fields _ ->
      raise
        (Error.Unimplemented (loc, "expression", fun fmt -> FMTAST.expr fmt x))

and exprs (loc : AST.l) (fmt : PP.formatter) (es : AST.expr list) : unit =
  commasep fmt (expr loc fmt) es

and pattern (loc : AST.l) (fmt : PP.formatter) (x : AST.pattern) : unit =
  match x with
  | Pat_LitInt l -> intLit fmt l
  | Pat_LitHex l -> hexLit fmt l
  | Pat_LitBits l -> bitsLit fmt l
  | Pat_LitMask l -> maskLit fmt l
  | Pat_Const v -> varname fmt v
  | Pat_Wildcard -> delimiter fmt ".*"
  | Pat_Single e -> expr loc fmt e
  | Pat_Range (lo, hi) ->
      expr loc fmt lo;
      nbsp fmt;
      dot_dot fmt;
      nbsp fmt;
      expr loc fmt hi
  (* unimplemented *)
  | Pat_Set _
  | Pat_Tuple _ ->
      raise
        (Error.Unimplemented (loc, "pattern", fun fmt -> FMTAST.pattern fmt x))

let assign (loc : AST.l) (fmt : PP.formatter) (l : unit -> unit) (r : AST.expr) : unit =
  l ();
  nbsp fmt;
  eq fmt;
  nbsp fmt;
  expr loc fmt r;
  semicolon fmt

let rec lexpr (loc : AST.l) (fmt : PP.formatter) (x : AST.lexpr) : unit =
  ( match x with
  | LExpr_Var v -> varname fmt v
  (* one special case of Field - easy to generalise this one *)
  | LExpr_Field (l, f) ->
    lexpr loc fmt l;
    dot fmt;
    varname fmt f
  (* one special case of slices are supported *)
  | LExpr_Slices (_, LExpr_Var v, ss) ->
    braces fmt (fun _ ->
      commasep fmt
        (fun s ->
          varname fmt v;
          brackets fmt (fun _ -> slice loc fmt s))
        ss)
  | LExpr_BitTuple(ws, es) ->
    braces fmt (fun _ -> lexprs loc fmt es)
  | LExpr_Array (a, i) ->
    lexpr loc fmt a;
    brackets fmt (fun _ -> expr loc fmt i)
  (* unimplemented *)
  | LExpr_Wildcard | LExpr_Fields _ | LExpr_Slices _
  | LExpr_Tuple _
  | LExpr_Write _
  | LExpr_ReadWrite _ ->
      raise
        (Error.Unimplemented
           (loc, "l-expression", fun fmt -> FMTAST.lexpr fmt x))
  )

and lexprs (loc : AST.l) (fmt: PP.formatter) (ps: AST.lexpr list): unit =
  commasep fmt (lexpr loc fmt) ps

let lexpr_assign (loc : AST.l) (fmt : PP.formatter) (x : AST.lexpr) (r : AST.expr): unit =
  ( match x with
  | LExpr_Wildcard ->
    expr loc fmt r;
    semicolon fmt
  | _ ->
    assign loc fmt (fun _ -> lexpr loc fmt x) r
  )

let rec declitem (loc : AST.l) (fmt : PP.formatter) (x : AST.decl_item) =
  match x with
  | DeclItem_Var (v, Some t) ->
      varty loc fmt v t;
      semicolon fmt
  | DeclItem_Tuple dis -> cutsep fmt (declitem loc fmt) dis
  | DeclItem_BitTuple dbs ->
      let pp fmt = FMTAST.decl_item fmt x in
      raise (Error.Unimplemented (loc, "declitem: bittuple", pp))
  | DeclItem_Var (v, None) ->
      raise
        (Error.Unimplemented
           ( loc,
             "decl: type of variable unknown",
             fun fmt -> FMTAST.varname fmt v ))
  | DeclItem_Wildcard _ ->
      raise
        (Error.Unimplemented (loc, "decl: use of wildcard", fun fmt -> ()))

let decl (fmt : PP.formatter) (x : AST.stmt) : unit =
  match x with
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      List.iter (fun v ->
          varty loc fmt v t;
          semicolon fmt;
          cut fmt
      )
      vs
  | Stmt_VarDecl (di, i, loc)
  | Stmt_ConstDecl (di, i, loc) ->
      declitem loc fmt di;
      cut fmt
  | _ -> ()

let stmt_line_info (fmt : PP.formatter) (x : AST.stmt) : unit =
  if !include_line_info then
    ( match Asl_utils.stmt_loc x with
    | Range (p, _) ->
        let fname = p.Lexing.pos_fname in
        let line = p.Lexing.pos_lnum in
        PP.fprintf fmt "`line %d \"%s\" 0@," line fname
    | _ -> ()
    )

let rec stmt (fmt : PP.formatter) (x : AST.stmt) : unit =
  stmt_line_info fmt x;

  match x with
  | Stmt_ConstDecl (DeclItem_Var (v, _), Expr_TApply (f, tes, es, true), loc) ->
      (* special case for functions that throw exceptions *)
      PP.fprintf fmt "%a = %a;"
        varname v
        (fun fmt _ -> funcall fmt f tes es loc) ();
      rethrow_stmt loc fmt
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      (* handled by decl *)
      ()
  | Stmt_VarDecl (DeclItem_Var (v, _), i, loc)
  | Stmt_ConstDecl (DeclItem_Var (v, _), i, loc) ->
      assign loc fmt (fun _ -> varname fmt v) i
  | Stmt_Assign (l, r, loc) -> lexpr_assign loc fmt l r
  | Stmt_TCall (f, tes, args, throws, loc) ->
      funcall fmt f tes args loc;
      semicolon fmt;
      if (throws) then rethrow_stmt loc fmt
  | Stmt_FunReturn (e, loc) ->
      kw_return fmt;
      nbsp fmt;
      expr loc fmt e;
      semicolon fmt
  | Stmt_ProcReturn loc ->
      kw_return fmt;
      semicolon fmt
  | Stmt_Assert (e, loc) ->
      (* todo: not clear that this is the best translation *)
      kw_assert fmt;
      parens fmt (fun _ -> expr loc fmt e);
      semicolon fmt
  | Stmt_Block (ss, _) ->
      kw_begin fmt;
      indented_block fmt ss;
      cut fmt;
      kw_end fmt
  | Stmt_If (c, t, els, (e, el), loc) ->
      vbox fmt (fun _ ->
          kw_priority fmt;
          nbsp fmt;
          kw_if fmt;
          nbsp fmt;
          parens fmt (fun _ -> expr loc fmt c);
          nbsp fmt;
          kw_begin fmt;
          indented_block fmt t;
          map fmt
            (fun (AST.S_Elsif_Cond (c, s, loc)) ->
              cut fmt;
              kw_end fmt;
              nbsp fmt;
              kw_else fmt;
              nbsp fmt;
              kw_if fmt;
              nbsp fmt;
              parens fmt (fun _ -> expr loc fmt c);
              nbsp fmt;
              kw_begin fmt;
              indented_block fmt s)
            els;
          if e <> [] then (
            cut fmt;
            kw_end fmt;
            nbsp fmt;
            kw_else fmt;
            nbsp fmt;
            kw_begin fmt;
            indented_block fmt e);
          cut fmt;
          kw_end fmt)
  | Stmt_Case (e, alts, ob, loc) ->
      vbox fmt (fun _ ->
          kw_casez fmt;
          nbsp fmt;
          parens fmt (fun _ -> expr loc fmt e);
          indented fmt (fun _ ->
              cutsep fmt
                (fun (AST.Alt_Alt (ps, oc, ss, loc)) ->
                  if Option.is_some oc then
                    raise (Error.Unimplemented (loc, "pattern_guard", fun fmt -> ()));
                  commasep fmt (pattern loc fmt) ps;
                  colon fmt;
                  nbsp fmt;
                  kw_begin fmt;
                  indented_block fmt ss;
                  cut fmt;
                  kw_end fmt)
                alts;
              PP.pp_print_option
                (fun _ (b, bl) ->
                  cut fmt;
                  kw_default fmt;
                  colon fmt;
                  nbsp fmt;
                  kw_begin fmt;
                  indented_block fmt b;
                  cut fmt;
                  kw_end fmt)
                fmt ob);
          cut fmt;
          kw_endcase fmt)
  | Stmt_Throw (e, loc) ->
      PP.fprintf fmt "ASL_exception = %a;@," (expr loc) e;
      ( match !current_function with
      | None ->
          raise (Error.Unimplemented (loc, "exception throw outside a function", fun fmt -> ()))
      | Some (f, None) ->
          PP.fprintf fmt
            "return;"
      | Some (f, oty) ->
          PP.fprintf fmt
            "return %a;"
            varname f
      )

  (* unimplemented *)
  | Stmt_VarDecl (_, _, loc)
  | Stmt_ConstDecl (_, _, loc)
  | Stmt_For (_, _, _, _, _, loc)
  | Stmt_While (_, _, loc)
  | Stmt_Repeat (_, _, _, loc)
  | Stmt_Try (_, _, _, _, loc) ->
      raise
        (Error.Unimplemented (loc, "statement", fun fmt -> FMTAST.stmt fmt x))

and indented_block (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if xs <> [] then begin
    indented fmt (fun _ ->
      map fmt (decl fmt) xs;
      cutsep fmt (stmt fmt) xs)
  end

let formal (loc : AST.l) (fmt : PP.formatter) (x : Ident.t * AST.ty) : unit =
  let (v, t) = x in
  kw_input fmt;
  nbsp fmt;
  varty loc fmt v t

let formals (loc : AST.l) (fmt : PP.formatter) (xs : (Ident.t * AST.ty) list) : unit =
  commasep fmt (formal loc fmt) xs

let function_header (loc : AST.l) (fmt : PP.formatter) (ot : AST.ty option) (f : Ident.t)
    (args : unit -> unit) : unit =
  kw_function fmt;
  nbsp fmt;
  kw_automatic fmt;
  nbsp fmt;
  PP.pp_print_option
    ~none:(fun _ _ -> kw_void fmt; nbsp fmt; varname fmt f)
    (fun _ t -> varty loc fmt f t) fmt ot;
  parens fmt args;
  semicolon fmt

let typedef (fmt : PP.formatter) (pp : unit -> unit) : unit =
  kw_typedef fmt;
  nbsp fmt;
  pp ();
  semicolon fmt;
  cut fmt

let declaration (fmt : PP.formatter) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      match x with
      | Decl_BuiltinType (tc, loc) when Ident.equal tc string_ident ->
        comment fmt "typedef string string;"
      | Decl_BuiltinType (tc, loc) -> (
              raise
                (Error.Unimplemented
                   (AST.Unknown, "builtin type", fun fmt -> FMTAST.tycon fmt tc))
          )
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
            tycon fmt tc
          );
          cut fmt
      | Decl_Typedef (tc, [], t, loc) ->
          typedef fmt (fun _ -> varty loc fmt tc t);
          cut fmt
      | Decl_Enum (tc, es, loc) ->
          if tc = boolean_ident then ()
          else (
            typedef fmt (fun _ ->
                kw_enum fmt;
                nbsp fmt;
                braces fmt (fun _ -> commasep fmt (varname fmt) es);
                nbsp fmt;
                tycon fmt tc
              );
            cut fmt)
      | Decl_FunType (f, ps, args, t, loc) ->
          ()
      | Decl_FunDefn (f, ps, args, t, b, loc) ->
          current_function := Some (f, Some t);
          function_header loc fmt (Some t) f (fun _ -> formals loc fmt args);
          indented_block fmt b;
          cut fmt;
          kw_endfunction fmt;
          nbsp fmt;
          colon fmt;
          nbsp fmt;
          funname fmt f;
          current_function := None;
          cut fmt;
          cut fmt
      | Decl_ProcType (f, ps, args, loc) ->
          ()
      | Decl_ProcDefn (f, ps, args, b, loc) ->
          current_function := Some (f, None);
          function_header loc fmt None f (fun _ -> formals loc fmt args);
          indented_block fmt b;
          cut fmt;
          kw_endfunction fmt;
          nbsp fmt;
          colon fmt;
          nbsp fmt;
          funname fmt f;
          current_function := None;
          cut fmt;
          cut fmt
      | Decl_Var (v, ty, loc) ->
          varty loc fmt v ty;
          semicolon fmt;
          cut fmt;
          cut fmt
      | Decl_ArrayGetterDefn (f, ps, args, t, b, loc) ->
          current_function := Some (f, Some t);
          function_header loc fmt (Some t) f (fun _ -> formals loc fmt args);
          indented_block fmt b;
          cut fmt;
          kw_endfunction fmt;
          nbsp fmt;
          colon fmt;
          nbsp fmt;
          funname fmt f;
          current_function := None;
          cut fmt;
          cut fmt
      | Decl_ArraySetterDefn (f, ps, args, v, t, b, loc) ->
          current_function := Some (f, None);
          function_header loc fmt None f (fun _ -> formals loc fmt (args @ [ (v, t) ]));
          indented_block fmt b;
          cut fmt;
          kw_endfunction fmt;
          nbsp fmt;
          colon fmt;
          nbsp fmt;
          funname fmt f;
          current_function := None;
          cut fmt;
          cut fmt
      (* ignored *)
      | Decl_BuiltinFunction (f, ps, args, t, loc) -> ()
      | Decl_Operator1 (op, fs, loc) -> ()
      | Decl_Operator2 (op, fs, loc) -> ()
      | _ ->
          raise
            (Error.Unimplemented
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
    PP.fprintf fmt "typedef enum { ASL_no_exception %a } ASL_exception_tag_t;@,@,"
      (fun fmt -> cutsep fmt (fun (tc, _, _) -> PP.fprintf fmt ", tag_%a" tycon tc)) excs;
    List.iter (fun (tc, fs, loc) ->
      typedef fmt (fun _ ->
          kw_struct fmt;
          nbsp fmt;
          braces fmt (fun _ ->
              indented fmt (fun _ ->
                  PP.fprintf fmt "ASL_exception_tag_t ASL_tag;@,";
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
    PP.fprintf fmt "typedef union {\n    %s\n%a\n} ASL_exception_t;@,"
      "struct { ASL_exception_tag_t ASL_tag; } _exc;"
      (PP.pp_print_list
         (fun fmt (tc, _, _) -> PP.fprintf fmt "    %a _%a;"
             tycon tc
             tycon tc))
      excs
  )

let exceptions_init (fmt : PP.formatter) : unit =
  vbox fmt (fun _ ->
    PP.fprintf fmt "ASL_exception_t ASL_exception = '{ _exc: '{ ASL_tag: ASL_no_exception } };@,"
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
    | Decl_ProcType _
      -> Some x

    (* Add Fun/Proc-Type declarations for any functions that have been created
     * after typechecking such as functions created during monomorphization.
     * Since the typechecker creates Fun/Proc-Type declarations for all functions
     * in the original spec, this will result in duplicate function prototypes
     * for many functions.
     *)
    | Decl_FunDefn (f, ps, args, t, _, loc) -> Some (Decl_FunType (f, ps, args, t, loc))
    | Decl_ProcDefn (f, ps, args, _, loc) -> Some (Decl_ProcType (f, ps, args, loc))

    | Decl_Const _
    | Decl_Exception _
    | Decl_Var _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_VarGetterType _
    | Decl_VarGetterDefn _
    | Decl_ArrayGetterType _
    | Decl_ArrayGetterDefn _
    | Decl_VarSetterType _
    | Decl_VarSetterDefn _
    | Decl_ArraySetterType _
    | Decl_ArraySetterDefn _
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
    | Decl_ProcType _
    | Decl_FunDefn _
    | Decl_ProcDefn _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_VarGetterType _
    | Decl_VarGetterDefn _
    | Decl_ArrayGetterType _
    | Decl_ArrayGetterDefn _
    | Decl_VarSetterType _
    | Decl_VarSetterDefn _
    | Decl_ArraySetterType _
    | Decl_ArraySetterDefn _
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
    | Decl_ProcDefn _
      -> true

    | Decl_Var _
    | Decl_Const _
    | Decl_Enum _
    | Decl_Record _
    | Decl_Exception _
    | Decl_Typedef _
    | Decl_FunType _
    | Decl_ProcType _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_VarGetterType _
    | Decl_VarGetterDefn _
    | Decl_ArrayGetterType _
    | Decl_ArrayGetterDefn _
    | Decl_VarSetterType _
    | Decl_VarSetterDefn _
    | Decl_ArraySetterType _
    | Decl_ArraySetterDefn _
    | Decl_Operator1 _
    | Decl_Operator2 _
    | Decl_Config _
      -> false
    )
  in
  List.filter is_fun_decl xs

let generate_files (dirname : string) (basename : string) (ds : AST.declaration list) : unit =
  let basename = Filename.concat dirname basename in
  Utils.to_file (basename ^ "_types.svh") (fun fmt ->
      Format.fprintf fmt "typedef bit signed[%d : 0] asl_integer;@," (!int_width - 1);
      Format.fprintf fmt "typedef bit asl_boolean;@,";

      type_decls ds |> Asl_utils.topological_sort |> List.rev |> declarations fmt;

      exceptions fmt ds
  );

  Utils.to_file (basename ^ "_vars.svh") (fun fmt ->
      var_decls ds |> declarations fmt
  );

  Utils.to_file (basename ^ "_funs.sv") (fun fmt ->
      fun_decls ds |> declarations fmt;

      exceptions_init fmt
  )

(****************************************************************
 * Command: :generate_verilog
 ****************************************************************)

let _ =
  let opt_dirname = ref "" in
  let opt_basename : string ref = ref "asl2v" in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    generate_files !opt_dirname !opt_basename !Commands.declarations;
    true
  in
  let flags = Arg.align [
        ("--output-dir",   Arg.Set_string opt_dirname,  "<dirname>  Directory for output files");
        ("--basename",     Arg.Set_string opt_basename, "<basename> Basename of output files");
        ("--intwidth",     Arg.Set_int    int_width,    "<num>      Maximum integer width");
        ("--line-info",    Arg.Set include_line_info,   " Insert line number information");
        ("--no-line-info", Arg.Clear include_line_info, " Do not insert line number information");
      ]
  in
  Commands.registerCommand "generate_verilog" flags [] [] "Generate SystemVerilog" cmd

(****************************************************************
 * End
 ****************************************************************)
