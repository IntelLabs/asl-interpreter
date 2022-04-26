(****************************************************************
 * ASL to Verilog backend
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to Verilog backend *)

module PP = Format
module AST = Asl_ast
module FMTAST = Asl_fmt
module V = Value
open Format_utils

exception Unimplemented of (AST.l * string * (Format.formatter -> unit))

let mangle (s: string): string =
    (* todo: this should detect whether s is a reserved name in System Verilog and rename to avoid conflict *)
    s

let comment (fmt: PP.formatter) (x: string): unit =
    PP.pp_print_string fmt ("// "^ x);
    cut fmt

let delimiter (fmt: PP.formatter) (s: string): unit = PP.pp_print_string fmt s
let keyword (fmt: PP.formatter) (s: string): unit = PP.pp_print_string fmt s
let constant (fmt: PP.formatter) (s: string): unit = PP.pp_print_string fmt s

let ident_str (fmt: PP.formatter) (x: string): unit = PP.pp_print_string fmt x
let ident     (fmt: PP.formatter) (x: AST.ident): unit =
  (match x with
  | Ident s -> ident_str fmt (mangle s)
  | FIdent (s, t) -> ident_str fmt (s ^ "_" ^ string_of_int t)
  )
let tycon     (fmt: PP.formatter) (x: AST.ident): unit = ident fmt x
let funname   (fmt: PP.formatter) (x: AST.ident): unit = ident fmt x
let varname   (fmt: PP.formatter) (x: AST.ident): unit = ident fmt x
let fieldname (fmt: PP.formatter) (x: AST.ident): unit = ident fmt x

(* integer representation is a signed value of some width
 * this width should be large enough that data does not overflow
 *)
let int_width = 66

(* All the Verilog delimiters *)

let amp_amp            (fmt: PP.formatter): unit = delimiter fmt "&&"
let bang               (fmt: PP.formatter): unit = delimiter fmt "!"
let bang_eq            (fmt: PP.formatter): unit = delimiter fmt "!="
let bar_bar            (fmt: PP.formatter): unit = delimiter fmt "||"
let caret              (fmt: PP.formatter): unit = delimiter fmt "^"
let colon              (fmt: PP.formatter): unit = delimiter fmt ":"
let comma              (fmt: PP.formatter): unit = delimiter fmt ","
let dot                (fmt: PP.formatter): unit = delimiter fmt "."
let dot_dot            (fmt: PP.formatter): unit = delimiter fmt ".."
let eq                 (fmt: PP.formatter): unit = delimiter fmt "="
let eq_eq              (fmt: PP.formatter): unit = delimiter fmt "=="
let eq_gt              (fmt: PP.formatter): unit = delimiter fmt "=>"
let gt                 (fmt: PP.formatter): unit = delimiter fmt ">"
let gt_eq              (fmt: PP.formatter): unit = delimiter fmt ">="
let gt_gt              (fmt: PP.formatter): unit = delimiter fmt ">>"
let lbrace             (fmt: PP.formatter): unit = delimiter fmt "{"
let lbrack             (fmt: PP.formatter): unit = delimiter fmt "["
let lparen             (fmt: PP.formatter): unit = delimiter fmt "("
let lt                 (fmt: PP.formatter): unit = delimiter fmt "<"
let lt_eq              (fmt: PP.formatter): unit = delimiter fmt "<="
let lt_lt              (fmt: PP.formatter): unit = delimiter fmt "<<"
let minus              (fmt: PP.formatter): unit = delimiter fmt "-"
let plus               (fmt: PP.formatter): unit = delimiter fmt "+"
let plus_colon         (fmt: PP.formatter): unit = delimiter fmt "+:"
let plus_plus          (fmt: PP.formatter): unit = delimiter fmt "++"
let rbrace             (fmt: PP.formatter): unit = delimiter fmt "}"
let rbrack             (fmt: PP.formatter): unit = delimiter fmt "]"
let rparen             (fmt: PP.formatter): unit = delimiter fmt ")"
let semicolon          (fmt: PP.formatter): unit = delimiter fmt ";"
let slash              (fmt: PP.formatter): unit = delimiter fmt "/"
let star               (fmt: PP.formatter): unit = delimiter fmt "*"

(* Verilog keywords *)

let kw_array                (fmt: PP.formatter): unit = keyword fmt "array"
let kw_assert               (fmt: PP.formatter): unit = keyword fmt "assert"
let kw_assume               (fmt: PP.formatter): unit = keyword fmt "assume"
let kw_automatic            (fmt: PP.formatter): unit = keyword fmt "automatic"
let kw_begin                (fmt: PP.formatter): unit = keyword fmt "begin"
let kw_bit                  (fmt: PP.formatter): unit = keyword fmt "bit"
let kw_break                (fmt: PP.formatter): unit = keyword fmt "break"
let kw_case                 (fmt: PP.formatter): unit = keyword fmt "case"
let kw_casez                (fmt: PP.formatter): unit = keyword fmt "casez"
let kw_const                (fmt: PP.formatter): unit = keyword fmt "const"
let kw_constant             (fmt: PP.formatter): unit = keyword fmt "constant"
let kw_continue             (fmt: PP.formatter): unit = keyword fmt "continue"
let kw_default              (fmt: PP.formatter): unit = keyword fmt "default"
let kw_do                   (fmt: PP.formatter): unit = keyword fmt "do"
let kw_else                 (fmt: PP.formatter): unit = keyword fmt "else"
let kw_end                  (fmt: PP.formatter): unit = keyword fmt "end"
let kw_endcase              (fmt: PP.formatter): unit = keyword fmt "endcase"
let kw_endfunction          (fmt: PP.formatter): unit = keyword fmt "endfunction"
let kw_enum                 (fmt: PP.formatter): unit = keyword fmt "enum"
let kw_function             (fmt: PP.formatter): unit = keyword fmt "function"
let kw_if                   (fmt: PP.formatter): unit = keyword fmt "if"
let kw_iff                  (fmt: PP.formatter): unit = keyword fmt "iff"
let kw_implies              (fmt: PP.formatter): unit = keyword fmt "implies"
let kw_integer              (fmt: PP.formatter): unit = keyword fmt "asl_integer"
let kw_input                (fmt: PP.formatter): unit = keyword fmt "input"
let kw_inout                (fmt: PP.formatter): unit = keyword fmt "inout"
let kw_inside               (fmt: PP.formatter): unit = keyword fmt "inside"
let kw_int                  (fmt: PP.formatter): unit = keyword fmt "int"
let kw_let                  (fmt: PP.formatter): unit = keyword fmt "let"
let kw_logic                (fmt: PP.formatter): unit = keyword fmt "logic"
let kw_packed               (fmt: PP.formatter): unit = keyword fmt "packed"
let kw_priority             (fmt: PP.formatter): unit = keyword fmt "priority"
let kw_record               (fmt: PP.formatter): unit = keyword fmt "record"
let kw_return               (fmt: PP.formatter): unit = keyword fmt "return"
let kw_signed               (fmt: PP.formatter): unit = keyword fmt "signed"
let kw_static               (fmt: PP.formatter): unit = keyword fmt "static"
let kw_string               (fmt: PP.formatter): unit = keyword fmt "string"
let kw_type                 (fmt: PP.formatter): unit = keyword fmt "type"
let kw_typedef              (fmt: PP.formatter): unit = keyword fmt "typedef"
let kw_typeof               (fmt: PP.formatter): unit = keyword fmt "typeof"
let kw_union                (fmt: PP.formatter): unit = keyword fmt "union"
let kw_unsigned             (fmt: PP.formatter): unit = keyword fmt "unsigned"
let kw_var                  (fmt: PP.formatter): unit = keyword fmt "var"
let kw_void                 (fmt: PP.formatter): unit = keyword fmt "void"
let kw_with                 (fmt: PP.formatter): unit = keyword fmt "with"

(* pseudo-keywords *)
let kw_false = "1'b0"
let kw_true = "1'b1"

(* Verilog system functions *)

let fn_write                (fmt: PP.formatter): unit = keyword fmt "$write"
let fn_typeof               (fmt: PP.formatter): unit = keyword fmt "$typeof"
let fn_onehot               (fmt: PP.formatter): unit = keyword fmt "$onehot"

(* ASL symbols : todo - this is a hacky, unreliable way of referring to them *)
let asl_false = AST.Expr_Var (Ident "FALSE")
let asl_true  = AST.Expr_Var (Ident "TRUE")

let braces   (fmt: PP.formatter) (pp: unit -> unit) = surround fmt lbrace rbrace pp
let parens   (fmt: PP.formatter) (pp: unit -> unit) = surround fmt lparen rparen pp
let brackets (fmt: PP.formatter) (pp: unit -> unit) = surround fmt lbrack rbrack pp

let commasep (fmt: PP.formatter) (pp: 'a -> unit) (xs: 'a list): unit = sepby fmt (fun _ -> comma fmt; nbsp fmt) pp xs

let varnames  (fmt: PP.formatter) (xs: AST.ident list): unit = commasep fmt (varname fmt) xs

let intLit  (fmt: PP.formatter) (x: AST.intLit): unit = constant fmt x
let hexLit  (fmt: PP.formatter) (x: AST.hexLit): unit = constant fmt (string_of_int int_width ^ "'sh" ^ x)
let bitsLit (fmt: PP.formatter) (x: AST.bitsLit): unit =
    let bits = Value.drop_chars x ' ' in (* todo: better to just change spaces to _ *)
    let width = String.length bits in
    constant fmt (string_of_int width ^ "'b" ^ bits)
let maskLit (fmt: PP.formatter) (x: AST.maskLit): unit =
    let bits = Value.drop_chars x ' ' in (* todo: better to just change spaces to _ *)
    let bits = String.map (fun c -> if c = 'x' then '?' else c) bits in
    let width = String.length bits in
    constant fmt (string_of_int width ^ "'b" ^ bits)
let realLit (fmt: PP.formatter) (x: AST.realLit): unit = constant fmt x
let strLit  (fmt: PP.formatter) (x: string): unit = constant fmt ("\"" ^ x ^ "\"")

let valueLit (fmt: PP.formatter) (x: V.value): unit =
    (match x with
    | VBool   b       -> constant fmt (if b then kw_true else kw_false)
    | VEnum   (e, _)  -> ident fmt e
    | VInt    i       -> intLit fmt (Z.to_string i)
    | VBits   b       -> let s = Z.format "%0b" b.v in
                         let pad = String.make (b.n - String.length s) '0' in
                         bitsLit fmt (pad ^ s)
    | VString s       -> strLit fmt s
    | _ -> raise (Unimplemented(AST.Unknown, "constant value type", (fun fmt -> ())))
    )

let ones (fmt: PP.formatter) (x: int): unit =
    braces fmt (fun _ -> intLit fmt (string_of_int x); braces fmt (fun _ -> bitsLit fmt "1"))

let rec ty (fmt: PP.formatter) (x: AST.ty): unit =
  ( match x with
  | Type_Constructor(tc) -> tycon fmt tc
  | Type_Integer _ -> kw_integer fmt
  | Type_Bits(n) -> kw_bit fmt; nbsp fmt; brackets fmt (fun _ -> constant fmt (string_of_int (const_int_expr n - 1)); colon fmt; intLit fmt "0")
  | Type_OfExpr(e) -> fn_typeof fmt; parens fmt (fun _ -> expr fmt e)
  | Type_Register (n, _) -> kw_bit fmt; nbsp fmt; brackets fmt (fun _ -> constant fmt (n^"-1"); colon fmt; intLit fmt "0")

  (* unimplemented *)
  | Type_App (_, _)
  | Type_Array (_, _)
  | Type_Tuple _
  -> raise (Unimplemented(AST.Unknown, "type", (fun fmt -> FMTAST.ty fmt x)))
  )

(* todo: indices need to be reduced to ceil(log2(width(operand))) *)
and slice (fmt: PP.formatter) (x: AST.slice): unit =
  (match x with
  | Slice_Single(e) -> expr fmt e
  | Slice_HiLo(hi, lo) -> expr fmt hi; nbsp fmt; colon fmt; nbsp fmt; expr fmt lo
  | Slice_LoWd(lo, wd) -> expr fmt lo; nbsp fmt; plus_colon fmt; nbsp fmt; expr fmt wd
  )

and ixtype (fmt: PP.formatter) (x: AST.ixtype): unit =
  (match x with
  | Index_Enum(tc) -> tycon fmt tc
  | Index_Range(lo, hi) -> expr fmt lo; nbsp fmt; dot_dot fmt; nbsp fmt; expr fmt hi
  )

and unknown (fmt: PP.formatter) (t: AST.ty): unit =
  (match t with
  | Type_Bits(k) -> expr fmt k; constant fmt "'X"
  | _ -> raise (Unimplemented(AST.Unknown, "UNKNOWN", (fun fmt -> FMTAST.ty fmt t)))
  )

and const_expr (x: AST.expr): V.value =
    (match x with
    | Expr_LitInt(i) -> V.from_intLit i
    | Expr_LitHex(i) -> V.from_hexLit i
    | Expr_LitReal(r) -> V.from_realLit r
    | Expr_LitBits(b) -> V.from_bitsLit b
    | Expr_LitMask(b) -> V.from_maskLit b
    | Expr_LitString(s) -> V.from_stringLit s
    | _ -> failwith ("const_expr: not literal constant '" ^ Asl_utils.pp_expr x ^ "'")
    )

and const_int_expr (x: AST.expr): int =
    (match const_expr x with
    | VInt i -> Z.to_int i
    | _ -> failwith "const_int_expr: integer expected"
    )

and apply (fmt: PP.formatter) (f: unit -> unit) (args: AST.expr list): unit =
    f (); parens fmt (fun _ -> exprs fmt args)

and cast (fmt: PP.formatter) (f: string) (x: unit -> unit): unit =
    delimiter fmt (f ^ "'"); parens fmt x

and zero_extend_bits (fmt: PP.formatter) (target_width: int) (x: AST.expr): unit =
    cast fmt (string_of_int target_width) (fun _ -> expr fmt x)

and sign_extend_bits (fmt: PP.formatter) (target_width: int) (x: AST.expr): unit =
    cast fmt (string_of_int target_width) (fun _ -> cast fmt "signed" (fun _ -> expr fmt x))

(** calculate mask of the form '11111100000' with (intwidth-x) ones and x zeros *)
and notmask_int (fmt: PP.formatter) (x: AST.expr): unit =
    parens fmt (fun _ -> ones fmt int_width; nbsp fmt; delimiter fmt "<<"; nbsp fmt; expr fmt x)

and binop (fmt: PP.formatter) (op: string) (args: AST.expr list): unit =
  (match args with
  | [x; y] -> parens fmt (fun _ -> expr fmt x; nbsp fmt; delimiter fmt op; nbsp fmt; expr fmt y)
  | _ -> failwith ("binop: " ^ op)
  )

and unop (fmt: PP.formatter) (op: string) (args: AST.expr list): unit =
  (match args with
  | [x] -> parens fmt (fun _ -> delimiter fmt op; nbsp fmt; expr fmt x)
  | _ -> failwith "unop"
  )

and cond_cont (fmt: PP.formatter) (c: AST.expr) (x: AST.expr) (y: unit -> unit): unit =
  (* TODO: confirm that SV only evaluates either x or y, not both *)
  parens fmt (fun _ ->
      expr fmt c;
      nbsp fmt; delimiter fmt "?"; nbsp fmt;
      expr fmt x;
      nbsp fmt; delimiter fmt ":"; nbsp fmt;
      y ()
  )
and cond (fmt: PP.formatter) (c: AST.expr) (x: AST.expr) (y: AST.expr): unit =
  (* TODO: confirm that SV only evaluates either x or y, not both *)
  cond_cont fmt c x (fun _ -> expr fmt y)

and conds (fmt: PP.formatter) (cts: (AST.expr * AST.expr) list) (e: AST.expr): unit =
    (match cts with
    | [] -> expr fmt e
    | ((c, t) :: cts') -> cond_cont fmt c t (fun _ -> conds fmt cts' e)
    )

and funcall (fmt: PP.formatter) (f: AST.ident) (tes: AST.expr list) (args: AST.expr list) (loc: AST.l) =
    (match (f, args) with
    | (FIdent ("eq_bool", _), _) -> binop fmt "==" args
    | (FIdent ("ne_bool", _), _) -> binop fmt "!=" args
    | (FIdent ("not_bool", _), _) -> unop fmt "!" args
    | (FIdent ("and_bool", _), [x; y]) -> cond fmt x y asl_false
    | (FIdent ("or_bool", _), [x; y]) -> cond fmt x asl_true y
    | (FIdent ("equiv_bool", _), _) -> binop fmt "==" args
    | (FIdent ("implies_bool", _), [x; y]) -> cond fmt x y asl_true
    | (FIdent ("eq_int", _), _) -> binop fmt "==" args
    | (FIdent ("ne_int", _), _) -> binop fmt "!=" args
    | (FIdent ("gt_int", _), _) -> binop fmt ">" args
    | (FIdent ("ge_int", _), _) -> binop fmt ">=" args
    | (FIdent ("le_int", _), _) -> binop fmt "<=" args
    | (FIdent ("lt_int", _), _) -> binop fmt "<" args
    | (FIdent ("is_pow2_int", _), _) -> apply fmt (fun _ -> fn_onehot fmt) args
    | (FIdent ("add_int", _), _) -> binop fmt "+" args
    | (FIdent ("neg_int", _), _) -> unop fmt "-" args
    | (FIdent ("sub_int", _), _) -> binop fmt "-" args
    | (FIdent ("shl_int", _), _) -> binop fmt "<<" args
    | (FIdent ("shr_int", _), _) -> binop fmt ">>" args
    | (FIdent ("mul_int", _), _) -> binop fmt "*" args
    | (FIdent ("zdiv_int", _), _) -> binop fmt "/" args (* todo: check behaviour on -ve values *)
    | (FIdent ("zrem_int", _), _) -> binop fmt "%" args (* todo: check behaviour on -ve values *)
    (* todo: I am fairly sure that these are incorrect
    | (FIdent ("fdiv_int", _), _) -> binop fmt "/" args
    | (FIdent ("frem_int", _), _) -> binop fmt "%" args
    *)
    | (FIdent ("mod_pow2_int", _), [x; y]) -> parens fmt (fun _ -> expr fmt x; nbsp fmt; delimiter fmt "&"; nbsp fmt; delimiter fmt "~"; notmask_int fmt y)
    | (FIdent ("align_int", _), [x; y]) -> parens fmt (fun _ -> expr fmt x; nbsp fmt; delimiter fmt "&"; nbsp fmt; notmask_int fmt y)
    | (FIdent ("pow2_int", _), [x]) -> parens fmt (fun _ -> intLit fmt "1"; nbsp fmt; delimiter fmt "<<"; nbsp fmt; expr fmt x)
    (* todo: real is not supported at the moment
    | (FIdent ("cvt_int_real", _), _) -> binop fmt "" args
    | (FIdent ("eq_real", _), _) -> binop fmt "" args
    | (FIdent ("ne_real", _), _) -> binop fmt "" args
    | (FIdent ("le_real", _), _) -> binop fmt "" args
    | (FIdent ("lt_real", _), _) -> binop fmt "" args
    | (FIdent ("gt_real", _), _) -> binop fmt "" args
    | (FIdent ("ge_real", _), _) -> binop fmt "" args
    | (FIdent ("add_real", _), _) -> binop fmt "" args
    | (FIdent ("neg_real", _), _) -> unop fmt "" args
    | (FIdent ("sub_real", _), _) -> binop fmt "" args
    | (FIdent ("mul_real", _), _) -> binop fmt "" args
    | (FIdent ("divide_real", _), _) -> binop fmt "" args
    | (FIdent ("pow2_real", _), _) -> binop fmt "" args
    | (FIdent ("round_tozero_real", _), _) -> binop fmt "" args
    | (FIdent ("round_down_real", _), _) -> binop fmt "" args
    | (FIdent ("round_up_real", _), _) -> binop fmt "" args
    | (FIdent ("sqrt_real", _), _) -> binop fmt "" args
    *)
    | (FIdent ("cvt_int_bits", _), [x; n]) -> expr fmt x; brackets fmt (fun _ -> intLit fmt "0"; plus_colon fmt; expr fmt n)
    | (FIdent ("cvt_bits_sint", _), [x]) -> sign_extend_bits fmt int_width x
    | (FIdent ("cvt_bits_uint", _), [x]) -> zero_extend_bits fmt int_width x
    | (FIdent ("in_mask", _), _) -> binop fmt "=?=" args
    | (FIdent ("notin_mask", _), _) -> parens fmt (fun _ -> delimiter fmt "!"; binop fmt "=?=" args)
    | (FIdent ("eq_bits", _), _) -> binop fmt "==" args
    | (FIdent ("ne_bits", _), _) -> binop fmt "!=" args
    | (FIdent ("add_bits", _), _) -> binop fmt "+" args
    | (FIdent ("sub_bits", _), _) -> binop fmt "-" args
    | (FIdent ("mul_bits", _), _) -> binop fmt "*" args
    | (FIdent ("frem_bits_int", _), _) -> binop fmt "/" args (* todo: check behaviour on -ve values *)
    | (FIdent ("and_bits", _), _) -> binop fmt "&" args
    | (FIdent ("or_bits", _), _) -> binop fmt "|" args
    | (FIdent ("eor_bits", _), _) -> binop fmt "^" args
    | (FIdent ("not_bits", _), _) -> unop fmt "~" args
    | (FIdent ("zeros_bits", _), []) -> let x = List.hd tes in bitsLit fmt (String.make (const_int_expr x) '0')
    | (FIdent ("ones_bits", _), []) -> let x = List.hd tes in bitsLit fmt (String.make (const_int_expr x) '1')
    | (FIdent ("replicate_bits", _), [x; y]) -> braces fmt (fun _ -> valueLit fmt (const_expr y); braces fmt (fun _ -> expr fmt x))
    (* todo: string operations not supported at the moment
    | (FIdent ("cvt_int_hexstr", _), _) -> ...
    | (FIdent ("cvt_int_decstr", _), _) -> ...
    | (FIdent ("cvt_bool_str", _), _) -> ...
    | (FIdent ("cvt_bits_str", _), _) -> ...
    | (FIdent ("cvt_real_str", _), _) -> ...
    | (FIdent ("append_str_str", _), _) -> ...
    | (FIdent ("eq_str", _), _) -> ...
    | (FIdent ("ne_str", _), _) -> ...
    *)
    | (FIdent ("print_str", _), _) -> apply fmt (fun _ -> fn_write fmt) args
    | (FIdent ("print_bits", _), _) -> apply fmt (fun _ -> fn_write fmt) args
    | (FIdent ("print_char", _), [c]) -> fn_write fmt; parens fmt (fun _ -> constant fmt "\"%c\""; comma fmt; expr fmt c)
    | _ -> apply fmt (fun _ -> funname fmt f) args
    )

and expr (fmt: PP.formatter) (x: AST.expr): unit = 
  (match x with
  | Expr_If(c, t, els, e) ->
    let els1 = List.map (function (AST.E_Elsif_Cond (c, e)) -> (c, e)) els in
    conds fmt ((c, t)::els1) e
  | Expr_Field(e, f) -> expr fmt e; dot fmt; fieldname fmt f
  (* two special cases of slices are supported *)
  | Expr_Slices(e, [s]) -> expr fmt e; brackets fmt (fun _ -> slice fmt s)
  | Expr_Slices((Expr_Var v) as e, ss) ->
    braces fmt (fun _ ->
        commasep fmt (fun s -> expr fmt e; brackets fmt (fun _ -> slice fmt s)) ss
    )
  | Expr_Var(v) ->
    (match v with
    | (Ident "TRUE") -> constant fmt kw_true
    | (Ident "FALSE") -> constant fmt kw_false
    | _ -> varname fmt v
    )
  | Expr_Parens e -> expr fmt e
  | Expr_TApply(f, tes, es) ->
    funcall fmt f tes es AST.Unknown
  | Expr_Concat(es) -> braces fmt (fun _ -> exprs fmt es)
  | Expr_Unknown(t) -> unknown fmt t
  | Expr_LitInt    l -> intLit  fmt l
  | Expr_LitHex    l -> hexLit fmt l
  | Expr_LitReal   l -> realLit fmt l
  | Expr_LitBits   l -> bitsLit fmt l
  | Expr_LitMask   l -> maskLit fmt l
  | Expr_LitString l -> strLit  fmt l

  | Expr_Array (a, i) -> expr fmt a; brackets fmt (fun _ -> expr fmt i)

  (* unimplemented *)
  | Expr_Slices _
  | Expr_In _
  | Expr_Binop _
  | Expr_Unop _
  | Expr_ImpDef (_, _)
  | Expr_Tuple _
  | Expr_Fields _
  | Expr_AsConstraint _
  | Expr_AsType _
  -> raise (Unimplemented(AST.Unknown, "expression", (fun fmt -> FMTAST.expr fmt x)))
  )

and exprs (fmt: PP.formatter) (es: AST.expr list): unit = commasep fmt (expr fmt) es

and pattern (fmt: PP.formatter) (x: AST.pattern): unit =
  ( match x with
  | Pat_LitInt    l -> intLit  fmt l
  | Pat_LitHex    l -> hexLit fmt l
  | Pat_LitBits   l -> bitsLit fmt l
  | Pat_LitMask   l -> maskLit fmt l
  | Pat_Const     v -> varname fmt v
  | Pat_Wildcard  -> delimiter fmt ".*"
  | Pat_Set(ps)   -> braces fmt (fun _ -> patterns fmt ps)
  | Pat_Single(e) -> expr fmt e
  | Pat_Range(lo, hi) -> expr fmt lo; nbsp fmt; dot_dot fmt; nbsp fmt; expr fmt hi

  (* unimplemented *)
  | Pat_Tuple _
  -> raise (Unimplemented(AST.Unknown, "patterns", (fun fmt -> FMTAST.pattern fmt x)))
  )

and patterns (fmt: PP.formatter) (ps: AST.pattern list): unit = 
    (match ps with
    | [p] -> pattern fmt p
    | _ -> raise (Unimplemented(AST.Unknown, "patterns", (fun fmt -> FMTAST.patterns fmt ps)))
    )

let assign (fmt: PP.formatter) (l: unit -> unit) (r: AST.expr): unit =
  l (); nbsp fmt; eq fmt; nbsp fmt; expr fmt r; semicolon fmt

let lexpr (fmt: PP.formatter) (x: AST.lexpr) (r: AST.expr): unit =
  ( match x with
  (*
  | LExpr_Wildcard -> minus fmt
  *)
  | LExpr_Var(v) -> assign fmt (fun _ -> varname fmt v) r
  (* one special case of Field - easy to generalise this one *)
  | LExpr_Field(LExpr_Var(v), f) -> assign fmt (fun _ -> varname fmt v; dot fmt; fieldname fmt f) r
  (* one special case of slices are supported *)
  | LExpr_Slices(LExpr_Var v, ss) ->
    assign fmt (fun _ ->
        braces fmt (fun _ ->
           commasep fmt (fun s -> varname fmt v; brackets fmt (fun _ -> slice fmt s)) ss
        ))
        r
  (*
  | LExpr_BitTuple(es) ->
    assign (fun _ -> braces fmt (fun _ -> lexprs fmt es)) r
  *)
  | LExpr_Write (f, [], es) ->
      apply fmt (fun _ -> funname fmt f) (es @ [r]);
      semicolon fmt
  | LExpr_Array (LExpr_Var(a), i) -> assign fmt (fun _ -> varname fmt a; brackets fmt (fun _ -> expr fmt i)) r
  (* unimplemented *)
  | LExpr_Wildcard
  | LExpr_Field _
  | LExpr_Fields _
  | LExpr_Slices _
  | LExpr_BitTuple _
  | LExpr_Tuple _
  | LExpr_Write (_, _, _)
  | LExpr_ReadWrite (_, _, _, _)
  | LExpr_Array (_, _)
  -> raise (Unimplemented(AST.Unknown, "l-expression", (fun fmt -> FMTAST.lexpr fmt x)))
  )

(*
and lexprs (fmt: PP.formatter) (ps: AST.lexpr list): unit = commasep fmt (lexpr fmt) ps
*)

let varty (fmt: PP.formatter) (v: AST.ident) (t: AST.ty): unit =
    ( match t with
    | Type_Array (Index_Range(lo, hi), ety) ->
      ty fmt ety; nbsp fmt;
      varname fmt v;
      brackets fmt (fun _ -> expr fmt hi; colon fmt; expr fmt lo)
    | _ -> ty fmt t; nbsp fmt; varname fmt v
    )

let decl (fmt: PP.formatter) (x: AST.stmt): unit =
  (match x with
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
    ty fmt t; nbsp fmt; varnames fmt vs; semicolon fmt;
    cut fmt
  | Stmt_VarDecl (v, Some t, i, loc) ->
    varty fmt v t; semicolon fmt;
    cut fmt
  | Stmt_ConstDecl (v, Some t, i, loc) ->
    varty fmt v t; semicolon fmt;
    cut fmt

  | Stmt_VarDecl (v, None, i, loc)
  | Stmt_ConstDecl (v, None, i, loc)
  -> raise (Unimplemented(AST.Unknown, "decl: type of variable unknown", (fun fmt -> FMTAST.varname fmt v)))

  | _ ->
    ()
  )

let rec stmt (fmt: PP.formatter) (x: AST.stmt): unit =
  (match x with
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
    (* handled by decl *)
    ()
  | Stmt_VarDecl (v, t, i, loc) ->
    varname fmt v; nbsp fmt; eq fmt; nbsp fmt; expr fmt i; semicolon fmt;
  | Stmt_ConstDecl (v, t, i, loc) ->
    (* kw_constant fmt; nbsp fmt; *)
    varname fmt v; nbsp fmt; eq fmt; nbsp fmt; expr fmt i; semicolon fmt;
  | Stmt_Assign (l, r, loc) ->
    lexpr fmt l r
  | Stmt_TCall (f, tes, args, loc) ->
    funcall fmt f tes args loc;
    semicolon fmt
  | Stmt_FunReturn (e, loc) ->
    kw_return fmt; nbsp fmt; expr fmt e; semicolon fmt
  | Stmt_ProcReturn (loc) ->
    kw_return fmt; semicolon fmt
  | Stmt_Assert (e, loc) ->
    (* todo: not clear what best translation is *)
    (*
    kw_assert fmt; nbsp fmt; expr fmt e; semicolon fmt
    *)
    ()
  | Stmt_Block (ss, _) ->
      kw_begin fmt;
      indented_block fmt ss;
      cut fmt; kw_end fmt
  | Stmt_If (c, t, els, (e, el), loc) ->
    vbox fmt (fun _ ->
      kw_priority fmt; nbsp fmt;
      kw_if fmt; nbsp fmt; parens fmt (fun _ -> expr fmt c); nbsp fmt; kw_begin fmt;
      indented_block fmt t;
      map fmt
        (fun (AST.S_Elsif_Cond (c, s, loc)) ->
          cut fmt; kw_end fmt; nbsp fmt; kw_else fmt; nbsp fmt;
          kw_if fmt; nbsp fmt; parens fmt (fun _ -> expr fmt c);
          nbsp fmt; kw_begin fmt;
          indented_block fmt s;
        )
        els;
      if e <> [] then begin
        cut fmt; kw_end fmt; nbsp fmt; kw_else fmt;
        nbsp fmt; kw_begin fmt;
        indented_block fmt e
      end;
      cut fmt; kw_end fmt
    )
  | Stmt_Case (e, alts, ob, loc) ->
    vbox fmt (fun _ ->
      kw_casez fmt; nbsp fmt; parens fmt (fun _ -> expr fmt e);
      indented fmt (fun _ ->
        cutsep fmt (fun (AST.Alt_Alt(ps, oc, ss, loc)) ->
          patterns fmt ps;
          if Option.is_some oc then raise (Unimplemented(loc, "pattern_guard", (fun fmt -> ())));
          (* PP.pp_print_option (fun _ c -> nbsp fmt; amp_amp fmt; nbsp fmt; expr fmt c) fmt oc; *)
          colon fmt; nbsp fmt; kw_begin fmt;
          indented_block fmt ss;
          cut fmt; kw_end fmt
        ) alts;
        PP.pp_print_option (fun _ (b, bl) ->
          cut fmt; kw_default fmt;
          colon fmt; nbsp fmt; kw_begin fmt;
          indented_block fmt b;
          cut fmt; kw_end fmt
        ) fmt ob
      );
      cut fmt; kw_endcase fmt
    )

  (* unimplemented *)
  | Stmt_Unpred _
  | Stmt_ConstrainedUnpred _
  | Stmt_ImpDef (_, _)
  | Stmt_Undefined _
  | Stmt_ExceptionTaken _
  | Stmt_Dep_Unpred _
  | Stmt_Dep_ImpDef (_, _)
  | Stmt_Dep_Undefined _
  | Stmt_See (_, _)
  | Stmt_Throw (_, _)
  | Stmt_DecodeExecute (_, _, _)
  | Stmt_For (_, _, _, _, _, _)
  | Stmt_While (_, _, _)
  | Stmt_Repeat (_, _, _, _)
  | Stmt_Try (_, _, _, _, _, _)
  -> raise (Unimplemented(AST.Unknown, "statement", (fun fmt -> FMTAST.stmt fmt x)))
  )

and indented_block (fmt: PP.formatter) (xs: AST.stmt list): unit =
  indented fmt (fun _ ->
    map fmt (decl fmt) xs;
    cutsep fmt (stmt fmt) xs
  )

let formal (fmt: PP.formatter) (x: (AST.ident * AST.ty)): unit =
  let (v, t) = x in
  kw_input fmt; nbsp fmt;
  varty fmt v t

let formals (fmt: PP.formatter) (xs: (AST.ident * AST.ty) list): unit = commasep fmt (formal fmt) xs

let sformal (fmt: PP.formatter) (x: AST.sformal): unit =
  ( match x with
  | Formal_In(v, t) ->
      kw_input fmt; nbsp fmt;
      varty fmt v t
  | Formal_InOut(v, t) ->
      kw_inout fmt; nbsp fmt;
      varty fmt v t
  )

let sformals (fmt: PP.formatter) (xs: AST.sformal list): unit = commasep fmt (sformal fmt) xs

let function_header (fmt: PP.formatter) (ot: AST.ty option) (f: AST.ident) (args: unit -> unit): unit =
    kw_function fmt; nbsp fmt;
    kw_automatic fmt; nbsp fmt;
    PP.pp_print_option ~none: (fun _ _ -> kw_void fmt) (fun _ t -> ty fmt t) fmt ot;
    nbsp fmt;
    funname fmt f;
    parens fmt args;
    semicolon fmt

let typedef (fmt: PP.formatter) (tc: AST.ident) (pp: unit -> unit): unit =
    kw_typedef fmt; nbsp fmt; pp (); nbsp fmt; tycon fmt tc; semicolon fmt; cut fmt

let declaration (fmt: PP.formatter) (x: AST.declaration): unit =
  vbox fmt (fun _ ->
  (match x with
  | Decl_BuiltinType (tc, loc) ->
    (match tc with
    | Ident("string") -> comment fmt "typedef string string;";
    | _ -> raise (Unimplemented(AST.Unknown, "builtin type", (fun fmt -> FMTAST.tycon fmt tc)))
    )
  | Decl_Record (tc, fs, loc) ->
    kw_record fmt; nbsp fmt; tycon fmt tc;
    braces fmt (fun _ ->
      indented fmt (fun _ ->
        cutsep fmt (fun (f, t) ->
            varty fmt f t; semicolon fmt
        ) fs
      );
      cut fmt
    );
    semicolon fmt;
    cut fmt;
    cut fmt
  | Decl_Typedef (tc, t, loc) ->
    kw_typedef fmt; nbsp fmt; tycon fmt tc; nbsp fmt; eq fmt; nbsp fmt; ty fmt t; semicolon fmt;
    cut fmt;
    cut fmt
  | Decl_Enum (tc, es, loc) ->
    if tc = Ident("boolean") then begin
        typedef fmt tc (fun _ -> kw_bit fmt)
    end else begin
        kw_typedef fmt; nbsp fmt; kw_enum fmt; nbsp fmt;
        braces fmt (fun _ -> commasep fmt (varname fmt) es);
        nbsp fmt; tycon fmt tc; semicolon fmt;
        cut fmt;
        cut fmt
    end
  | Decl_FunType (f, ps, args, t, loc) ->
    function_header fmt (Some t) f (fun _ -> formals fmt args); semicolon fmt;
    cut fmt
  | Decl_FunDefn (f, ps, args, t, b, loc) ->
    function_header fmt (Some t) f (fun _ -> formals fmt args);
    indented_block fmt b;
    cut fmt;
    kw_endfunction fmt; nbsp fmt; colon fmt; nbsp fmt; funname fmt f;
    cut fmt;
    cut fmt
  | Decl_ProcType (f, ps, args, loc) ->
    function_header fmt None f (fun _ -> formals fmt args); semicolon fmt;
    cut fmt;
    cut fmt
  | Decl_ProcDefn (f, ps, args, b, loc) ->
    function_header fmt None f (fun _ -> formals fmt args);
    indented_block fmt b;
    cut fmt;
    kw_endfunction fmt; nbsp fmt; colon fmt; nbsp fmt; funname fmt f;
    cut fmt;
    cut fmt

  | Decl_Var(v, ty, loc) ->
    varty fmt v ty;
    semicolon fmt;
    cut fmt;
    cut fmt

  | Decl_ArrayGetterDefn (f, ps, args, t, b, loc) ->
    function_header fmt (Some t) f (fun _ -> formals fmt args);
    indented_block fmt b;
    cut fmt;
    kw_endfunction fmt; nbsp fmt; colon fmt; nbsp fmt; funname fmt f;
    cut fmt;
    cut fmt
  | Decl_ArraySetterDefn (f, ps, args, v, t, b, loc) ->
    function_header fmt None f (fun _ -> sformals fmt (args @ [Formal_In(v, t)]));
    indented_block fmt b;
    cut fmt;
    kw_endfunction fmt; nbsp fmt; colon fmt; nbsp fmt; funname fmt f;
    cut fmt;
    cut fmt

  (* ignored *)
  | Decl_BuiltinFunction (f, ps, args, t, loc) -> ()
  | Decl_Operator1 (op, fs, loc) -> ()
  | Decl_Operator2 (op, fs, loc) -> ()

  | _ -> raise (Unimplemented(AST.Unknown, "declaration", (fun fmt -> FMTAST.declaration fmt x)))
  )
  )

let decl_integer (fmt: PP.formatter): unit =
  typedef fmt (Ident "asl_integer")
      (fun _ -> kw_bit fmt; nbsp fmt; kw_signed fmt; brackets fmt (fun _ -> constant fmt (string_of_int (int_width - 1)); colon fmt; constant fmt "0"))

let declarations (fmt: PP.formatter) (xs: AST.declaration list): unit =
  decl_integer fmt;
  vbox fmt (fun _ -> map fmt (declaration fmt) xs)

(****************************************************************
 * End
 ****************************************************************)
