(****************************************************************
 * ASL format
 *
 * Copyright Intel Inc (c) 2021-2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL format *)

module PP = Format
module AST = Asl_ast
module ColorT = Ocolor_types
open Format_utils

let show_tyargs = true

let loc (fmt : PP.formatter) (x : AST.l) : unit =
  PP.pp_print_string fmt (AST.pp_loc x)

let delimiter (fmt : PP.formatter) (s : string) : unit =
  with_color fmt ColorT.magenta (fun _ -> PP.pp_print_string fmt s)

let keyword (fmt : PP.formatter) (s : string) : unit =
  with_color fmt ColorT.red (fun _ -> PP.pp_print_string fmt s)

let constant (fmt : PP.formatter) (s : string) : unit =
  with_color fmt ColorT.blue (fun _ -> PP.pp_print_string fmt s)

let ident (fmt : PP.formatter) (color : ColorT.color4) (x : AST.ident) : unit =
  with_color fmt color (fun _ -> PP.pp_print_string fmt (AST.pprint_ident x))

let tycon (fmt : PP.formatter) (x : AST.ident) : unit = ident fmt ColorT.green x

let funname (fmt : PP.formatter) (x : AST.ident) : unit =
  ident fmt ColorT.hi_cyan x

let varname (fmt : PP.formatter) (x : AST.ident) : unit =
  ident fmt ColorT.cyan x

let fieldname (fmt : PP.formatter) (x : AST.ident) : unit =
  ident fmt ColorT.yellow x

(* ASL delimiters *)

let amp (fmt : PP.formatter) : unit = delimiter fmt "&"
let amp_amp (fmt : PP.formatter) : unit = delimiter fmt "&&"
let bang (fmt : PP.formatter) : unit = delimiter fmt "!"
let bang_eq (fmt : PP.formatter) : unit = delimiter fmt "!="
let bar_bar (fmt : PP.formatter) : unit = delimiter fmt "||"
let caret (fmt : PP.formatter) : unit = delimiter fmt "^"
let colon (fmt : PP.formatter) : unit = delimiter fmt ":"
let coloncolon (fmt : PP.formatter) : unit = delimiter fmt "::"
let dot (fmt : PP.formatter) : unit = delimiter fmt "."
let dot_dot (fmt : PP.formatter) : unit = delimiter fmt ".."
let eq (fmt : PP.formatter) : unit = delimiter fmt "="
let eq_eq (fmt : PP.formatter) : unit = delimiter fmt "=="
let eq_gt (fmt : PP.formatter) : unit = delimiter fmt "=>"
let gt (fmt : PP.formatter) : unit = delimiter fmt ">"
let gt_eq (fmt : PP.formatter) : unit = delimiter fmt ">="
let gt_gt (fmt : PP.formatter) : unit = delimiter fmt ">>"
let lbrace_lbrace (fmt : PP.formatter) : unit = delimiter fmt "{{"
let lt (fmt : PP.formatter) : unit = delimiter fmt "<"
let lt_eq (fmt : PP.formatter) : unit = delimiter fmt "<="
let lt_lt (fmt : PP.formatter) : unit = delimiter fmt "<<"
let lt_minus_gt (fmt : PP.formatter) : unit = delimiter fmt "<->"
let minus (fmt : PP.formatter) : unit = delimiter fmt "-"
let minus_minus_gt (fmt : PP.formatter) : unit = delimiter fmt "-->"
let plus (fmt : PP.formatter) : unit = delimiter fmt "+"
let plus_colon (fmt : PP.formatter) : unit = delimiter fmt "+:"
let plus_plus (fmt : PP.formatter) : unit = delimiter fmt "++"
let rbrace_rbrace (fmt : PP.formatter) : unit = delimiter fmt "}}"
let semicolon (fmt : PP.formatter) : unit = delimiter fmt ";"
let slash (fmt : PP.formatter) : unit = delimiter fmt "/"
let star (fmt : PP.formatter) : unit = delimiter fmt "*"

(* keywords that are used as operators *)
let kw_and (fmt : PP.formatter) : unit = delimiter fmt "AND"
let kw_div (fmt : PP.formatter) : unit = delimiter fmt "DIV"
let kw_eor (fmt : PP.formatter) : unit = delimiter fmt "EOR"
let kw_in (fmt : PP.formatter) : unit = delimiter fmt "IN"
let kw_mod (fmt : PP.formatter) : unit = delimiter fmt "MOD"
let kw_not (fmt : PP.formatter) : unit = delimiter fmt "NOT"
let kw_or (fmt : PP.formatter) : unit = delimiter fmt "OR"
let kw_quot (fmt : PP.formatter) : unit = delimiter fmt "QUOT"
let kw_rem (fmt : PP.formatter) : unit = delimiter fmt "REM"
let kw_array (fmt : PP.formatter) : unit = keyword fmt "array"
let kw_as (fmt : PP.formatter) : unit = delimiter fmt "as"
let kw_assert (fmt : PP.formatter) : unit = keyword fmt "assert"
let kw_begin (fmt : PP.formatter) : unit = keyword fmt "begin"
let kw_bits (fmt : PP.formatter) : unit = keyword fmt "bits"
let kw_case (fmt : PP.formatter) : unit = keyword fmt "case"
let kw_catch (fmt : PP.formatter) : unit = keyword fmt "catch"
let kw_config (fmt : PP.formatter) : unit = keyword fmt "config"
let kw_constant (fmt : PP.formatter) : unit = keyword fmt "constant"
let kw_do (fmt : PP.formatter) : unit = keyword fmt "do"
let kw_downto (fmt : PP.formatter) : unit = keyword fmt "downto"
let kw_else (fmt : PP.formatter) : unit = keyword fmt "else"
let kw_elsif (fmt : PP.formatter) : unit = keyword fmt "elsif"
let kw_end (fmt : PP.formatter) : unit = keyword fmt "end"
let kw_enumeration (fmt : PP.formatter) : unit = keyword fmt "enumeration"
let kw_func (fmt : PP.formatter) : unit = keyword fmt "func"
let kw_getter (fmt : PP.formatter) : unit = keyword fmt "getter"
let kw_for (fmt : PP.formatter) : unit = keyword fmt "for"
let kw_if (fmt : PP.formatter) : unit = keyword fmt "if"

let kw_implementation_defined (fmt : PP.formatter) : unit =
  keyword fmt "IMPLEMENTATION_DEFINED"

let kw_let (fmt : PP.formatter) : unit = keyword fmt "let"
let kw_of (fmt : PP.formatter) : unit = keyword fmt "of"
let kw_otherwise (fmt : PP.formatter) : unit = keyword fmt "otherwise"
let kw_record (fmt : PP.formatter) : unit = keyword fmt "record"
let kw_repeat (fmt : PP.formatter) : unit = keyword fmt "repeat"
let kw_return (fmt : PP.formatter) : unit = keyword fmt "return"
let kw_see (fmt : PP.formatter) : unit = keyword fmt "SEE"
let kw_setter (fmt : PP.formatter) : unit = keyword fmt "setter"
let kw_then (fmt : PP.formatter) : unit = keyword fmt "then"
let kw_throw (fmt : PP.formatter) : unit = keyword fmt "throw"
let kw_to (fmt : PP.formatter) : unit = keyword fmt "to"
let kw_try (fmt : PP.formatter) : unit = keyword fmt "try"
let kw_type (fmt : PP.formatter) : unit = keyword fmt "type"
let kw_typeof (fmt : PP.formatter) : unit = keyword fmt "typeof"
let kw_underscore_array (fmt : PP.formatter) : unit = keyword fmt "__array"
let kw_underscore_builtin (fmt : PP.formatter) : unit = keyword fmt "__builtin"

let kw_underscore_conditional (fmt : PP.formatter) : unit =
  keyword fmt "__conditional"

let kw_underscore_decode (fmt : PP.formatter) : unit = keyword fmt "__decode"

let kw_underscore_encoding (fmt : PP.formatter) : unit =
  keyword fmt "__encoding"

let kw_underscore_event (fmt : PP.formatter) : unit = keyword fmt "__event"
let kw_underscore_execute (fmt : PP.formatter) : unit = keyword fmt "__execute"
let kw_underscore_field (fmt : PP.formatter) : unit = keyword fmt "__field"
let kw_underscore_guard (fmt : PP.formatter) : unit = keyword fmt "__guard"

let kw_underscore_instruction (fmt : PP.formatter) : unit =
  keyword fmt "__instruction"

let kw_underscore_instruction_set (fmt : PP.formatter) : unit =
  keyword fmt "__instruction_set"

let kw_underscore_map (fmt : PP.formatter) : unit = keyword fmt "__map"

let kw_underscore_newevent (fmt : PP.formatter) : unit =
  keyword fmt "__newevent"

let kw_underscore_newmap (fmt : PP.formatter) : unit = keyword fmt "__newmap"
let kw_underscore_nop (fmt : PP.formatter) : unit = keyword fmt "__NOP"
let kw_underscore_opcode (fmt : PP.formatter) : unit = keyword fmt "__opcode"

let kw_underscore_operator1 (fmt : PP.formatter) : unit =
  keyword fmt "__operator1"

let kw_underscore_operator2 (fmt : PP.formatter) : unit =
  keyword fmt "__operator2"

let kw_underscore_postdecode (fmt : PP.formatter) : unit =
  keyword fmt "__postdecode"

let kw_underscore_readwrite (fmt : PP.formatter) : unit =
  keyword fmt "__readwrite"

let kw_underscore_unallocated (fmt : PP.formatter) : unit =
  keyword fmt "__UNALLOCATED"

let kw_underscore_unpredictable (fmt : PP.formatter) : unit =
  keyword fmt "__UNPREDICTABLE"

let kw_underscore_unpredictable_unless (fmt : PP.formatter) : unit =
  keyword fmt "__unpredictable_unless"

let kw_underscore_write (fmt : PP.formatter) : unit = keyword fmt "__write"
let kw_unknown (fmt : PP.formatter) : unit = keyword fmt "UNKNOWN"
let kw_until (fmt : PP.formatter) : unit = keyword fmt "until"
let kw_var (fmt : PP.formatter) : unit = keyword fmt "var"
let kw_when (fmt : PP.formatter) : unit = keyword fmt "when"
let kw_while (fmt : PP.formatter) : unit = keyword fmt "while"

type comment = Lexing.position * Lexing.position * string

let comment_list : comment list ref = ref []

let rec get_comments (p : comment -> bool) (acc : comment list) : comment list =
  match !comment_list with
  | [] -> List.rev acc
  | c :: cs ->
      if p c then (
        comment_list := cs;
        get_comments p (c :: acc))
      else List.rev acc

(* insert all comments up to the current position and a hardline *)
let insert_comment (fmt : PP.formatter) (p : comment -> bool) : unit =
  let cs = get_comments p [] in
  match cs with
  | [] -> () (* PP.pp_print_string fmt " // no comment"; cut fmt *)
  | [ (_, _, c) ] ->
      nbsp fmt;
      PP.pp_print_string fmt c
  | _ -> PP.pp_print_string fmt " // HELP: too many comments"

(* insert all comments up to the current position and a hardline *)
let insert_comments (fmt : PP.formatter) (p : comment -> bool) : unit =
  let cs = get_comments p [] in
  match cs with
  | [] -> () (* PP.pp_print_string fmt " // no comments"; cut fmt *)
  | cs ->
      vbox fmt (fun _ ->
          map fmt
            (fun (s, f, c) ->
              PP.pp_print_string fmt c;
              cut fmt)
            cs)

let comments_before (fmt : PP.formatter) (loc : AST.l) : unit =
  (* is comment before loc? *)
  let before ((s, f, c) : comment) : bool =
    match loc with
    | Range (p1, p2) -> p1.pos_fname = f.pos_fname && p1.pos_lnum > f.pos_lnum
    | _ -> true
  in
  insert_comments fmt before

let comment_start (fmt : PP.formatter) (loc : AST.l) : unit =
  (* is comment on same line as loc? *)
  let here ((s, f, c) : comment) : bool =
    match loc with
    | Range (p1, p2) -> p1.pos_fname = s.pos_fname && p1.pos_lnum = s.pos_lnum
    | _ -> true
  in
  insert_comment fmt here

(*
let comment_end (fmt: PP.formatter) (loc: AST.l): unit =
    (* is comment on same line as loc? *)
    let here ((s, f, c): comment): bool =
        (match loc with
        | Range (p1, p2) -> p2.pos_fname = s.pos_fname && p2.pos_lnum = s.pos_lnum
        | _ -> true
        )
    in
    PP.pp_print_string fmt (AST.pp_loc loc);
    insert_comment fmt here
*)

let comment_here (fmt : PP.formatter) (pos : Lexing.position) : unit =
  (* is comment on same line as loc? *)
  let here ((s, f, c) : comment) : bool =
    pos.pos_fname = s.pos_fname && pos.pos_lnum = s.pos_lnum
  in
  insert_comment fmt here

let varnames (fmt : PP.formatter) (xs : AST.ident list) : unit =
  commasep fmt (varname fmt) xs

let funnames (fmt : PP.formatter) (xs : AST.ident list) : unit =
  commasep fmt (funname fmt) xs

let binop (fmt : PP.formatter) (x : AST.binop) : unit =
  match x with
  | Binop_Eq -> eq_eq fmt
  | Binop_NtEq -> bang_eq fmt
  | Binop_Gt -> gt fmt
  | Binop_GtEq -> gt_eq fmt
  | Binop_Lt -> lt fmt
  | Binop_LtEq -> lt_eq fmt
  | Binop_Plus -> plus fmt
  | Binop_Minus -> minus fmt
  | Binop_Multiply -> star fmt
  | Binop_Divide -> slash fmt
  | Binop_Power -> caret fmt
  | Binop_Quot -> kw_quot fmt
  | Binop_Rem -> kw_rem fmt
  | Binop_Div -> kw_div fmt
  | Binop_Mod -> kw_mod fmt
  | Binop_ShiftL -> lt_lt fmt
  | Binop_ShiftR -> gt_gt fmt
  | Binop_BoolAnd -> amp_amp fmt
  | Binop_BoolOr -> bar_bar fmt
  | Binop_BoolIff -> lt_minus_gt fmt
  | Binop_BoolImplies -> minus_minus_gt fmt
  | Binop_BitOr -> kw_or fmt
  | Binop_BitEor -> kw_eor fmt
  | Binop_BitAnd -> kw_and fmt
  | Binop_Append -> plus_plus fmt
  | Binop_DUMMY ->
      PP.pp_print_string fmt "Binop_DUMMY" (* todo: throw an error? *)

let unop (fmt : PP.formatter) (x : AST.unop) : unit =
  match x with
  | Unop_Negate -> minus fmt
  | Unop_BoolNot -> bang fmt
  | Unop_BitsNot -> kw_not fmt

let int (fmt : PP.formatter) (x : int) : unit = constant fmt (string_of_int x)
let intLit (fmt : PP.formatter) (x : AST.intLit) : unit = constant fmt x

let bitsLit (fmt : PP.formatter) (x : AST.bitsLit) : unit =
  constant fmt ("'" ^ x ^ "'")

let maskLit (fmt : PP.formatter) (x : AST.maskLit) : unit =
  constant fmt ("'" ^ x ^ "'")

let realLit (fmt : PP.formatter) (x : AST.realLit) : unit = constant fmt x
let hexLit (fmt : PP.formatter) (x : AST.hexLit) : unit = constant fmt x

let strLit (fmt : PP.formatter) (x : string) : unit =
  let escape (c : char) : unit =
    if c == '\n' then constant fmt "\\n"
    else if c == '\t' then constant fmt "\\t"
    else if c == '\\' then constant fmt "\\\\"
    else if c == '\"' then constant fmt "\\\""
    else constant fmt (String.make 1 c)
  in
  constant fmt "\"";
  String.iter escape x;
  constant fmt "\""

let rec ty (fmt : PP.formatter) (x : AST.ty) : unit =
  match x with
  | Type_Constructor tc -> tycon fmt tc
  | Type_Integer ocrs -> (
      tycon fmt (Ident "integer");
      match ocrs with None -> () | Some crs -> constraints fmt crs)
  | Type_Bits n ->
      tycon fmt (Ident "bits");
      parens fmt (fun _ -> expr fmt n)
  | Type_App (tc, es) ->
      tycon fmt tc;
      parens fmt (fun _ -> exprs fmt es)
  | Type_OfExpr e ->
      kw_typeof fmt;
      parens fmt (fun _ -> expr fmt e)
  | Type_Register (wd, fs) ->
      tycon fmt (Ident "bits");
      parens fmt (fun _ -> expr fmt wd);
      nbsp fmt;
      braces fmt (fun _ ->
          indented fmt (fun _ -> cutsep fmt (regfield fmt) fs);
          cut fmt)
  | Type_Array (ixty, ety) ->
      kw_array fmt;
      nbsp fmt;
      brackets fmt (fun _ -> ixtype fmt ixty);
      nbsp fmt;
      kw_of fmt;
      nbsp fmt;
      ty fmt ety
  | Type_Tuple tys -> parens fmt (fun _ -> types fmt tys)

and types (fmt : PP.formatter) (tys : AST.ty list) : unit =
  commasep fmt (ty fmt) tys

and constraint_range (fmt : PP.formatter) (x : AST.constraint_range) : unit =
  match x with
  | Constraint_Single e -> expr fmt e
  | Constraint_Range (lo, hi) ->
      expr fmt lo;
      dot_dot fmt;
      expr fmt hi

and constraints (fmt : PP.formatter) (x : AST.constraint_range list) : unit =
  braces fmt (fun _ -> commasep fmt (constraint_range fmt) x)

and regfield (fmt : PP.formatter) (rf : AST.slice list * AST.ident) : unit =
  brackets fmt (fun _ -> commasep fmt (slice fmt) (fst rf));
  nbsp fmt;
  fieldname fmt (snd rf)

and slice (fmt : PP.formatter) (x : AST.slice) : unit =
  match x with
  | Slice_Single e -> expr fmt e
  | Slice_HiLo (hi, lo) ->
      expr fmt hi;
      nbsp fmt;
      colon fmt;
      nbsp fmt;
      expr fmt lo
  | Slice_LoWd (lo, wd) ->
      expr fmt lo;
      nbsp fmt;
      plus_colon fmt;
      nbsp fmt;
      expr fmt wd

and slices (fmt : PP.formatter) (ss : AST.slice list) : unit =
  commasep fmt (slice fmt) ss

and ixtype (fmt : PP.formatter) (x : AST.ixtype) : unit =
  match x with
  | Index_Enum tc -> tycon fmt tc
  | Index_Range (lo, hi) ->
      expr fmt lo;
      nbsp fmt;
      dot_dot fmt;
      nbsp fmt;
      expr fmt hi

and expr (fmt : PP.formatter) (x : AST.expr) : unit =
  match x with
  | Expr_If (c, t, els, e) ->
      kw_if fmt;
      nbsp fmt;
      expr fmt c;
      nbsp fmt;
      kw_then fmt;
      map fmt
        (fun (AST.E_Elsif_Cond (c, e)) ->
          nbsp fmt;
          kw_elsif fmt;
          nbsp fmt;
          expr fmt c;
          nbsp fmt;
          kw_then fmt;
          nbsp fmt;
          expr fmt e)
        els;
      nbsp fmt;
      kw_else fmt;
      nbsp fmt;
      expr fmt e
  | Expr_Binop (a, op, b) ->
      expr fmt a;
      nbsp fmt;
      binop fmt op;
      nbsp fmt;
      expr fmt b
  | Expr_Field (e, f) ->
      expr fmt e;
      dot fmt;
      fieldname fmt f
  | Expr_Fields (e, fs) ->
      expr fmt e;
      dot fmt;
      brackets fmt (fun _ -> commasep fmt (fieldname fmt) fs)
  | Expr_Slices (e, ss) ->
      expr fmt e;
      brackets fmt (fun _ -> slices fmt ss)
  | Expr_RecordInit (tc, fas) ->
      tycon fmt tc;
      braces fmt (fun _ -> commasep fmt (field_assignment fmt) fas)
  | Expr_In (e, p) ->
      expr fmt e;
      nbsp fmt;
      kw_in fmt;
      nbsp fmt;
      pattern fmt p
  | Expr_Var v -> varname fmt v
  | Expr_Parens e -> parens fmt (fun _ -> expr fmt e)
  | Expr_TApply (f, tes, es) ->
      funname fmt f;
      if show_tyargs then (
        lbrace_lbrace fmt;
        exprs fmt tes;
        rbrace_rbrace fmt);
      parens fmt (fun _ -> exprs fmt es)
  | Expr_Tuple es -> parens fmt (fun _ -> exprs fmt es)
  | Expr_Concat es -> brackets fmt (fun _ -> exprs fmt es)
  | Expr_Unop (op, e) ->
      unop fmt op;
      nbsp fmt;
      expr fmt e
  | Expr_Unknown t ->
      kw_unknown fmt;
      nbsp fmt;
      coloncolon fmt;
      nbsp fmt;
      ty fmt t
  | Expr_ImpDef (os, t) ->
      kw_implementation_defined fmt;
      nbsp fmt;
      PP.pp_print_option strLit fmt os;
      nbsp fmt;
      coloncolon fmt;
      nbsp fmt;
      ty fmt t
  | Expr_Array (a, e) ->
      expr fmt a;
      brackets fmt (fun _ -> expr fmt e)
  | Expr_LitInt l -> intLit fmt l
  | Expr_LitHex l -> hexLit fmt l
  | Expr_LitReal l -> realLit fmt l
  | Expr_LitBits l -> bitsLit fmt l
  | Expr_LitMask l -> maskLit fmt l
  | Expr_LitString l -> strLit fmt l
  | Expr_AsConstraint (e, c) ->
      expr fmt e;
      nbsp fmt;
      kw_as fmt;
      nbsp fmt;
      constraints fmt c
  | Expr_AsType (e, t) ->
      expr fmt e;
      nbsp fmt;
      kw_as fmt;
      nbsp fmt;
      ty fmt t

and exprs (fmt : PP.formatter) (es : AST.expr list) : unit =
  commasep fmt (expr fmt) es

and field_assignment (fmt : PP.formatter) (x : AST.ident * AST.expr) : unit =
  match x with
  | f, e ->
      fieldname fmt f;
      nbsp fmt;
      eq fmt;
      nbsp fmt;
      expr fmt e

and pattern (fmt : PP.formatter) (x : AST.pattern) : unit =
  match x with
  | Pat_LitInt l -> intLit fmt l
  | Pat_LitHex l -> hexLit fmt l
  | Pat_LitBits l -> bitsLit fmt l
  | Pat_LitMask l -> maskLit fmt l
  | Pat_Const v -> varname fmt v
  | Pat_Wildcard -> minus fmt
  | Pat_Tuple ps -> parens fmt (fun _ -> patterns fmt ps)
  | Pat_Set ps -> braces fmt (fun _ -> patterns fmt ps)
  | Pat_Single e -> expr fmt e
  | Pat_Range (lo, hi) ->
      expr fmt lo;
      nbsp fmt;
      dot_dot fmt;
      nbsp fmt;
      expr fmt hi

and patterns (fmt : PP.formatter) (ps : AST.pattern list) : unit =
  commasep fmt (pattern fmt) ps

let rec lexpr (fmt : PP.formatter) (x : AST.lexpr) : unit =
  match x with
  | LExpr_Wildcard -> minus fmt
  | LExpr_Var v -> varname fmt v
  | LExpr_Field (e, f) ->
      lexpr fmt e;
      dot fmt;
      fieldname fmt f
  | LExpr_Fields (e, fs) ->
      lexpr fmt e;
      dot fmt;
      brackets fmt (fun _ -> commasep fmt (fieldname fmt) fs)
  | LExpr_Slices (e, ss) ->
      lexpr fmt e;
      brackets fmt (fun _ -> slices fmt ss)
  | LExpr_BitTuple es -> brackets fmt (fun _ -> lexprs fmt es)
  | LExpr_Tuple es -> parens fmt (fun _ -> lexprs fmt es)
  | LExpr_Array (a, e) ->
      nbsp fmt;
      lexpr fmt a;
      brackets fmt (fun _ -> expr fmt e)
  | LExpr_Write (f, tes, es) ->
      kw_underscore_write fmt;
      nbsp fmt;
      funname fmt f;
      if show_tyargs then (
        lbrace_lbrace fmt;
        exprs fmt tes;
        rbrace_rbrace fmt);
      parens fmt (fun _ -> exprs fmt es)
  | LExpr_ReadWrite (f, g, tes, es) ->
      kw_underscore_readwrite fmt;
      nbsp fmt;
      funname fmt f;
      nbsp fmt;
      funname fmt g;
      if show_tyargs then (
        lbrace_lbrace fmt;
        exprs fmt tes;
        rbrace_rbrace fmt);
      parens fmt (fun _ -> exprs fmt es)

and lexprs (fmt : PP.formatter) (ps : AST.lexpr list) : unit =
  commasep fmt (lexpr fmt) ps

let varty (fmt : PP.formatter) (v : AST.ident) (t : AST.ty) : unit =
  varname fmt v;
  nbsp fmt;
  coloncolon fmt;
  nbsp fmt;
  ty fmt t

let varoty (fmt : PP.formatter) (v : AST.ident) (ot : AST.ty option) : unit =
  match ot with None -> varname fmt v | Some t -> varty fmt v t

let direction (fmt : PP.formatter) (x : AST.direction) : unit =
  match x with Direction_Up -> kw_to fmt | Direction_Down -> kw_downto fmt

let rec stmt (fmt : PP.formatter) (x : AST.stmt) : unit =
  match x with
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      comments_before fmt loc;
      kw_var fmt;
      nbsp fmt;
      varnames fmt vs;
      nbsp fmt;
      coloncolon fmt;
      nbsp fmt;
      ty fmt t;
      semicolon fmt;
      comment_start fmt loc
  | Stmt_VarDecl (v, ot, i, loc) ->
      comments_before fmt loc;
      kw_var fmt;
      nbsp fmt;
      varoty fmt v ot;
      nbsp fmt;
      eq fmt;
      nbsp fmt;
      expr fmt i;
      semicolon fmt;
      comment_start fmt loc
  | Stmt_ConstDecl (v, ot, i, loc) ->
      comments_before fmt loc;
      kw_let fmt;
      nbsp fmt;
      varoty fmt v ot;
      nbsp fmt;
      eq fmt;
      nbsp fmt;
      expr fmt i;
      semicolon fmt;
      comment_start fmt loc
  | Stmt_Assign (l, r, loc) ->
      comments_before fmt loc;
      lexpr fmt l;
      nbsp fmt;
      eq fmt;
      nbsp fmt;
      expr fmt r;
      semicolon fmt;
      comment_start fmt loc
  | Stmt_TCall (f, tes, args, loc) ->
      comments_before fmt loc;
      funname fmt f;
      if show_tyargs then (
        lbrace_lbrace fmt;
        exprs fmt tes;
        rbrace_rbrace fmt);
      parens fmt (fun _ -> exprs fmt args);
      semicolon fmt;
      comment_start fmt loc
  | Stmt_FunReturn (e, loc) ->
      comments_before fmt loc;
      kw_return fmt;
      nbsp fmt;
      expr fmt e;
      semicolon fmt;
      comment_start fmt loc
  | Stmt_ProcReturn loc ->
      comments_before fmt loc;
      kw_return fmt;
      semicolon fmt;
      comment_start fmt loc
  | Stmt_Assert (e, loc) ->
      comments_before fmt loc;
      kw_assert fmt;
      nbsp fmt;
      expr fmt e;
      semicolon fmt;
      comment_start fmt loc
  | Stmt_Throw (v, loc) ->
      comments_before fmt loc;
      kw_throw fmt;
      nbsp fmt;
      varname fmt v;
      comment_start fmt loc
  | Stmt_DecodeExecute (i, e, loc) ->
      comments_before fmt loc;
      kw_underscore_decode fmt;
      nbsp fmt;
      varname fmt i;
      nbsp fmt;
      expr fmt e;
      semicolon fmt;
      comment_start fmt loc
  | Stmt_Block (ss, loc) ->
      comments_before fmt loc;
      kw_begin fmt;
      comment_start fmt loc;
      indented_block fmt ss;
      cut fmt;
      kw_end fmt
  | Stmt_If (c, t, els, (e, el), loc) ->
      comments_before fmt loc;
      vbox fmt (fun _ ->
          kw_if fmt;
          nbsp fmt;
          expr fmt c;
          nbsp fmt;
          kw_then fmt;
          comment_start fmt loc;
          indented_block fmt t;
          map fmt
            (fun (AST.S_Elsif_Cond (c, s, loc)) ->
              cut fmt;
              kw_elsif fmt;
              nbsp fmt;
              expr fmt c;
              nbsp fmt;
              kw_then fmt;
              comment_start fmt loc;
              indented_block fmt s)
            els;
          if e <> [] then (
            cut fmt;
            kw_else fmt;
            comment_start fmt el;
            indented_block fmt e);
          cut fmt;
          kw_end fmt)
  | Stmt_Case (e, alts, ob, loc) ->
      comments_before fmt loc;
      vbox fmt (fun _ ->
          kw_case fmt;
          nbsp fmt;
          expr fmt e;
          nbsp fmt;
          kw_of fmt;
          comment_start fmt loc;
          indented fmt (fun _ ->
              cutsep fmt
                (fun (AST.Alt_Alt (ps, oc, ss, loc)) ->
                  kw_when fmt;
                  nbsp fmt;
                  patterns fmt ps;
                  PP.pp_print_option
                    (fun _ c ->
                      nbsp fmt;
                      amp_amp fmt;
                      nbsp fmt;
                      expr fmt c)
                    fmt oc;
                  nbsp fmt;
                  eq_gt fmt;
                  comment_start fmt loc;
                  indented_block fmt ss)
                alts;
              PP.pp_print_option
                (fun _ (b, bl) ->
                  cut fmt;
                  kw_otherwise fmt;
                  comment_start fmt bl;
                  indented_block fmt b)
                fmt ob);
          cut fmt;
          kw_end fmt)
  | Stmt_For (v, f, dir, t, b, loc) ->
      comments_before fmt loc;
      kw_for fmt;
      nbsp fmt;
      varname fmt v;
      nbsp fmt;
      eq fmt;
      nbsp fmt;
      expr fmt f;
      nbsp fmt;
      direction fmt dir;
      nbsp fmt;
      expr fmt t;
      nbsp fmt;
      kw_do fmt;
      comment_start fmt loc;
      indented_block fmt b;
      cut fmt;
      kw_end fmt
  | Stmt_While (c, b, loc) ->
      comments_before fmt loc;
      kw_while fmt;
      nbsp fmt;
      expr fmt c;
      nbsp fmt;
      kw_do fmt;
      comment_start fmt loc;
      indented_block fmt b;
      cut fmt;
      kw_end fmt
  | Stmt_Repeat (b, c, pos, loc) ->
      comments_before fmt loc;
      kw_repeat fmt;
      comment_start fmt loc;
      indented_block fmt b;
      cut fmt;
      kw_until fmt;
      nbsp fmt;
      expr fmt c;
      semicolon fmt;
      comment_here fmt pos
  | Stmt_Try (b, v, pos, cs, ob, loc) ->
      comments_before fmt loc;
      kw_try fmt;
      comment_start fmt loc;
      indented_block fmt b;
      cut fmt;
      kw_catch fmt;
      nbsp fmt;
      varname fmt v;
      comment_here fmt pos;
      indented fmt (fun _ ->
          cutsep fmt
            (fun (AST.Catcher_Guarded (e, b, loc)) ->
              kw_when fmt;
              nbsp fmt;
              expr fmt e;
              comment_start fmt loc;
              indented_block fmt b)
            cs;
          PP.pp_print_option
            (fun _ (b, bl) ->
              cut fmt;
              kw_otherwise fmt;
              comment_start fmt bl;
              indented_block fmt b)
            fmt ob);
      cut fmt;
      kw_end fmt

and indented_block (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  indented fmt (fun _ -> cutsep fmt (stmt fmt) xs)

let parameter (fmt : PP.formatter) (x : AST.ident * AST.ty option) : unit =
  let v, ot = x in
  varoty fmt v ot

let parameters (fmt : PP.formatter) (xs : (AST.ident * AST.ty option) list) :
    unit =
  commasep fmt (parameter fmt) xs

let formal (fmt : PP.formatter) (x : AST.ident * AST.ty) : unit =
  let v, t = x in
  varty fmt v t

let formals (fmt : PP.formatter) (xs : (AST.ident * AST.ty) list) : unit =
  commasep fmt (formal fmt) xs

let instr_field (fmt : PP.formatter) (x : AST.instr_field) : unit =
  match x with
  | IField_Field (f, lo, wd) ->
      kw_underscore_field fmt;
      nbsp fmt;
      fieldname fmt f;
      nbsp fmt;
      int fmt lo;
      nbsp fmt;
      plus_colon fmt;
      nbsp fmt;
      int fmt wd

let opcode_value (fmt : PP.formatter) (x : AST.opcode_value) : unit =
  match x with Opcode_Bits b -> bitsLit fmt b | Opcode_Mask m -> maskLit fmt m

let encoding (fmt : PP.formatter) (x : AST.encoding) : unit =
  match x with
  | Encoding_Block (nm, iset, fs, op, e, ups, b, loc) ->
      kw_underscore_encoding fmt;
      nbsp fmt;
      varname fmt nm;
      indented fmt (fun _ ->
          kw_underscore_instruction_set fmt;
          nbsp fmt;
          varname fmt iset;
          cut fmt;
          map fmt
            (fun f ->
              instr_field fmt f;
              cut fmt)
            fs;
          kw_underscore_opcode fmt;
          nbsp fmt;
          opcode_value fmt op;
          cut fmt;
          kw_underscore_guard fmt;
          nbsp fmt;
          expr fmt e;
          cut fmt;
          map fmt
            (fun (i, b) ->
              kw_underscore_unpredictable_unless fmt;
              nbsp fmt;
              int fmt i;
              nbsp fmt;
              eq_eq fmt;
              nbsp fmt;
              bitsLit fmt b;
              cut fmt)
            ups;
          kw_underscore_decode fmt;
          indented_block fmt b)

let decode_slice (fmt : PP.formatter) (x : AST.decode_slice) : unit =
  match x with
  | DecoderSlice_Slice (lo, wd) ->
      int fmt lo;
      nbsp fmt;
      plus_colon fmt;
      nbsp fmt;
      int fmt wd
  | DecoderSlice_FieldName f -> fieldname fmt f
  | DecoderSlice_Concat fs -> sepby fmt (fun _ -> colon fmt) (fieldname fmt) fs

let rec decode_pattern (fmt : PP.formatter) (x : AST.decode_pattern) : unit =
  match x with
  | DecoderPattern_Bits b -> bitsLit fmt b
  | DecoderPattern_Mask m -> maskLit fmt m
  | DecoderPattern_Wildcard _ -> delimiter fmt "_"
  | DecoderPattern_Not p ->
      bang fmt;
      decode_pattern fmt p

let rec decode_body (fmt : PP.formatter) (x : AST.decode_body) : unit =
  match x with
  | DecoderBody_UNPRED loc -> kw_underscore_unpredictable fmt
  | DecoderBody_UNALLOC loc -> kw_underscore_unallocated fmt
  | DecoderBody_NOP loc -> kw_underscore_nop fmt
  | DecoderBody_Encoding (e, loc) ->
      kw_underscore_encoding fmt;
      nbsp fmt;
      varname fmt e
  | DecoderBody_Decoder (fs, c, loc) ->
      indented fmt (fun _ ->
          map fmt
            (fun f ->
              instr_field fmt f;
              cut fmt)
            fs;
          decode_case fmt c)

and decode_case (fmt : PP.formatter) (x : AST.decode_case) : unit =
  match x with
  | DecoderCase_Case (ss, alts, loc) ->
      kw_case fmt;
      nbsp fmt;
      parens fmt (fun _ -> commasep fmt (decode_slice fmt) ss);
      nbsp fmt;
      kw_of fmt;
      indented fmt (fun _ ->
          cutsep fmt
            (fun (AST.DecoderAlt_Alt (ps, b)) ->
              kw_when fmt;
              parens fmt (fun _ -> commasep fmt (decode_pattern fmt) ps);
              nbsp fmt;
              eq_gt fmt;
              decode_body fmt b)
            alts)

let function_header (fmt : PP.formatter) (ot : AST.ty option) (f : AST.ident)
    (ps : (AST.ident * AST.ty option) list) (args : unit -> unit) : unit =
  funname fmt f;
  braces fmt (fun _ -> parameters fmt ps);
  parens fmt args;
  PP.pp_print_option
    (fun _ t ->
      nbsp fmt;
      eq_gt fmt;
      nbsp fmt;
      ty fmt t)
    fmt ot

let declaration (fmt : PP.formatter) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      match x with
      | Decl_BuiltinType (tc, loc) ->
          comments_before fmt loc;
          kw_underscore_builtin fmt;
          nbsp fmt;
          kw_type fmt;
          nbsp fmt;
          tycon fmt tc;
          semicolon fmt
      | Decl_Forward (tc, loc) ->
          comments_before fmt loc;
          kw_type fmt;
          nbsp fmt;
          tycon fmt tc;
          semicolon fmt
      | Decl_Record (tc, fs, loc) ->
          comments_before fmt loc;
          kw_record fmt;
          nbsp fmt;
          tycon fmt tc;
          braces fmt (fun _ ->
              indented fmt (fun _ ->
                  cutsep fmt
                    (fun (f, t) ->
                      fieldname fmt f;
                      nbsp fmt;
                      ty fmt t;
                      semicolon fmt)
                    fs);
              cut fmt);
          semicolon fmt
      | Decl_Typedef (tc, t, loc) ->
          comments_before fmt loc;
          kw_type fmt;
          nbsp fmt;
          tycon fmt tc;
          nbsp fmt;
          kw_of fmt;
          nbsp fmt;
          ty fmt t;
          semicolon fmt
      | Decl_Enum (tc, es, loc) ->
          comments_before fmt loc;
          kw_enumeration fmt;
          nbsp fmt;
          tycon fmt tc;
          nbsp fmt;
          braces fmt (fun _ -> commasep fmt (varname fmt) es);
          semicolon fmt
      | Decl_Var (v, t, loc) ->
          comments_before fmt loc;
          kw_var fmt;
          nbsp fmt;
          varty fmt v t;
          semicolon fmt
      | Decl_Const (v, t, e, loc) ->
          comments_before fmt loc;
          kw_constant fmt;
          nbsp fmt;
          varty fmt v t;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          expr fmt e;
          semicolon fmt
      | Decl_BuiltinFunction (f, ps, args, t, loc) ->
          comments_before fmt loc;
          kw_underscore_builtin fmt;
          nbsp fmt;
          kw_func fmt;
          nbsp fmt;
          function_header fmt (Some t) f ps (fun _ -> formals fmt args);
          semicolon fmt
      | Decl_FunType (f, ps, args, t, loc) ->
          comments_before fmt loc;
          kw_func fmt;
          nbsp fmt;
          function_header fmt (Some t) f ps (fun _ -> formals fmt args);
          semicolon fmt
      | Decl_FunDefn (f, ps, args, t, b, loc) ->
          comments_before fmt loc;
          kw_func fmt;
          nbsp fmt;
          function_header fmt (Some t) f ps (fun _ -> formals fmt args);
          indented_block fmt b;
          cut fmt;
          kw_end fmt
      | Decl_ProcType (f, ps, args, loc) ->
          comments_before fmt loc;
          kw_func fmt;
          nbsp fmt;
          function_header fmt None f ps (fun _ -> formals fmt args);
          semicolon fmt
      | Decl_ProcDefn (f, ps, args, b, loc) ->
          comments_before fmt loc;
          kw_func fmt;
          nbsp fmt;
          function_header fmt None f ps (fun _ -> formals fmt args);
          indented_block fmt b;
          cut fmt;
          kw_end fmt
      | Decl_VarGetterType (f, ps, t, loc) ->
          kw_getter fmt;
          nbsp fmt;
          funname fmt f;
          nbsp fmt;
          eq_gt fmt;
          nbsp fmt;
          ty fmt t;
          semicolon fmt
      | Decl_VarGetterDefn (f, ps, t, b, loc) ->
          comments_before fmt loc;
          kw_getter fmt;
          nbsp fmt;
          funname fmt f;
          nbsp fmt;
          eq_gt fmt;
          nbsp fmt;
          ty fmt t;
          indented_block fmt b;
          cut fmt;
          kw_end fmt
      | Decl_ArrayGetterType (f, ps, args, t, loc) ->
          comments_before fmt loc;
          kw_getter fmt;
          nbsp fmt;
          funname fmt f;
          braces fmt (fun _ -> parameters fmt ps);
          brackets fmt (fun _ -> formals fmt args);
          nbsp fmt;
          eq_gt fmt;
          nbsp fmt;
          ty fmt t;
          semicolon fmt
      | Decl_ArrayGetterDefn (f, ps, args, t, b, loc) ->
          comments_before fmt loc;
          kw_getter fmt;
          nbsp fmt;
          funname fmt f;
          braces fmt (fun _ -> parameters fmt ps);
          brackets fmt (fun _ -> formals fmt args);
          nbsp fmt;
          eq_gt fmt;
          nbsp fmt;
          ty fmt t;
          indented_block fmt b;
          cut fmt;
          kw_end fmt
      | Decl_VarSetterType (f, ps, v, t, loc) ->
          comments_before fmt loc;
          kw_setter fmt;
          nbsp fmt;
          funname fmt f;
          braces fmt (fun _ -> parameters fmt ps);
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          varname fmt v;
          nbsp fmt;
          ty fmt t;
          semicolon fmt
      | Decl_VarSetterDefn (f, ps, v, t, b, loc) ->
          comments_before fmt loc;
          kw_setter fmt;
          nbsp fmt;
          funname fmt f;
          braces fmt (fun _ -> parameters fmt ps);
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          varname fmt v;
          nbsp fmt;
          ty fmt t;
          indented_block fmt b;
          cut fmt;
          kw_end fmt
      | Decl_ArraySetterType (f, ps, args, v, t, loc) ->
          comments_before fmt loc;
          kw_setter fmt;
          nbsp fmt;
          funname fmt f;
          braces fmt (fun _ -> parameters fmt ps);
          brackets fmt (fun _ -> formals fmt args);
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          varname fmt v;
          nbsp fmt;
          ty fmt t;
          semicolon fmt
      | Decl_ArraySetterDefn (f, ps, args, v, t, b, loc) ->
          comments_before fmt loc;
          kw_setter fmt;
          nbsp fmt;
          funname fmt f;
          braces fmt (fun _ -> parameters fmt ps);
          brackets fmt (fun _ -> formals fmt args);
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          varname fmt v;
          nbsp fmt;
          ty fmt t;
          indented_block fmt b;
          cut fmt;
          kw_end fmt
      | Decl_InstructionDefn (d, es, opd, c, ex, loc) ->
          comments_before fmt loc;
          kw_underscore_instruction fmt;
          nbsp fmt;
          varname fmt d;
          indented fmt (fun _ ->
              cutsep fmt (encoding fmt) es;
              PP.pp_print_option
                (fun _ pd ->
                  kw_underscore_postdecode fmt;
                  indented_block fmt pd)
                fmt opd;
              kw_underscore_execute fmt;
              if c then kw_underscore_conditional fmt;
              nbsp fmt;
              indented_block fmt ex);
          cut fmt;
          kw_end fmt
      | Decl_DecoderDefn (d, dc, loc) ->
          comments_before fmt loc;
          kw_underscore_decode fmt;
          varname fmt d;
          indented fmt (fun _ -> decode_case fmt dc);
          cut fmt;
          kw_end fmt
      | Decl_Operator1 (op, fs, loc) ->
          comments_before fmt loc;
          kw_underscore_operator1 fmt;
          nbsp fmt;
          unop fmt op;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          funnames fmt fs;
          semicolon fmt
      | Decl_Operator2 (op, fs, loc) ->
          comments_before fmt loc;
          kw_underscore_operator2 fmt;
          nbsp fmt;
          binop fmt op;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          funnames fmt fs;
          semicolon fmt
      | Decl_NewEventDefn (f, ps, args, loc) ->
          comments_before fmt loc;
          kw_underscore_newevent fmt;
          nbsp fmt;
          function_header fmt None f ps (fun _ -> formals fmt args);
          semicolon fmt
      | Decl_EventClause (f, b, loc) ->
          comments_before fmt loc;
          kw_underscore_event fmt;
          nbsp fmt;
          funname fmt f;
          indented_block fmt b;
          cut fmt;
          kw_end fmt
      | Decl_NewMapDefn (f, ps, args, t, b, loc) ->
          comments_before fmt loc;
          kw_underscore_newevent fmt;
          nbsp fmt;
          function_header fmt (Some t) f ps (fun _ -> formals fmt args);
          indented_block fmt b;
          cut fmt;
          kw_end fmt
      | Decl_MapClause (f, fs, oc, b, loc) ->
          comments_before fmt loc;
          kw_underscore_event fmt;
          nbsp fmt;
          funname fmt f;
          parens fmt (fun _ ->
              commasep fmt
                (fun (AST.MapField_Field (f, p)) ->
                  varname fmt f;
                  nbsp fmt;
                  eq fmt;
                  nbsp fmt;
                  pattern fmt p)
                fs);
          PP.pp_print_option
            (fun _ c ->
              kw_when fmt;
              expr fmt c)
            fmt oc;
          kw_then fmt;
          indented_block fmt b;
          cut fmt;
          kw_end fmt
      | Decl_Config (v, t, e, loc) ->
          comments_before fmt loc;
          kw_config fmt;
          nbsp fmt;
          varty fmt v t;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          expr fmt e;
          semicolon fmt)

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ ->
      map fmt
        (fun d ->
          declaration fmt d;
          cut fmt)
        xs)

(****************************************************************
 * End
 ****************************************************************)
