(****************************************************************
 * ASL AST definitions
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type id = string
type intLit = string
type bitsLit = string
type maskLit = string
type realLit = string
type hexLit = string

(** Location tracking *)
type l =
    | Unknown
    | Int of string * l option
    | Generated of l
    | Range of Lexing.position * Lexing.position

type 'a annot = l * 'a

type pos = Lexing.position

let pp_lexing_position (p: Lexing.position): string =
    Printf.sprintf  "file \"%s\" line %d char %d"
        p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

let rec pp_loc (l: l): string =  match l with
    | Unknown -> "no location information available"
    | Generated l -> Printf.sprintf "Generated: %s"  (pp_loc l)
    | Range(p1, p2) ->
        if String.equal p1.Lexing.pos_fname p2.Lexing.pos_fname then begin
            if p1.Lexing.pos_lnum = p2.Lexing.pos_lnum then
                Printf.sprintf "file \"%s\" line %d char %d - %d"
                    p1.Lexing.pos_fname
                    p1.Lexing.pos_lnum
                    (p1.Lexing.pos_cnum - p1.Lexing.pos_bol)
                    (p2.Lexing.pos_cnum - p2.Lexing.pos_bol)
            else
                Printf.sprintf "file \"%s\" line %d char %d - line %d char %d"
                    p1.Lexing.pos_fname
                    p1.Lexing.pos_lnum
                    (p1.Lexing.pos_cnum - p1.Lexing.pos_bol)
                    p2.Lexing.pos_lnum
                    (p2.Lexing.pos_cnum - p2.Lexing.pos_bol)
        end else begin
            Printf.sprintf "file \"%s\" line %d char %d - file \"%s\" line %d char %d"
                p1.Lexing.pos_fname
                p1.Lexing.pos_lnum
                (p1.Lexing.pos_cnum - p1.Lexing.pos_bol)
                p2.Lexing.pos_fname
                p2.Lexing.pos_lnum
                (p2.Lexing.pos_cnum - p2.Lexing.pos_bol)
        end
    | Int(s,lo) -> Printf.sprintf "%s %s" s (match lo with Some l -> pp_loc l | None -> "none")

(** Parsing exceptions (1/2) *)
exception Parse_error_locn of l * string

type
binop =
   Binop_Eq
 | Binop_NtEq
 | Binop_Gt
 | Binop_GtEq
 | Binop_Lt
 | Binop_LtEq
 | Binop_Plus
 | Binop_Minus
 | Binop_Multiply
 | Binop_Divide
 | Binop_Power
 | Binop_Quot
 | Binop_Rem
 | Binop_Div
 | Binop_Mod
 | Binop_ShiftL
 | Binop_ShiftR
 | Binop_BoolAnd
 | Binop_BoolOr
 | Binop_BoolIff
 | Binop_BoolImplies
 | Binop_BitOr
 | Binop_BitEor
 | Binop_BitAnd
 | Binop_Append
 | Binop_DUMMY

type
unop =
   Unop_Negate
 | Unop_BoolNot
 | Unop_BitsNot

type
pattern =
   Pat_LitInt of intLit
 | Pat_LitHex of hexLit
 | Pat_LitBits of bitsLit
 | Pat_LitMask of maskLit
 | Pat_Const of Ident.t
 | Pat_Wildcard
 | Pat_Tuple of pattern list
 | Pat_Set of pattern list
 | Pat_Range of expr * expr
 | Pat_Single of expr

and expr =
   Expr_If of expr * expr * e_elsif list * expr
 | Expr_Binop of expr * binop * expr
 | Expr_Unop of unop * expr (* unary operator *)
 | Expr_Field of expr * Ident.t (* field selection *)
 | Expr_Fields of expr * Ident.t list (* multiple field selection *)
 | Expr_Slices of ty * expr * slice list (* bitslice *)
 | Expr_RecordInit of Ident.t * expr list * (Ident.t * expr) list
 | Expr_In of expr * pattern (* pattern match *)
 | Expr_Var of Ident.t
 | Expr_Parens of expr
 | Expr_Tuple of expr list (* tuple *)
 | Expr_Unknown of ty
 | Expr_ImpDef of string option * ty
 | Expr_AsConstraint of expr * constraint_range list
 | Expr_AsType of expr * ty
 | Expr_TApply of Ident.t * expr list * expr list * bool (* function call with explicit type parameters *)
 | Expr_Concat of expr list * expr list (* bitvector concatenation *)
 | Expr_Array of expr * expr (* array accesses *)
 | Expr_LitInt of intLit (* literal decimal integer *)
 | Expr_LitHex of hexLit (* literal hexadecimal integer *)
 | Expr_LitReal of realLit (* literal real *)
 | Expr_LitBits of bitsLit (* literal bitvector *)
 | Expr_LitMask of maskLit (* literal bitmask *)
 | Expr_LitString of string (* literal string *)

and e_elsif =
   E_Elsif_Cond of expr * expr

and slice =
   Slice_Single of expr
 | Slice_HiLo of expr * expr
 | Slice_LoWd of expr * expr

and ixtype =
   Index_Enum of Ident.t
 | Index_Int of expr

and ty =
 | Type_Integer of constraint_range list option
 | Type_Bits of expr
 | Type_Constructor of Ident.t * expr list
 | Type_OfExpr of expr
 | Type_Register of expr * (slice list * Ident.t) list
 | Type_Array of ixtype * ty
 | Type_Tuple of ty list

and constraint_range =
   Constraint_Single of expr
 | Constraint_Range of expr * expr

type
lexpr =
   LExpr_Wildcard
 | LExpr_Var of Ident.t
 | LExpr_Field of lexpr * Ident.t
 | LExpr_Fields of lexpr * Ident.t list
 | LExpr_Slices of ty * lexpr * slice list
 | LExpr_BitTuple of expr list * lexpr list
 | LExpr_Tuple of lexpr list
 | LExpr_Array of lexpr * expr (* array assignment *)
 | LExpr_Write of Ident.t * expr list * expr list * bool (* setter procedure call *)
 | LExpr_ReadWrite of Ident.t * Ident.t * expr list * expr list * bool (* read-modify-write function+procedure call *)

type
decl_item =
   DeclItem_Var of Ident.t * ty option
 | DeclItem_Tuple of decl_item list
 | DeclItem_BitTuple of (Ident.t option * ty) list
 | DeclItem_Wildcard of ty option

type
direction =
   Direction_Up
 | Direction_Down

type
catcher =
   Catcher_Guarded of Ident.t * Ident.t * stmt list * l

and stmt =
   Stmt_Block of stmt list * l
 | Stmt_VarDecl of decl_item * expr * l
 | Stmt_ConstDecl of decl_item * expr * l
 | Stmt_Assign of lexpr * expr * l
 | Stmt_FunReturn of expr * l (* function return *)
 | Stmt_ProcReturn of l (* procedure return *)
 | Stmt_Assert of expr * l (* assertion *)
 | Stmt_Throw of expr * l
 | Stmt_TCall of Ident.t * expr list * expr list * bool * l (* procedure call with explicit type parameters *)
 | Stmt_VarDeclsNoInit of Ident.t list * ty * l
 | Stmt_If of expr * stmt list * s_elsif list * (stmt list * l) * l
 | Stmt_Case of expr * alt list * (stmt list * l) option * l
 | Stmt_For of Ident.t * expr * direction * expr * stmt list * l
 | Stmt_While of expr * stmt list * l
 | Stmt_Repeat of stmt list * expr * pos * l
 | Stmt_Try of stmt list * pos * catcher list * (stmt list * l) option * l

and s_elsif =
   S_Elsif_Cond of expr * stmt list * l

and alt =
   Alt_Alt of pattern list * expr option * stmt list * l

type
declaration =
   Decl_BuiltinType of Ident.t * l
 | Decl_Forward of Ident.t * l
 | Decl_Record of Ident.t * Ident.t list * (Ident.t * ty) list * l
 | Decl_Exception of Ident.t * (Ident.t * ty) list * l
 | Decl_Typedef of Ident.t * Ident.t list * ty * l
 | Decl_Enum of Ident.t * Ident.t list * l
 | Decl_Var of Ident.t * ty * l
 | Decl_Const of Ident.t * ty * expr * l
 | Decl_BuiltinFunction of Ident.t * (Ident.t * ty option) list * (Ident.t * ty) list * ty * l
 | Decl_FunType of Ident.t * (Ident.t * ty option) list * (Ident.t * ty) list * ty * l
 | Decl_FunDefn of Ident.t * (Ident.t * ty option) list * (Ident.t * ty) list * ty * stmt list * l
 | Decl_ProcType of Ident.t * (Ident.t * ty option) list * (Ident.t * ty) list * l
 | Decl_ProcDefn of Ident.t * (Ident.t * ty option) list * (Ident.t * ty) list * stmt list * l
 | Decl_VarGetterType of Ident.t * (Ident.t * ty option) list * ty * l
 | Decl_VarGetterDefn of Ident.t * (Ident.t * ty option) list * ty * stmt list * l
 | Decl_ArrayGetterType of Ident.t * (Ident.t * ty option) list * (Ident.t * ty) list * ty * l
 | Decl_ArrayGetterDefn of Ident.t * (Ident.t * ty option) list * (Ident.t * ty) list * ty * stmt list * l
 | Decl_VarSetterType of Ident.t * (Ident.t * ty option) list * Ident.t * ty * l
 | Decl_VarSetterDefn of Ident.t * (Ident.t * ty option) list * Ident.t * ty * stmt list * l
 | Decl_ArraySetterType of Ident.t * (Ident.t * ty option) list * (Ident.t * ty) list * Ident.t * ty * l
 | Decl_ArraySetterDefn of Ident.t * (Ident.t * ty option) list * (Ident.t * ty) list * Ident.t * ty * stmt list * l
 | Decl_Operator1 of unop * Ident.t list * l
 | Decl_Operator2 of binop * Ident.t list * l
 | Decl_Config of Ident.t * ty * expr * l

type
factor =
   Factor_BinOp of binop * expr

type
impdef_command =
   CLI_Impdef of string * expr

let associativeOperators: binop list =
    [ Binop_Plus
    ; Binop_Multiply
    ; Binop_BoolAnd
    ; Binop_BoolOr
    ; Binop_BitOr
    ; Binop_BitEor
    ; Binop_BitAnd
    ; Binop_Append
    ]

(* boolean operators bind least tightly *)
let booleanOperators: binop list =
    [ Binop_BoolAnd
    ; Binop_BoolOr
    ; Binop_BoolIff
    ; Binop_BoolImplies
    ]

(* comparision operators bind less tightly than arithmetic, etc. *)
let comparisionOperators: binop list =
    [ Binop_Eq
    ; Binop_NtEq
    ; Binop_Gt
    ; Binop_GtEq
    ; Binop_Lt
    ; Binop_LtEq
    ]

(* arithmetic and similar operations bind more tightly than comparisions and &&/|| *)
let miscOperators: binop list =
    [ Binop_Plus
    ; Binop_Minus
    ; Binop_Multiply
    ; Binop_Divide
    ; Binop_Power
    ; Binop_Quot
    ; Binop_Rem
    ; Binop_Div
    ; Binop_Mod
    ; Binop_ShiftL
    ; Binop_ShiftR
    ; Binop_BitOr
    ; Binop_BitEor
    ; Binop_BitAnd
    ]

let isAssociative (x: binop): bool = List.mem x associativeOperators
let isBoolean     (x: binop): bool = List.mem x booleanOperators
let isComparision (x: binop): bool = List.mem x comparisionOperators
let isMisc        (x: binop): bool = List.mem x miscOperators

(* Is operator x higher priority than y
 * (Binop_DUMMY acts as the lowest priority operation - see below)
 *)
let higherPriorityThan (x: binop) (y: binop): bool option =
    if                            y = Binop_DUMMY    then Some(true)
    else if x = Binop_Power    && y = Binop_Multiply then Some(true)
    else if x = Binop_Power    && y = Binop_Divide   then Some(true)
    else if x = Binop_Power    && y = Binop_Plus     then Some(true)
    else if x = Binop_Power    && y = Binop_Minus    then Some(true)
    else if x = Binop_Multiply && y = Binop_Plus     then Some(true)
    else if x = Binop_Multiply && y = Binop_Minus    then Some(true)
    else if x = Binop_Plus     && y = Binop_Minus    then Some(true)
    else if isMisc x           && isBoolean y        then Some(true)
    else if isMisc x           && isComparision y    then Some(true)
    else if isComparision x    && isBoolean y        then Some(true)

    else if                       x = Binop_DUMMY    then Some(false)
    else if y = Binop_Power    && x = Binop_Multiply then Some(false)
    else if y = Binop_Power    && x = Binop_Divide   then Some(false)
    else if y = Binop_Power    && x = Binop_Plus     then Some(false)
    else if y = Binop_Power    && x = Binop_Minus    then Some(false)
    else if y = Binop_Multiply && x = Binop_Plus     then Some(false)
    else if y = Binop_Multiply && x = Binop_Minus    then Some(false)
    else if isMisc y           && isBoolean x        then Some(false)
    else if isMisc y           && isComparision x    then Some(false)
    else if isComparision y    && isBoolean x        then Some(false)

    (* The following rules might be a mistake - though they do seem
     * to match common usage.
     *)
    else if x = Binop_Minus    && y = Binop_Plus     then Some(true)
    else if x = Binop_Minus    && y = Binop_Minus    then Some(true)

    else None

(** Parsing exceptions (2/2) *)
exception PrecedenceError of l * binop * binop

(* Support function for parsing expression trees of the form
 *
 *     ... op x op_1 y_1 op_2 y_2 ... op_n y_n
 *
 * Consumes input until it finds an operator y_i of lower precedence
 * than op returning
 *
 * 1) an expression representing "x op_1 ... y_i-1"
 * 2) the remainder if the input "op_i y_i ... op_n y_n"
 *
 * As in Dijkstra's "Shunting Yard" algorithm, we work left to right across
 * the expression comparing the next two operators:
 * - op1 > op2 => (x op1 y1) op2 ...
 * - op1 < op2 => x op1 (y1 op2 ...) ...
 * - op1 = op2 => (x op1 y1) op2 ...     if op1 is associative
 * - _         => error
 *)
let rec buildExpr (op: binop) (x: expr) (ys: factor list) (loc: l): (expr * factor list) =
    ( match ys with
    | [] ->
        (x, [])
    | (Factor_BinOp(op1, y1) :: ys1) ->
        ( match higherPriorityThan op op1 with
        | Some(false) ->
            ( match ys1 with
            | (Factor_BinOp(op2, _) :: _) ->
                ( match higherPriorityThan op1 op2 with
                | Some(true) ->
                    buildExpr op (Expr_Binop(x, op1, y1)) ys1 loc
                | Some(false) ->
                    let (r, rs) = buildExpr op1 y1 ys1 loc in
                    buildExpr op (Expr_Binop(x, op1, r)) rs loc
                | None ->
                    if op1 = op2 && isAssociative(op1) then
                        buildExpr op (Expr_Binop(x, op1, y1)) ys1 loc
                    else
                        raise (PrecedenceError(loc, op1, op2))
                )
            | [] ->
                (Expr_Binop(x, op1, y1), [])
            )
        | _ -> (x, ys)
        )
    )

(* Construct an expression tree based on precedence rules
 *
 * Given parser output of the form  x op_1 y_1 op_2 y_2 ...op_n y_n,
 * construct a tree based on the relative priorities of op1, ... opn.
 * If any adjacent operators op_i, op_i+1 are unordered, report
 * a parsing ambiguity.
 *
 * We use a recursive variant on Dijkstra's Shunting Yard algorithm to
 * parse a list of operator-expression pairs into an expression tree
 * based on operator precedences
 * All operators are treated as left-associative
 *)

let buildExpression (x: expr) (fs: factor list) (loc: l): expr =
    ( match buildExpr Binop_DUMMY x fs loc with
    | (e, []) -> e
    | (_, _) -> raise (Parse_error_locn(loc, "Impossible: unable to resolve precedence"))
    )
