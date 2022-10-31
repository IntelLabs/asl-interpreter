(****************************************************************
 * ASL grammar file
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

%{
open Asl_ast
%}

%token IMPLEMENTATION_UNDERSCORE_DEFINED  (* IMPLEMENTATION_DEFINED *)
%token UNDERSCORE_UNDERSCORE_ARRAY  (* __array *)
%token UNDERSCORE_UNDERSCORE_BUILTIN  (* __builtin *)
%token UNDERSCORE_UNDERSCORE_EVENT  (* __event *)
%token UNDERSCORE_UNDERSCORE_MAP  (* __map *)
%token UNDERSCORE_UNDERSCORE_NEWEVENT  (* __newevent *)
%token UNDERSCORE_UNDERSCORE_NEWMAP  (* __newmap *)
%token UNDERSCORE_UNDERSCORE_OPERATOR_ONE  (* __operator1 *)
%token UNDERSCORE_UNDERSCORE_OPERATOR_TWO  (* __operator2 *)
%token UNDERSCORE_UNDERSCORE_READWRITE  (* __readwrite *)
%token UNDERSCORE_UNDERSCORE_WRITE  (* __write *)

%token COLON  (* : *)
%token COLON_COLON  (* :: *)
%token COMMA  (* , *)
%token DOT  (* . *)
%token DOT_DOT  (* .. *)
%token EQ  (* = *)
%token LBRACE  (* { *)
%token LBRACK  (* [ *)
%token LPAREN  (* ( *)
%token RBRACE  (* } *)
%token RBRACK  (* ] *)
%token RPAREN  (* ) *)
%token SEMICOLON  (* ; *)

%token <string> BITSLIT  (* metavarroot bitsLit *)
%token <string> HEXLIT  (* metavarroot hexLit *)
%token <string> INTLIT  (* metavarroot intLit *)
%token <string> ID  (* metavarroot id *)
%token <string> MASKLIT  (* metavarroot maskLit *)
%token <string> REALLIT  (* metavarroot realLit *)
%token <string> STRINGLIT  (* metavarroot stringLit *)

(* storage tokens *)
%token CONSTANT  (* constant *)
%token CONFIG  (* config *)
%token EQ_GT  (* => *)
%token FUNC  (* func *)
%token GETTER  (* getter *)
%token LET  (* let *)
%token SETTER  (* setter *)
%token VAR  (* var *)

(* type tokens *)
%token ARRAY  (* array *)
%token BITS  (* bits *)
%token ENUMERATION  (* enumeration *)
%token INTEGER  (* integer *)
%token OF  (* of *)
%token RECORD  (* record *)
%token TYPE  (* type *)
%token TYPEOF  (* typeof *)

(* slice tokens *)
%token PLUS_COLON  (* +: *)

(* statement tokens *)
%token ASSERT  (* assert *)
%token BEGIN  (* begin *)
%token CASE  (* case *)
%token CATCH  (* catch *)
%token DO  (* do *)
%token DOWNTO  (* downto *)
%token END  (* end *)
%token FOR  (* for *)
%token IF  (* if *)
%token OTHERWISE  (* otherwise *)
%token REPEAT  (* repeat *)
%token RETURN  (* return *)
%token THEN  (* then *)
%token THROW  (* throw *)
%token TO  (* to *)
%token TRY  (* try *)
%token UNTIL  (* until *)
%token WHEN  (* when *)
%token WHERE  (* where *)
%token WHILE  (* while *)

(* expressions tokens *)
%token AS  (* as *)
%token ELSE  (* else *)
%token ELSIF  (* elsif *)
%token IN  (* IN *)
%token UNKNOWN  (* UNKNOWN *)

(* binop tokens *)
%token AMPERSAND_AMPERSAND  (* && *)
%token AND  (* AND *)
%token BANG_EQ  (* != *)
%token BAR_BAR  (* || *)
%token CARET  (* ^ *)
%token DIV  (* DIV *)
%token EQ_EQ  (* == *)
%token EOR  (* EOR *)
%token GT  (* > *)
%token GT_EQ  (* >= *)
%token GT_GT  (* >> *)
%token LT  (* < *)
%token LT_EQ  (* <= *)
%token LT_LT  (* << *)
%token LT_MINUS_GT  (* <-> *)
%token MINUS  (* - *)
%token MINUS_MINUS_GT  (* --> *)
%token MOD  (* MOD *)
%token OR  (* OR *)
%token PLUS  (* + *)
%token PLUS_PLUS  (* ++ *)
%token QUOT  (* QUOT *)
%token REM  (* REM *)
%token SLASH  (* / *)
%token STAR  (* * *)

(* unop tokens *)
%token BANG  (* ! *)
%token NOT  (* NOT *)

%token EOF

%start <Asl_ast.declaration list> declarations_start
%start <Asl_ast.expr> expr_command_start
%start <Asl_ast.stmt> stmt_command_start
%start <Asl_ast.impdef_command> impdef_command_start


%%

declarations_start:
| declarations = declarations EOF { declarations }

expr_command_start:
| expr_command = expr_command EOF { expr_command }

stmt_command_start:
| stmt_command = stmt_command EOF { stmt_command }

impdef_command_start:
| impdef_command = impdef_command EOF { impdef_command }

pos:
| { $symbolstartpos }

ident:
| id = ID { Ident id }

declarations:
| declaration0 = list(declaration) { declaration0 }

declaration:
| type_declaration = type_declaration { type_declaration }
| variable_declaration = variable_declaration { variable_declaration }
| function_declaration = function_declaration { function_declaration }
| procedure_declaration = procedure_declaration { procedure_declaration }
| getter_declaration = getter_declaration { getter_declaration }
| setter_declaration = setter_declaration { setter_declaration }
| internal_definition = internal_definition { internal_definition }

type_declaration:
| UNDERSCORE_UNDERSCORE_BUILTIN TYPE v = ident SEMICOLON
    { Decl_BuiltinType(v, Range($symbolstartpos, $endpos)) }
| TYPE v = ident SEMICOLON
    { Decl_Forward(v, Range($symbolstartpos, $endpos)) }
| RECORD v = ident LBRACE fs = nonempty_list(field) RBRACE SEMICOLON
    { Decl_Record(v, fs, Range($symbolstartpos, $endpos)) }
| TYPE v = ident OF ty = ty SEMICOLON
    { Decl_Typedef(v, ty, Range($symbolstartpos, $endpos)) }
| ENUMERATION v = ident LBRACE es = separated_list(COMMA, ident) RBRACE SEMICOLON
    { Decl_Enum(v, es, Range($symbolstartpos, $endpos)) }

field:
| ident = ident COLON_COLON ty = ty SEMICOLON { (ident, ty) }

variable_declaration:
| VAR v = ident COLON_COLON ty = ty SEMICOLON
    { Decl_Var(v, ty, Range($symbolstartpos, $endpos)) }
| CONSTANT v = ident COLON_COLON ty = ty EQ e = expr SEMICOLON
    { Decl_Const(v, ty, e, Range($symbolstartpos, $endpos)) }
| LET v = ident COLON_COLON ty = ty EQ e = expr SEMICOLON
    { Decl_Const(v, ty, e, Range($symbolstartpos, $endpos)) }

ixtype:
| ident = ident { Index_Enum(ident) }
| expr = expr { Index_Int(expr) }

function_declaration:
| UNDERSCORE_UNDERSCORE_BUILTIN FUNC f = ident ps = parameters_opt LPAREN args = formal_list RPAREN EQ_GT ty = ty SEMICOLON
    { Decl_BuiltinFunction(f, ps, args, ty, Range($symbolstartpos, $endpos)) }
| FUNC f = ident ps = parameters_opt LPAREN args = formal_list RPAREN EQ_GT ty = ty SEMICOLON
    { Decl_FunType(f, ps, args, ty, Range($symbolstartpos, $endpos)) }
| FUNC f = ident ps = parameters_opt LPAREN args = formal_list RPAREN EQ_GT ty = ty b = block END
    { Decl_FunDefn(f, ps, args, ty, b, Range($symbolstartpos, $endpos)) }

procedure_declaration:
| FUNC f = ident ps = parameters_opt LPAREN args = formal_list RPAREN SEMICOLON
    { Decl_ProcType(f, ps, args, Range($symbolstartpos, $endpos)) }
| FUNC f = ident ps = parameters_opt LPAREN args = formal_list RPAREN b = block END
    { Decl_ProcDefn(f, ps, args, b, Range($symbolstartpos, $endpos)) }

parameters_opt:
| LBRACE pars = parameter_list RBRACE { pars }
| { [] }

parameter_list:
| pars = separated_nonempty_list(COMMA, parameter) { pars }

parameter:
| par = ident ty = ty_opt { (par, ty) }

ty_opt:
| COLON_COLON ty = ty { Some ty }
| { None }

formal_list:
| formal0 = separated_list(COMMA, formal) { formal0 }

formal:
| ident = ident COLON_COLON ty = ty { (ident, ty) }

getter_declaration:
| GETTER f = ident ps = parameters_opt EQ_GT ty = ty SEMICOLON
    { Decl_VarGetterType(f, ps, ty, Range($symbolstartpos, $endpos)) }
| GETTER f = ident ps = parameters_opt EQ_GT ty = ty b = block END
    { Decl_VarGetterDefn(f, ps, ty, b, Range($symbolstartpos, $endpos)) }
| GETTER f = ident ps = parameters_opt LBRACK args = formal_list RBRACK EQ_GT ty = ty SEMICOLON
    { Decl_ArrayGetterType(f, ps, args, ty, Range($symbolstartpos, $endpos)) }
| GETTER f = ident ps = parameters_opt LBRACK args = formal_list RBRACK EQ_GT ty = ty b = block END
    { Decl_ArrayGetterDefn(f, ps, args, ty, b, Range($symbolstartpos, $endpos)) }

setter_declaration:
| SETTER f = ident ps = parameters_opt EQ v = ident COLON_COLON ty = ty SEMICOLON
    { Decl_VarSetterType(f, ps, v, ty, Range($symbolstartpos, $endpos)) }
| SETTER f = ident ps = parameters_opt EQ v = ident COLON_COLON ty = ty b = block END
    { Decl_VarSetterDefn(f, ps, v, ty, b, Range($symbolstartpos, $endpos)) }
| SETTER f = ident ps = parameters_opt LBRACK args = formal_list RBRACK EQ v = ident COLON_COLON ty = ty SEMICOLON
    { Decl_ArraySetterType(f, ps, args, v, ty, Range($symbolstartpos, $endpos)) }
| SETTER f = ident ps = parameters_opt LBRACK args = formal_list RBRACK EQ v = ident COLON_COLON ty = ty b = block END
    { Decl_ArraySetterDefn(f, ps, args, v, ty, b, Range($symbolstartpos, $endpos)) }

internal_definition:
| UNDERSCORE_UNDERSCORE_OPERATOR_ONE op = unop EQ vs = separated_nonempty_list(COMMA, ident) SEMICOLON
    { Decl_Operator1(op, vs, Range($symbolstartpos, $endpos)) }
| UNDERSCORE_UNDERSCORE_OPERATOR_TWO op = binop EQ vs = separated_nonempty_list(COMMA, ident) SEMICOLON
    { Decl_Operator2(op, vs, Range($symbolstartpos, $endpos)) }
| UNDERSCORE_UNDERSCORE_NEWEVENT v = ident ps = parameters_opt LPAREN args = formal_list RPAREN SEMICOLON
    { Decl_NewEventDefn(v, ps, args, Range($symbolstartpos, $endpos)) }
| UNDERSCORE_UNDERSCORE_EVENT v = ident b = block END
    { Decl_EventClause(v, b, Range($symbolstartpos, $endpos)) }
| UNDERSCORE_UNDERSCORE_NEWMAP v = ident ps = parameters_opt LPAREN args = formal_list RPAREN EQ_GT ty = ty b = block END
    { Decl_NewMapDefn(v, ps, args, ty, b, Range($symbolstartpos, $endpos)) }
| UNDERSCORE_UNDERSCORE_MAP v = ident vs = separated_list(COMMA, mapfield) oc = optmapcond THEN b = block END
    { Decl_MapClause(v, vs, oc, b, Range($symbolstartpos, $endpos)) }
| CONFIG v = ident COLON_COLON ty = ty EQ e = expr SEMICOLON
    { Decl_Config(v, ty, e, Range($symbolstartpos, $endpos)) }

operator:
| op = unop { Utils.to_string (Asl_parser_pp.pp_unop op) }
| op = binop  { Utils.to_string (Asl_parser_pp.pp_binop op) }
| COLON { ":" }

optmapcond:
| WHEN expr = expr { Some(expr) }
| { None }

mapfield:
| f = ident EQ p = pattern { MapField_Field(f, p) }

ty:
| ident = ident
    { Type_Constructor(ident) }
| INTEGER ocrs = constraint_opt
    { Type_Integer(ocrs) }
| BITS LPAREN n = expr RPAREN
    { Type_Bits(n) }
| tc = ident LPAREN es = separated_nonempty_list(COMMA, expr) RPAREN
    { Type_App(tc, es) }
| TYPEOF LPAREN e = expr RPAREN
    { Type_OfExpr(e) }
| BITS LPAREN wd = expr RPAREN LBRACE fs = regfields RBRACE
    { Type_Register(wd, fs) }
| ARRAY LBRACK ixtype = ixtype RBRACK OF ty = ty
    { Type_Array(ixtype, ty) }
| LPAREN tys = separated_list(COMMA, ty) RPAREN
    { Type_Tuple(tys) }

constraint_opt:
| crs = constraints { Some crs }
| { None }

constraints:
| LBRACE crs = separated_nonempty_list(COMMA, constraint_range) RBRACE
    { crs }

constraint_range:
| c = expr { Constraint_Single(c) }
| c1 = expr DOT_DOT c2 = expr { Constraint_Range(c1, c2) }

regfields:
| fs = list(regfield) { fs }
| f = regfield COMMA fs = regfields { f :: fs }

regfield:
| LBRACK slices = separated_nonempty_list(COMMA, slice) RBRACK ident = ident
    { (slices, ident) }

stmt:
| simple_stmt = simple_stmt { simple_stmt }
| compound_stmt = compound_stmt { compound_stmt }

compound_stmt:
| conditional_stmt = conditional_stmt { conditional_stmt }
| repetitive_stmt = repetitive_stmt { repetitive_stmt }
| catch_stmt = catch_stmt { catch_stmt }
| BEGIN block = block END { Stmt_Block(block, Range($symbolstartpos, $endpos)) }

block:
| stmts = list(stmt) { stmts }

assignment_stmt:
| VAR v = ident COMMA vs = separated_list(COMMA, ident) COLON_COLON ty = ty SEMICOLON
    { Stmt_VarDeclsNoInit(v :: vs, ty, Range($symbolstartpos, $endpos)) }
| VAR v = ident COLON_COLON ty = ty SEMICOLON
    { Stmt_VarDeclsNoInit([v], ty, Range($symbolstartpos, $endpos)) }
| VAR dis = decl_item EQ i = expr SEMICOLON
    { Stmt_VarDecl(dis, i, Range($symbolstartpos, $endpos)) }
| LET dis = decl_item EQ i = expr SEMICOLON
    { Stmt_ConstDecl(dis, i, Range($symbolstartpos, $endpos)) }
| l = lexpr EQ r = expr SEMICOLON
    { Stmt_Assign(l, r, Range($symbolstartpos, $endpos)) }

decl_item:
| v = ident  oty = ty_opt
    { DeclItem_Var(v, oty) }
| LPAREN dis = separated_nonempty_list(COMMA, decl_item) RPAREN
    { DeclItem_Tuple(dis) }
| MINUS oty = ty_opt
    { DeclItem_Wildcard(oty) }

lexpr:
| MINUS
    { LExpr_Wildcard }
| v = ident
    { LExpr_Var(v) }
| e = lexpr DOT f = ident
    { LExpr_Field(e, f) }
| e = lexpr DOT LBRACK fs = separated_nonempty_list(COMMA, ident) RBRACK
    { LExpr_Fields(e, fs) }
| e = lexpr LBRACK ss = separated_list(COMMA, slice) RBRACK
    { LExpr_Slices(e, ss) }
| LBRACK es = separated_nonempty2_list(COMMA, lexpr) RBRACK
    { LExpr_BitTuple(es) }
| LPAREN es = separated_nonempty2_list(COMMA, lexpr) RPAREN
    { LExpr_Tuple(es) }
| LPAREN e = lexpr RPAREN
    { e }

lexpr_spice:
| UNDERSCORE_UNDERSCORE_ARRAY a = lexpr LBRACK e = expr RBRACK
    { LExpr_Array(a, e) }
| UNDERSCORE_UNDERSCORE_WRITE f = ident LBRACE tes = separated_list(COMMA, expr) RBRACE LBRACK es = separated_list(COMMA, expr) RBRACK
    { LExpr_Write(f, tes, es) }
| UNDERSCORE_UNDERSCORE_READWRITE f = ident g = ident LBRACE tes = separated_list(COMMA, expr) RBRACE LBRACK es = separated_list(COMMA, expr) RBRACK
    { LExpr_ReadWrite(f, g, tes, es) }

simple_stmt:
| assignment_stmt = assignment_stmt
    { assignment_stmt }
| f = ident LPAREN args = separated_list(COMMA, expr) RPAREN SEMICOLON
    { Stmt_TCall(f, [], args, Range($symbolstartpos, $endpos)) }
| RETURN e = expr SEMICOLON
    { Stmt_FunReturn(e, Range($symbolstartpos, $endpos)) }
| RETURN SEMICOLON
    { Stmt_ProcReturn(Range($symbolstartpos, $endpos)) }
| ASSERT e = expr SEMICOLON
    { Stmt_Assert(e, Range($symbolstartpos, $endpos)) }
| THROW v = ident SEMICOLON
    { Stmt_Throw(v, Range($symbolstartpos, $endpos)) }

stmt_spice:
| f = ident LBRACE tes = separated_list(COMMA, expr) RBRACE LPAREN args = separated_list(COMMA, expr) RPAREN SEMICOLON
    { Stmt_TCall(f, tes, args) }
| VAR vs = list(ident) COLON_COLON ty = ty SEMICOLON
    { Stmt_VarDeclsNoInit(vs, ty) }

conditional_stmt:
| IF c = expr THEN t = block els = list(s_elsif) f = optional_else END
    { Stmt_If(c, t, els, f, Range($symbolstartpos, $endpos)) }
| CASE e = expr OF alts = nonempty_list(alt) ob = opt_otherwise END
    { Stmt_Case(e, alts, ob, Range($symbolstartpos, $endpos)) }

s_elsif:
| ELSIF c = expr THEN b = block
    { S_Elsif_Cond(c, b, Range($symbolstartpos, $endpos)) }

optional_else:
| ELSE b = block { (b, Range($symbolstartpos, $endpos)) }
| { ([], Range($symbolstartpos, $endpos)) }

alt:
| WHEN ps = separated_nonempty_list(COMMA, pattern) oalt = opt_altcond COLON b = block
    { Alt_Alt(ps, oalt, b, Range($symbolstartpos, $endpos)) }

opt_otherwise:
| OTHERWISE COLON b = block { Some(b, Range($symbolstartpos, $endpos)) }
| { None }

opt_altcond:
| WHERE e = expr { Some(e) }
| { None }

pattern:
| i = INTLIT { Pat_LitInt(i) }
| h = HEXLIT { Pat_LitHex(h) }
| b = BITSLIT { Pat_LitBits(b) }
| m = MASKLIT { Pat_LitMask(m) }
| c = ident { Pat_Const(c) }
| MINUS { Pat_Wildcard }
| LPAREN ps = separated_nonempty2_list(COMMA, pattern) RPAREN { Pat_Tuple(ps) }
| LBRACE aps = separated_list(COMMA, apattern) RBRACE { Pat_Set(aps) }

apattern:
| p1 = expr DOT_DOT p2 = expr { Pat_Range(p1, p2) }
| p = expr { Pat_Single(p) }

repetitive_stmt:
| FOR v = ident EQ f = expr dir = direction t = expr DO b = block END
    { Stmt_For(v, f, dir, t, b, Range($symbolstartpos, $endpos)) }
| WHILE c = expr DO b = block END
    { Stmt_While(c, b, Range($symbolstartpos, $endpos)) }
| REPEAT b = block UNTIL c = expr SEMICOLON pos = pos
    { Stmt_Repeat(b, c, pos, Range($symbolstartpos, $endpos)) }

direction:
| TO { Direction_Up }
| DOWNTO { Direction_Down }

catch_stmt:
| TRY b = block CATCH v = ident pos = pos cs = list(catcher) ob = opt_otherwise END
    { Stmt_Try(b, v, pos, cs, ob, Range($symbolstartpos, $endpos)) }

catcher:
| WHEN c = expr COLON b = block
    { Catcher_Guarded(c, b, Range($symbolstartpos, $endpos)) }

expr:
| ce = conditional_expression { ce }

conditional_expression:
| IF c = cexpr THEN t = expr els = list(e_elsif) ELSE e = expr
    { Expr_If(c, t, els, e) }
| cexpr = cexpr { cexpr }

e_elsif:
| ELSIF c = expr THEN e = expr { E_Elsif_Cond(c, e) }

cexpr:
| bexpr = bexpr fs = list(factor)
    { buildExpression bexpr fs (Range($startpos(bexpr), $endpos(fs))) }

zexpr:
| a = expr op = binop b = expr { Expr_Binop(a, op, b) }

factor:
| op = binop e = bexpr { Factor_BinOp(op, e) }

binop:
| EQ_EQ { Binop_Eq }
| BANG_EQ { Binop_NtEq }
| GT { Binop_Gt }
| GT_EQ { Binop_GtEq }
| LT { Binop_Lt }
| LT_EQ { Binop_LtEq }
| PLUS { Binop_Plus }
| MINUS { Binop_Minus }
| STAR { Binop_Multiply }
| SLASH { Binop_Divide }
| CARET { Binop_Power }
| QUOT { Binop_Quot }
| REM { Binop_Rem }
| DIV { Binop_Div }
| MOD { Binop_Mod }
| LT_LT { Binop_ShiftL }
| GT_GT { Binop_ShiftR }
| AMPERSAND_AMPERSAND { Binop_BoolAnd }
| BAR_BAR { Binop_BoolOr }
| LT_MINUS_GT { Binop_BoolIff }
| MINUS_MINUS_GT { Binop_BoolImplies }
| OR { Binop_BitOr }
| EOR { Binop_BitEor }
| AND { Binop_BitAnd }
| PLUS_PLUS { Binop_Append }

dummy_binop:
| { Binop_DUMMY }

bexpr:
| op = unop e = fexpr { Expr_Unop(op, e) }
| e = fexpr { e }

fexpr:
| e = fexpr DOT f = ident
    { Expr_Field(e, f) }
| e = fexpr DOT LBRACK fs = separated_nonempty_list(COMMA, ident) RBRACK
    { Expr_Fields(e, fs) }
| e = fexpr LBRACK ss = separated_list(COMMA, slice) RBRACK
    { Expr_Slices(e, ss) }
| tc = ident LBRACE fas = separated_nonempty_list(COMMA, field_assignment) RBRACE
    { Expr_RecordInit(tc, fas) }
| e = fexpr IN p = pattern
    { Expr_In(e, p) }
| e = aexpr
    { e }

aexpr:
| literal_expression = literal_expression
    { literal_expression }
| v = ident
    { Expr_Var(v) }
| f = ident LPAREN es = separated_list(COMMA, expr) RPAREN
    { Expr_TApply(f, [], es) }
| LPAREN e = expr RPAREN
    { Expr_Parens(e) }
| LPAREN es = separated_nonempty2_list(COMMA, expr) RPAREN
    { Expr_Tuple(es) }
| LBRACK es = separated_nonempty2_list(COMMA, expr) RBRACK
    { Expr_Concat([], es) }
| UNKNOWN COLON_COLON t = ty
    { Expr_Unknown(t) }
| IMPLEMENTATION_UNDERSCORE_DEFINED os = opt_stringLit COLON_COLON t = ty
    { Expr_ImpDef(os, t) }
| e = aexpr AS c = constraints
    { Expr_AsConstraint(e, c) }
| e = aexpr AS t = ty
    { Expr_AsType(e, t) }

expr_spice:
| f = ident LBRACE tes = separated_list(COMMA, expr) RBRACE LPAREN es = separated_list(COMMA, expr) RPAREN
    { Expr_TApply(f, tes, es) }
| LBRACE ws = separated_list(COMMA, expr) RBRACE LBRACK es = separated_nonempty2_list(COMMA, expr) RBRACK
    { Expr_Concat(ws, es) }
| UNDERSCORE_UNDERSCORE_ARRAY a = expr LBRACK e = expr RBRACK
    { Expr_Array(a, e) }

field_assignment:
| ident = ident EQ expr = expr { (ident, expr) }

opt_stringLit:
| stringLit = STRINGLIT { Some(stringLit) }
| { None }

unop:
| MINUS { Unop_Negate }
| BANG { Unop_BoolNot }
| NOT { Unop_BitsNot }

slice:
| e = expr  { Slice_Single(e) }
| hi = expr COLON lo = expr { Slice_HiLo(hi, lo) }
| lo = expr PLUS_COLON wd = expr { Slice_LoWd(lo, wd) }

literal_expression:
| i = INTLIT { Expr_LitInt(i) }
| h = HEXLIT { Expr_LitHex(h) }
| r = REALLIT { Expr_LitReal(r) }
| b = BITSLIT { Expr_LitBits(b) }
| m = MASKLIT { Expr_LitMask(m) }
| s = STRINGLIT { Expr_LitString(s) }

expr_command:
| expr = expr { expr }

stmt_command:
| stmt = stmt { stmt }

impdef_command:
| s = STRINGLIT EQ e = expr { CLI_Impdef(s, e) }

/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Paris-Rocquencourt                            */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2015 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the GNU Library General Public License, with the   */
/*  special exception on linking described in file LICENSE.               */
/*                                                                        */
/**************************************************************************/

/* nonempty2 variants of the menhir standard library lists, Peter Sewell, 2017-05 */

(* [separated_nonempty2_list(separator, X)] recognizes list of
   two or more [X]'s, separated with [separator]'s. It produces a value of type
   ['a list] if [X] produces a value of type ['a]. The front element
   of the list is the first element that was parsed. *)

%public separated_nonempty2_list(separator, X):
  x1 = X; separator; x2 = X
    { [ x1; x2 ] }
| x = X; separator; xs = separated_nonempty2_list(separator, X)
    { x :: xs }
