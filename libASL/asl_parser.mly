%{
open Asl_ast
%}

%token UNDERSCORE_UNDERSCORE_OPERATOR_ONE  (* __operator1 *)
%token UNDERSCORE_UNDERSCORE_OPERATOR_TWO  (* __operator2 *)
%token IMPLEMENTATION_UNDERSCORE_DEFINED  (* IMPLEMENTATION_DEFINED *)
%token UNDERSCORE_UNDERSCORE_READWRITE  (* __readwrite *)
%token UNDERSCORE_UNDERSCORE_NEWEVENT  (* __newevent *)
%token UNDERSCORE_UNDERSCORE_BUILTIN  (* __builtin *)
%token UNDERSCORE_UNDERSCORE_NEWMAP  (* __newmap *)
%token UNDERSCORE_UNDERSCORE_ARRAY  (* __array *)
%token UNDERSCORE_UNDERSCORE_EVENT  (* __event *)
%token UNDERSCORE_UNDERSCORE_WRITE  (* __write *)
%token UNDERSCORE_UNDERSCORE_MAP  (* __map *)
%token AMPERSAND_AMPERSAND  (* && *)
%token MINUS_MINUS_GT  (* --> *)
%token COLON_COLON  (* :: *)
%token ENUMERATION  (* enumeration *)
%token LT_MINUS_GT  (* <-> *)
%token PLUS_COLON  (* +: *)
%token OTHERWISE  (* otherwise *)
%token PLUS_PLUS  (* ++ *)
%token SEMICOLON  (* ; *)
%token CONSTANT  (* constant *)
%token BANG_EQ  (* != *)
%token BAR_BAR  (* || *)
%token DOT_DOT  (* .. *)
%token INTEGER  (* integer *)
%token UNKNOWN  (* UNKNOWN *)
%token ASSERT  (* assert *)
%token CONFIG  (* config *)
%token DOWNTO  (* downto *)
%token GETTER  (* getter *)
%token LBRACE  (* { *)
%token LBRACK  (* [ *)
%token LPAREN  (* ( *)
%token RBRACE  (* } *)
%token RBRACK  (* ] *)
%token RECORD  (* record *)
%token REPEAT  (* repeat *)
%token RETURN  (* return *)
%token RPAREN  (* ) *)
%token SETTER  (* setter *)
%token TYPEOF  (* typeof *)
%token ARRAY  (* array *)
%token BEGIN  (* begin *)
%token CARET  (* ^ *)
%token CATCH  (* catch *)
%token COLON  (* : *)
%token COMMA  (* , *)
%token ELSIF  (* elsif *)
%token EQ_EQ  (* == *)
%token EQ_GT  (* => *)
%token GT_EQ  (* >= *)
%token GT_GT  (* >> *)
%token LT_EQ  (* <= *)
%token LT_LT  (* << *)
%token MINUS  (* - *)
%token SLASH  (* / *)
%token THROW  (* throw *)
%token UNTIL  (* until *)
%token WHERE  (* where *)
%token WHILE  (* while *)
%token BANG  (* ! *)
%token BITS  (* bits *)
%token CASE  (* case *)
%token ELSE  (* else *)
%token FUNC  (* func *)
%token PLUS  (* + *)
%token QUOT  (* QUOT *)
%token STAR  (* * *)
%token THEN  (* then *)
%token TYPE  (* type *)
%token WHEN  (* when *)
%token AND  (* AND *)
%token DIV  (* DIV *)
%token DOT  (* . *)
%token END  (* end *)
%token EOR  (* EOR *)
%token FOR  (* for *)
%token LET  (* let *)
%token MOD  (* MOD *)
%token NOT  (* NOT *)
%token REM  (* REM *)
%token TRY  (* try *)
%token VAR  (* var *)
%token AS  (* as *)
%token DO  (* do *)
%token EQ  (* = *)
%token GT  (* > *)
%token IF  (* if *)
%token IN  (* IN *)
%token LT  (* < *)
%token OF  (* of *)
%token OR  (* OR *)
%token TO  (* to *)
%token <string> STRINGLIT  (* metavarroot stringLit *)
%token <string> BITSLIT  (* metavarroot bitsLit *)
%token <string> MASKLIT  (* metavarroot maskLit *)
%token <string> REALLIT  (* metavarroot realLit *)
%token <string> HEXLIT  (* metavarroot hexLit *)
%token <string> INTLIT  (* metavarroot intLit *)
%token <string> ID  (* metavarroot id *)
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
| UNDERSCORE_UNDERSCORE_BUILTIN TYPE ident = ident SEMICOLON
    { Decl_BuiltinType(ident,Range($symbolstartpos,$endpos)) }
| TYPE ident = ident SEMICOLON
    { Decl_Forward(ident,Range($symbolstartpos,$endpos)) }
| RECORD ident = ident LBRACE  field0 = nonempty_list(field) RBRACE SEMICOLON
    { Decl_Record(ident,field0,Range($symbolstartpos,$endpos)) }
| TYPE ident = ident OF ty = ty SEMICOLON
    { Decl_Typedef(ident,ty,Range($symbolstartpos,$endpos)) }
| ENUMERATION ident = ident LBRACE ident0 = separated_list(COMMA,ident) RBRACE SEMICOLON
    { Decl_Enum(ident,ident0,Range($symbolstartpos,$endpos)) }

field:
| ident = ident COLON_COLON ty = ty SEMICOLON { (ident, ty) }

variable_declaration:
| VAR ident = ident COLON_COLON ty = ty SEMICOLON
    { Decl_Var(ident,ty,Range($symbolstartpos,$endpos)) }
| CONSTANT ident = ident COLON_COLON ty = ty EQ expr = expr SEMICOLON
    { Decl_Const(ident,ty,expr,Range($symbolstartpos,$endpos)) }
| LET ident = ident COLON_COLON ty = ty EQ expr = expr SEMICOLON
    { Decl_Const(ident, ty, expr, Range($symbolstartpos,$endpos)) }

ixtype:
| ident = ident { Index_Enum(ident) }
| expr = expr { Index_Int(expr) }

function_declaration:
| UNDERSCORE_UNDERSCORE_BUILTIN FUNC ident = ident parameters_opt = parameters_opt LPAREN formal_list = formal_list RPAREN EQ_GT ty = ty SEMICOLON
    { Decl_BuiltinFunction(ident,parameters_opt,formal_list,ty,Range($symbolstartpos,$endpos)) }
| FUNC ident = ident parameters_opt = parameters_opt LPAREN formal_list = formal_list RPAREN EQ_GT ty = ty SEMICOLON
    { Decl_FunType(ident,parameters_opt,formal_list,ty,Range($symbolstartpos,$endpos)) }
| FUNC ident = ident parameters_opt = parameters_opt LPAREN formal_list = formal_list RPAREN EQ_GT ty = ty block = block END
    { Decl_FunDefn(ident,parameters_opt,formal_list,ty,block,Range($symbolstartpos,$endpos)) }

procedure_declaration:
| FUNC ident = ident parameters_opt = parameters_opt LPAREN formal_list = formal_list RPAREN SEMICOLON
    { Decl_ProcType(ident,parameters_opt,formal_list,Range($symbolstartpos,$endpos)) }
| FUNC ident = ident parameters_opt = parameters_opt LPAREN formal_list = formal_list RPAREN block = block END
    { Decl_ProcDefn(ident,parameters_opt,formal_list,block,Range($symbolstartpos,$endpos)) }

parameters_opt:
| LBRACE parameter_list = parameter_list RBRACE { parameter_list }
| { [] }

parameter_list:
| parameter0 = separated_nonempty_list(COMMA,parameter) { parameter0 }

parameter:
| ident = ident ty_opt = ty_opt { (ident, ty_opt) }

ty_opt:
| COLON_COLON ty = ty { Some ty }
| { None }

formal_list:
| formal0 = separated_list(COMMA,formal) { formal0 }

formal:
| ident = ident COLON_COLON ty = ty { (ident, ty) }

getter_declaration:
| GETTER ident = ident parameters_opt = parameters_opt EQ_GT ty = ty SEMICOLON
    { Decl_VarGetterType(ident,parameters_opt,ty,Range($symbolstartpos,$endpos)) }
| GETTER ident = ident parameters_opt = parameters_opt EQ_GT ty = ty block = block END
    { Decl_VarGetterDefn(ident,parameters_opt,ty,block,Range($symbolstartpos,$endpos)) }
| GETTER ident = ident parameters_opt = parameters_opt LBRACK formal_list = formal_list RBRACK EQ_GT ty = ty SEMICOLON
    { Decl_ArrayGetterType(ident,parameters_opt,formal_list,ty,Range($symbolstartpos,$endpos)) }
| GETTER ident = ident parameters_opt = parameters_opt LBRACK formal_list = formal_list RBRACK EQ_GT ty = ty block = block END
    { Decl_ArrayGetterDefn(ident,parameters_opt,formal_list,ty,block,Range($symbolstartpos,$endpos)) }

setter_declaration:
| SETTER ident1 = ident parameters_opt = parameters_opt EQ ident2 = ident COLON_COLON ty = ty SEMICOLON
    { Decl_VarSetterType(ident1,parameters_opt,ident2,ty,Range($symbolstartpos,$endpos)) }
| SETTER ident1 = ident parameters_opt = parameters_opt EQ ident2 = ident COLON_COLON ty = ty block = block END
    { Decl_VarSetterDefn(ident1,parameters_opt,ident2,ty,block,Range($symbolstartpos,$endpos)) }
| SETTER ident1 = ident parameters_opt = parameters_opt LBRACK formal_list = formal_list RBRACK EQ ident2 = ident COLON_COLON ty = ty SEMICOLON
    { Decl_ArraySetterType(ident1,parameters_opt,formal_list,ident2,ty,Range($symbolstartpos,$endpos)) }
| SETTER ident1 = ident parameters_opt = parameters_opt LBRACK formal_list = formal_list RBRACK EQ ident2 = ident COLON_COLON ty = ty block = block END
    { Decl_ArraySetterDefn(ident1,parameters_opt,formal_list,ident2,ty,block,Range($symbolstartpos,$endpos)) }

internal_definition:
| UNDERSCORE_UNDERSCORE_OPERATOR_ONE unop = unop EQ ident0 = separated_nonempty_list(COMMA,ident) SEMICOLON
    { Decl_Operator1(unop,ident0,Range($symbolstartpos,$endpos)) }
| UNDERSCORE_UNDERSCORE_OPERATOR_TWO binop = binop EQ ident0 = separated_nonempty_list(COMMA,ident) SEMICOLON
    { Decl_Operator2(binop,ident0,Range($symbolstartpos,$endpos)) }
| UNDERSCORE_UNDERSCORE_NEWEVENT ident = ident parameters_opt = parameters_opt LPAREN formal_list = formal_list RPAREN SEMICOLON
    { Decl_NewEventDefn(ident,parameters_opt,formal_list,Range($symbolstartpos,$endpos)) }
| UNDERSCORE_UNDERSCORE_EVENT ident = ident block = block END
    { Decl_EventClause(ident,block,Range($symbolstartpos,$endpos)) }
| UNDERSCORE_UNDERSCORE_NEWMAP ident = ident parameters_opt = parameters_opt LPAREN formal_list = formal_list RPAREN EQ_GT ty = ty block = block END
    { Decl_NewMapDefn(ident,parameters_opt,formal_list,ty,block,Range($symbolstartpos,$endpos)) }
| UNDERSCORE_UNDERSCORE_MAP ident = ident mapfield0 = separated_list(COMMA,mapfield) optmapcond = optmapcond THEN block = block END
    { Decl_MapClause(ident,mapfield0,optmapcond,block,Range($symbolstartpos,$endpos)) }
| CONFIG ident = ident COLON_COLON ty = ty EQ expr = expr SEMICOLON
    { Decl_Config(ident,ty,expr,Range($symbolstartpos,$endpos)) }

operator:
| unop = unop { Utils.to_string (Asl_parser_pp.pp_unop unop) }
| binop = binop  { Utils.to_string (Asl_parser_pp.pp_binop binop) }
| COLON { ":" }

optmapcond:
| WHEN expr = expr { Some(expr) }
| { None }

mapfield:
| ident = ident EQ pattern = pattern { MapField_Field(ident,pattern) }

ty:
| ident = ident
    { Type_Constructor(ident) }
| INTEGER constraint_opt = constraint_opt
    { Type_Integer(constraint_opt) }
| BITS LPAREN expr = expr RPAREN
    { Type_Bits(expr) }
| ident = ident LPAREN expr0 = separated_nonempty_list(COMMA,expr) RPAREN
    { Type_App(ident,expr0) }
| TYPEOF LPAREN expr = expr RPAREN
    { Type_OfExpr(expr) }
| BITS LPAREN expr = expr RPAREN LBRACE regfields = regfields RBRACE
    { Type_Register(expr,regfields) }
| ARRAY LBRACK ixtype = ixtype RBRACK OF ty = ty
    { Type_Array(ixtype,ty) }
| LPAREN ty0 = separated_list(COMMA,ty) RPAREN
    { Type_Tuple(ty0) }

constraint_opt:
| constraints = constraints { Some constraints }
| { None }

constraints:
| LBRACE constraint_range0 = separated_nonempty_list(COMMA,constraint_range) RBRACE
    { constraint_range0 }

constraint_range:
| expr = expr { Constraint_Single(expr) }
| expr1 = expr DOT_DOT expr2 = expr { Constraint_Range(expr1,expr2) }

regfields:
| regfield0 = list(regfield) { regfield0 }
| regfield = regfield COMMA regfields = regfields { regfield :: regfields }

regfield:
| LBRACK slice0 = separated_nonempty_list(COMMA,slice) RBRACK ident = ident
    { (slice0, ident) }

stmt:
| simple_stmt = simple_stmt { simple_stmt }
| compound_stmt = compound_stmt { compound_stmt }

compound_stmt:
| conditional_stmt = conditional_stmt { conditional_stmt }
| repetitive_stmt = repetitive_stmt { repetitive_stmt }
| catch_stmt = catch_stmt { catch_stmt }
| BEGIN block = block END { Stmt_Block(block,Range($symbolstartpos,$endpos)) }

block:
| stmt0 = list(stmt) { stmt0 }

assignment_stmt:
| VAR ident = ident COMMA ident0 = separated_list(COMMA,ident) COLON_COLON ty = ty SEMICOLON
    { Stmt_VarDeclsNoInit(ident :: ident0,ty,Range($symbolstartpos,$endpos)) }
| VAR ident = ident COLON_COLON ty = ty SEMICOLON
    { Stmt_VarDeclsNoInit([ident],ty,Range($symbolstartpos, $endpos)) }
| VAR decl_item = decl_item EQ expr = expr SEMICOLON
    { Stmt_VarDecl(decl_item,expr,Range($symbolstartpos,$endpos)) }
| LET decl_item = decl_item EQ expr = expr SEMICOLON
    { Stmt_ConstDecl(decl_item,expr,Range($symbolstartpos,$endpos)) }
| lexpr = lexpr EQ expr = expr SEMICOLON
    { Stmt_Assign(lexpr,expr,Range($symbolstartpos,$endpos)) }

decl_item:
| ident = ident  ty_opt = ty_opt
    { DeclItem_Var(ident,ty_opt) }
| LPAREN decl_item0 = separated_nonempty_list(COMMA,decl_item) RPAREN
    { DeclItem_Tuple(decl_item0) }
| MINUS ty_opt = ty_opt
    { DeclItem_Wildcard(ty_opt) }

lexpr:
| MINUS
    { LExpr_Wildcard }
| ident = ident
    { LExpr_Var(ident) }
| lexpr = lexpr DOT ident = ident
    { LExpr_Field(lexpr,ident) }
| lexpr = lexpr DOT LBRACK ident0 = separated_nonempty_list(COMMA,ident) RBRACK
    { LExpr_Fields(lexpr,ident0) }
| lexpr = lexpr LBRACK slice0 = separated_list(COMMA,slice) RBRACK
    { LExpr_Slices(lexpr,slice0) }
| LBRACK lexpr0 = separated_nonempty2_list(COMMA,lexpr) RBRACK
    { LExpr_BitTuple(lexpr0) }
| LPAREN lexpr0 = separated_nonempty2_list(COMMA,lexpr) RPAREN
    { LExpr_Tuple(lexpr0) }
| LPAREN lexpr = lexpr RPAREN
    { lexpr }

lexpr_spice:
| UNDERSCORE_UNDERSCORE_ARRAY lexpr = lexpr LBRACK expr = expr RBRACK
    { LExpr_Array(lexpr,expr) }
| UNDERSCORE_UNDERSCORE_WRITE ident = ident LBRACE expr0_prime = separated_list(COMMA,expr) RBRACE LBRACK expr0 = separated_list(COMMA,expr) RBRACK
    { LExpr_Write(ident,expr0_prime,expr0) }
| UNDERSCORE_UNDERSCORE_READWRITE ident1 = ident ident2 = ident LBRACE expr0_prime = separated_list(COMMA,expr) RBRACE LBRACK expr0 = separated_list(COMMA,expr) RBRACK
    { LExpr_ReadWrite(ident1,ident2,expr0_prime,expr0) }

simple_stmt:
| assignment_stmt = assignment_stmt
    { assignment_stmt }
| ident = ident LPAREN expr0 = separated_list(COMMA,expr) RPAREN SEMICOLON
    { Stmt_TCall(ident, [], expr0, Range($symbolstartpos,$endpos)) }
| RETURN expr = expr SEMICOLON
    { Stmt_FunReturn(expr,Range($symbolstartpos,$endpos)) }
| RETURN SEMICOLON
    { Stmt_ProcReturn(Range($symbolstartpos,$endpos)) }
| ASSERT expr = expr SEMICOLON
    { Stmt_Assert(expr,Range($symbolstartpos,$endpos)) }
| THROW ident = ident SEMICOLON
    { Stmt_Throw(ident,Range($symbolstartpos,$endpos)) }

stmt_spice:
| ident = ident LBRACE expr0_prime = separated_list(COMMA,expr) RBRACE LPAREN expr0 = separated_list(COMMA,expr) RPAREN SEMICOLON
    { Stmt_TCall(ident,expr0_prime,expr0) }
| VAR ident0 = list(ident) COLON_COLON ty = ty SEMICOLON
    { Stmt_VarDeclsNoInit(ident0,ty) }

conditional_stmt:
| IF expr = expr THEN block = block s_elsif0 = list(s_elsif) optional_else = optional_else END
    { Stmt_If(expr,block,s_elsif0,optional_else,Range($symbolstartpos,$endpos)) }
| CASE expr = expr OF alt0 = nonempty_list(alt) opt_otherwise = opt_otherwise END
    { Stmt_Case(expr,alt0,opt_otherwise,Range($symbolstartpos,$endpos)) }

s_elsif:
| ELSIF expr = expr THEN block = block
    { S_Elsif_Cond(expr,block,Range($symbolstartpos,$endpos)) }

optional_else:
| ELSE block = block { (block, Range($symbolstartpos,$endpos)) }
| { ([], Range($symbolstartpos,$endpos)) }

alt:
| WHEN pattern0 = separated_nonempty_list(COMMA,pattern) opt_altcond = opt_altcond COLON block = block
    { Alt_Alt(pattern0,opt_altcond,block,Range($symbolstartpos,$endpos)) }

opt_otherwise:
| OTHERWISE COLON block = block { Some(block, Range($symbolstartpos,$endpos)) }
| { None }

opt_altcond:
| WHERE expr = expr { Some(expr) }
| { None }

pattern:
| intLit = INTLIT { Pat_LitInt(intLit) }
| hexLit = HEXLIT { Pat_LitHex(hexLit) }
| bitsLit = BITSLIT { Pat_LitBits(bitsLit) }
| maskLit = MASKLIT { Pat_LitMask(maskLit) }
| ident = ident  { Pat_Const(ident) }
| MINUS { Pat_Wildcard }
| LPAREN pattern0 = separated_nonempty2_list(COMMA,pattern) RPAREN
    { Pat_Tuple(pattern0) }
| LBRACE apattern0 = separated_list(COMMA,apattern) RBRACE
    { Pat_Set(apattern0) }

apattern:
| expr1 = expr DOT_DOT expr2 = expr { Pat_Range(expr1,expr2) }
| expr = expr { Pat_Single(expr) }

repetitive_stmt:
| FOR ident = ident EQ expr1 = expr direction = direction expr2 = expr DO block = block END
    { Stmt_For(ident,expr1,direction,expr2,block,Range($symbolstartpos,$endpos)) }
| WHILE expr = expr DO block = block END
    { Stmt_While(expr,block,Range($symbolstartpos,$endpos)) }
| REPEAT block = block UNTIL expr = expr SEMICOLON pos = pos
    { Stmt_Repeat(block,expr,pos,Range($symbolstartpos,$endpos)) }

direction:
| TO { Direction_Up }
| DOWNTO { Direction_Down }

catch_stmt:
| TRY block = block CATCH ident = ident pos = pos catcher0 = list(catcher) opt_otherwise = opt_otherwise END
    { Stmt_Try(block,ident,pos,catcher0,opt_otherwise,Range($symbolstartpos,$endpos)) }

catcher:
| WHEN expr = expr COLON block = block
    { Catcher_Guarded(expr,block,Range($symbolstartpos,$endpos)) }

expr:
| conditional_expression = conditional_expression
    { conditional_expression }

conditional_expression:
| IF cexpr1 = cexpr THEN expr1 = expr e_elsif0 = list(e_elsif) ELSE expr2 = expr
    { Expr_If(cexpr1,expr1,e_elsif0,expr2) }
| cexpr = cexpr { cexpr }

e_elsif:
| ELSIF expr1 = expr THEN expr2 = expr { E_Elsif_Cond(expr1,expr2) }

cexpr:
| bexpr = bexpr factor0 = list(factor)
    { buildExpression bexpr factor0 (Range($startpos(bexpr),$endpos(factor0))) }

zexpr:
| expr1 = expr binop = binop expr2 = expr { Expr_Binop(expr1,binop,expr2) }

factor:
| binop = binop bexpr = bexpr { Factor_BinOp(binop,bexpr) }

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
| unop = unop fexpr = fexpr { Expr_Unop(unop,fexpr) }
| fexpr = fexpr { fexpr }

fexpr:
| fexpr = fexpr DOT ident = ident
    { Expr_Field(fexpr,ident) }
| fexpr = fexpr DOT LBRACK ident0 = separated_nonempty_list(COMMA,ident) RBRACK
    { Expr_Fields(fexpr,ident0) }
| fexpr = fexpr LBRACK slice0 = separated_list(COMMA,slice) RBRACK
    { Expr_Slices(fexpr,slice0) }
| ident = ident LBRACE field_assignment0 = separated_nonempty_list(COMMA,field_assignment) RBRACE
    { Expr_RecordInit(ident,field_assignment0) }
| fexpr = fexpr IN pattern = pattern
    { Expr_In(fexpr,pattern) }
| aexpr = aexpr
    { aexpr }

aexpr:
| literal_expression = literal_expression
    { literal_expression }
| ident = ident
    { Expr_Var(ident) }
| ident = ident LPAREN expr0 = separated_list(COMMA,expr) RPAREN
    { Expr_TApply(ident, [], expr0) }
| LPAREN expr = expr RPAREN
    { Expr_Parens(expr) }
| LPAREN expr0 = separated_nonempty2_list(COMMA,expr) RPAREN
    { Expr_Tuple(expr0) }
| LBRACK expr0 = separated_nonempty2_list(COMMA,expr) RBRACK
    { Expr_Concat([], expr0) }
| UNKNOWN COLON_COLON ty = ty
    { Expr_Unknown(ty) }
| IMPLEMENTATION_UNDERSCORE_DEFINED opt_stringLit = opt_stringLit COLON_COLON ty = ty
    { Expr_ImpDef(opt_stringLit,ty) }
| aexpr = aexpr AS constraints = constraints
    { Expr_AsConstraint(aexpr,constraints) }
| aexpr = aexpr AS ty = ty
    { Expr_AsType(aexpr,ty) }

expr_spice:
| ident = ident LBRACE expr0_prime = separated_list(COMMA,expr) RBRACE LPAREN expr0 = separated_list(COMMA,expr) RPAREN
    { Expr_TApply(ident,expr0_prime,expr0) }
| LBRACE expr0_prime = separated_list(COMMA,expr) RBRACE LBRACK expr0 = separated_nonempty2_list(COMMA,expr) RBRACK
    { Expr_Concat(expr0_prime,expr0) }
| UNDERSCORE_UNDERSCORE_ARRAY expr1 = expr LBRACK expr2 = expr RBRACK
    { Expr_Array(expr1,expr2) }

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
| expr = expr  { Slice_Single(expr) }
| expr1 = expr COLON expr2 = expr { Slice_HiLo(expr1,expr2) }
| expr1 = expr PLUS_COLON expr2 = expr { Slice_LoWd(expr1,expr2) }

literal_expression:
| intLit = INTLIT { Expr_LitInt(intLit) }
| hexLit = HEXLIT { Expr_LitHex(hexLit) }
| realLit = REALLIT { Expr_LitReal(realLit) }
| bitsLit = BITSLIT { Expr_LitBits(bitsLit) }
| maskLit = MASKLIT { Expr_LitMask(maskLit) }
| stringLit = STRINGLIT { Expr_LitString(stringLit) }

expr_command:
| expr = expr { expr }

stmt_command:
| stmt = stmt { stmt }

impdef_command:
| stringLit = STRINGLIT EQ expr = expr { CLI_Impdef(stringLit,expr) }

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
