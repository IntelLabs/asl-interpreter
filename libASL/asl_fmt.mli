(****************************************************************
 * ASL pretty printer
 *
 * Copyright Intel Inc (c) 2021-2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

open Format

type comment = (Lexing.position * Lexing.position * string)
val comment_list : comment list ref

val loc            : formatter -> AST.l                 -> unit
val tycon          : formatter -> AST.ident             -> unit
val varname        : formatter -> AST.ident             -> unit
val funname        : formatter -> AST.ident             -> unit
val fieldname      : formatter -> AST.ident             -> unit
val unop           : formatter -> AST.unop              -> unit
val binop          : formatter -> AST.binop             -> unit
val ty             : formatter -> AST.ty                -> unit
val expr           : formatter -> AST.expr              -> unit
val pattern        : formatter -> AST.pattern           -> unit
val patterns       : formatter -> AST.pattern list      -> unit
val lexpr          : formatter -> AST.lexpr             -> unit
val stmt           : formatter -> AST.stmt              -> unit
val indented_block : formatter -> AST.stmt list         -> unit
val formals        : formatter -> (AST.ident * AST.ty) list -> unit
val sformals       : formatter -> AST.sformal list      -> unit
val declaration    : formatter -> AST.declaration       -> unit
val declarations   : formatter -> AST.declaration list  -> unit

val braces   : formatter -> (unit -> unit) -> unit
val parens   : formatter -> (unit -> unit) -> unit
val brackets : formatter -> (unit -> unit) -> unit

val commasep
    :  formatter
    -> ('a -> unit)
    -> 'a list
    -> unit

val delimiter : formatter -> string -> unit
val keyword   : formatter -> string -> unit

val amp                : formatter -> unit
val amp_amp            : formatter -> unit
val bang               : formatter -> unit
val bang_eq            : formatter -> unit
val bar_bar            : formatter -> unit
val caret              : formatter -> unit
val colon              : formatter -> unit
val coloncolon         : formatter -> unit
val comma              : formatter -> unit
val dot                : formatter -> unit
val dot_dot            : formatter -> unit
val eq                 : formatter -> unit
val eq_eq              : formatter -> unit
val eq_gt              : formatter -> unit
val gt                 : formatter -> unit
val gt_eq              : formatter -> unit
val gt_gt              : formatter -> unit
val lbrace             : formatter -> unit
val lbrace_lbrace      : formatter -> unit
val lbrack             : formatter -> unit
val lparen             : formatter -> unit
val lt                 : formatter -> unit
val lt_eq              : formatter -> unit
val lt_lt              : formatter -> unit
val minus              : formatter -> unit
val plus               : formatter -> unit
val plus_colon         : formatter -> unit
val plus_plus          : formatter -> unit
val rbrace             : formatter -> unit
val rbrace_rbrace      : formatter -> unit
val rbrack             : formatter -> unit
val rparen             : formatter -> unit
val semicolon          : formatter -> unit
val slash              : formatter -> unit
val star               : formatter -> unit

val kw_and                             : formatter -> unit
val kw_array                           : formatter -> unit
val kw_assert                          : formatter -> unit
val kw_bits                            : formatter -> unit
val kw_case                            : formatter -> unit
val kw_catch                           : formatter -> unit
val kw_constant                        : formatter -> unit
val kw_constrained_unpredictable       : formatter -> unit
val kw_div                             : formatter -> unit
val kw_do                              : formatter -> unit
val kw_downto                          : formatter -> unit
val kw_else                            : formatter -> unit
val kw_elsif                           : formatter -> unit
val kw_enumeration                     : formatter -> unit
val kw_end                             : formatter -> unit
val kw_eor                             : formatter -> unit
val kw_for                             : formatter -> unit
val kw_func                            : formatter -> unit
val kw_getter                          : formatter -> unit
val kw_if                              : formatter -> unit
val kw_iff                             : formatter -> unit
val kw_implementation_defined          : formatter -> unit
val kw_implies                         : formatter -> unit
val kw_in                              : formatter -> unit
val kw_is                              : formatter -> unit
val kw_mod                             : formatter -> unit
val kw_not                             : formatter -> unit
val kw_of                              : formatter -> unit
val kw_or                              : formatter -> unit
val kw_otherwise                       : formatter -> unit
val kw_quot                            : formatter -> unit
val kw_record                          : formatter -> unit
val kw_rem                             : formatter -> unit
val kw_repeat                          : formatter -> unit
val kw_return                          : formatter -> unit
val kw_see                             : formatter -> unit
val kw_setter                          : formatter -> unit
val kw_then                            : formatter -> unit
val kw_throw                           : formatter -> unit
val kw_to                              : formatter -> unit
val kw_try                             : formatter -> unit
val kw_type                            : formatter -> unit
val kw_typeof                          : formatter -> unit
val kw_undefined                       : formatter -> unit
val kw_underscore_array                : formatter -> unit
val kw_underscore_builtin              : formatter -> unit
val kw_underscore_conditional          : formatter -> unit
val kw_underscore_config               : formatter -> unit
val kw_underscore_decode               : formatter -> unit
val kw_underscore_encoding             : formatter -> unit
val kw_underscore_event                : formatter -> unit
val kw_underscore_exceptiontaken       : formatter -> unit
val kw_underscore_execute              : formatter -> unit
val kw_underscore_field                : formatter -> unit
val kw_underscore_guard                : formatter -> unit
val kw_underscore_instruction          : formatter -> unit
val kw_underscore_instruction_set      : formatter -> unit
val kw_underscore_map                  : formatter -> unit
val kw_underscore_newevent             : formatter -> unit
val kw_underscore_newmap               : formatter -> unit
val kw_underscore_nop                  : formatter -> unit
val kw_underscore_opcode               : formatter -> unit
val kw_underscore_operator1            : formatter -> unit
val kw_underscore_operator2            : formatter -> unit
val kw_underscore_postdecode           : formatter -> unit
val kw_underscore_readwrite            : formatter -> unit
val kw_underscore_register             : formatter -> unit
val kw_underscore_unallocated          : formatter -> unit
val kw_underscore_unpredictable        : formatter -> unit
val kw_underscore_unpredictable_unless : formatter -> unit
val kw_underscore_write                : formatter -> unit
val kw_unknown                         : formatter -> unit
val kw_unpredictable                   : formatter -> unit
val kw_until                           : formatter -> unit
val kw_when                            : formatter -> unit
val kw_while                           : formatter -> unit


(*****************************************
 * End
 *****************************************)
