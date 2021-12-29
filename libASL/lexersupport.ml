(****************************************************************
 * ASL lexer support
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL lexer support *)

open Lexing
open Asl_parser

let string_of_token (t: Asl_parser.token): string =
    ( match t with
    | AMPERSAND           -> "amp"
    | AMPERSAND_AMPERSAND -> "ampamp"
    | AND                 -> "and"
    | ARRAY               -> "array"
    | ASSERT              -> "assert"
    | BANG                -> "bang"
    | BAR_BAR             -> "barbar"
    | BITS                -> "bits"
    | BEGIN               -> "begin"
    | BITSLIT(x)          -> "bin:"^x
    | UNDERSCORE_UNDERSCORE_ARRAY                           -> "__array"
    | UNDERSCORE_UNDERSCORE_BUILTIN                         -> "__builtin"
    | UNDERSCORE_UNDERSCORE_CONDITIONAL                     -> "__conditional"
    | UNDERSCORE_UNDERSCORE_DECODE                          -> "__decode"
    | UNDERSCORE_UNDERSCORE_ENCODING                        -> "__encoding"
    | UNDERSCORE_UNDERSCORE_EXCEPTIONTAKEN                  -> "__ExceptionTaken"
    | UNDERSCORE_UNDERSCORE_EXECUTE                         -> "__execute"
    | UNDERSCORE_UNDERSCORE_EVENT                           -> "__event"
    | UNDERSCORE_UNDERSCORE_FIELD                           -> "__field"
    | UNDERSCORE_UNDERSCORE_GUARD                           -> "__guard"
    | UNDERSCORE_UNDERSCORE_INSTRUCTION                     -> "__instruction"
    | UNDERSCORE_UNDERSCORE_INSTRUCTION_UNDERSCORE_SET      -> "__instruction_set"
    | UNDERSCORE_UNDERSCORE_MAP                             -> "__map"
    | UNDERSCORE_UNDERSCORE_NOP                             -> "__NOP"
    | UNDERSCORE_UNDERSCORE_NEWEVENT                        -> "__newevent"
    | UNDERSCORE_UNDERSCORE_NEWMAP                          -> "__newmap"
    | UNDERSCORE_UNDERSCORE_OPCODE                          -> "__opcode"
    | UNDERSCORE_UNDERSCORE_OPERATOR_ONE                    -> "__operator1"
    | UNDERSCORE_UNDERSCORE_OPERATOR_TWO                    -> "__operator2"
    | UNDERSCORE_UNDERSCORE_POSTDECODE                      -> "__postdecode"
    | UNDERSCORE_UNDERSCORE_READWRITE                       -> "__readwrite"
    | UNDERSCORE_UNDERSCORE_REGISTER                        -> "__register"
    | UNDERSCORE_UNDERSCORE_UNALLOCATED                     -> "__UNALLOCATED"
    | UNDERSCORE_UNDERSCORE_UNPREDICTABLE_UNDERSCORE_UNLESS -> "__unpredictable_unless"
    | UNDERSCORE_UNDERSCORE_UNPREDICTABLE                   -> "__UNPREDICTABLE"
    | UNDERSCORE_UNDERSCORE_WRITE                           -> "__write"
    | CARET               -> "caret"
    | CASE                -> "case"
    | CATCH               -> "catch"
    | COLON               -> "colon"
    | COLON_COLON         -> "coloncolon"
    | COMMA               -> "comma"
    | CONFIG              -> "config"
    | CONSTANT            -> "constant"
    | CONSTRAINED_UNDERSCORE_UNPREDICTABLE -> "constrained_unpredictable"
    | DIV                 -> "div"
    | DO                  -> "do"
    | DOT                 -> "dot"
    | DOT_DOT             -> "dotdot"
    | DOWNTO              -> "downto"
    | ELSE                -> "else"
    | ELSIF               -> "elsif"
    | ENUMERATION         -> "enum"
    | EOF                 -> "eof"
    | EOR                 -> "eor"
    | EQ                  -> "eq"
    | EQ_EQ               -> "eqeq"
    | EQ_GT               -> "eqgt"
    | REALLIT(x)          -> "real:"^x
    | END                 -> "end"
    | FOR                 -> "for"
    | FUNC                -> "func"
    | GETTER              -> "getter"
    | GT                  -> "gt"
    | GT_EQ               -> "gteq"
    | GT_GT               -> "gtgt"
    | HEXLIT(x)           -> "hex:"^x
    | ID(x)               -> "ident:"^x
    | IF                  -> "if"
    | IMPLEMENTATION_UNDERSCORE_DEFINED -> "impdef"
    | IN                  -> "in"
    | IFF                 -> "iff"
    | IMPLIES             -> "implies"
    | INTEGER             -> "integer"
    | INTLIT(x)           -> "int:" ^ x
    | IS                  -> "is"
    | LBRACE              -> "lbrace"
    | LBRACE_LBRACE       -> "{{"
    | LBRACK              -> "lbrack"
    | LET                 -> "let"
    | LPAREN              -> "lparen"
    | LT                  -> "lt"
    | LT_EQ               -> "lteq"
    | LT_LT               -> "ltlt"
    | MASKLIT(x)          -> "mask:"^x
    | MINUS               -> "minus"
    | MOD                 -> "mod"
    | BANG_EQ             -> "neq"
    | NOT                 -> "not"
    | OF                  -> "of"
    | OR                  -> "or"
    | OTHERWISE           -> "otherwise"
    | PLUS                -> "plus"
    | PLUS_PLUS           -> "plusplus"
    | PLUS_COLON          -> "pluscolon"
    | QUALIFIER(x)        -> "qualifier:"^x
    | QUOT                -> "quot"
    | RBRACE              -> "rbrace"
    | RBRACE_RBRACE       -> "}}"
    | RBRACK              -> "rbrack"
    | RECORD              -> "record"
    | REM                 -> "rem"
    | REPEAT              -> "repeat"
    | RETURN              -> "return"
    | RPAREN              -> "rparen"
    | SEE                 -> "see"
    | SEMICOLON           -> "semi"
    | SETTER              -> "setter"
    | SLASH               -> "slash"
    | STAR                -> "star"
    | STRINGLIT(x)        -> "string:" ^ x
    | THEN                -> "then"
    | THROW               -> "throw"
    | TYPEID(x)           -> "tident:"^x
    | TO                  -> "to"
    | TRY                 -> "try"
    | TYPE                -> "type"
    | TYPEOF              -> "typeof"
    | UNDEFINED           -> "undefined"
    | UNKNOWN             -> "unknown"
    | UNPREDICTABLE       -> "unpredictable"
    | UNTIL               -> "until"
    | VAR                 -> "var"
    | WHEN                -> "when"
    | WHERE               -> "where"
    | WHILE               -> "while"
    )

let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "%s:%d:%d" pos.pos_fname
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(****************************************************************
 * End
 ****************************************************************)
