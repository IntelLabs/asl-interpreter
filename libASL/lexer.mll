(****************************************************************
 * ASL lexer
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

{
open Asl_parser       (* The type token is defined in parser.mli *)
open Asl_ast

exception Eof

let keywords : (string * Asl_parser.token) list = [
    ("AND",                    AND);
    ("DIV",                    DIV);
    ("EOR",                    EOR);
    ("IMPLEMENTATION_DEFINED", IMPLEMENTATION_UNDERSCORE_DEFINED);
    ("IN",                     IN);
    ("MOD",                    MOD);
    ("NOT",                    NOT);
    ("OR",                     OR);
    ("QUOT",                   QUOT);
    ("REM",                    REM);
    ("UNKNOWN",                UNKNOWN);
    ("__NOP",                  UNDERSCORE_UNDERSCORE_NOP);
    ("__UNALLOCATED",          UNDERSCORE_UNDERSCORE_UNALLOCATED);
    ("__UNPREDICTABLE",        UNDERSCORE_UNDERSCORE_UNPREDICTABLE);
    ("__array",                UNDERSCORE_UNDERSCORE_ARRAY);
    ("__builtin",              UNDERSCORE_UNDERSCORE_BUILTIN);
    ("__conditional",          UNDERSCORE_UNDERSCORE_CONDITIONAL);
    ("__decode",               UNDERSCORE_UNDERSCORE_DECODE);
    ("__encoding",             UNDERSCORE_UNDERSCORE_ENCODING);
    ("__event",                UNDERSCORE_UNDERSCORE_EVENT);
    ("__execute",              UNDERSCORE_UNDERSCORE_EXECUTE);
    ("__field",                UNDERSCORE_UNDERSCORE_FIELD);
    ("__guard",                UNDERSCORE_UNDERSCORE_GUARD);
    ("__instruction",          UNDERSCORE_UNDERSCORE_INSTRUCTION);
    ("__instruction_set",      UNDERSCORE_UNDERSCORE_INSTRUCTION_UNDERSCORE_SET);
    ("__map",                  UNDERSCORE_UNDERSCORE_MAP);
    ("__newmap",               UNDERSCORE_UNDERSCORE_NEWMAP);
    ("__newevent",             UNDERSCORE_UNDERSCORE_NEWEVENT);
    ("__operator1",            UNDERSCORE_UNDERSCORE_OPERATOR_ONE);
    ("__operator2",            UNDERSCORE_UNDERSCORE_OPERATOR_TWO);
    ("__opcode",               UNDERSCORE_UNDERSCORE_OPCODE);
    ("__postdecode",           UNDERSCORE_UNDERSCORE_POSTDECODE);
    ("__readwrite",            UNDERSCORE_UNDERSCORE_READWRITE);
    ("__register",             UNDERSCORE_UNDERSCORE_REGISTER);
    ("__unpredictable_unless", UNDERSCORE_UNDERSCORE_UNPREDICTABLE_UNDERSCORE_UNLESS);
    ("__write",                UNDERSCORE_UNDERSCORE_WRITE);
    ("array",                  ARRAY);
    ("as",                     AS);
    ("assert",                 ASSERT);
    ("begin",                  BEGIN);
    ("bits",                   BITS);
    ("case",                   CASE);
    ("catch",                  CATCH);
    ("config",                 CONFIG);
    ("constant",               CONSTANT);
    ("do",                     DO);
    ("downto",                 DOWNTO);
    ("else",                   ELSE);
    ("elsif",                  ELSIF);
    ("enumeration",            ENUMERATION);
    ("end",                    END);
    ("for",                    FOR);
    ("func",                   FUNC);
    ("getter",                 GETTER);
    ("if",                     IF);
    ("integer",                INTEGER);
    ("let",                    LET);
    ("of",                     OF);
    ("otherwise",              OTHERWISE);
    ("record",                 RECORD);
    ("repeat",                 REPEAT);
    ("return",                 RETURN);
    ("setter",                 SETTER);
    ("then",                   THEN);
    ("throw",                  THROW);
    ("to",                     TO);
    ("try",                    TRY);
    ("type",                   TYPE);
    ("typeof",                 TYPEOF);
    ("until",                  UNTIL);
    ("var",                    VAR);
    ("when",                   WHEN);
    ("where",                  WHERE);
    ("while",                  WHILE);
]

(* To allow us to retain comments when pretty-printing, we record
 * all comments and their locations.
 *)
let comments: (Lexing.position * Lexing.position * string) list ref = ref []

let get_comments (): (Lexing.position * Lexing.position * string) list = begin
    let cs = !comments in
    comments := [];
    List.rev cs
end

let record_comment (start: Lexing.position) (finish: Lexing.position) (lxm: string): unit = begin
    if false then begin
        Printf.printf "Comment %s:%d:%d-%d = '%s'\n"
            start.pos_fname
            start.pos_lnum
            (start.pos_cnum - start.pos_bol)
            (finish.pos_cnum - finish.pos_bol)
            lxm
    end;
    let comment = (start, finish, lxm) in
    comments := comment :: !comments
end

}

rule token = parse
    (* whitespace and comments *)
    | ['\n']                      { Lexing.new_line lexbuf; token lexbuf }
    | [' ' '\t']                  { token lexbuf }
    | '/' '/' [^'\n']*     as lxm { record_comment lexbuf.lex_start_p lexbuf.lex_curr_p lxm; token lexbuf }
    | '#' [^'\n']*                { token lexbuf }
    | '/' '*'                     { comment 1 lexbuf }

    (* numbers, strings and identifiers *)
    | '"' [^'"']* '"'                        as lxm { STRINGLIT(String.sub lxm 1 (String.length lxm - 2)) }
    | '\'' ['0' '1' ' ']* '\''               as lxm { BITSLIT(String.sub lxm 1 (String.length lxm - 2)) }
    | '\'' ['0' '1' 'x' ' ']* '\''           as lxm { MASKLIT(String.sub lxm 1 (String.length lxm - 2)) }
    | '0''x'['0'-'9' 'A' - 'F' 'a'-'f' '_']+ as lxm { HEXLIT(String.sub lxm 2 (String.length lxm - 2)) }
    | ['0'-'9']+ '.' ['0'-'9']+              as lxm { REALLIT(lxm) }
    | ['0'-'9']+                             as lxm { INTLIT(lxm) }
    | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm {
           ( match List.assoc_opt lxm keywords with
           | Some x -> x
           | None   -> if isTypeIdent(lxm) then TYPEID(lxm)
                       else if String.equal lxm "AArch32" then QUALIFIER(lxm)
                       else if String.equal lxm "AArch64" then QUALIFIER(lxm)
                       else ID(lxm)
           )
    }

    (* delimiters *)
    | '!'            { BANG       }
    | '!' '='        { BANG_EQ    }
    | '&' '&'        { AMPERSAND_AMPERSAND }
    | '&'            { AMPERSAND  }
    | '('            { LPAREN     }
    | ')'            { RPAREN     }
    | '*'            { STAR       }
    | '+' '+'        { PLUS_PLUS  }
    | '+'            { PLUS       }
    | '+' ':'        { PLUS_COLON }
    | ','            { COMMA      }
    | '-'            { MINUS      }
    | '-' '-' '>'    { MINUS_MINUS_GT }
    | '.'            { DOT        }
    | '.' '.'        { DOT_DOT    }
    | '/'            { SLASH      }
    | ':' ':'        { COLON_COLON }
    | ':'            { COLON      }
    | ';'            { SEMICOLON  }
    | '<'            { LT         }
    | '<' '<'        { LT_LT      }
    | '<' '-' '>'    { LT_MINUS_GT }
    | '<' '='        { LT_EQ      }
    | '='            { EQ         }
    | '=' '='        { EQ_EQ      }
    | '=' '>'        { EQ_GT      }
    | '>'            { GT         }
    | '>' '='        { GT_EQ      }
    | '>' '>'        { GT_GT      }
    | '['            { LBRACK     }
    | ']'            { RBRACK     }
    | '^'            { CARET      }
    | '{'            { LBRACE     }
    | '|' '|'        { BAR_BAR    }
    | '}'            { RBRACE     }
    | eof            { EOF  }
    | _ as c         { Printf.printf "%s:%d Unrecognized character '%c'\n"
                           lexbuf.lex_curr_p.pos_fname
                           lexbuf.lex_curr_p.pos_lnum
                           c;
                       exit 0 }

and comment depth = parse
      '/' '*' { comment (depth+1) lexbuf }
    | '*' '/' { if depth = 1 then token lexbuf else comment (depth-1) lexbuf }
    | '\n'    { Lexing.new_line lexbuf; comment depth lexbuf }
    | _       { comment depth lexbuf }

(****************************************************************
 * End
 ****************************************************************)
