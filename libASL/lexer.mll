(****************************************************************
 * ASL lexer
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

{
open Asl_parser       (* The type token is defined in parser.mli *)

exception Eof

let keywords : (string * Asl_parser.token) list = [
    ("AND",                    AND);
    ("DIV",                    DIV);
    ("DIVRM",                  DIVRM);
    ("EOR",                    XOR);
    ("IN",                     IN);
    ("MOD",                    MOD);
    ("NOT",                    NOT);
    ("OR",                     OR);
    ("QUOT",                   QUOT);
    ("REM",                    REM);
    ("UNKNOWN",                UNKNOWN);
    ("XOR",                    XOR);
    ("__assert",               UNDERSCORE_UNDERSCORE_ASSERT);
    ("__builtin",              UNDERSCORE_UNDERSCORE_BUILTIN);
    ("__in",                   UNDERSCORE_UNDERSCORE_IN);
    ("__let",                  UNDERSCORE_UNDERSCORE_LET);
    ("__operator1",            UNDERSCORE_UNDERSCORE_OPERATOR_ONE);
    ("__operator2",            UNDERSCORE_UNDERSCORE_OPERATOR_TWO);
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
    ("exception",              EXCEPTION);
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
    ("with",                   WITH);
]

let prev_else_token_pos = ref (-1)

let update_location lexbuf opt_file line =
    let pos = lexbuf.Lexing.lex_curr_p in
    let new_file = match opt_file with
                   | None -> pos.Lexing.pos_fname
                   | Some f -> f
    in
    lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_fname = new_file;
        Lexing.pos_lnum = line;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

rule token = parse
    (* whitespace and comments *)
    | "```"                       { fenced_code_block lexbuf }
    | ['\n']                      { Lexing.new_line lexbuf; token lexbuf }
    | [' ' '\t']                  { token lexbuf }
    | '/' '/' [^'\n']*            { token lexbuf }
    | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']* ('\"' ([^ '\"']* as name) '\"')? [' ' '\t']* '\n'
                                  { update_location lexbuf name (int_of_string num); token lexbuf }
    | '/' '*'                     { comment 1 lexbuf }

    (* numbers, strings and identifiers *)
    | '"'                         { let startpos = Lexing.lexeme_start_p lexbuf in
                                    let buffer   = Buffer.create 10 in
                                    ignore (string buffer lexbuf);
                                    lexbuf.lex_start_p <- startpos;
                                    STRINGLIT(Buffer.contents buffer) }

    | '\'' (['0' '1' ' ']* as bits) '\''
      { let x = Utils.drop_chars bits ' ' in
        BITSLIT(Primops.mkBits (String.length x) (Z.of_string_base 2 x)) }
    | (['0'-'9']+ as len) '\'' 'b' (['0'-'1' '_']+ as bits)
      { let x = Utils.drop_chars bits '_' in
        BITSLIT(Primops.mkBits (int_of_string len) (Z.of_string_base 2 x)) }
    | (['0'-'9']+ as len) '\'' 'd' (['0'-'9' '_']+ as digits)
      { BITSLIT(Primops.mkBits (int_of_string len) (Z.of_string_base 10 digits)) }
    | (['0'-'9']+ as len) '\'' 'x' (['0'-'9' 'A'-'F' 'a'-'f' '_']+ as nibbles)
      { BITSLIT(Primops.mkBits (int_of_string len) (Z.of_string_base 16 nibbles)) }
    | '\'' (['0' '1' 'x' ' ']* as bits) '\''
      { MASKLIT(bits) }
    | '0''x'(['0'-'9' 'A'-'F' 'a'-'f' '_']+ as nibbles) { INTLIT(None, Z.of_string_base 16 nibbles) }
    | 'i' (['0'-'9']+ as len) '\'' 'b' (['0'-'1']+ as bits) { INTLIT(Some (int_of_string len), Z.of_string_base 2 bits) }
    | 'i' (['0'-'9']+ as len) '\'' 'd' (['0'-'9']+ as digits) { INTLIT(Some (int_of_string len), Z.of_string digits) }
    | 'i' (['0'-'9']+ as len) '\'' 'x' (['0'-'9' 'A'-'F' 'a'-'f']+ as nibbles) { INTLIT(Some (int_of_string len), Z.of_string_base 16 nibbles) }
    | ['0'-'9']+ '.' ['0'-'9']+              as lxm { REALLIT(lxm) }
    | ['0'-'9'] ['0'-'9' '_']*               as digits { INTLIT(None, Z.of_string digits) }
    | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm {
         let tok =
           ( match List.assoc_opt lxm keywords with
           | Some x -> x
           | None   -> ID(lxm)
           )
         in
         (* If the code accidentally uses "else if" instead of "elsif",
          * then parsing will eventually fail with a report about a missing
          * "end". But that error report can come much later (e.g., at the end
          * of the function and it is hard to find the actual cause of the error.
          *
          * So, the following code performs a heuristic check for "else" followed
          * by "if" in the same line - in the hope of catching most places that
          * this can go wrong.
          *)
         let follows_within x y dist = x < y && (y-x) < dist
         in
         if tok = IF && follows_within (!prev_else_token_pos) (Lexing.lexeme_start lexbuf) 2 then begin
             let pos = Lexing.lexeme_start_p lexbuf in
             Printf.printf "Warning: 'else if' should possibly be written as 'elsif' at %s:%d\n%!"
               pos.pos_fname pos.pos_lnum
         end;
         if tok = ELSE then prev_else_token_pos := Lexing.lexeme_end lexbuf;
         tok
    }

    (* delimiters *)
    | '!'            { BANG       }
    | '!' '='        { BANG_EQ    }
    | '&' '&'        { AMPERSAND_AMPERSAND }
    | '('            { LPAREN     }
    | ')'            { RPAREN     }
    | '*'            { STAR       }
    | '+' '+'        { PLUS_PLUS  }
    | '+'            { PLUS       }
    | '+' ':'        { PLUS_COLON }
    | '-' ':'        { MINUS_COLON }
    | '*' ':'        { STAR_COLON }
    | ','            { COMMA      }
    | '-'            { MINUS      }
    | '-' '-' '>'    { MINUS_MINUS_GT }
    | '.'            { DOT        }
    | '.' '.'        { DOT_DOT    }
    | '/'            { SLASH      }
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
    | '?'            { QUERY      }
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
    | '/' '*' { comment (depth+1) lexbuf }
    | '*' '/' { if depth = 1 then token lexbuf else comment (depth-1) lexbuf }
    | '\n'    { Lexing.new_line lexbuf; comment depth lexbuf }
    | _       { comment depth lexbuf }

(* Fenced code blocks (i.e., characters marked by ``` in column 0 at start
 * and end are treated as comments.
 * This makes it easy to include metadata in ASL files.
 *)
and fenced_code_block = parse
  | "\n```"   { Lexing.new_line lexbuf; token lexbuf }
  | '\n'      { Lexing.new_line lexbuf; fenced_code_block lexbuf }
  | _         { fenced_code_block lexbuf }

and string b = parse
  | '\\' 'n'                  { Buffer.add_char b '\n'; string b lexbuf }
  | '\\' 't'                  { Buffer.add_char b '\t'; string b lexbuf }
  | '\\' '\\'                 { Buffer.add_char b '\\'; string b lexbuf }
  | '\\' '"'                  { Buffer.add_char b '"'; string b lexbuf }
  | '"'                       { () }
  | _ as c                    { Buffer.add_char b c; string b lexbuf }

(****************************************************************
 * End
 ****************************************************************)
