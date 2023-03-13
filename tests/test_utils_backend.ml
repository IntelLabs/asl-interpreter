(****************************************************************
 * Test cases and utilities for use in backend tests
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
module AST = Asl_ast
module TC = Tcheck

type backend = Backend_C | Backend_Verilog
type test_case = string * backend list * string

let test_cases_expr : test_case list =
  [
    ( "if",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer return if FALSE then 0 else 0; end" );

    ( "elsif",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer return if FALSE then 0 elsif FALSE then 0 else 0; end" );

    ( "binary operation",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer return 1 + 1; end" );

    ( "field selection",
      [ Backend_C; Backend_Verilog ],
      "record X { i :: integer; }; func F(x :: X) => integer return x.i; end" );

    ( "bitslice single",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: bits(16)) => bits(1) return x[4]; end" );

    ( "bitslice single (> 64b)",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: bits(65)) => bits(1) return x[4]; end" );

    ( "bitslice hilo",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: bits(16)) => bits(8) return x[11:4]; end" );

    ( "bitslice lowd",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: bits(16)) => bits(8) return x[4 +: 8]; end" );

    ( "bitslice lowd (> 64b)",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: bits(129)) => bits(65) return x[4 +: 65]; end" );

    ( "record initializer",
      [ Backend_C; Backend_Verilog ],
      "record X { i :: integer; }; func F() => X return X { i = 1 }; end" );

    ( "pattern match (literal mask)",
      [ Backend_C ],
      "enumeration boolean { FALSE, TRUE };
       func F(x :: bits(4)) => boolean return x IN '11xx'; end" );

    ( "literal integer",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer return 0; end" );

    ( "literal hexadecimal",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer return 0x1; end" );

    ( "literal bitvector",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(8) return '1111 0000'; end" );

    ( "literal string",
      [ Backend_C; Backend_Verilog ],
      "func F() => string return \"str\"; end" );

    ( "literal string with escapes",
      [ Backend_C; Backend_Verilog ],
      "func F() => string return \"Hello \\\" World\"; end" );

    ( "variable",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: integer) => integer return x; end" );

    ( "variable boolean",
      [ Backend_C; Backend_Verilog ],
      "enumeration boolean { FALSE, TRUE };
       func F() => boolean return FALSE; end" );

    ( "function call",
      [ Backend_C; Backend_Verilog ],
      "func B() => integer return 0; end func F() => integer return B(); end" );

    ( "built-in fun call (eq_enum)",
      [ Backend_C; Backend_Verilog ],
      "enumeration boolean { FALSE, TRUE };
       enumeration T { A, B, C };
       func F() => boolean return A == B; end" );

    ( "built-in fun call (and_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) return '1' AND '0'; end" );

    ( "built-in fun call (cvt_bits_uint)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer return cvt_bits_uint('1'); end" );

    ( "built-in fun call (cvt_int_bits)",
      [ Backend_C ], (* TODO fails for verilog: part-select on literal *)
      "func F() => bits(1) return cvt_int_bits(1, 1); end" );

    ( "built-in fun call (eor_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) return '1' EOR '0'; end" );

    ( "built-in fun call (eq_bits)",
      [ Backend_C; Backend_Verilog ],
      "enumeration boolean { FALSE, TRUE };
       func F() => boolean return '1' == '0'; end" );

    ( "built-in fun call (lsl_bits)",
      [ Backend_C ],
      "func F() => bits(2) return lsl_bits('01', 1); end" );

    ( "built-in fun call (lsr_bits)",
      [ Backend_C ],
      "func F() => bits(2) return lsr_bits('10', 1); end" );

    ( "built-in fun call (mk_mask)",
      [ Backend_C ],
      "func F() => bits(2) return mk_mask(1, 2); end" );

    ( "built-in fun call (ne_bits)",
      [ Backend_C; Backend_Verilog ],
      "enumeration boolean { FALSE, TRUE };
       func F() => boolean return '1' != '0'; end" );

    ( "built-in fun call (not_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) return NOT '0'; end" );

    ( "built-in fun call (or_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) return '1' OR '0'; end" );

    ( "parentheses",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer return ( 0 ); end" );

    ( "bitvector concatenation",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: bits(8), y :: bits(4), z :: bits(2)) => bits(14) return [x, y, z]; end" );

    ( "array",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) var x :: array [1] of bits(1); return x[0]; end" );
  ]

let test_cases_fun_decl : test_case list  =
  [
    ( "built-in",
      [ Backend_C; Backend_Verilog ],
      "__builtin func f() => integer;" );

    ( "type",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer;" );

    ( "definition",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer end" );

    ( "definition with params",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: integer, y :: integer) => integer end" );
  ]

let test_cases_proc_decl : test_case list  =
  [
    ( "type",
      [ Backend_C; Backend_Verilog ],
      "func F();" );

    ( "definition",
      [ Backend_C; Backend_Verilog ],
      "func F() end" );

    ( "definition with params",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: integer, y :: integer) end" );
  ]

let test_cases_stmt : test_case list  =
  [
    ( "uninitialized variable",
      [ Backend_C; Backend_Verilog ],
      "func F() var x :: integer; end" );

    ( "uninitialized variables",
      [ Backend_C; Backend_Verilog ],
      "func F() var x, y :: integer; end" );

    ( "variable",
      [ Backend_C; Backend_Verilog ],
      "func F() var x = 0; end" );

    ( "variable (wildcard)",
      [ Backend_C ],
      "func F() var - = 0; end" );

    ( "variable (__RAM)",
      [ Backend_C ],
      "func F() var x :: __RAM(8); end" );

    ( "constant",
      [ Backend_C; Backend_Verilog ],
      "func F() let x = 0; end" );

    ( "assignment",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: integer, y :: integer) x = y; end" );

    ( "assignment to slice",
      [ Backend_C; Backend_Verilog ],
      "func F(x :: bits(8)) x[4 +: 2] = '10'; end" );

    ( "assignment to field",
      [ Backend_C; Backend_Verilog ],
      "record X { i :: integer; }; func F(x :: X) x.i = 0; end" );

    ( "assignment to wildcard",
      [ Backend_C ],
      "func F() - = 0; end" );

    ( "assignment to array element",
      [ Backend_C; Backend_Verilog ],
      "func F() var x :: array [1] of bits(1); x[0] = '0'; end" );

    ( "procedure call",
      [ Backend_C; Backend_Verilog ],
      "func B() end func F() B(); end" );

    ( "procedure call with argument",
      [ Backend_C; Backend_Verilog ],
      "func B(x :: integer) end func F() B(0); end" );

    ( "procedure return",
      [ Backend_C; Backend_Verilog ],
      "func F() return; end" );

    ( "function return",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer return 0; end" );

    ( "assert",
      [ Backend_C; Backend_Verilog ],
      "func F() assert FALSE; end" );

    ( "if",
      [ Backend_C; Backend_Verilog ],
      "func F() if FALSE then return; elsif FALSE then return; else return; end end" );

    ( "if with several elsifs",
      [ Backend_C; Backend_Verilog ],
      "func F() if FALSE then return; elsif FALSE then return; elsif FALSE then return; end end" );

    ( "case",
      [ Backend_C; Backend_Verilog ],
      "func F() case 0 of when 0: return; otherwise: return; end end" );

    ( "case with several whens",
      [ Backend_C; Backend_Verilog ],
      "func F() case 0 of when 0: return; when 1: return; end end" );

    ( "for loop (direction to)",
      [ Backend_C ],
      "func F() for x = 0 to 1 do return; end end" );

    ( "for loop (direction downto)",
      [ Backend_C ],
      "func F() for x = 1 downto 0 do return; end end" );

    ( "while loop",
      [ Backend_C ],
      "func F() while TRUE do return; end end" );

    ( "repeat loop",
      [ Backend_C ],
      "func F() repeat return; until TRUE; end" );

    ( "block",
      [ Backend_C; Backend_Verilog ],
      "func F() begin end end" );
  ]

let test_cases_type_decl : test_case list  =
  [
    ( "built-in (real)",
      [ Backend_C ],
      "__builtin type real;" );

    ( "built-in (string)",
      [ Backend_C; Backend_Verilog ],
      "__builtin type string;" );

    ( "built-in (__mask)",
      [ Backend_C ],
      "__builtin type __mask;" );

    ( "built-in (__Exception)",
      [ Backend_C ],
      "__builtin type __Exception;" );

    ( "built-in (__RAM)",
      [ Backend_C ],
      "__builtin type __RAM;" );

    ( "record",
      [ Backend_C; Backend_Verilog ],
      "type bit of bits(1);
       record X { i :: integer; b :: bit; };" );

    ( "typedef",
      [ Backend_C; Backend_Verilog ],
      "type Byte of bits(8);" );

    ( "typedef (register)",
      [ Backend_C; Backend_Verilog ],
      "type Reg of bits(9) { [8] i [1] b };" );

    ( "enumeration",
      [ Backend_C; Backend_Verilog ],
      "enumeration signal { LOW, HIGH };" );

    ( "enumeration (boolean)",
      [ Backend_C; Backend_Verilog ],
      "enumeration boolean { FALSE, TRUE };" );
  ]

let test_cases_var_decl : test_case list  =
  [
    ( "bits",
      [ Backend_C; Backend_Verilog ],
      "var x :: bits(8);" );

    ( "integer",
      [ Backend_C; Backend_Verilog ],
      "var x :: integer;" );

    ( "array",
      [ Backend_C; Backend_Verilog ],
      "var x :: array [1] of integer;" );

    ( "array2",
      [ Backend_C; Backend_Verilog ],
      "var x :: array [1] of array [2] of integer;" );

    ( "const (integer)",
      [ Backend_C ],
      "let x :: integer = 0;" );
  ]

let make_tests (b : backend) (test_fun : string -> string -> unit)
    (test_cases : test_case list) : unit Alcotest.test_case list =
  List.filter_map
    (fun (name, backends, s) ->
      if List.mem b backends
      then Some (name, `Quick, fun _ -> test_fun name s)
      else None)
    test_cases

let check_declaration (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit)
    (check_ext : string -> string -> unit) (name : string) (s : string) : unit =
  let tcenv = TC.GlobalEnv.clone tcenv in
  let ds = LoadASL.read_declarations tcenv s in
  Alcotest.(check pass) name () (decls ds);

  let s = Format.flush_str_formatter () in
  check_ext name s

let check_compiler
    (language : string)
    (suffix : string)
    (prog : string)
    (args : string list)
    (name : string)
    (header : string)
    (body : string)
  : unit =
  let (tmp, chan) = Filename.open_temp_file "test" suffix in
  Out_channel.output_string chan header;
  Out_channel.output_string chan body;
  Out_channel.close chan;
  let args' = List.append args [ tmp ] in

  if true then begin
    (* output test to log - for ease of debugging *)
    output_string stdout header;
    output_string stdout body;

    Printf.printf "Wrote to %s\n" tmp;
    Printf.printf "Executing %s %s\n" prog (String.concat " " args');
    flush stdout
  end;

  let p = Unix.open_process_args_out prog (Array.of_list (prog :: args')) in
  let status = Unix.close_process_out p in
  let exit_status =
    match status with
    | Unix.WEXITED s ->
        if s <> 0 then Printf.printf "%s exited with %d\n" prog s;
        s
    | Unix.WSIGNALED _ ->
        Printf.printf "%s killed\n" prog;
        1
    | Unix.WSTOPPED _ ->
        Printf.printf "%s stopped\n" prog;
        1
  in
  Sys.remove tmp;
  Alcotest.(check int) (language ^ " syntax: " ^ name) 0 exit_status

(****************************************************************
 * End
 ****************************************************************)
