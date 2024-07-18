(****************************************************************
 * Test cases for backends
 *
 * Copyright (C) 2024-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type backend = Backend_C | Backend_Verilog
type test_case = string * backend list * string

let expr : test_case list =
  [
    ( "if",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return if FALSE then 0 else 0; end" );

    ( "elsif",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return if FALSE then 0 elsif FALSE then 0 else 0; end" );

    ( "field selection",
      [ Backend_C; Backend_Verilog ],
      "record X { i : integer; }; func F(x : X) => integer begin return x.i; end" );

    ( "bitslice lowd",
      [ Backend_C; Backend_Verilog ],
      "func F(x : bits(16)) => bits(8) begin return x[4 +: 8]; end" );

    ( "bitslice lowd (> 64b)",
      [ Backend_C; Backend_Verilog ],
      "func F(x : bits(129)) => bits(65) begin return x[4 +: 65]; end" );

    ( "record initializer",
      [ Backend_C; Backend_Verilog ],
      "record X { i : integer; }; func F() => X begin return X { i = 1 }; end" );

    ( "pattern match (literal mask)",
      [ Backend_C; Backend_Verilog ],
      "func F(x : bits(4)) => boolean begin return x IN '11xx'; end" );

    ( "literal int",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 01_000; end" );

    ( "literal int (negative)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -01_000; end" );

    ( "literal int (int64 max)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 9223372036854775807; end" );

    ( "literal int (int64 max + 1)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 9223372036854775808; end" );

    ( "literal int (int64 min)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -9223372036854775808; end" );

    ( "literal int (int64 min - 1)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -9223372036854775809; end" );

    ( "literal int (int128 max)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 170141183460469231731687303715884105727; end" );

    ( "literal int (int128 min)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -170141183460469231731687303715884105728; end" );

    ( "literal hex",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 0x01_0; end" );

    ( "literal hex (negative)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -0x01_0; end" );

    ( "literal bitvector",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(8) begin return '1111 0000'; end" );

    ( "literal bitvector (> 64b)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(65)
       begin
           return '1 0111111111111111111111111111111111111111111111111111111111110000';
       end" );

    ( "literal string",
      [ Backend_C; Backend_Verilog ],
      "func F() => string begin return \"str\"; end" );

    ( "literal string with escapes",
      [ Backend_C; Backend_Verilog ],
      "func F() => string begin return \"Hello \\\" World\"; end" );

    ( "variable",
      [ Backend_C; Backend_Verilog ],
      "func F(x : integer) => integer begin return x; end" );

    ( "variable boolean",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return FALSE; end" );

    ( "function call",
      [ Backend_C; Backend_Verilog ],
      "func B() => integer begin return 0; end func F() => integer begin return B(); end" );

    ( "parentheses",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return ( 0 ); end" );

    ( "bitvector concatenation",
      [ Backend_Verilog ],
      "func F(x : bits(8), y : bits(4), z : bits(2)) => bits(14) begin return [x, y, z]; end" );

    ( "as constraint",
      [ Backend_C; Backend_Verilog ],
      "func F(x : integer) => integer {0 .. 1} begin return x as {0 .. 1}; end" );

    ( "as type",
      [ Backend_C; Backend_Verilog ],
      "func F(x : integer) => integer begin return x as integer; end" );

    ( "array",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) begin var x : array [1] of bits(1); return x[0]; end" );

    ( "let",
      [ Backend_C ],
      "func F(x : integer) => integer begin return __let r : integer = x*x __in r+r; end" );
  ]

let int_ops : test_case list =
  [
    ( "integer operation (add_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 1 + 1; end" );

    ( "integer operation (mul_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 1 * 1; end" );

    ( "integer operation (sub_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 1 - 1; end" );

    ( "integer operation (neg_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return - 1; end" );

    ( "integer operation (shr_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 14 >> 2; end" );

    ( "integer operation (shl_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 14 << 2; end" );

    ( "integer operation (eq_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return 1 == 0; end" );

    ( "integer operation (ne_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return 1 != 0; end" );

    ( "integer operation (ge_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return 1 > 0; end" );

    ( "integer operation (gt_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return 1 >= 0; end" );

    ( "integer operation (le_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return 1 <= 0; end" );

    ( "integer operation (lt_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return 1 < 0; end" );

    ( "integer operation (zdiv_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return zdiv_int(13, 4); end" );

    ( "integer operation (zrem_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return zrem_int(13, 4); end" );

    (* commented out because they are failing on some backends at the moment
    ( "integer operation (fdiv_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return fdiv_int(13, 4); end" );

    ( "integer operation (frem_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return frem_int(13, 4); end" );
    *)

    ( "integer operation (exact_div_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return exact_div_int(16, 4); end" );

    ( "integer operation (pow2_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return pow2_int(4); end" );

    ( "integer operation (is_pow2_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return is_pow2_int(16); end" );

    ( "integer operation (align_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return align_int(19, 4); end" );

    ( "integer operation (mod_pow2_int)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return mod_pow2_int(19, 4); end" );

    ( "built-in integer procedure call (print_int_dec)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin print_int_dec(42); end" );

    ( "built-in integer procedure call (print_int_hex)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin print_int_hex(42); end" );

  ]

let enum_ops : test_case list =
  [
    ( "built-in fun call (eq_enum)",
      [ Backend_C; Backend_Verilog ],
      "enumeration T { A, B, C };
       func F() => boolean begin return A == B; end" );

    ( "built-in fun call (ne_enum)",
      [ Backend_C; Backend_Verilog ],
      "enumeration T { A, B, C };
       func F() => boolean begin return A != B; end" );
  ]

let bit_ops : test_case list =
  [
    ( "bitvector operation (eq_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return '1' == '0'; end" );

    ( "bitvector operation (ne_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => boolean begin return '1' != '0'; end" );

    ( "bitvector operation (add_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) begin return '1' + '0'; end" );

    ( "bitvector operation (sub_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) begin return '1' - '0'; end" );

    ( "bitvector operation (mul_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) begin return '1' * '0'; end" );

    ( "bitvector operation (and_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) begin return '1' AND '0'; end" );

    ( "bitvector operation (or_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) begin return '1' OR '0'; end" );

    ( "bitvector operation (eor_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) begin return '1' EOR '0'; end" );

    ( "bitvector operation (not_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(1) begin return NOT '0'; end" );

    ( "bitvector operation (lsl_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(2) begin return lsl_bits('01', 1); end" );

    ( "bitvector operation (lsr_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(2) begin return lsr_bits('10', 1); end" );

    ( "bitvector operation (asr_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(2) begin return asr_bits('10', 1); end" );

    ( "bitvector operation (cvt_bits_sint)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return cvt_bits_sint('1'); end" );

    ( "bitvector operation (cvt_bits_uint)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return cvt_bits_uint('1'); end" );

    ( "bitvector operation (cvt_int_bits)",
      [ Backend_C ], (* TODO fails for verilog: part-select on literal *)
      "func F() => bits(1) begin return cvt_int_bits(1, 1); end" );

    ( "bitvector operation (zeros_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(2) begin return zeros_bits(2); end" );

    ( "bitvector operation (ones_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(2) begin return ones_bits(2); end" );

    ( "bitvector operation (mk_mask)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(2) begin return mk_mask(1, 2); end" );

    ( "bitvector operation (zero_extend_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(4) begin return zero_extend_bits('10', 4); end" );

    ( "bitvector operation (append_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(3) begin return append_bits('1', '11'); end" );

    ( "bitvector operation (replicate_bits)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(2) begin return replicate_bits('1', 2); end" );

    ( "bitvector operation (print_bits_hex)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin print_bits_hex('0'); end" );
  ]

let ram_ops : test_case list =
  [
    ( "built-in ram procedure call (ram_init)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var m : __RAM(12); ram_init(12, 1, m, '00000001'); end" );

    ( "built-in ram fun call (ram_read)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(8) begin var m : __RAM(12); return ram_read(12, 1, m, '100000000000'); end" );

    ( "built-in ram procedure call (ram_write)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var m : __RAM(12); ram_write(12, 1, m, '100000000000', '00000001'); end" );
  ]

let misc_ops : test_case list =
  [
    ( "built-in procedure call (print_char)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin print_char(0); end" );

    ( "built-in procedure call (print_str)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin print_str(\"a string\"); end" );
  ]

let fun_decl : test_case list  =
  [
    ( "built-in",
      [ Backend_C; Backend_Verilog ],
      "__builtin func f() => integer;" );

    ( "type",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer;" );

    ( "definition",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin end" );

    ( "definition with params",
      [ Backend_C; Backend_Verilog ],
      "func F(x : integer, y : integer) => integer begin end" );
  ]

let proc_decl : test_case list  =
  [
    ( "type",
      [ Backend_C; Backend_Verilog ],
      "func F();" );

    ( "definition",
      [ Backend_C; Backend_Verilog ],
      "func F() begin end" );

    ( "definition with params",
      [ Backend_C; Backend_Verilog ],
      "func F(x : integer, y : integer) begin end" );
  ]

let stmt : test_case list  =
  [
    ( "uninitialized variable",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x : integer; end" );

    ( "uninitialized variables",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x, y : integer; end" );

    ( "uninitialized variables (__RAM)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x, y : __RAM(8); end" );

    ( "variable",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x = 0; end" );

    ( "variable (wildcard)",
      [ Backend_C ],
      "func F() begin var - = 0; end" );

    ( "variable (__RAM)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x : __RAM(8); end" );

    ( "constant",
      [ Backend_C; Backend_Verilog ],
      "func F() begin let x = 0; end" );

    ( "assignment",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x : integer; x = 0; end" );

    ( "assignment to slice",
      [ Backend_Verilog ],
      "func F() begin var x : bits(8); x[4 +: 2] = '10'; end" );

    ( "assignment to field",
      [ Backend_C; Backend_Verilog ],
      "record X { i : integer; }; func F() begin var x : X; x.i = 0; end" );

    ( "assignment to wildcard",
      [ Backend_C ],
      "func F() begin - = 0; end" );

    ( "assignment to array element",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x : array [1] of bits(1); x[0] = '0'; end" );

    ( "procedure call",
      [ Backend_C; Backend_Verilog ],
      "func B() begin end func F() begin B(); end" );

    ( "procedure call with argument",
      [ Backend_C; Backend_Verilog ],
      "func B(x : integer) begin end func F() begin B(0); end" );

    ( "procedure return",
      [ Backend_C; Backend_Verilog ],
      "func F() begin return; end" );

    ( "function return",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 0; end" );

    ( "assert",
      [ Backend_C; Backend_Verilog ],
      "func F() begin assert FALSE; end" );

    ( "if",
      [ Backend_C; Backend_Verilog ],
      "func F() begin if FALSE then return; elsif FALSE then return; else return; end end" );

    ( "if with several elsifs",
      [ Backend_C; Backend_Verilog ],
      "func F() begin if FALSE then return; elsif FALSE then return; elsif FALSE then return; end end" );

    ( "case",
      [ Backend_C; Backend_Verilog ],
      "func F() begin case 0 of when 0 => return; otherwise => return; end end" );

    ( "case with several whens",
      [ Backend_C; Backend_Verilog ],
      "func F() begin case 0 of when 0 => return; when 1 => return; end end" );

    ( "case with pattern list",
      [ Backend_C; Backend_Verilog ],
      "func F() begin case 0 of when 0, 1, 2 => return; when 3 => return; end end" );

    ( "for loop (direction to)",
      [ Backend_C ],
      "func F() begin for x = 0 to 1 do return; end end" );

    ( "for loop (direction downto)",
      [ Backend_C ],
      "func F() begin for x = 1 downto 0 do return; end end" );

    ( "while loop",
      [ Backend_C ],
      "func F() begin while TRUE do return; end end" );

    ( "repeat loop",
      [ Backend_C ],
      "func F() begin repeat return; until TRUE; end" );

    ( "block",
      [ Backend_C; Backend_Verilog ],
      "func F() begin begin end end" );
  ]

let type_decl : test_case list  =
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
       record X { i : integer; b : bit; };" );

    ( "typedef",
      [ Backend_C; Backend_Verilog ],
      "type Byte of bits(8);" );

    ( "typedef (register)",
      [ Backend_C; Backend_Verilog ],
      "type Reg of bits(9) { [8] i [1] b };" );

    ( "enumeration",
      [ Backend_C; Backend_Verilog ],
      "enumeration MyEnum { A, B };" );
  ]

let var_decl : test_case list  =
  [
    ( "bits",
      [ Backend_C; Backend_Verilog ],
      "var x : bits(8);" );

    ( "integer",
      [ Backend_C; Backend_Verilog ],
      "var x : integer;" );

    ( "array",
      [ Backend_C; Backend_Verilog ],
      "var x : array [1] of integer;" );

    ( "array2",
      [ Backend_C; Backend_Verilog ],
      "var x : array [1] of array [2] of integer;" );

    ( "__RAM",
      [ Backend_C; Backend_Verilog ],
      "var x : __RAM(8);" );

    ( "const (integer)",
      [ Backend_C ],
      "let x : integer = 0;" );

    ( "const (array)",
      [ Backend_C ],
      "let x : array [3] of integer = array(3, 4, 5);" );

    ( "config (integer)",
      [ Backend_C ],
      "config x : integer = 0;" );
  ]

(****************************************************************
 * End
 ****************************************************************)
