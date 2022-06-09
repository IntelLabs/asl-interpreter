////////////////////////////////////////////////////////////////
// ASL standard prelude
//
// Copyright Arm Limited (c) 2017-2019
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

__builtin type real;
__builtin type string;
__builtin type __mask; // todo: should have a type parameter
__builtin type __Exception;
__builtin type __RAM; // todo: should have a type parameter

type bit = bits(1);

enumeration boolean { FALSE, TRUE };
enumeration signal { LOW, HIGH };

__builtin func eq_bool(x :: boolean, y :: boolean) => boolean;
__builtin func ne_bool(x :: boolean, y :: boolean) => boolean;
__builtin func not_bool(x :: boolean) => boolean;
__builtin func and_bool(x :: boolean, y :: boolean) => boolean;
__builtin func or_bool(x :: boolean, y :: boolean) => boolean;
__builtin func equiv_bool(x :: boolean, y :: boolean) => boolean;
__builtin func implies_bool(x :: boolean, y :: boolean) => boolean;

__builtin func eq_int(x :: integer, y :: integer) => boolean;
__builtin func ne_int(x :: integer, y :: integer) => boolean;
__builtin func gt_int(x :: integer, y :: integer) => boolean;
__builtin func ge_int(x :: integer, y :: integer) => boolean;
__builtin func le_int(x :: integer, y :: integer) => boolean;
__builtin func lt_int(x :: integer, y :: integer) => boolean;
__builtin func is_pow2_int(x :: integer) => boolean;
__builtin func add_int(x :: integer, y :: integer) => integer;
__builtin func neg_int(x :: integer) => integer;
__builtin func sub_int(x :: integer, y :: integer) => integer;
__builtin func shl_int(x :: integer, y :: integer) => integer;
__builtin func shr_int(x :: integer, y :: integer) => integer;
__builtin func mul_int(x :: integer, y :: integer) => integer;
__builtin func zdiv_int(x :: integer, y :: integer) => integer;
__builtin func zrem_int(x :: integer, y :: integer) => integer;
__builtin func fdiv_int(x :: integer, y :: integer) => integer;
__builtin func frem_int(x :: integer, y :: integer) => integer;
__builtin func mod_pow2_int(x :: integer, y :: integer) => integer;
__builtin func align_int(x :: integer, y :: integer) => integer;
__builtin func pow2_int(y :: integer) => integer;

__builtin func cvt_int_real(x :: integer) => real;
__builtin func eq_real(x :: real, y :: real) => boolean;
__builtin func ne_real(x :: real, y :: real) => boolean;
__builtin func le_real(x :: real, y :: real) => boolean;
__builtin func lt_real(x :: real, y :: real) => boolean;
__builtin func gt_real(x :: real, y :: real) => boolean;
__builtin func ge_real(x :: real, y :: real) => boolean;
__builtin func add_real(x :: real,    y :: real) => real;
__builtin func neg_real(x :: real) => real;
__builtin func sub_real(x :: real, y :: real) => real;
__builtin func mul_real(x :: real, y :: real) => real;
__builtin func divide_real(x :: real, y :: real) => real;
__builtin func pow2_real(y :: integer) => real;
__builtin func round_tozero_real(x :: real) => integer;
__builtin func round_down_real(x :: real) => integer;
__builtin func round_up_real(x :: real) => integer;
__builtin func sqrt_real(x :: real) => real;

__builtin func cvt_int_bits(x :: integer, N :: integer) => bits(N);
__builtin func cvt_bits_sint{N}(x :: bits(N)) => integer;
__builtin func cvt_bits_uint{N}(x :: bits(N)) => integer;
__builtin func in_mask{N}(x :: bits(N), y :: __mask(N)) => boolean;
__builtin func notin_mask{N}(x :: bits(N), y :: __mask(N)) => boolean;
__builtin func eq_bits{N}(x :: bits(N), y :: bits(N)) => boolean;
__builtin func ne_bits{N}(x :: bits(N), y :: bits(N)) => boolean;
__builtin func add_bits{N}(x :: bits(N), y :: bits(N)) => bits(N);
__builtin func sub_bits{N}(x :: bits(N), y :: bits(N)) => bits(N);
__builtin func mul_bits{N}(x :: bits(N), y :: bits(N)) => bits(N);
__builtin func frem_bits_int{N}(x :: bits(N), y :: integer) => integer;
__builtin func and_bits{N}(x :: bits(N), y :: bits(N)) => bits(N);
__builtin func or_bits{N}(x :: bits(N), y :: bits(N)) => bits(N);
__builtin func eor_bits{N}(x :: bits(N), y :: bits(N)) => bits(N);
__builtin func not_bits{N}(x :: bits(N)) => bits(N);
__builtin func zeros_bits{N}() => bits(N);
__builtin func ones_bits{N}() => bits(N);

func add_bits_int{N}(x :: bits(N), y :: integer) => bits(N)
    return add_bits(x, cvt_int_bits(y, N));
end

func sub_bits_int{N}(x :: bits(N), y :: integer) => bits(N)
    return sub_bits(x, cvt_int_bits(y, N));
end

// Bit slice helper functions used in some backends
func asl_extract_int(x :: integer, lo :: integer, W :: integer) => bits(W)
    return x[lo +: W];
end

// Bit slice helper functions used in some backends
func asl_extract_bits(x :: bits(N), lo :: integer, W :: integer) => bits(W)
    return x[lo +: W];
end

// Bit slice helper functions used in some backends
func asl_bits_set(x :: bits(N), lo :: integer, v :: bits(W))
    x[lo +: W] = v;
end

__operator2 + = add_int, add_real, add_bits, add_bits_int;
__operator2 - = sub_int, sub_real, sub_bits, sub_bits_int;
__operator1 - = neg_int, neg_real;
__operator2 * = mul_int, mul_real, mul_bits;
__operator2 / = divide_real;

__builtin func replicate_bits{M}(x :: bits(M), N :: integer) => bits(M*N);
__builtin func append_bits{M, N}(x :: bits(M), y :: bits(N)) => bits(M+N);

__builtin func is_cunpred_exc(ex :: __Exception) => boolean;
__builtin func is_exctaken_exc(ex :: __Exception) => boolean;
__builtin func is_impdef_exc(ex :: __Exception) => boolean;
__builtin func is_see_exc(ex :: __Exception) => boolean;
__builtin func is_undefined_exc(ex :: __Exception) => boolean;
__builtin func is_unpred_exc(ex :: __Exception) => boolean;

__builtin func cvt_int_hexstr(x :: integer) => string;
__builtin func cvt_int_decstr(x :: integer) => string;
__builtin func cvt_bool_str(x :: boolean) => string;
__builtin func cvt_bits_str(N :: integer, x :: bits(N)) => string;
__builtin func cvt_real_str(x :: real) => string;
__builtin func append_str_str(x :: string, y :: string) => string;
__builtin func eq_str(x :: string, y :: string) => boolean;
__builtin func ne_str(x :: string, y :: string) => boolean;
__builtin func print_str(x :: string) => ();
__builtin func print_char(x :: integer) => ();
__builtin func print_bits(x :: bits(N)) => ();

__builtin func asl_pragma(x :: string) => ();

__builtin func asl_file_open(name :: string, mode :: string) => integer;
__builtin func asl_file_write(fd :: integer, data :: string) => integer;
__builtin func asl_file_getc(fd :: integer) => integer;

__builtin func ram_init(A :: integer, N :: integer, ram :: __RAM(A), val :: bits(8*N)) => ();
__builtin func ram_read(A :: integer, N :: integer, ram :: __RAM(A), address :: bits(A)) => bits(8*N);
__builtin func ram_write(A :: integer, N :: integer, ram :: __RAM(A), address :: bits(A), val :: bits(8*N)) => ();

func __InitRAM(A :: integer, N :: integer, ram :: __RAM(A), val :: bits(8*N))
    ram_init(A, N, ram, val);
end

func __ReadRAM(A :: integer, N :: integer, ram :: __RAM(A), address :: bits(A)) => bits(8*N)
    return ram_read(A, N, ram, address);
end

func __WriteRAM(A :: integer, N :: integer, ram :: __RAM(A), address :: bits(A), val :: bits(8*N))
    ram_write(A, N, ram, address, val);
end

__builtin func trace_memory_write{A}(N :: integer, address :: bits(A), val :: bits(8*N)) => ();
__builtin func trace_memory_read{A}(N :: integer, address :: bits(A), val :: bits(8*N)) => ();
__builtin func trace_event(event :: string) => ();

func __tarmacEvent(event :: string)
    trace_event(event);
end

__builtin func sleep_request() => ();
__builtin func wakeup_request() => ();
__builtin func program_end() => ();

__builtin func decodeInstr_A64(instr :: bits(32)) => ();
__builtin func decodeInstr_A32(instr :: bits(32)) => ();
__builtin func decodeInstr_T32(instr :: bits(32)) => ();
__builtin func decodeInstr_T16(instr :: bits(16)) => ();

func print{N}(x :: bits(N))
    print_bits(x);
end

func print(x :: string)
    print_str(x);
end

func println()
    print_char(10);
end

func println(x :: string)
    print_str(x);
    print_char(10);
end

func putchar(c :: integer)
    print_char(c);
end

func __abort()
    program_end();
end

__operator1 !       = not_bool;
__operator2 &&      = and_bool;
__operator2 ||      = or_bool;
__operator2 <->     = equiv_bool;
__operator2 -->     = implies_bool;

// omit since they are auto-generated
// __operator2 == = eq_bool;
// __operator2 != = ne_bool;

__operator2 == = eq_int, eq_real, eq_bits, eq_str, in_mask;
__operator2 != = ne_int, ne_real, ne_bits, ne_str, notin_mask;
__operator2 <= = le_int, le_real;
__operator2 >= = ge_int, ge_real;
__operator2 <  = lt_int, lt_real;
__operator2 >  = gt_int, gt_real;

func eq_bits_int(x :: bits(N), y :: integer) => boolean
    return x == y[0 +: N];
end

__operator2 == = eq_bits_int; // workaround

func shift_left_int(x :: integer, y :: integer) => integer
    return if y >= 0 then shl_int(x, y) else shr_int(x, -y);
end

func shift_right_int(x :: integer, y :: integer) => integer
    return if y >= 0 then shr_int(x, y) else shl_int(x, -y);
end

__operator2 << = shift_left_int;
__operator2 >> = shift_right_int;

func IsPowerOfTwo(x :: integer) => boolean
    return is_pow2_int(x);
end

func pow_int_int(x :: integer, y :: integer) => integer
    if x == 2 then
        return pow2_int(y); // optimized case
    else
        assert y >= 0;
        var result = 1;
        for i = 1 to y do
            result = result * x;
        end
        return result;
    end
end

func pow_real_int(x :: real, y :: integer) => real
    assert x == 2.0;
    return pow2_real(y);
end

__operator2 ^ = pow_int_int, pow_real_int;

func Real(x :: integer) => real
    return cvt_int_real(x);
end

func frem_bits_int{N}(x :: bits(N), y :: integer) => integer
    assert y > 0;
    return frem_int(cvt_bits_uint(x), y);
end

// Division: round to zero
__operator2 QUOT = zdiv_int;
__operator2 REM  = zrem_int;

// Division: round to -infinity (floor)
__operator2 DIV  = fdiv_int;
__operator2 MOD  = frem_int, frem_bits_int;

__operator2 AND  = and_bits;
__operator2 OR   = or_bits;
__operator2 EOR  = eor_bits;
__operator1 NOT  = not_bits;

func HexStr(x :: integer) => string
    return cvt_int_hexstr(x);
end

func DecStr(x :: integer) => string
    return cvt_int_decstr(x);
end

func append_str_bool(x :: string, y :: boolean) => string
    return append_str_str(x, cvt_bool_str(y));
end

func append_bool_str(x :: boolean, y :: string) => string
    return append_str_str(cvt_bool_str(x), y);
end

func append_str_bits{N}(x :: string, y :: bits(N)) => string
    return append_str_str(x, cvt_bits_str(N, y));
end

func append_bits_str{N}(x :: bits(N), y :: string) => string
    return append_str_str(cvt_bits_str(N, x), y);
end

func append_str_real(x :: string, y :: real) => string
    return append_str_str(x, cvt_real_str(y));
end

func append_real_str(x :: real, y :: string) => string
    return append_str_str(cvt_real_str(x), y);
end

func append_str_int(x :: string, y :: integer) => string
    return append_str_str(x, DecStr(y));
end

func append_int_str(x :: integer, y :: string) => string
    return append_str_str(DecStr(x), y);
end

__operator2 ++ = append_str_str;
__operator2 ++ = append_str_bool, append_bool_str;
__operator2 ++ = append_str_real, append_real_str;
__operator2 ++ = append_str_bits, append_bits_str;
__operator2 ++ = append_str_int,  append_int_str;

func Replicate{M}(x :: bits(M), N :: integer) => bits(M*N)
    return replicate_bits(x, N);
end

func Replicate{M, N}(x :: bits(M)) => bits(N)
    assert N MOD M == 0;
    return replicate_bits(x, N DIV M);
end

func Zeros(N :: integer) => bits(N)
    return zeros_bits();
end

func Ones(N :: integer) => bits(N)
    return ones_bits();
end

func Zeros{N}() => bits(N)
    return zeros_bits();
end

func Ones{N}() => bits(N)
    return ones_bits();
end

func IsOnes{N}(x :: bits(N)) => boolean
    return x == Ones();
end

func IsZero{N}(x :: bits(N)) => boolean
    return x == Zeros();
end

func SignExtend{M}(x :: bits(M), N :: integer) => bits(N)
    assert N >= M;
    let sign = x[M-1];
    return [Replicate(sign, N-M), x];
end

func ZeroExtend{M}(x :: bits(M), N :: integer) => bits(N)
    assert N >= M;
    return [Zeros(N-M), x];
end

func Sqrt(x :: real) => real
    return sqrt_real(x);
end

func RoundTowardsZero(x :: real) => integer
    return round_tozero_real(x);
end

func RoundDown(x :: real) => integer
    return round_down_real(x);
end

func RoundUp(x :: real) => integer
    return round_up_real(x);
end

func IsUNDEFINED(x :: __Exception) => boolean
    return is_undefined_exc(x);
end

func IsUNPREDICTABLE(x :: __Exception) => boolean
    return is_unpred_exc(x);
end

func IsSEE(x :: __Exception) => boolean
    return is_see_exc(x);
end

func IsExceptionTaken(x :: __Exception) => boolean
    return is_exctaken_exc(x);
end

func UInt(N :: integer, x :: bits(N)) => integer
    return cvt_bits_uint(x);
end

func UInt(x :: bits(N)) => integer
    return cvt_bits_uint(x);
end

func SInt(N :: integer, x :: bits(N)) => integer
    return cvt_bits_sint(x);
end

func SInt{N}(x :: bits(N)) => integer
    return cvt_bits_sint(x);
end

func Align{N}(x :: bits(N), y :: integer) => bits(N)
    return align_int(cvt_bits_uint(x), y)[N-1:0];
end

func Align(x :: integer, y :: integer) => integer
    return align_int(x, y);
end


////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
