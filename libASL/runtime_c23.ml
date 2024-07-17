(****************************************************************
 * Bit-precise runtime library support
 * This uses the C23 "Bit-precise integer" feature "_BitInt"
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C runtime library support (fallback) *)

module PP = Format
module V = Value
module RT = Runtime
open Utils

module Runtime : RT.RuntimeLib = struct

  (* All definitions in the runtime library use the "ASL_" prefix *)
  let asl_keyword (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt ("ASL_" ^ s)

  let int_width = 128

  (* signed and unsigned ints
   *
   * Note that _BitInt(0) is not supported so we have to use _BitInt(1) instead
   * and some of the operations that produce a bits(0) value have to make sure
   * that the value is always 0 (to make other operations that consume a bits(0)
   * value simpler).
   *
   * In addition, when calling external functions, all backends
   * need to pass values using the same representation so,
   * for simplicity, we use 'uint<n>_t' for bits(8|16|32|64)
   * and 'unsigned|signed __int128' for bits128 and integer
   *)
  let ty_sint (fmt : PP.formatter) (width : int) : unit =
    if width = 128 then
      PP.fprintf fmt "__int128"
    else if List.mem width [8; 16; 32; 64] then
      PP.fprintf fmt "int%d_t" width
    else
      PP.fprintf fmt "signed _BitInt(%d)" (max 1 width)

  let ty_uint (fmt : PP.formatter) (width : int) : unit =
    if width = 128 then
      PP.fprintf fmt "unsigned __int128"
    else if List.mem width [8; 16; 32; 64] then
      PP.fprintf fmt "uint%d_t" width
    else
      PP.fprintf fmt "unsigned _BitInt(%d)" (max 1 width)

  (* #defines that are needed by this runtime variant *)
  let c_defines : string list = [
      "#ifndef ASL_C23";
      "#define ASL_C23";
      "#endif"
  ]

  let ty_int (fmt : PP.formatter) : unit = ty_sint fmt int_width
  let ty_bits (fmt : PP.formatter) (width : int) : unit = ty_uint fmt width
  let ty_ram (fmt : PP.formatter) : unit = asl_keyword fmt "ram_t"

  let maxint = Z.sub (Z.shift_left Z.one (int_width - 1)) Z.one

  let int_literal (fmt : PP.formatter) (x : Z.t) : unit =
    if Z.geq x Z.zero then
      PP.fprintf fmt "((%a)0x%swb)"
        ty_sint int_width
        (Z.format "%x" x)
    else (* negative values *)
      PP.fprintf fmt "(-(%a)0x%swb)"
        ty_sint int_width
        (Z.format "%x" (Z.neg x))

  let empty_bits (fmt : PP.formatter) : unit = PP.pp_print_string fmt "0uwb"

  let bits_literal (fmt : PP.formatter) (x : Primops.bitvector) : unit =
    if x.n = 0 then
      empty_bits fmt
    else
      PP.fprintf fmt "((%a)0x%suwb)"
        ty_bits x.n
        (Z.format "%x" x.v)

  let unop (fmt : PP.formatter) (op : string) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "(%s %a)"
      op
      RT.pp_expr x

  let binop (fmt : PP.formatter) (op : string) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a %s %a)"
      RT.pp_expr x
      op
      RT.pp_expr y

  (* Calculate mask with x ones *)
  let mask_int (fmt : PP.formatter) (x : RT.rt_expr) : unit =
      PP.fprintf fmt "(%a >> (%d - %a))"
      int_literal maxint
      int_width
      RT.pp_expr x

  let add_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "+" x y
  let mul_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "*" x y
  let sub_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "-" x y
  let neg_int (fmt : PP.formatter) (x : RT.rt_expr) : unit = unop fmt "-" x
  let zdiv_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "/" x y
  let zrem_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "%" x y
  let shr_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<<" x y
  let shl_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">>" x y

  let eq_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "==" x y
  let ne_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "!=" x y
  let ge_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">=" x y
  let gt_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">" x y
  let le_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<=" x y
  let lt_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<" x y

  let exact_div_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "/" x y
  let fdiv_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "/" x y
  let frem_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "%" x y

  let is_pow2_int (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "({ @[%a __tmp = %a; "
      ty_sint int_width
      RT.pp_expr x;
    PP.fprintf fmt "__tmp != 0 && (__tmp & (__tmp - 1)) == 0; })"

  let print_int_hex (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    (* Print small numbers in decimal, large numbers in hex *)
    PP.fprintf fmt "@[{ %a __tmp = %a;@,"
      ty_sint 128
      RT.pp_expr x;
    PP.fprintf fmt "  %a __top = (%a)(__tmp >> 64);@,"
      ty_sint 64
      ty_sint 64;
    PP.fprintf fmt "  %a __bottom = (%a)__tmp;@,"
      ty_sint 64
      ty_sint 64;
    PP.fprintf fmt "  if (__top == 0 && __bottom >= 0) {@,";
    PP.fprintf fmt "    printf(\"%%#lx\", __bottom);@,";
    PP.fprintf fmt "  } else if (__top == -1 && __bottom < 0 && __bottom != INT64_MIN) {@,";
    PP.fprintf fmt "    printf(\"-%%#lx\", -__bottom);@,";
    PP.fprintf fmt "  } else {@,";
    PP.fprintf fmt "    printf(\"0x%%08lx_%%08lx\", __top, __bottom);@,";
    PP.fprintf fmt "  }@,";
    PP.fprintf fmt "}@]"

  let print_int_dec (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    (* Print small numbers in decimal, large numbers in hex *)
    PP.fprintf fmt "@[{ %a __tmp = %a;@,"
      ty_sint 128
      RT.pp_expr x;
    PP.fprintf fmt "  %a __top = (%a)(__tmp >> 64);@,"
      ty_sint 64
      ty_sint 64;
    PP.fprintf fmt "  %a __bottom = (%a)__tmp;@,"
      ty_sint 64
      ty_sint 64;
    PP.fprintf fmt "  if ((__top == 0 && __bottom >= 0) || (__top == -1 && __bottom < 0)) {@,";
    PP.fprintf fmt "    printf(\"%%ld\", __bottom);@,";
    PP.fprintf fmt "  } else {@,";
    PP.fprintf fmt "    printf(\"0x%%08lx_%%08lx\", __top, __bottom);@,";
    PP.fprintf fmt "  }@,";
    PP.fprintf fmt "}@]"

  let pow2_int (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a << %a)"
      int_literal Z.one
      RT.pp_expr x

  (* todo: remove redundancy with mod_pow2_int *)
  let align_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a & ~%a)"
      RT.pp_expr x
      mask_int y

  let mod_pow2_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a & ~%a)"
      RT.pp_expr x
      mask_int y

  let slice_lowd (fmt : PP.formatter) (n : int) (w : int) (x : RT.rt_expr) (i : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)(%a >> %a))"
      ty_bits w
      RT.pp_expr x
      RT.pp_expr i

  let eq_bits  (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "==" x y
  let ne_bits  (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "!=" x y
  let add_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "+" x y
  let sub_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "-" x y
  let mul_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "*" x y
  let and_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "&" x y
  let or_bits  (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "|" x y
  let eor_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "^" x y

  let not_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; 0uwb; })" RT.pp_expr x
    else
      unop fmt "~" x

  let lsl_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<<" x y
  let lsr_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">>" x y

  let asr_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)(((%a)%a) >> %a))"
      ty_uint n
      ty_sint n
      RT.pp_expr x
      RT.pp_expr y

  let cvt_bits_sint (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)((%a)%a))"
      ty_sint int_width
      ty_uint int_width
      RT.pp_expr x

  let cvt_bits_uint (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)"
      ty_uint int_width
      RT.pp_expr x

  let cvt_int_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; 0uwb; })" RT.pp_expr x
    else
      PP.fprintf fmt "((%a)((%a)%a))"
        ty_uint n
        ty_uint int_width
        RT.pp_expr x

  let zeros_bits (fmt : PP.formatter) (n : int) : unit =
    PP.fprintf fmt "((%a)0uwb)"
      ty_uint n

  let ones_bits (fmt : PP.formatter) (n : int) : unit =
    if n = 0 then
      empty_bits fmt
    else
      PP.fprintf fmt "(~((%a)0uwb))"
        ty_uint n

  let mk_mask (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; 0uwb; })" RT.pp_expr x
    else
      PP.fprintf fmt "(%a >> (%d - %a))"
      ones_bits n
      n
      RT.pp_expr x

  let zero_extend_bits (fmt : PP.formatter) (m : int) (n : int)  (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)"
    ty_uint n
    RT.pp_expr x

  let print_bits_hex (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then begin
      (* Although we print empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "(void)%a;@," RT.pp_expr x;
      PP.fprintf fmt "printf(\"%%08llx\", 0ULL)"
    end else begin
      let chunks = (n+63) / 64 in
      PP.fprintf fmt "{ @[%a __tmp = %a; "
        ty_bits n
        RT.pp_expr x;

      PP.fprintf fmt "printf(\"%d'x\"); " n;
      for i = 0 to chunks-1 do
          PP.fprintf fmt "printf(\"%%08llx\", (long long)(__tmp >> %d)); "
            (64 * (chunks - i - 1))
      done;
      PP.fprintf fmt "@]}"
    end

  let in_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (m : Primops.mask) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; true; })" RT.pp_expr x
    else
      PP.fprintf fmt "((%a & %s) == %s)"
        RT.pp_expr x
        (Z.format "%#xuwb" m.m)
        (Z.format "%#xuwb" m.v)

  let append_bits (fmt : PP.formatter) (m : int) (n : int)  (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    let result_width = m + n in
    let x' fmt = zero_extend_bits fmt m result_width x in
    let y' fmt = zero_extend_bits fmt n result_width y in
    PP.fprintf fmt "((%a << %d) | %a)"
      RT.pp_expr x'
      n
      RT.pp_expr y'

  (* Replicate{m,n}(x : bits(m), n : integer) 
   *
   * This generates inline code that calculates the result in O(log2(n))
   * by ORing together the result of appending 2^p copies of x.
   *
   * Replicate{m,11}(x, 11)   (0d11 == 0b1101)
   * ==>
   * ({
   * bits(11*m) _tmp1 = (5*m)x;
   * bits(11*m) _tmp2 = (tmp1 << (1*m)) | tmp1;
   * bits(11*m) _tmp4 = (tmp2 << (2*m)) | tmp2;
   * bits(11*m) _tmp8 = (tmp4 << (4*m)) | tmp4;
   * 0 | tmp1 << (0*m) | _tmp4 << (1*m) | tmp8 << (5*m)
   * })
   *
   * This is a significant improvement over the more obvious O(n) algorithm
   * because n is often 8, 32 or even 512.
   *)
  let replicate_bits (fmt : PP.formatter) (m : int) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = begin
    let result_width = m * n in
    let n = Z.of_int n in
    let logn = Z.log2 n in

    (* calculate intermediate results *)
    PP.fprintf fmt "({ @[<v>%a __tmp1 = ((%a)%a);@,"
      ty_bits result_width
      ty_bits result_width
      RT.pp_expr x;
    for i = 1 to logn do
      PP.fprintf fmt "%a __tmp%d = (__tmp%d << %d) | __tmp%d;@,"
        ty_bits result_width
        (pow2 i)
        (pow2 (i-1))
        (m * pow2 (i-1))
        (pow2 (i-1))
    done;

    (* calculate final result *)
    zeros_bits fmt result_width;
    let shift = ref 0 in (* amount to shift next value by *)
    for i = 0 to logn do
      if Z.testbit n i then begin
        PP.fprintf fmt " | __tmp%d << %d" (pow2 i) !shift;
        shift := !shift + m * pow2 i
      end
    done;
    PP.fprintf fmt "; @]})"
  end

  let ram_init (fmt : PP.formatter) (a : int) (n : int) (ram : RT.rt_expr) (v : RT.rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %a, %a)"
      asl_keyword "ram_init"
      a
      n
      RT.pp_expr ram
      RT.pp_expr v

  let ram_read (fmt : PP.formatter) (a : int) (n : int) (ram : RT.rt_expr) (addr : RT.rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %a, %a)"
      asl_keyword "ram_read"
      a
      n
      RT.pp_expr ram
      RT.pp_expr addr

  let ram_write (fmt : PP.formatter) (a : int) (n : int) (ram : RT.rt_expr) (addr : RT.rt_expr) (v : RT.rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %a, %a, %a)"
      asl_keyword "ram_write"
      a
      n
      RT.pp_expr ram
      RT.pp_expr addr
      RT.pp_expr v

  let print_char (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "putchar(%a)" RT.pp_expr x

  let print_str (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "fputs(%a, stdout)" RT.pp_expr x
end

(****************************************************************
 * End
 ****************************************************************)
