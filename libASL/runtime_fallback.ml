(****************************************************************
 * Fallback runtime library support
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C runtime library support (fallback) *)

module AST = Asl_ast
module FMT = Asl_fmt
module PP = Format
module V = Value
open Asl_utils
open Format_utils

let commasep (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  PP.pp_print_list
    ~pp_sep:(fun fmt' _ -> PP.pp_print_string fmt' ", ")
    pp
    fmt
    xs

(* Representation of an expression used by runtime libraries.
 *
 * The runtime does not directly examine expressions but it needs
 * to be able to print subexpressions.
 *
 * This type should be treated as an abstract type because we will
 * likely want to add other information later.
 *)
type rt_expr = PP.formatter -> unit

let pp_expr (fmt : PP.formatter) (x : rt_expr) : unit = x fmt
let mk_rt_expr (pp : PP.formatter -> AST.expr -> unit) (x : AST.expr) : rt_expr =
    (fun fmt -> pp fmt x)


module type RuntimeLib = sig
  (* types *)
  val ty_int : PP.formatter -> unit
  val ty_ram : PP.formatter -> unit
  val ty_bits : PP.formatter -> int -> unit

  (* literal constants *)
  val int_literal : PP.formatter -> Z.t -> unit
  val bits_literal : PP.formatter -> Primops.bitvector -> unit

  (* integer functions *)
  val add_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val mul_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val sub_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val neg_int : PP.formatter -> rt_expr -> unit
  val zdiv_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val zrem_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val shr_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val shl_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val eq_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val ne_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val ge_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val gt_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val le_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val lt_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val exact_div_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val fdiv_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val frem_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val is_pow2_int : PP.formatter -> rt_expr -> unit
  val print_int_dec : PP.formatter -> rt_expr -> unit
  val print_int_hex : PP.formatter -> rt_expr -> unit
  val align_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val pow2_int : PP.formatter -> rt_expr -> unit
  val mod_pow2_int : PP.formatter -> rt_expr -> rt_expr -> unit

  (* bitvector functions *)
  val slice_lowd : PP.formatter -> int -> int -> rt_expr -> rt_expr -> unit
  val eq_bits  : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val ne_bits  : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val add_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val sub_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val mul_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val and_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val or_bits  : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val eor_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val not_bits : PP.formatter -> int -> rt_expr -> unit
  val lsl_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val lsr_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val asr_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val cvt_bits_sint : PP.formatter -> int -> rt_expr -> unit
  val cvt_bits_uint : PP.formatter -> int -> rt_expr -> unit
  val cvt_int_bits : PP.formatter -> int -> rt_expr -> unit
  val zeros_bits : PP.formatter -> int -> unit
  val ones_bits : PP.formatter -> int -> unit
  val mk_mask : PP.formatter -> int -> rt_expr -> unit
  val zero_extend_bits : PP.formatter -> int -> int -> rt_expr -> unit
  val print_bits_hex : PP.formatter -> int -> rt_expr -> unit
  val in_bits : PP.formatter -> int -> rt_expr -> Primops.mask -> unit
  val append_bits : PP.formatter -> int -> int -> rt_expr -> rt_expr -> unit
  val replicate_bits : PP.formatter -> int -> int -> rt_expr -> rt_expr -> unit

  (* RAM functions *)
  val ram_init : PP.formatter -> int -> int -> rt_expr -> rt_expr -> unit
  val ram_read : PP.formatter -> int -> int -> rt_expr -> rt_expr -> unit
  val ram_write : PP.formatter -> int -> int -> rt_expr -> rt_expr -> rt_expr -> unit

end


module Runtime : RuntimeLib = struct

  (* All definitions in the runtime library use the "ASL_" prefix *)
  let asl_keyword (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt ("ASL_" ^ s)

  let min_int (num_bits : int) : Z.t = Z.shift_left Z.minus_one (num_bits - 1)
  let max_int (num_bits : int) : Z.t = Z.lognot (min_int num_bits)

  (* Round up to the next power of 2 *)
  let round_up_to_pow2 (x : int) : int =
    let x = Z.log2up (Z.of_int x) in
    Z.to_int (Z.shift_left Z.one x)

  (* Return the number of bits necessary to represent an integer in binary,
     including the sign bit *)
  let bit_length (x : Z.t) : int =
    let x' = if Z.sign x = -1 then Z.succ x else x in
    (* +1 for sign bit, not taken into account by Z.numbits *)
    Z.numbits x' + 1

  (* C types defined elsewhere *)
  let ty_int (fmt : PP.formatter) : unit = asl_keyword fmt "int_t"
  let ty_ram (fmt : PP.formatter) : unit = asl_keyword fmt "ram_t"

  let c_int_width_64up (width : int) : int =
    if width > 64 then round_up_to_pow2 width else 64

  let ty_bits (fmt : PP.formatter) (width : int) : unit =
    asl_keyword fmt ("bits" ^ string_of_int (c_int_width_64up width) ^ "_t")

  (* Generate ty_int<n>_MIN macro constant *)
  let minint_constant (fmt : PP.formatter) (n : int) : unit =
    PP.pp_print_string fmt ("ASL_INT" ^ string_of_int n ^ "_MIN")

  (* Generate ty_int<n>_MAX macro constant *)
  let maxint_constant (fmt : PP.formatter) (n : int) : unit =
    PP.pp_print_string fmt ("ASL_INT" ^ string_of_int n ^ "_MAX")

  (* Try generating min/max macro constants *)
  let int_constant (fmt : PP.formatter) (n : int) (x : Z.t)
      (f : PP.formatter -> Z.t -> unit) : unit =
    if Z.equal x (min_int n) then
      minint_constant fmt n
    else if Z.equal x (max_int n) then
      maxint_constant fmt n
    else
      f fmt x

  let int_literal (fmt : PP.formatter) (x : Z.t) : unit =
    let int_literal_fit_int64 (fmt : PP.formatter) (x : Z.t) : unit =
      int_constant fmt 64 x (fun fmt x -> PP.pp_print_string fmt (Z.format "%d" x ^ "LL"))
    in

    (* Integer literal which does not fit 64-bit integer.
     * Generates a function invocation of the form ty_int_N(.., a1, a0)
     * where a0, a1, ... are 64-bit slices of the literal with a0 as the least
     * significant slice. The N of the name ty_int_N is the resulting integer
     * width rounded to the power of 2. e.g. 128, 256, ...
     *)
    let int_literal_not_fit_int64 (fmt : PP.formatter) (x : Z.t) : unit =
      let num_bits = round_up_to_pow2 (bit_length x) in
      int_constant fmt num_bits x (fun fmt x ->
          let hex_string =
            Z.format
              ("%0" ^ string_of_int (num_bits / 4) ^ "x")
              (Z.extract x 0 num_bits)
          in
          let num_limbs = num_bits / 64 in
          let limbs =
            List.init num_limbs (fun i ->
                let pos = i * 16 in
                "0x" ^ String.sub hex_string pos 16 ^ "ULL")
          in
          asl_keyword fmt ("int_" ^ string_of_int num_bits);
          parens fmt (fun _ -> commasep PP.pp_print_string fmt limbs)
      )
    in

    if Z.fits_int64 x then
      int_literal_fit_int64 fmt x
    else
      int_literal_not_fit_int64 fmt x

  let bits_literal (fmt : PP.formatter) (x : Primops.bitvector) : unit =
    let bit_to_hex (b : Z.t) : string =
      Z.format "%#x" b ^ "ULL"
    in
    if x.n <= 64 then begin
        PP.pp_print_string fmt (bit_to_hex x.v)
    end else begin
      let num_bits = round_up_to_pow2 x.n in
      let num_limbs = (num_bits / 64) in
      let limbs = List.init
          num_limbs
          (fun i -> bit_to_hex (Z.extract x.v (i * 64) 64))
      in
      asl_keyword fmt "bits";
      parens fmt (fun _ ->
          commasep PP.pp_print_string fmt (string_of_int num_bits :: limbs))
    end

  let unop (fmt : PP.formatter) (op : string) (x : rt_expr) : unit =
    PP.fprintf fmt "(%s %a)"
      op
      pp_expr x

  let binop (fmt : PP.formatter) (op : string) (x : rt_expr) (y : rt_expr) : unit =
    PP.fprintf fmt "(%a %s %a)"
      pp_expr x
      op
      pp_expr y

  let apply1 (fmt : PP.formatter) (f : string) (x : rt_expr) : unit =
    PP.fprintf fmt "%a(%a)"
      asl_keyword f
      pp_expr x

  let apply2 (fmt : PP.formatter) (f : string) (x : rt_expr) (y : rt_expr) : unit =
    PP.fprintf fmt "%a(%a, %a)"
      asl_keyword f
      pp_expr x
      pp_expr y

  (* Calculate mask with x ones *)
  let mask_int (fmt : PP.formatter) (x : rt_expr) : unit =
    apply1 fmt "mask_int" x

  let apply_bits_1_0 (fmt : PP.formatter) (f : string) (n : int) : unit =
    PP.fprintf fmt "%a(%d, %d)"
      asl_keyword f
      (c_int_width_64up n)
      n

  let apply_bits_1_1 (fmt : PP.formatter) (f : string) (n : int) (x : rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %a)"
      asl_keyword f
      (c_int_width_64up n)
      n
      pp_expr x

  let apply_bits_1_2 (fmt : PP.formatter) (f : string) (n : int) (x : rt_expr) (y : rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %a, %a)"
      asl_keyword f
      (c_int_width_64up n)
      n
      pp_expr x
      pp_expr y

  let add_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "+" x y
  let mul_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "*" x y
  let sub_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "-" x y
  let neg_int (fmt : PP.formatter) (x : rt_expr) : unit = unop fmt "-" x
  let zdiv_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "/" x y
  let zrem_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "%" x y
  let shr_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "<<" x y
  let shl_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt ">>" x y

  let eq_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "==" x y
  let ne_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "!=" x y
  let ge_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt ">=" x y
  let gt_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt ">" x y
  let le_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "<=" x y
  let lt_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = binop fmt "<" x y

  let exact_div_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = apply2 fmt "exact_div_int" x y
  let fdiv_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = apply2 fmt "fdiv_int" x y
  let frem_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit = apply2 fmt "frem_int" x y
  let is_pow2_int (fmt : PP.formatter) (x : rt_expr) : unit = apply1 fmt "is_pow2_int" x
  let print_int_dec (fmt : PP.formatter) (x : rt_expr) : unit = apply1 fmt "print_int_dec" x
  let print_int_hex (fmt : PP.formatter) (x : rt_expr) : unit = apply1 fmt "print_int_hex" x

  let pow2_int (fmt : PP.formatter) (x : rt_expr) : unit =
    PP.fprintf fmt "(((ASL_bits64_t)1) << %a)"
      pp_expr x

  (* todo: remove redundancy with mod_pow2_int *)
  let align_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit =
    PP.fprintf fmt "(%a & ~%a)"
      pp_expr x
      mask_int y

  let mod_pow2_int (fmt : PP.formatter) (x : rt_expr) (y : rt_expr) : unit =
    PP.fprintf fmt "(%a & ~%a)"
      pp_expr x
      mask_int y

  let slice_lowd (fmt : PP.formatter) (n : int) (w : int) (x : rt_expr) (i : rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %a, %a, %d)"
      asl_keyword "slice_lowd"
      (c_int_width_64up n)
      (c_int_width_64up w)
      pp_expr x
      pp_expr i
      w

  let eq_bits  (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "eq_bits" n x y
  let ne_bits  (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "ne_bits" n x y
  let add_bits (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "add_bits" n x y
  let sub_bits (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "sub_bits" n x y
  let mul_bits (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "mul_bits" n x y
  let and_bits (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "and_bits" n x y
  let or_bits  (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "or_bits" n x y
  let eor_bits (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "eor_bits" n x y
  let not_bits (fmt : PP.formatter) (n : int) (x : rt_expr) : unit = apply_bits_1_1 fmt "not_bits" n x
  let lsl_bits (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "lsl_bits" n x y
  let lsr_bits (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "lsr_bits" n x y
  let asr_bits (fmt : PP.formatter) (n : int) (x : rt_expr) (y : rt_expr) : unit = apply_bits_1_2 fmt "asr_bits" n x y
  let cvt_bits_sint (fmt : PP.formatter) (n : int) (x : rt_expr) : unit = apply_bits_1_1 fmt "cvt_bits_sint" n x
  let cvt_bits_uint (fmt : PP.formatter) (n : int) (x : rt_expr) : unit = apply_bits_1_1 fmt "cvt_bits_uint" n x
  let cvt_int_bits (fmt : PP.formatter) (n : int) (x : rt_expr) : unit = apply_bits_1_1 fmt "cvt_int_bits" n x
  let zeros_bits (fmt : PP.formatter) (n : int) : unit = apply_bits_1_0 fmt "zeros_bits" n
  let ones_bits (fmt : PP.formatter) (n : int) : unit = apply_bits_1_0 fmt "ones_bits" n

  let mk_mask (fmt : PP.formatter) (n : int) (x : rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %a)"
        asl_keyword "mk_mask"
        (c_int_width_64up n)
        pp_expr x

  let zero_extend_bits (fmt : PP.formatter) (m : int) (n : int)  (x : rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %d, %d, %a, %d)"
        asl_keyword "zero_extend_bits"
        (c_int_width_64up m)
        (c_int_width_64up n)
        m
        pp_expr x
        n

  let print_bits_hex (fmt : PP.formatter) (n : int) (x : rt_expr) : unit = apply_bits_1_1 fmt "print_bits_hex" n x

  let in_bits (fmt : PP.formatter) (n : int) (x : rt_expr) (m : Primops.mask) : unit =
      PP.fprintf fmt "((%a & %s) == %s)"
        pp_expr x
        (Z.format "%#x" m.v)
        (Z.format "%#x" m.m)

  let append_bits (fmt : PP.formatter) (m : int) (n : int)  (x : rt_expr) (y : rt_expr) : unit =
      let result_width = m + n in
      let x' fmt = zero_extend_bits fmt m result_width x in
      let y' fmt = zero_extend_bits fmt n result_width y in
      PP.fprintf fmt "%a(%d, %d, %d, %a, %a)"
        asl_keyword "append_bits"
        (c_int_width_64up result_width)
        m
        n
        pp_expr x'
        pp_expr y'

  let replicate_bits (fmt : PP.formatter) (m : int) (n : int) (x : rt_expr) (y : rt_expr) : unit =
      let result_width = m * n in
      let x' fmt = zero_extend_bits fmt m result_width x in
      PP.fprintf fmt "%a(%d, %d, %a, %d)"
        asl_keyword "replicate_bits"
        (c_int_width_64up result_width)
        (c_int_width_64up m)
        pp_expr x'
        n

  let ram_init (fmt : PP.formatter) (a : int) (n : int) (ram : rt_expr) (v : rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %d, %a, %a)"
        asl_keyword "ram_init"
        a
        n
        pp_expr ram
        pp_expr v

  let ram_read (fmt : PP.formatter) (a : int) (n : int) (ram : rt_expr) (addr : rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %d, %a, %a)"
        asl_keyword "ram_read"
        a
        n
        pp_expr ram
        pp_expr addr

  let ram_write (fmt : PP.formatter) (a : int) (n : int) (ram : rt_expr) (addr : rt_expr) (v : rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %d, %a, %a, %a)"
        asl_keyword "ram_write"
        a
        n
        pp_expr ram
        pp_expr addr
        pp_expr v

end

(****************************************************************
 * End
 ****************************************************************)
