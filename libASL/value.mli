(****************************************************************
 * ASL interpreter values
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

type value =
  | VBool of bool (* optimised special case of VEnum *)
  | VEnum of (Ident.t * int)
  | VInt of Primops.bigint
  | VReal of Primops.real
  | VBits of Primops.bitvector
  | VMask of Primops.mask
  | VString of string
  | VExc of (AST.l * Ident.t * value Asl_utils.Bindings.t)
  | VTuple of value list
  | VRecord of value Asl_utils.Bindings.t
  | VArray of (value Primops.ImmutableArray.t * value)
  | VRAM of Primops.ram
  | VUninitialized

exception Return of value option
exception EvalError of (AST.l * string)
exception Throw of (AST.l * Ident.t * value Asl_utils.Bindings.t)

val pp_value : Format.formatter -> value -> unit
val string_of_value : value -> string
val eq_value : value -> value -> bool

val eval_prim : Ident.t -> value list -> value list -> value option
val impure_prims : string list

(* value constructors and destructors *)

val from_bool : bool -> value
val to_bool : AST.l -> value -> bool
val int_one : value
val to_integer : AST.l -> value -> Primops.bigint
val to_int : AST.l -> value -> int
val to_bits : AST.l -> value -> Primops.bitvector
val to_mask : AST.l -> value -> Primops.mask
val to_string : AST.l -> value -> string
val to_exc : AST.l -> value -> (AST.l * Ident.t * value Asl_utils.Bindings.t)
val to_tuple : value list -> value
val of_tuple : AST.l -> value -> value list
val mkrecord : (Ident.t * value) list -> value
val get_field : AST.l -> value -> Ident.t -> value
val set_field : AST.l -> value -> Ident.t -> value -> value
val empty_array : value -> value
val init_array : (int * value) list -> value -> value
val get_array : AST.l -> value -> value -> value
val set_array : AST.l -> value -> value -> value -> value


(* todo: does not belong in this module *)
val drop_chars : string -> char -> string

(* convert AST nodes to values *)

val from_intLit : AST.intLit -> value
val from_hexLit : AST.hexLit -> value
val from_realLit : AST.realLit -> value
val from_bitsLit : AST.bitsLit -> value
val from_maskLit : AST.maskLit -> value
val from_stringLit : string -> value

(* bitvector manipulation *)

val extract_bits : AST.l -> value -> value -> value -> value
val extract_bits' : AST.l -> value -> int -> int -> value
val extract_bits'' : AST.l -> value -> value -> value -> value
val insert_bits : AST.l -> value -> value -> value -> value -> value
val insert_bits' : AST.l -> value -> int -> int -> value -> value

(* functions used in interpreter *)

val eval_eq : AST.l -> value -> value -> bool
val eval_leq : AST.l -> value -> value -> bool
val eval_eq_int : AST.l -> value -> value -> bool
val eval_eq_bits : AST.l -> value -> value -> bool
val eval_inmask : AST.l -> value -> value -> bool
val eval_add_int : AST.l -> value -> value -> value
val eval_mul_int : AST.l -> value -> value -> value
val eval_sub_int : AST.l -> value -> value -> value
val eval_concat : AST.l -> value list -> value

(* unknowns of various types *)

val eval_unknown_bits : Primops.bigint -> value
val eval_unknown_ram : Primops.bigint -> value
val eval_unknown_integer : unit -> value
val eval_unknown_real : unit -> value
val eval_unknown_string : unit -> value

(* tracing support *)

module type Tracer = sig
  val trace_next : unit -> unit

  val trace_physical_memory : is_read:bool -> is_data:bool -> phys_addr:Z.t -> data:Primops.bitvector -> unit

  val trace_virtual_memory : is_read:bool -> is_data:bool -> context:Z.t -> virt_addr:Z.t -> phys_addr:Z.t -> data:Primops.bitvector -> unit

  val trace_memory_pte : context:Z.t -> level:Z.t -> phys_addr:Z.t -> data:Primops.bitvector -> unit

  val trace_error : kind:string -> string list -> unit

  val trace_event : kind:string -> string list -> unit

  val trace_function : is_prim:bool -> is_return:bool -> Ident.t -> value list -> value list -> unit

  val trace_var : is_local:bool -> is_read:bool -> Ident.t -> value -> unit
end

val tracer : (module Tracer) ref

module TextTracer : Tracer

(****************************************************************
 * End
 ****************************************************************)
