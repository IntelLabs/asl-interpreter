(****************************************************************
 * ASL interpreter values
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL interpreter values *)

open Primops
module AST = Asl_ast
open Builtin_idents
open Asl_utils

(****************************************************************)
(** {2 Values}                                                  *)
(****************************************************************)

(** This union type is for use in an interpreter *)

type value =
  | VBool of bool (* optimised special case of VEnum *)
  | VEnum of (Ident.t * int)
  | VInt of bigint
  | VReal of real
  | VBits of bitvector
  | VMask of mask
  | VString of string
  | VExc of (AST.l * Ident.t * value Asl_utils.Bindings.t)
  | VTuple of value list
  | VRecord of value Bindings.t
  | VArray of (value ImmutableArray.t * value)
  | VRAM of ram
  | VUninitialized
(* initial value of scalars with no explicit initialization *)

(****************************************************************)
(** {2 Exceptions thrown by interpreter}                        *)
(****************************************************************)

exception Return of value option
exception EvalError of (AST.l * string)
exception Throw of (AST.l * Ident.t * value Asl_utils.Bindings.t)

(****************************************************************)
(** {2 Printer for values}                                      *)
(****************************************************************)

let rec pp_value (fmt : Format.formatter) (x : value) : unit =
  match x with
  | VBool b -> Format.pp_print_string fmt (prim_cvt_bool_str b)
  | VEnum (e, _) -> Format.pp_print_string fmt (Ident.pprint e)
  | VInt i -> Format.pp_print_string fmt (prim_cvt_int_decstr i)
  | VReal r -> Format.pp_print_string fmt (prim_cvt_real_str r)
  | VBits b -> Format.pp_print_string fmt (prim_cvt_bits_hexstr (Z.of_int b.n) b)
  | VMask m -> Format.pp_print_string fmt "todo: mask"
  | VString s -> Format.fprintf fmt "\"%s\"" s
  | VExc (loc, exc, fs) ->
      Format.fprintf fmt "%a{%a}@%a"
        Asl_fmt.tycon exc
        (Fun.flip Format_utils.commasep (pp_field_value fmt)) (Bindings.bindings fs)
        Asl_fmt.loc loc
  | VTuple vs ->
    Format.fprintf fmt "(%a)"
      (Fun.flip Format_utils.commasep (pp_value fmt)) vs
  | VRecord fs ->
    Format.fprintf fmt "{%a}"
      (Fun.flip Format_utils.commasep (pp_field_value fmt)) (Bindings.bindings fs)
  | VArray (a, _) ->
    Format.fprintf fmt "[%a]"
      (Fun.flip Format_utils.commasep (pp_array_value fmt)) (ImmutableArray.bindings a)
  | VRAM _ -> Format.pp_print_string fmt "RAM"
  | VUninitialized -> Format.pp_print_string fmt "UNINITIALIZED"

and pp_field_value (fmt : Format.formatter) ((f, v) : Ident.t * value) : unit =
  Format.fprintf fmt "%a = %a"
    Asl_fmt.fieldname f
    pp_value v

and pp_array_value (fmt : Format.formatter) ((i, v) : int * value) : unit =
  Format.fprintf fmt "%d:%a"
    i
    pp_value v

let string_of_value (x : value) : string = Utils.to_string2 (Fun.flip pp_value x)

(****************************************************************)
(** {2 Equality check on values}                                     *)
(****************************************************************)

let rec eq_value (x : value) (y : value) : bool =
  match (x, y) with
  | VBool x', VBool y' -> prim_eq_bool x' y'
  | VEnum x', VEnum y' -> snd x' = snd y'
  | VInt x', VInt y' -> prim_eq_int x' y'
  | VReal x', VReal y' -> prim_eq_real x' y'
  | VBits x', VBits y' -> prim_eq_bits x' y'
  | VString x', VString y' -> String.equal x' y'
  | VTuple xs, VTuple ys -> List.for_all2 eq_value xs ys
  (* todo: add missing cases *)
  | _ -> failwith "eq_value"

(****************************************************************)
(** {2 Functions on values}                                     *)
(****************************************************************)

let from_bool (x : bool) : value = VBool x

let to_bool (loc : AST.l) (x : value) : bool =
  match x with
  | VBool b -> b
  | _ -> raise (EvalError (loc, "boolean expected.  Got " ^ string_of_value x))

let to_integer (loc : AST.l) (x : value) : bigint =
  match x with
  | VInt i -> i
  | _ -> raise (EvalError (loc, "integer expected. Got " ^ string_of_value x))

(* todo: this should raise an exception if out of range *)
let to_int (loc : AST.l) (x : value) : int =
  match x with
  | VInt i -> Z.to_int i
  | _ -> raise (EvalError (loc, "integer expected. Got " ^ string_of_value x))

let to_bits (loc : AST.l) (x : value) : bitvector =
  match x with
  | VBits b -> b
  | _ -> raise (EvalError (loc, "bits expected. Got " ^ string_of_value x))

let to_mask (loc : AST.l) (x : value) : mask =
  match x with
  | VMask m -> m
  | _ -> raise (EvalError (loc, "mask expected. Got " ^ string_of_value x))

let to_string (loc : AST.l) (x : value) : string =
  match x with
  | VString s -> s
  | _ -> raise (EvalError (loc, "string expected. Got " ^ string_of_value x))

let to_exc (loc : AST.l) (x : value) : AST.l * Ident.t * value Asl_utils.Bindings.t =
  match x with
  | VExc (loc, exc, fs) -> (loc, exc, fs)
  | _ -> raise (EvalError (loc, "exception expected. Got " ^ string_of_value x))

let to_tuple (xs : value list) : value = VTuple xs

let of_tuple (loc : AST.l) (x : value) : value list =
  match x with
  | VTuple xs -> xs
  | _ -> raise (EvalError (loc, "tuple expected. Got " ^ string_of_value x))

let mkrecord (fs : (Ident.t * value) list) : value = VRecord (mk_bindings fs)

let get_field (loc : AST.l) (x : value) (f : Ident.t) : value =
  match x with
  | VRecord fs ->
      ( match Bindings.find_opt f fs with
      | Some r -> r
      | None -> raise (EvalError (loc, "Field " ^ Ident.pprint f ^ " not found in " ^ string_of_value x))
      )
  | _ -> raise (EvalError (loc, "record expected. Got " ^ string_of_value x))

let set_field (loc : AST.l) (x : value) (f : Ident.t) (v : value) : value =
  match x with
  | VRecord fs -> VRecord (Bindings.add f v fs)
  | _ -> raise (EvalError (loc, "record expected. Got " ^ string_of_value x))

let empty_array (d : value) : value = VArray (prim_empty_array, d)

let get_array (loc : AST.l) (a : value) (i : value) : value =
  match (a, i) with
  | VArray (x, d), VInt i' -> prim_read_array x (Z.to_int i') d
  | VArray (x, d), VEnum i' -> prim_read_array x (snd i') d
  | VArray (x, d), _ ->
      raise (EvalError (loc, "array index expected. Got " ^ string_of_value i))
  | _ -> raise (EvalError (loc, "array expected. Got " ^ string_of_value a))

let set_array (loc : AST.l) (a : value) (i : value) (v : value) : value =
  match (a, i) with
  | VArray (x, d), VInt i' -> VArray (prim_write_array x (Z.to_int i') v, d)
  | VArray (x, d), VEnum i' -> VArray (prim_write_array x (snd i') v, d)
  | VArray (x, d), _ ->
      raise (EvalError (loc, "array index expected. Got " ^ string_of_value i))
  | _ -> raise (EvalError (loc, "array expected. Got " ^ string_of_value a))

(** Delete all characters matching 'c' from string 'x' *)
let drop_chars (x : string) (c : char) : string =
  (* First calculate final length *)
  let len = ref 0 in
  String.iter (fun t -> if t <> c then len := !len + 1) x;

  (* search for next character not matching c *)
  let i = ref 0 in
  let rec next_char (_ : int) : char =
    let r = String.get x !i in
    i := !i + 1;
    if r = c then next_char 0 else r
  in

  (* create result *)
  String.init !len next_char

let from_intLit (x : AST.intLit) : value = VInt (Z.of_string x)

let from_hexLit (x : AST.hexLit) : value =
  VInt (Z.of_string_base 16 (drop_chars x '_'))

let int_one : value = VInt (Z.of_int 1)

let from_realLit (x : AST.realLit) : value =
  let pt = String.index x '.' in
  let fracsz = String.length x - pt - 1 in
  let intpart = String.sub x 0 pt in
  let frac = String.sub x (pt + 1) fracsz in
  let numerator = Z.of_string (intpart ^ frac) in
  let denominator = Z.pow (Z.of_int 10) fracsz in
  VReal (Q.make numerator denominator)

let from_bitsLit (x : AST.bitsLit) : value =
  let x' = drop_chars x ' ' in
  VBits (mkBits (String.length x') (Z.of_string_base 2 x'))

let from_maskLit (x : AST.maskLit) : value =
  let x' = drop_chars x ' ' in
  let n = String.length x' in
  let v = String.map (function 'x' -> '0' | c -> c) x' in
  let m = String.map (function 'x' -> '0' | c -> '1') x' in
  VMask (mkMask n (Z.of_string_base 2 v) (Z.of_string_base 2 m))

let from_stringLit (x : string) : value =
  let r = ref "" in
  let rec unescape (i : int) : unit =
    if i < String.length x then
      let c = String.get x i in
      if c = '\\' then (
        assert (i + 1 < String.length x);
        let c = String.get x (i + 1) in
        if c = '\\' then r := !r ^ String.make 1 '\\'
        else if c = 'n' then r := !r ^ String.make 1 '\n'
        else assert false;
        unescape (i + 2))
      else (
        r := !r ^ String.make 1 c;
        unescape (i + 1))
  in
  unescape 0;
  VString !r

(****************************************************************)
(** {2 Control over trace generation}                           *)
(****************************************************************)

(** Debugging output on every instruction fetch *)
let enable_trace_memory_insn = ref false

(** Debugging output on every page table access *)
let enable_trace_memory_pte = ref false

(** Debugging output on every memory read *)
let enable_trace_memory_read = ref false

(** Debugging output on every memory write *)
let enable_trace_memory_write = ref false

(** Debugging output on every local variable read *)
let enable_trace_local_read = ref false

(** Debugging output on every global variable read *)
let enable_trace_global_read = ref false

(** Debugging output on every local variable write *)
let enable_trace_local_write = ref false

(** Debugging output on every global variable write *)
let enable_trace_global_write = ref false

(** Debugging output on every function call *)
let enable_trace_functions = ref false

(** Debugging output on every primitive function call *)
let enable_trace_primops = ref false

(** Debugging output: errors *)
let enable_trace_errors = ref false

(** Debugging output: user-defined events *)
let enable_trace_events = ref false

let _ = begin
  Flags.registerFlag "trace:global_read" enable_trace_global_read "Instruction trace: register reads";
  Flags.registerFlag "trace:global_write" enable_trace_global_write "Instruction trace: register writes";
  Flags.registerFlag "trace:memory_read" enable_trace_memory_read "Instruction trace: data memory reads";
  Flags.registerFlag "trace:memory_write" enable_trace_memory_write "Instruction trace: data memory writes";
  Flags.registerFlag "trace:instruction_fetch" enable_trace_memory_insn "Instruction trace: instruction memory reads";
  Flags.registerFlag "trace:page_table_entry" enable_trace_memory_pte "Instruction trace: page table accesses";
  Flags.registerFlag "trace:error" enable_trace_errors "Instruction trace: errors";
  Flags.registerFlag "trace:event" enable_trace_events "Instruction trace: user defined events";
  Flags.registerFlag "trace:local_read" enable_trace_local_read "ASL trace: local variable reads";
  Flags.registerFlag "trace:local_write" enable_trace_local_write "ASL trace: local variable writes";
  Flags.registerFlag "trace:primop" enable_trace_primops "ASL trace: calls to builtin functions";
  Flags.registerFlag "trace:function" enable_trace_functions "ASL trace: calls to functions";
end

(****************************************************************)
(** {2 Trace generation}                                        *)
(****************************************************************)

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

(** Tracer module that produces textual trace on stdout
 *
 * All lines of output are of the form
 *
 *     TRACE <cycle> <kind ..>: <args>
 *)
module TextTracer = struct
  let cycle = ref 0

  let trace (kind : string) (params : string list) =
    Printf.printf "TRACE %d %s:" !cycle kind;
    List.iter (fun p -> Printf.printf " %s" p) params;
    Printf.printf "\n"

  let trace_next () : unit =
    cycle := !cycle + 1

  let trace_physical_memory ~(is_read : bool) ~(is_data : bool) ~(phys_addr : Z.t) ~(data : bitvector) : unit =
    let enabled = if is_data then
                    (if is_read then !enable_trace_memory_read else !enable_trace_memory_write)
                  else
                    !enable_trace_memory_insn
    in
    if enabled then
      let kind = if is_data then "D" else "I" in
      trace
        (if is_read then "phys_mem_read_"^kind else "phys_mem_write_"^kind)
        [ string_of_int data.n
        ; Z.format "%#08x" phys_addr
        ; Z.format "%#x" data.v
        ]

  let trace_virtual_memory ~(is_read : bool) ~(is_data : bool) ~(context : Z.t) ~(virt_addr : Z.t) ~(phys_addr : Z.t) ~(data : bitvector) : unit =
    let enabled = if is_data then
                    (if is_read then !enable_trace_memory_read else !enable_trace_memory_write)
                  else
                    !enable_trace_memory_insn
    in
    if enabled then
      let kind = if is_data then "D" else "I" in
      trace
        (if is_read then "virt_mem_read_"^kind else "virt_mem_write_"^kind)
        [ string_of_int data.n
        ; Z.format "%#04x" context
        ; Z.format "%#08x" virt_addr
        ; Z.format "%#08x" phys_addr
        ; Z.format "%#x" data.v
        ]

  let trace_memory_pte ~(context : Z.t) ~(level : Z.t) ~(phys_addr : Z.t) ~(data : bitvector) : unit =
    if !enable_trace_memory_pte then
      trace
        "memory_read_pte"
        [ string_of_int data.n
        ; Z.format "%#04x" context
        ; Z.format "%d" level
        ; Z.format "%#08x" phys_addr
        ; Z.format "%#08x" data.v
        ]

  let trace_var ~(is_local : bool) ~(is_read : bool) (name : Ident.t) (data : value) : unit =
    let enabled = if is_local
                  then (if is_read then !enable_trace_local_read else !enable_trace_local_write)
                  else (if is_read then !enable_trace_global_read else !enable_trace_global_write)
    in
    if enabled then
      trace
        ("var_" ^ (if is_local then "local_" else "global_") ^ (if is_read then "read" else "write"))
        [ Ident.pprint name
        ; string_of_value data
        ]

  let trace_error ~(kind : string) (vs : string list) : unit =
    if !enable_trace_errors then
      trace
        ("error " ^ kind)
        vs

  let trace_event ~(kind : string) (vs : string list) : unit =
    if !enable_trace_events then
      trace
        ("event " ^ kind)
        vs

  let trace_function ~(is_prim : bool) ~(is_return : bool) (name : Ident.t) (tvs : value list) (vs : value list) : unit =
    let enabled = if is_prim then !enable_trace_primops else !enable_trace_functions
    in
    if enabled then
      trace
        ("function_" ^ (if is_return then "return" else "call"))
        ( Ident.pprint name :: "{" :: List.append (List.map string_of_value tvs) ("}" :: List.map string_of_value vs))

end

let tracer = ref (module TextTracer : Tracer)


(****************************************************************)
(** {2 Primop dispatch on values}                               *)
(****************************************************************)

(** Returns None iff function does not exist or arguments have wrong type *)

let eval_prim (f : Ident.t) (tvs : value list) (vs : value list) : value option =
  match (tvs, vs) with
  (* The reason we need direct checks against eq_enum and ne_enum is because
     the identifier eq_enum with tag 0 doesn't have a root. And we need this
     function identifier in the xform_case transform right now *)
  | [], [ VEnum x; VEnum y ] when Ident.equal f eq_enum ->
      Some (VBool (snd x = snd y))
  | [], [ VBool x; VBool y ] when Ident.equal f eq_enum ->
      Some (VBool (x = y))
  | [], [ VEnum x; VEnum y ] when Ident.equal f ne_enum ->
      Some (VBool (snd x <> snd y))
  | [], [ VBool x; VBool y ] when Ident.equal f ne_enum ->
      Some (VBool (x <> y))
  | [], [ VEnum x; VEnum y ] when Ident.root_equal f ~root:eq_enum ->
      Some (VBool (snd x = snd y))
  | [], [ VBool x; VBool y ] when Ident.root_equal f ~root:eq_enum ->
      Some (VBool (x = y))
  | [], [ VEnum x; VEnum y ] when Ident.root_equal f ~root:ne_enum ->
      Some (VBool (snd x <> snd y))
  | [], [ VBool x; VBool y ] when Ident.root_equal f ~root:ne_enum ->
      Some (VBool (x <> y))
  | [], [ VBool x; VBool y ] when Ident.equal f eq_bool ->
      Some (VBool (prim_eq_bool x y))
  | [], [ VBool x; VBool y ] when Ident.equal f ne_bool ->
      Some (VBool (prim_ne_bool x y))
  | [], [ VBool x; VBool y ] when Ident.equal f equiv_bool ->
      Some (VBool (prim_equiv_bool x y))
  | [], [ VBool x ] when Ident.equal f not_bool -> Some (VBool (prim_not_bool x))
  | [], [ VInt x; VInt y ] when Ident.equal f eq_int ->
      Some (VBool (prim_eq_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f ne_int ->
      Some (VBool (prim_ne_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f le_int ->
      Some (VBool (prim_le_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f lt_int ->
      Some (VBool (prim_lt_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f ge_int ->
      Some (VBool (prim_ge_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f gt_int ->
      Some (VBool (prim_gt_int x y))
  | [], [ VInt x ] when Ident.equal f is_pow2_int ->
      Some (VBool (prim_is_pow2_int x))
  | [], [ VInt x ] when Ident.equal f neg_int -> Some (VInt (prim_neg_int x))
  | [], [ VInt x; VInt y ] when Ident.equal f add_int ->
      Some (VInt (prim_add_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f sub_int ->
      Some (VInt (prim_sub_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f shl_int ->
      Some (VInt (prim_shl_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f shr_int ->
      Some (VInt (prim_shr_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f mul_int ->
      Some (VInt (prim_mul_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f zdiv_int ->
      Some (VInt (prim_zdiv_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f zrem_int ->
      Some (VInt (prim_zrem_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f fdiv_int ->
      Some (VInt (prim_fdiv_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f frem_int ->
      Some (VInt (prim_frem_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f mod_pow2_int ->
      Some (VInt (prim_mod_pow2_int x y))
  | [], [ VInt x; VInt y ] when Ident.equal f align_int ->
      Some (VInt (prim_align_int x y))
  | [], [ VInt x ] when Ident.equal f pow2_int -> Some (VInt (prim_pow2_int x))
  | [], [ VInt x; VInt y ] when Ident.equal f pow_int_int ->
      Some (VInt (prim_pow_int_int x y))
  | [], [ VInt x ] when Ident.equal f cvt_int_real ->
      Some (VReal (prim_cvt_int_real x))
  | [], [ VReal x; VReal y ] when Ident.equal f eq_real ->
      Some (VBool (prim_eq_real x y))
  | [], [ VReal x; VReal y ] when Ident.equal f ne_real ->
      Some (VBool (prim_ne_real x y))
  | [], [ VReal x; VReal y ] when Ident.equal f le_real ->
      Some (VBool (prim_le_real x y))
  | [], [ VReal x; VReal y ] when Ident.equal f lt_real ->
      Some (VBool (prim_lt_real x y))
  | [], [ VReal x; VReal y ] when Ident.equal f ge_real ->
      Some (VBool (prim_ge_real x y))
  | [], [ VReal x; VReal y ] when Ident.equal f gt_real ->
      Some (VBool (prim_gt_real x y))
  | [], [ VReal x; VReal y ] when Ident.equal f add_real ->
      Some (VReal (prim_add_real x y))
  | [], [ VReal x ] when Ident.equal f neg_real ->
      Some (VReal (prim_neg_real x))
  | [], [ VReal x; VReal y ] when Ident.equal f sub_real ->
      Some (VReal (prim_sub_real x y))
  | [], [ VReal x; VReal y ] when Ident.equal f mul_real ->
      Some (VReal (prim_mul_real x y))
  | [], [ VReal x; VReal y ] when Ident.equal f divide_real ->
      Some (VReal (prim_div_real x y))
  | [], [ VInt x ] when Ident.equal f pow2_real -> Some (VReal (prim_pow2_real x))
  | [], [ VReal x ] when Ident.equal f round_tozero_real ->
      Some (VInt (prim_round_tozero_real x))
  | [], [ VReal x ] when Ident.equal f round_down_real ->
      Some (VInt (prim_round_down_real x))
  | [], [ VReal x ] when Ident.equal f round_up_real ->
      Some (VInt (prim_round_up_real x))
  | [], [ VReal x; VReal y ] when Ident.equal f sqrt_real ->
      Some (VReal (prim_sqrt_real x))
  | [ _ ], [ VInt x; VInt n ] when Ident.equal f cvt_int_bits ->
      Some (VBits (prim_cvt_int_bits n x))
  | [ VInt n ], [ VBits x ] when Ident.equal f cvt_bits_sint ->
      Some (VInt (prim_cvt_bits_sint x))
  | [ VInt n ], [ VBits x ] when Ident.equal f cvt_bits_uint ->
      Some (VInt (prim_cvt_bits_uint x))
  | [ VInt n ], [ VBits x; VMask y ] when Ident.equal f in_mask ->
      Some (VBool (prim_in_mask x y))
  | [ VInt n ], [ VBits x; VMask y ] when Ident.equal f notin_mask ->
      Some (VBool (prim_notin_mask x y))
  | [ VInt n ], [ VBits x; VBits y ] when Ident.equal f eq_bits ->
      Some (VBool (prim_eq_bits x y))
  | [ VInt n ], [ VBits x; VBits y ] when Ident.equal f ne_bits ->
      Some (VBool (prim_ne_bits x y))
  | [ VInt n ], [ VBits x; VBits y ] when Ident.equal f add_bits ->
      Some (VBits (prim_add_bits x y))
  | [ VInt n ], [ VBits x; VBits y ] when Ident.equal f sub_bits ->
      Some (VBits (prim_sub_bits x y))
  | [ VInt n ], [ VBits x; VBits y ] when Ident.equal f mul_bits ->
      Some (VBits (prim_mul_bits x y))
  | [ VInt n ], [ VBits x; VBits y ] when Ident.equal f and_bits ->
      Some (VBits (prim_and_bits x y))
  | [ VInt n ], [ VBits x; VBits y ] when Ident.equal f or_bits ->
      Some (VBits (prim_or_bits x y))
  | [ VInt n ], [ VBits x; VBits y ] when Ident.equal f eor_bits ->
      Some (VBits (prim_eor_bits x y))
  | [ VInt n ], [ VBits x ] when Ident.equal f not_bits ->
      Some (VBits (prim_not_bits x))
  | [ VInt n ], [_] when Ident.equal f zeros_bits ->
      Some (VBits (prim_zeros_bits n))
  | [ VInt n ], [_] when Ident.equal f ones_bits ->
      Some (VBits (prim_ones_bits n))
  | [_], [ VBits x; VInt d ] when Ident.equal f lsl_bits ->
      Some (VBits (prim_lsl x d))
  | [_], [ VBits x; VInt d ] when Ident.equal f lsr_bits ->
      Some (VBits (prim_lsr x d))
  | [_], [ VBits x; VInt d ] when Ident.equal f asr_bits ->
      Some (VBits (prim_asr x d))
  | [ _; _ ], [ VBits x; VInt y ] when Ident.equal f replicate_bits ->
      Some (VBits (prim_replicate_bits x y))
  | [ _; _ ], [ VBits x; VInt y ]  when Ident.equal f zero_extend_bits ->
      Some (VBits (prim_zero_extend_bits x y))
  | [ VInt m; VInt n ], [ VBits x; VBits y ] when Ident.equal f append_bits ->
      Some (VBits (prim_append_bits x y))
  | [ _ ], [ VInt w; VInt n ] when Ident.equal f Builtin_idents.mk_mask ->
      Some (VBits (prim_mk_mask w n))
  | [], [ VString x; VString y ] when Ident.equal f eq_str ->
      Some (VBool (prim_eq_str x y))
  | [], [ VString x; VString y ] when Ident.equal f ne_str ->
      Some (VBool (prim_ne_str x y))
  | [], [ VString x; VString y ] when Ident.equal f append_str_str ->
      Some (VString (prim_append_str x y))
  | [], [ VInt x ] when Ident.equal f cvt_int_hexstr ->
      Some (VString (prim_cvt_int_hexstr x))
  | [], [ VInt x ] when Ident.equal f cvt_int_decstr ->
      Some (VString (prim_cvt_int_decstr x))
  | [], [ VBool x ] when Ident.equal f cvt_bool_str ->
      Some (VString (prim_cvt_bool_str x))
  | [ _ ], [ VInt n; VBits x ] when Ident.equal f cvt_bits_str ->
      Some (VString (prim_cvt_bits_str n x))
  | [], [ VReal x ] when Ident.equal f cvt_real_str ->
      Some (VString (prim_cvt_real_str x))
  (* The remaining primops all have side effects *)
  | [],  [ VInt x ]          when Ident.equal f print_int_hex ->
      prim_print_int_hex x;    Some (VTuple [])
  | [],  [ VInt x ]          when Ident.equal f print_int_dec ->
      prim_print_int_dec x;    Some (VTuple [])
  | [VInt n], [ VBits x ] when Ident.equal f print_bits_hex ->
      prim_print_bits_hex n x; Some (VTuple [])
  | _, [ VInt a; VInt n; VRAM ram; VBits i ] when Ident.equal f ram_init ->
      Some
        (prim_init_ram a n ram i;
         VTuple [])
  | _, [ VInt a; VInt n; VRAM ram; VBits i ] when Ident.equal f ram_read ->
      Some (VBits (prim_read_ram a n ram i.v))
  | _, [ VInt a; VInt n; VRAM ram; VBits i; VBits x ] when
    Ident.equal f ram_write ->
      Some
        (prim_write_ram a n ram i.v x;
         VTuple [])

  | _, [ ] when Ident.equal f trace_next ->
      let module Tracer = (val (!tracer) : Tracer) in
      Some (Tracer.trace_next (); VTuple [])
  | _, [ VBool is_read; VBool is_data; VInt a; VInt n; VBits pa; VBits v ] when
    Ident.equal f trace_physical_memory ->
      let module Tracer = (val (!tracer) : Tracer) in
      Some (Tracer.trace_physical_memory
          ~is_read
          ~is_data
          ~phys_addr:pa.v
          ~data:v;
        VTuple [])
  | _, [ VBool is_read;
         VBool is_data;
         VInt vw; VInt pw;
         VInt n;
         VBits ctxt;
         VBits va;
         VBits pa;
         VBits v
       ] when
    Ident.equal f trace_virtual_memory ->
      let module Tracer = (val (!tracer) : Tracer) in
      Some (Tracer.trace_virtual_memory
          ~is_read
          ~is_data
          ~context:ctxt.v
          ~phys_addr:pa.v
          ~virt_addr:va.v
          ~data:v;
        VTuple [])
  | _, [ VInt pw; VInt n; VBits ctxt; VInt level; VBits pa; VBits v ] when
    Ident.equal f trace_page_table_walk ->
      let module Tracer = (val (!tracer) : Tracer) in
      Some (Tracer.trace_memory_pte
          ~context:ctxt.v
          ~level
          ~phys_addr:pa.v
          ~data:v;
        VTuple [])
  | _, [ VString kind; VString s ] when Ident.equal f trace_error ->
      let module Tracer = (val (!tracer) : Tracer) in
      Some (Tracer.trace_error ~kind [s]; VTuple [])
  | _, [ VString kind; VString s ] when Ident.equal f trace_event ->
      let module Tracer = (val (!tracer) : Tracer) in
      Some (Tracer.trace_event ~kind [s]; VTuple [])

  | _, [ VString name; VString mode ] when Ident.equal f asl_file_open ->
      Some (VInt (prim_open_file name mode))
  | _, [ VInt fd; VString data ] when Ident.equal f asl_file_write ->
      Some
        (prim_write_file fd data;
         VTuple [])
  | _, [ VInt fd ] when Ident.equal f asl_file_getc ->
      Some (VInt (prim_getc_file fd))
  | _, [ VString s ] when Ident.equal f print_str ->
      Some
        (prim_print_str s;
         VTuple [])
  | _, [ VInt c ] when Ident.equal f print_char ->
      Some
        (prim_print_char c;
         VTuple [])
  | [ VInt n ], [ VBits b ] when Ident.equal f print_bits ->
      Some
        (prim_print_bits_hex n b;
         VTuple [])
  (* No function matches *)
  | _ -> None

(* List of primops that are 'unsafe' because of things like
 * - result doesn't just depend on arguments
 * - modifies global state
 * - writes to file/stdout/...
 *)
let impure_prims =
  [
    "print_int_hex";
    "print_int_dec";
    "print_bits_hex";
    "print_bits_dec";
    "ram_init";
    "ram_read";
    "ram_write";
    "__TraceNext";
    "__TracePhysicalMemory";
    "__TraceVirtualMemory";
    "__TracePageTableWalk";
    "__TraceError";
    "__TraceEvent";
    "asl_file_open";
    "asl_file_write";
    "asl_file_getc";
    "print_str";
    "print_char";
    "print_bits";
  ]

(****************************************************************)
(** {2 Utility functions on Values}                             *)
(****************************************************************)

let extract_bits (loc : AST.l) (x : value) (i : value) (w : value) : value =
  VBits (prim_extract (to_bits loc x) (to_integer loc i) (to_integer loc w))

let extract_bits' (loc : AST.l) (x : value) (i : int) (w : int) : value =
  VBits (prim_extract (to_bits loc x) (Z.of_int i) (Z.of_int w))

let extract_bits'' (loc : AST.l) (x : value) (i : value) (w : value) : value =
  match x with
  | VInt x' -> VBits (prim_extract_int x' (to_integer loc i) (to_integer loc w))
  | VBits x' -> VBits (prim_extract x' (to_integer loc i) (to_integer loc w))
  | _ -> raise (EvalError (loc, "bits or integer expected. Got " ^ string_of_value x))

let insert_bits (loc : AST.l) (x : value) (i : value) (w : value) (y : value) :
    value =
  VBits
    (prim_insert (to_bits loc x) (to_integer loc i) (to_integer loc w)
       (to_bits loc y))

let insert_bits' (loc : AST.l) (x : value) (i : int) (w : int) (y : value) :
    value =
  VBits (prim_insert (to_bits loc x) (Z.of_int i) (Z.of_int w) (to_bits loc y))

let rec eval_eq (loc : AST.l) (x : value) (y : value) : bool =
  match (x, y) with
  | VBool x', VBool y' -> prim_eq_bool x' y'
  | VEnum x', VEnum y' -> snd x' = snd y'
  | VInt x', VInt y' -> prim_eq_int x' y'
  | VReal x', VReal y' -> prim_eq_real x' y'
  | VBits x', VBits y' -> prim_eq_bits x' y'
  | VString x', VString y' -> String.equal x' y'
  | VTuple xs, VTuple ys -> List.for_all2 (eval_eq loc) xs ys
  | _ ->
      raise
        (EvalError
           ( loc,
             "matchable types expected. Got " ^ string_of_value x ^ ", " ^ string_of_value y
           ))

let eval_leq (loc : AST.l) (x : value) (y : value) : bool =
  match (x, y) with
  | VInt x', VInt y' -> prim_le_int x' y'
  | _ -> raise (EvalError (loc, "integer expected + string_of_value x"))

let eval_eq_int (loc : AST.l) (x : value) (y : value) : bool =
  prim_eq_int (to_integer loc x) (to_integer loc y)

let eval_eq_bits (loc : AST.l) (x : value) (y : value) : bool =
  prim_eq_bits (to_bits loc x) (to_bits loc y)

(* todo: should m be a value or a mask? *)
let eval_inmask (loc : AST.l) (x : value) (m : value) : bool =
  prim_in_mask (to_bits loc x) (to_mask loc m)

let eval_add_int (loc : AST.l) (x : value) (y : value) : value =
  VInt (prim_add_int (to_integer loc x) (to_integer loc y))

let eval_mul_int (loc : AST.l) (x : value) (y : value) : value =
  VInt (prim_mul_int (to_integer loc x) (to_integer loc y))

let eval_sub_int (loc : AST.l) (x : value) (y : value) : value =
  VInt (prim_sub_int (to_integer loc x) (to_integer loc y))

let eval_concat (loc : AST.l) (xs : value list) : value =
  let xs' = List.map (to_bits loc) xs in
  VBits (prim_concat_bits xs')

(****************************************************************)
(** {2 Unknown handling}                                        *)
(****************************************************************)

(** We might want to change this in the future to model the expected
    non-determinism in the spec.
    And we might want to augment this with some form of support for
    uninitialized values (which would ideally trigger an error).
 *)

let eval_unknown_bits (wd : Primops.bigint) : value =
  VBits (Primops.mkBits (Z.to_int wd) Z.zero)

let eval_unknown_ram (a : Primops.bigint) : value =
  VRAM (Primops.init_ram (char_of_int 0))

let eval_unknown_integer () : value = VInt Z.zero
let eval_unknown_real () : value = VReal Q.zero
let eval_unknown_string () : value = VString "<UNKNOWN string>"

(****************************************************************
 * End
 ****************************************************************)
