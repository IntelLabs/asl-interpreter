(****************************************************************
 * CPU interface
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Builtin_idents

type cpu = {
  env : Eval.Env.t;
  setImpdef : string -> Value.value -> unit;
  reset : unit -> unit;
  step : unit -> unit;
  getPC : unit -> Primops.bigint;
  setPC : Primops.bigint -> unit;
  elfwrite8 : Int64.t -> char -> unit;
  elfwrite32 : Int64.t -> Int32.t -> unit;
}

let mkCPU (env : Eval.Env.t) : cpu =
  let genv = Eval.Env.globals env in
  let loc = Loc.Unknown in

  let setImpdef (key : string) (v : Value.value) : unit =
    Eval.GlobalEnv.set_impl_def genv key v
  and reset () : unit =
    Eval.eval_proccall loc env (take_cold_reset) [] []
  and step () : unit =
    Eval.eval_proccall loc env (instruction_execute) [] []
  and getPC () : Primops.bigint =
    let r = Eval.eval_funcall loc env (get_pc) [] [] in
    Value.to_integer loc r
  and setPC (x : Primops.bigint) : unit =
    let a = Value.VInt x in
    Eval.eval_proccall loc env (set_pc) [] [ a ]
  and elfwrite8 (addr : Int64.t) (b : char) : unit =
    let a = Value.VBits (Primops.mkBits 64 (Z.of_int64 addr)) in
    let b = Value.VBits (Primops.mkBits 8 (Z.of_int (Char.code b))) in
    Eval.eval_proccall loc env (elf_write_memory) [] [ a; b ]
  and elfwrite32 (addr : Int64.t) (value : Int32.t) : unit =
    let a = Value.VBits (Primops.mkBits 64 (Z.of_int64 addr)) in
    let b = Value.VBits (Primops.mkBits 32 (Z.of_int32 value)) in
    Eval.eval_proccall loc env (elf_write_memory32) [] [ a; b ]
  in
  { env; setImpdef; reset; step; getPC; setPC; elfwrite8; elfwrite32 }

(****************************************************************
 * End
 ****************************************************************)
