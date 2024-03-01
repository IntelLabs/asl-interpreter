(****************************************************************
 * CPU interface
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

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

val mkCPU : Eval.Env.t -> cpu

(****************************************************************
 * End
 ****************************************************************)
