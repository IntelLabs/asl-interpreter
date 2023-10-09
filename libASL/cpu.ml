(****************************************************************
 * CPU interface
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

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
  let loc = AST.Unknown in

  let setImpdef (key : string) (v : Value.value) : unit =
    Eval.GlobalEnv.set_impl_def genv key v
  and reset () : unit =
    Eval.eval_proccall loc env (Ident.FIdent ("__TakeColdReset", 0)) [] []
  and step () : unit =
    Eval.eval_proccall loc env (Ident.FIdent ("__InstructionExecute", 0)) [] []
  and getPC () : Primops.bigint =
    let r = Eval.eval_funcall loc env (Ident.FIdent ("__getPC", 0)) [] [] in
    Value.to_integer loc r
  and setPC (x : Primops.bigint) : unit =
    let a = Value.VInt x in
    Eval.eval_proccall loc env (Ident.FIdent ("__setPC", 0)) [] [ a ]
  and elfwrite8 (addr : Int64.t) (b : char) : unit =
    let a = Value.VBits (Primops.mkBits 64 (Z.of_int64 addr)) in
    let b = Value.VBits (Primops.mkBits 8 (Z.of_int (Char.code b))) in
    Eval.eval_proccall loc env (Ident.FIdent ("__ELFWriteMemory", 0)) [] [ a; b ]
  and elfwrite32 (addr : Int64.t) (value : Int32.t) : unit =
    let a = Value.VBits (Primops.mkBits 64 (Z.of_int64 addr)) in
    let b = Value.VBits (Primops.mkBits 32 (Z.of_int32 value)) in
    Eval.eval_proccall loc env (Ident.FIdent ("__ELFWriteMemory32", 0)) [] [ a; b ]
  in
  { env; setImpdef; reset; step; getPC; setPC; elfwrite8; elfwrite32 }

(****************************************************************
 * End
 ****************************************************************)
