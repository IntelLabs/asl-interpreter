(****************************************************************
 * CPU interface
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

type cpu = {
    env      : Eval.Env.t;
    setImpdef: string -> Value.value -> unit;
    reset    : unit -> unit;
    step     : unit -> unit;
    getPC    : unit -> Primops.bigint;
    setPC    : Primops.bigint -> unit;
    elfwrite : Int64.t -> char -> unit;
    opcode   : string -> Primops.bigint -> unit;
}

let mkCPU (env : Eval.Env.t): cpu =
    let genv = Eval.Env.globals env in
    let loc = AST.Unknown in

    let setImpdef (key: string) (v: Value.value): unit =
        Eval.GlobalEnv.setImpdef genv key v

    and reset (_ : unit): unit =
        Eval.eval_proccall loc env (AST.FIdent ("__TakeColdReset", 0)) [] []

    and step (_ : unit): unit =
        Eval.eval_proccall loc env (AST.FIdent ("__InstructionExecute", 0)) [] []

    and getPC (_ : unit): Primops.bigint =
        let r = Eval.eval_funcall loc env (AST.FIdent ("__getPC", 0)) [] [] in
        Value.to_integer loc r

    and setPC (x : Primops.bigint): unit =
        let a = Value.VInt x in
        Eval.eval_proccall loc env (AST.FIdent ("__setPC", 0)) [] [a]

    and elfwrite (addr: Int64.t) (b: char): unit =
        let a = Value.VBits (Primops.mkBits 64 (Z.of_int64 addr)) in
        let b = Value.VBits (Primops.mkBits  8 (Z.of_int (Char.code b))) in
        Eval.eval_proccall loc env (AST.FIdent ("__ELFWriteMemory", 0)) [] [a; b]

    and opcode (iset: string) (opcode: Primops.bigint): unit =
        let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) opcode) in
        let decoder = Eval.GlobalEnv.getDecoder genv (Ident iset) in
        Eval.eval_decode_case AST.Unknown env decoder op
    in
    {
        env      = env;
        setImpdef= setImpdef;
        reset    = reset;
        step     = step;
        getPC    = getPC;
        setPC    = setPC;
        elfwrite = elfwrite;
        opcode   = opcode
    }

(****************************************************************
 * End
 ****************************************************************)
