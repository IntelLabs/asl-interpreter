(****************************************************************
 * Utilities for use in tests
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
module TC = Tcheck
module AST = Asl_ast

(****************************************************************
 * Alcotest testable functions
 ****************************************************************)

let format_value f v = Format.fprintf f "%s" (Value.pp_value v)

let format_expr (e : AST.expr) : string =
  Utils.to_string2 (fun fmt -> Asl_fmt.expr fmt e)

let value = Alcotest.testable format_value ( = )

(****************************************************************
 * support code for parsing, typechecking and building environments
 ****************************************************************)

let extend_tcenv (globals : TC.GlobalEnv.t) (declarations : string) : (TC.Env.t * AST.declaration list) =
  let globals = TC.GlobalEnv.clone globals in
  let tcenv = TC.Env.mkEnv globals in
  let ds = LoadASL.read_declarations globals declarations in
  (tcenv, ds)

let extend_env (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
  : (TC.Env.t * Eval.Env.t) =
  let (tcenv, ds) = extend_tcenv globals decls in
  let env = Eval.build_evaluation_environment (List.append prelude ds) in
  (tcenv, env)

(****************************************************************
 * End
 ****************************************************************)
