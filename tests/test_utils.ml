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

let value = Alcotest.testable format_value ( = )

let expr = Alcotest.testable Asl_fmt.expr (=)

let format_expr (e : AST.expr) : string =
  Utils.to_string2 (fun fmt -> Asl_fmt.expr fmt e)

let value = Alcotest.testable format_value ( = )

let stmts =
  (* Format to strings and then compare to avoid comparing location information *)
  let eq_stmts (s1 : AST.stmt list) (s2 : AST.stmt list) : bool =
    let f1 = Utils.to_string2 (fun fmt -> Asl_fmt.indented_block fmt s1) in
    let f2 = Utils.to_string2 (fun fmt -> Asl_fmt.indented_block fmt s2) in
    f1 = f2
  in
  Alcotest.testable Asl_fmt.indented_block eq_stmts

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
 * Support code for checking transformations
 ****************************************************************)

type 'a xform = 'a -> 'a

(** Test statement transforms *)
let test_xform_stmts
    (f : (AST.stmt list) xform)
    (globals : TC.GlobalEnv.t) (prelude : AST.declaration list)
    (decls : string) (l : string) (r : string) () : unit =
  let (tcenv, _) = extend_tcenv globals decls in
  let l' = LoadASL.read_stmts tcenv l in
  let r' = LoadASL.read_stmts tcenv r in
  let x = f l' in
  let what = l ^ "\n==>\n" ^ r in
  Alcotest.check stmts what r' x

(****************************************************************
 * End
 ****************************************************************)
