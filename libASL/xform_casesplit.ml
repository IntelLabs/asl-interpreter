(****************************************************************
 * ASL case split transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

(** ASL case split transform *)

let xform_decl (d : AST.declaration) (cases : (AST.ident * AST.expr list) list)
    : AST.declaration list =
  match d with
  | Decl_FunDefn (f, ps, atys, rty, body, loc) ->
      Printf.printf "Case splitting %s\n" (AST.pprint_ident f);
      [ d ]
  | _ -> [ d ]

(****************************************************************
 * End
 ****************************************************************)
