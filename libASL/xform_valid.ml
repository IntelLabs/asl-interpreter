(****************************************************************
 * ASL transform to track valid bits
 *
 * Copyright Intel Inc (c) 2024
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Builtin_idents

let mk_call (args : AST.expr list) (loc : AST.l) : AST.stmt =
  AST.Stmt_TCall (asl_fuzz, [], args, false, loc)

class tracker (vars : int Bindings.t) =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vstmt s =
      match s with
      | Stmt_Assign
          ( LExpr_Slices (ty, LExpr_Var v, [ Slice_LoWd (lo, wd) ]),
            Expr_Unknown _,
            loc )
        when is_safe_to_replicate lo && is_safe_to_replicate wd ->
          Option.fold (Bindings.find_opt v vars)
            ~some:(fun index ->
              let call = mk_call [ mk_litint index; lo; wd ] loc in
              ChangeTo (s :: [ call ]))
            ~none:Visitor.DoChildren
      | _ -> DoChildren
  end

let indexed_vars (vars : Ident.t list) : int Bindings.t =
  mk_bindings (List.mapi (fun index v -> (v, index)) vars)

let xform_stmts (vars : Ident.t list) (ss : AST.stmt list) : AST.stmt list =
  let xform = new tracker (indexed_vars vars) in
  Asl_visitor.visit_stmts (xform :> Asl_visitor.aslVisitor) ss

let xform_decls (vars : Ident.t list) (ds : AST.declaration list) :
    AST.declaration list =
  let xform = new tracker (indexed_vars vars) in
  List.map (Asl_visitor.visit_decl (xform :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * End
 ****************************************************************)
