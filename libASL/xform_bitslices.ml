(****************************************************************
 * ASL bitslice transform
 *
 * This simplifies bitslice expressions where the width is not
 * a literal constant.
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Utils

let transform_non_slices (n : AST.expr) (w : AST.expr) (i : AST.expr)
    (x : AST.expr) : AST.expr =
  match x with
  | Expr_TApply (FIdent ("Ones", _), _, _, _) -> mk_lsl_bits n (mk_mask w n) i
  | Expr_TApply (FIdent ("Zeros", _), _, _, _) -> mk_zero_bits n
  | _ -> mk_lsl_bits n (mk_zero_extend_bits w n x) i

(** Transform expression 'x' of width 'w' to an expression of width 'n'
 * that is equivalent to 'zero_extend_bits(x, n) << i'.
 *
 * This transformation consists of a number of special cases with the
 * aim of avoiding creating intermediate values of width 'm'.
 *
 * (This is useful for transforming concatenations of expressions
 * where 'n' is the overall width of the concatenation and 'i'
 * is the bit-position that 'x' should be placed at.)
 *
 * This function will be extended with additional special cases in
 * the future.
 *)
let transform (n : AST.expr) (w : AST.expr) (i : AST.expr) (x : AST.expr) : AST.expr =
  ( match x with
  | Expr_Slices (_, _, [Slice_HiLo _]) ->
    raise (InternalError (__LOC__ ^ ": Slice_HiLo not expected"))
  | Expr_Slices (_, _, [Slice_Single _]) ->
    raise (InternalError (__LOC__ ^ ": Slice_Single not expected"))
  | Expr_Slices ((Type_Bits we | Type_Register (we, _)), e, [Slice_LoWd (lo, wd)]) ->
    (* generate "((zero_extend_bits(e, n) >> lo) AND mk_mask(wd, n)) << i" *)
    let e1 = mk_zero_extend_bits we n e in
    let e2 = mk_lsr_bits n e1 lo in
    let e3 = mk_and_bits n e2 (mk_mask wd n) in
    mk_lsl_bits n e3 i
  | _ -> transform_non_slices n w i x
  )

let transform_assignment (lident : AST.ident)
    (width : AST.expr)
    (slice_width : AST.expr)
    (shift : AST.expr)
    (rhs : AST.expr)
    (l : AST.l) =
  (* Generate masks for clearing affected bits in slice *)
  let slice_mask = mk_lsl_bits width (mk_mask slice_width width) shift in
  let slice_not_mask = mk_not_mask slice_mask width in

  (* Transform the rhs. The transformed rhs should already be correctly shifted
   * and masked *)
  let rhs' = transform_non_slices width slice_width shift rhs in

  (* lhs = (lhs & ~slice_mask) | rhs' *)
  let or_op1 = mk_and_bits width (AST.Expr_Var lident) slice_not_mask in
  let rhs'' = mk_or_bits width or_op1 rhs' in

  Visitor.ChangeTo [AST.Stmt_Assign (LExpr_Var lident, rhs'', l)]

class bitsliceClass =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      ( match x with
      | Expr_Concat (ws, es) when not (List.for_all is_literal_constant ws) ->
        let total_width = Xform_simplify_expr.mk_add_ints ws in
        (* Transform "{w1, .. wn}[ e1, .. en ]" to "e1' OR .. en'"
         *   where, for each index i in [1, .. n]
         *     wi' = sum of [wi .. wn]
         *     ei' == zero_extend_bits(ei, total_width) << wi'
         *)
        let (_, x') = List.fold_right2 (fun w e (i, e0) ->
            let e' = transform total_width w i e in
            let i' = Xform_simplify_expr.mk_add_int w i in
            let e0' = mk_or_bits total_width e' e0 in
            (i', e0')
          )
          ws es (zero, mk_zero_bits total_width)
        in
        ChangeTo x'
      | Expr_TApply (FIdent ("ZeroExtend", _), [w; n], [e; _], _) ->
        ChangeTo (transform n w zero e)
      | _ -> DoChildren
      )

    method! vstmt s =
      match s with
      | Stmt_Assign (
          LExpr_Slices (
            _,
            _,
            [Slice_HiLo _]),
          _,
          _) ->
        raise (InternalError (__LOC__ ^ ": Slice_HiLo not expected"))
      | Stmt_Assign (
          LExpr_Slices (
            Type_Bits (Expr_LitInt _ as w),
            LExpr_Var lident,
            [Slice_LoWd (lo, sw)]),
          rhs,
          l) ->
        transform_assignment lident w sw lo rhs l
      | _ -> DoChildren
  end

let xform_expr (x : AST.expr) : AST.expr =
  let simplify = new bitsliceClass in
  Asl_visitor.visit_expr (simplify :> Asl_visitor.aslVisitor) x

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let simplify = new bitsliceClass in
  Asl_visitor.visit_stmts (simplify :> Asl_visitor.aslVisitor) ss

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let simplify = new bitsliceClass in
  List.map (Asl_visitor.visit_decl (simplify :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * End
 ****************************************************************)
