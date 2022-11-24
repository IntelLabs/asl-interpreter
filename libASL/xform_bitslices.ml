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

(** Transform expression 'x' of width 'w' to an expression of width 'n'
 * that is equivalent to 'ZeroExtend(x, n) << i'.
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
  | Expr_Slices (t, e, [Slice_HiLo (hi, lo)]) ->
    (* generate "((ZeroExtend(e, n) AND mk_mask(hi + 1, n)) >> lo) << i" *)
    let we = Option.get (Asl_utils.width_of_type t) in
    let e1 = mk_zero_extend we n e in
    let e2 = mk_and_bits n e1 (mk_mask (Xform_simplify_expr.mk_add_int hi one) n) in
    let e3 = mk_lsr_bits n e2 lo in
    mk_lsl_bits n e3 i
  | Expr_Slices (t, e, [Slice_LoWd (lo, wd)]) ->
    (* generate "((ZeroExtend(e, n) >> lo) AND mk_mask(wd, n)) << i" *)
    let we = Option.get (Asl_utils.width_of_type t) in
    let e1 = mk_zero_extend we n e in
    let e2 = mk_lsr_bits n e1 lo in
    let e3 = mk_and_bits n e2 (mk_mask wd n) in
    mk_lsl_bits n e3 i
  | Expr_Slices (t, e, [Slice_Single ix]) ->
    (* generate "((ZeroExtend(e, n) >> ix) AND mk_mask(1, n)) << i" *)
    let we = Option.get (Asl_utils.width_of_type t) in
    let e1 = mk_zero_extend we n e in
    let e2 = mk_lsr_bits n e1 ix in
    let e3 = mk_and_bits n e2 (mk_mask one n) in
    mk_lsl_bits n e3 i
  | _ -> mk_lsl_bits n (mk_zero_extend w n x) i
  )

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
         *     ei' == ZeroExtend(ei, total_width) << wi'
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
      | _ -> DoChildren
      )
  end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let simplify = new bitsliceClass in
  List.map (Asl_visitor.visit_decl (simplify :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * End
 ****************************************************************)
