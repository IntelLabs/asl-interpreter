(****************************************************************
 * ASL function monomorphization transform
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL function monomorphization transform *)

module AST = Asl_ast
module TC = Tcheck
open Visitor
open Asl_visitor
open Utils
open Asl_utils

let const_int_expr (x : AST.expr) : Z.t option =
  match x with Expr_LitInt x -> Some (Z.of_string x) | _ -> None

let rec getFun (f : AST.ident) (ds : AST.declaration list) :
    AST.declaration option =
  match ds with
  | [] -> None
  | (Decl_FunDefn (f', ps, atys, rty, body, loc) as d) :: ds' ->
      if f = f' then Some d else getFun f ds'
  | (Decl_ProcDefn (f', ps, atys, body, loc) as d) :: ds' ->
      if f = f' then Some d else getFun f ds'
  | (Decl_ArrayGetterDefn (f', ps, atys, rty, body, loc) as d) :: ds' ->
      if f = f' then Some d else getFun f ds'
  | (Decl_ArraySetterDefn (f', ps, atys, v, t, body, loc) as d) :: ds' ->
      if f = f' then Some d else getFun f ds'
  | _ :: ds' -> getFun f ds'

module InstanceKey = struct
  type t = AST.ident * Z.t list

  let compare (x : t) (y : t) : int =
    AST.Id.compare (fst x) (fst y) <?> (List.compare Z.compare, snd x, snd y)
end

module Instances = Map.Make (InstanceKey)

class monoClass (genv : Eval.GlobalEnv.t) (ds : AST.declaration list) =
  object (self)
    inherit nopAslVisitor
    val mutable instances : AST.declaration Instances.t = Instances.empty
    method getInstances = List.map snd (Instances.bindings instances)

    method monomorphize (genv : Eval.GlobalEnv.t) (f : AST.ident)
        (d : AST.declaration) (szs : Z.t list) : AST.ident option =
      let tvs =
        match Eval.GlobalEnv.getFun Unknown genv f with
        | Some (tvs, _, _, _) -> tvs
        | _ -> failwith "monomorphize"
      in
      assert (List.length tvs = List.length szs);
      let suffices =
        List.map2
          (fun nm sz -> AST.pprint_ident nm ^ "_" ^ Z.to_string sz)
          tvs szs
      in
      let f' = AST.addSuffix f (String.concat "_" suffices) in
      let key = (f, szs) in
      if Instances.mem key instances then Some f'
      else (
        Printf.printf "Monomorphizing %s to %s\n" (AST.pprint_ident f)
          (AST.pprint_ident f');

        let env = Xform_constprop.Env.newEnv genv in
        List.iter2
          (fun sz tv ->
            Xform_constprop.Env.addLocalConst env tv
              (Xform_constprop.Values.singleton (VInt sz)))
          szs tvs;

        match d with
        | Decl_FunDefn (f, ps, atys, rty, body, loc) ->
            let rty' = Xform_constprop.xform_ty env rty in
            let atys' =
              List.map
                (fun (v, ty) -> (v, Xform_constprop.xform_ty env ty))
                atys
            in
            let body' = Xform_constprop.xform_stmts env body in
            let d' = AST.Decl_FunDefn (f', ps, atys', rty', body', loc) in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some f'
        | Decl_ProcDefn (f, ps, atys, body, loc) ->
            let atys' =
              List.map
                (fun (v, ty) -> (v, Xform_constprop.xform_ty env ty))
                atys
            in
            let body' = Xform_constprop.xform_stmts env body in
            let d' = AST.Decl_ProcDefn (f', ps, atys', body', loc) in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some f'
        | Decl_ArrayGetterDefn (f, ps, atys, rty, body, loc) ->
            let rty' = Xform_constprop.xform_ty env rty in
            let atys' =
              List.map
                (fun (v, ty) -> (v, Xform_constprop.xform_ty env ty))
                atys
            in
            let body' = Xform_constprop.xform_stmts env body in
            let d' =
              AST.Decl_ArrayGetterDefn (f', ps, atys', rty', body', loc)
            in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some f'
        | Decl_ArraySetterDefn (f, ps, atys, v, t, body, loc) ->
            let atys' =
              List.map
                (fun (v, ty) -> (v, Xform_constprop.xform_ty env ty))
                atys
            in
            let t' = Xform_constprop.xform_ty env t in
            let body' = Xform_constprop.xform_stmts env body in
            let d' =
              AST.Decl_ArraySetterDefn (f', ps, atys', v, t', body', loc)
            in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some f'
        | _ -> None)

    method! vexpr x =
      match x with
      | Expr_TApply (f, tys, args) -> (
          match Utils.flatten_map_option const_int_expr tys with
          | Some [] -> DoChildren
          | Some sizes ->
              Utils.from_option
                (Utils.bind_option (getFun f ds) (fun d ->
                     Utils.bind_option (self#monomorphize genv f d sizes)
                       (fun f' ->
                         Some
                           (ChangeDoChildrenPost
                              (AST.Expr_TApply (f', [], args), Fun.id)))))
                (fun _ -> DoChildren)
          | None -> DoChildren)
      | _ -> DoChildren

    method! vlexpr e =
      match e with
      | LExpr_Write (f, tes, es) -> (
          match Utils.flatten_map_option const_int_expr tes with
          | Some [] -> DoChildren
          | Some sizes ->
              Utils.from_option
                (Utils.bind_option (getFun f ds) (fun d ->
                     Utils.bind_option (self#monomorphize genv f d sizes)
                       (fun f' ->
                         Some
                           (ChangeDoChildrenPost
                              (AST.LExpr_Write (f', [], es), Fun.id)))))
                (fun _ -> DoChildren)
          | None -> DoChildren)
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_TCall (f, tys, args, loc) -> (
          match Utils.flatten_map_option const_int_expr tys with
          | Some [] -> DoChildren
          | Some sizes ->
              Utils.from_option
                (Utils.bind_option (getFun f ds) (fun d ->
                     Utils.bind_option (self#monomorphize genv f d sizes)
                       (fun f' ->
                         Some
                           (ChangeDoChildrenPost
                              ([AST.Stmt_TCall (f', [], args, loc)], Fun.id)))))
                (fun _ -> DoChildren)
          | None -> DoChildren)
      | _ -> DoChildren
  end

let monomorphize (ds : AST.declaration list) : AST.declaration list =
  let genv = Eval.build_constant_environment ds in
  let mono = new monoClass genv ds in
  let ds' = List.map (visit_decl (mono :> aslVisitor)) ds in
  ds' @ mono#getInstances

(****************************************************************
 * End
 ****************************************************************)
