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

module InstanceKey = struct
  type t = AST.ident * Z.t list

  let compare (x : t) (y : t) : int =
    AST.Ident.compare (fst x) (fst y) <?> (List.compare Z.compare, snd x, snd y)
end

module Instances = Map.Make (InstanceKey)

class monoClass (genv : Eval.GlobalEnv.t) (ds : AST.declaration list) =
  object (self)
    inherit nopAslVisitor
    val mutable instances : AST.declaration Instances.t = Instances.empty
    method getInstances = List.map snd (Instances.bindings instances)

    method monomorphize_type (genv : Eval.GlobalEnv.t) (tc : AST.ident)
        (d : AST.declaration) (szs : Z.t list)
      : AST.ident option =
      let ps =
        ( match Eval.GlobalEnv.get_typedef genv tc with
        | Some (ps, ty) -> ps
        | _ -> ( match Eval.GlobalEnv.get_record genv tc with
               | Some (ps, ty) -> ps
               | _ -> failwith "monomorphize_type"
               )
        )
      in
      List.iter (fun sz -> assert (Z.geq sz Z.zero)) szs; (* sanity check! *)
      let suffices =
        List.map2
          (fun p sz -> AST.pprint_ident p ^ "_" ^ Z.to_string sz)
          ps szs
      in
      let tc' = AST.addSuffix tc (String.concat "_" suffices) in
      let key = (tc, szs) in
      if Instances.mem key instances then (
        Some tc'
      ) else (
        let env = Xform_constprop.mkEnv genv (List.map2 (fun p sz -> (p, Value.VInt sz)) ps szs) in

        ( match d with
        | Decl_Typedef (_, ps, ty, loc) ->
            let ty' = Xform_constprop.xform_ty env ty in
            let d' = AST.Decl_Typedef (tc', [], ty', loc) in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some tc'
        | Decl_Record (_, ps, fs, loc) ->
            let fs' = List.map (fun (f, ty) -> (f, Xform_constprop.xform_ty env ty)) fs in
            let d' = AST.Decl_Record (tc', [], fs', loc) in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some tc'
        | _ ->
            None
        )
      )

    method monomorphize_fun (genv : Eval.GlobalEnv.t) (f : AST.ident)
        (d : AST.declaration) (szs : Z.t list) (args : AST.expr list)
      : (AST.ident * AST.expr list) option =
      let (tvs, arg_names) =
        match Eval.GlobalEnv.get_function genv f with
        | Some (tvs, arg_names, _, _) -> (tvs, arg_names)
        | _ -> failwith "monomorphize_fun"
      in
      List.iter (fun sz -> assert (Z.geq sz Z.zero)) szs; (* sanity check! *)
      let suffices =
        List.map2
          (fun nm sz -> AST.pprint_ident nm ^ "_" ^ Z.to_string sz)
          tvs szs
      in
      let f' = AST.addSuffix f (String.concat "_" suffices) in
      let args' : AST.expr list = Utils.filter_map2
          (fun nm arg -> if List.mem nm tvs then None else Some arg)
          arg_names args
      in
      let key = (f, szs) in
      if Instances.mem key instances then Some (f', args')
      else (
        let env = Xform_constprop.mkEnv genv (List.map2 (fun tv sz -> (tv, Value.VInt sz)) tvs szs) in

        match d with
        | Decl_FunDefn (f, ps, atys, rty, body, loc) ->
            let rty' = Xform_constprop.xform_ty env rty in
            let pnames = List.map fst ps in
            let atys = List.filter (fun (v, _) -> not (List.mem v pnames)) atys in
            let atys' =
              List.map
                (fun (v, ty) -> (v, Xform_constprop.xform_ty env ty))
                atys
            in
            let body' = Xform_constprop.xform_stmts env body in
            let d' = AST.Decl_FunDefn (f', [], atys', rty', body', loc) in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some (f', args')
        | Decl_ProcDefn (f, ps, atys, body, loc) ->
            let pnames = List.map fst ps in
            let atys = List.filter (fun (v, _) -> not (List.mem v pnames)) atys in
            let atys' =
              List.map
                (fun (v, ty) -> (v, Xform_constprop.xform_ty env ty))
                atys
            in
            let body' = Xform_constprop.xform_stmts env body in
            let d' = AST.Decl_ProcDefn (f', [], atys', body', loc) in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some (f', args')
        | Decl_ArrayGetterDefn (f, ps, atys, rty, body, loc) ->
            let rty' = Xform_constprop.xform_ty env rty in
            let pnames = List.map fst ps in
            let atys = List.filter (fun (v, _) -> not (List.mem v pnames)) atys in
            let atys' =
              List.map
                (fun (v, ty) -> (v, Xform_constprop.xform_ty env ty))
                atys
            in
            let body' = Xform_constprop.xform_stmts env body in
            let d' =
              AST.Decl_ArrayGetterDefn (f', [], atys', rty', body', loc)
            in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some (f', args')
        | Decl_ArraySetterDefn (f, ps, atys, v, t, body, loc) ->
            let pnames = List.map fst ps in
            let atys = List.filter (fun (v, _) -> not (List.mem v pnames)) atys in
            let atys' =
              List.map
                (fun (v, ty) -> (v, Xform_constprop.xform_ty env ty))
                atys
            in
            let t' = Xform_constprop.xform_ty env t in
            let body' = Xform_constprop.xform_stmts env body in
            let d' =
              AST.Decl_ArraySetterDefn (f', [], atys', v, t', body', loc)
            in
            let d' = visit_decl (self :> aslVisitor) d' in
            instances <- Instances.add key d' instances;
            Some (f', args')
        | _ -> None)

    method! vtype x =
      ( match x with
      | Type_Constructor (tc, es) -> (
          ( match Utils.flatten_map_option const_int_expr es with
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value
                (Option.bind (find_decl tc ds) (fun d ->
                 Option.bind (self#monomorphize_type genv tc d sizes) (fun tc' ->
                 Some
                   (ChangeDoChildrenPost
                     (AST.Type_Constructor (tc', []), Fun.id))))
                )
                ~default:DoChildren
          | None -> DoChildren
          )
        )
      | _ -> DoChildren
      )

    method! vexpr x =
      match x with
      | Expr_TApply (f, tys, args) -> (
          match Utils.flatten_map_option const_int_expr tys with
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value
                (Option.bind (find_decl f ds) (fun d ->
                     Option.bind (self#monomorphize_fun genv f d sizes args)
                       (fun (f', args') ->
                         Some
                           (ChangeDoChildrenPost
                              (AST.Expr_TApply (f', [], args'), Fun.id)))))
                ~default:DoChildren
          | None -> DoChildren)
      | _ -> DoChildren

    method! vlexpr e =
      match e with
      | LExpr_Write (f, tes, es) -> (
          match Utils.flatten_map_option const_int_expr tes with
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value
                (Option.bind (find_decl f ds) (fun d ->
                     Option.bind (self#monomorphize_fun genv f d sizes es)
                       (fun (f', es') ->
                         Some
                           (ChangeDoChildrenPost
                              (AST.LExpr_Write (f', [], es'), Fun.id)))))
                ~default:DoChildren
          | None -> DoChildren)
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_TCall (f, tys, args, loc) -> (
          match Utils.flatten_map_option const_int_expr tys with
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value
                (Option.bind (find_decl f ds) (fun d ->
                     Option.bind (self#monomorphize_fun genv f d sizes args)
                       (fun (f', args') ->
                         Some
                           (ChangeDoChildrenPost
                              ([AST.Stmt_TCall (f', [], args', loc)], Fun.id)))))
                ~default:DoChildren
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
