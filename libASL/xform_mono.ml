(****************************************************************
 * ASL function monomorphization transform
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL function monomorphization transform *)

module AST = Asl_ast
open Visitor
open Asl_visitor
open Utils
open Asl_utils

let enable_auto_case_split = ref false

let const_int_expr (x : AST.expr) : Z.t option =
  match x with Expr_LitInt x -> Some (Z.of_string x) | _ -> None

module InstanceKey = struct
  type t = Ident.t * Z.t list

  let compare (x : t) (y : t) : int =
    Ident.compare (fst x) (fst y) <?> (List.compare Z.compare, snd x, snd y)
end

module Instances = Map.Make (InstanceKey)

let ( let* ) = Option.bind

(* Class for recursively collecting identifiers from parameters for call
   expressions *)
class param_collector =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    val mutable params = IdentSet.empty

    method add_param (p : AST.expr) =
      match p with
      | Expr_Var id -> params <- IdentSet.add id params;
      | _           -> ()

    method! vexpr e =
      match e with
      | Expr_TApply (_, params, _, _) ->
          List.iter self#add_param params;
          DoChildren
      | _ -> DoChildren

    method get_params =
      IdentSet.elements params
  end

class monoClass
    (genv : Eval.GlobalEnv.t)
    (global_type_info : AST.ty Bindings.t)
    (ds : AST.declaration list) =
  object (self)
    inherit nopAslVisitor
    val mutable instances : AST.declaration Instances.t = Instances.empty

    val mutable local_type_info : AST.ty Bindings.t = Bindings.empty

    method update_local_type_info (t : Ident.t) (ty : AST.ty) : unit =
      local_type_info <- Bindings.add t ty local_type_info

    method clear_local_type_info () : unit =
      local_type_info <- Bindings.empty

    method get_type (i : Ident.t) : AST.ty option =
      orelse_option
        (Bindings.find_opt i global_type_info)
        (fun _ -> Bindings.find_opt i local_type_info)

    method getInstances = List.map snd (Instances.bindings instances)

    (* Return the constraints for a given parameter identifier, if constraints
       exist and they are of type integer. *)
    method param_to_constraints (p : Ident.t) : AST.constraint_range list option =
      match self#get_type p with
      | Some (AST.Type_Integer cs) -> cs
      | _ -> None

    (* Create a 'when' branch with statement [stmt] using the constraints [cs] as
       pattern. *)
    method constraints_to_when_branch (loc : AST.l) (stmt : AST.stmt) (cs : AST.constraint_range list)
      : AST.alt option =
      let to_pattern (c : AST.constraint_range) : AST.pattern option =
        match c with
        | Constraint_Single (Expr_LitHex s) -> Some (Pat_LitHex s)
        | Constraint_Single (Expr_LitInt s) -> Some (Pat_LitInt s)
        | Constraint_Single (Expr_LitBits s) -> Some (Pat_LitBits s)
        | _ -> None
      in
      let* ps = flatten_map_option to_pattern cs in
      Some (AST.Alt_Alt ([AST.Pat_Tuple ps], None, [stmt], loc))

    (* Create a 'Stmt_Case' from [stmt], creating when-branches for all
       combinations of parameter constraints using parameters [params]. *)
    method build_case_stmt (loc : AST.l) (stmt : AST.stmt) (params : Ident.t list)
      : AST.stmt option =
      if !enable_auto_case_split then (
        let* constraints = flatten_map_option self#param_to_constraints params in
        let constraints_combinations = cartesian_product constraints in
        let* when_branches = flatten_map_option (self#constraints_to_when_branch loc stmt) constraints_combinations in
        let params' = List.map (fun p -> AST.Expr_Var p) params in
        Some (AST.Stmt_Case ((Expr_Tuple params'), when_branches, None, loc))
      ) else (
        None
      )

    (* Collect the parameters from a list of expressions [e]. Also add the
       parameters from the list of [params]. *)
    method collect_params (e : AST.expr list) (params : AST.expr list)
      : Ident.t list =
      let collector = new param_collector in
      let _ = Asl_visitor.visit_exprs (collector :> Asl_visitor.aslVisitor) e in
      List.iter collector#add_param params;
      collector#get_params


    val decl_lookup_table =
      ds
      |> List.to_seq
      |> Seq.filter_map monomorphizable_decl_to_ident_and_decl
      |> IdentTable.of_seq

    method monomorphize_type (genv : Eval.GlobalEnv.t) (tc : Ident.t)
        (d : AST.declaration) (szs : Z.t list)
      : Ident.t option =
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
          (fun p sz -> Ident.pprint p ^ "_" ^ Z.to_string sz)
          ps szs
      in
      let tc' = Ident.add_suffix tc ~suffix:(String.concat "_" suffices) in
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

    method monomorphize_fun (genv : Eval.GlobalEnv.t) (f : Ident.t)
        (d : AST.declaration) (szs : Z.t list) (args : AST.expr list)
      : (Ident.t * AST.expr list) option =
      let (tvs, arg_names) =
        match Eval.GlobalEnv.get_function genv f with
        | Some (tvs, arg_names, _, _) -> (tvs, arg_names)
        | _ -> failwith (Printf.sprintf "monomorphize_fun: %s" (Ident.name_with_tag f))
      in
      List.iter (fun sz -> assert (Z.geq sz Z.zero)) szs; (* sanity check! *)
      let suffices =
        List.map2
          (fun nm sz -> Ident.pprint nm ^ "_" ^ Z.to_string sz)
          tvs szs
      in
      let f' = Ident.add_suffix f ~suffix:(String.concat "_" suffices) in
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
                (Option.bind (IdentTable.find_opt decl_lookup_table tc) (fun d ->
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
      | Expr_RecordInit (tc, tys, fs) -> (
          match Utils.flatten_map_option const_int_expr tys with
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value (
                Option.bind (IdentTable.find_opt decl_lookup_table tc) (fun d ->
                Option.bind (self#monomorphize_type genv tc d sizes) (fun tc' ->
                Some (ChangeDoChildrenPost (AST.Expr_RecordInit (tc', [], fs), Fun.id))
                )))
                ~default:DoChildren
          | None -> DoChildren)
      | Expr_TApply (f, tys, args, throws) -> (
          match Utils.flatten_map_option const_int_expr tys with
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value
                (Option.bind (IdentTable.find_opt decl_lookup_table f) (fun d ->
                     Option.bind (self#monomorphize_fun genv f d sizes args)
                       (fun (f', args') ->
                         Some
                           (ChangeDoChildrenPost
                              (AST.Expr_TApply (f', [], args', throws), Fun.id)))))
                ~default:DoChildren
          | None -> DoChildren)
      | _ -> DoChildren

    method! vlexpr e =
      match e with
      | LExpr_Write (f, tes, es, throws) -> (
          match Utils.flatten_map_option const_int_expr tes with
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value
                (Option.bind (IdentTable.find_opt decl_lookup_table f) (fun d ->
                     Option.bind (self#monomorphize_fun genv f d sizes es)
                       (fun (f', es') ->
                         Some
                           (ChangeDoChildrenPost
                              (AST.LExpr_Write (f', [], es', throws), Fun.id)))))
                ~default:DoChildren
          | None -> DoChildren)
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_VarDecl (d, e, loc)
      | Stmt_ConstDecl (d, e, loc) -> (

          let rec add_decl (d : AST.decl_item) : unit =
            match d with
            | AST.DeclItem_Tuple ds -> List.iter add_decl ds
            | DeclItem_Var (i, Some ty) -> self#update_local_type_info i ty
            | DeclItem_BitTuple ds ->
                List.iter (fun d -> match d with (Some i, ty) -> self#update_local_type_info i ty | _ -> ()) ds
            | _ -> ()
          in

          (* possibly update type info *)
          add_decl d;

          match d with
          | DeclItem_Var (i, Some ty) -> (
            match self#collect_params [e] [] with
            | [] -> DoChildren
            | params -> (
                let decl = AST.Stmt_VarDeclsNoInit ([i], ty, loc) in
                let assign_stmt = AST.Stmt_Assign (AST.LExpr_Var i, e, loc) in
                match self#build_case_stmt loc assign_stmt params with
                | None -> DoChildren
                | Some case_stmt ->
                  let env = Xform_constprop.mkEnv genv [] in
                  let case_stmts' = Xform_constprop.xform_stmts env [ case_stmt ] in
                  (* Now monomorphize the calls in each 'when' *)
                  ChangeDoChildrenPost (decl :: case_stmts', Fun.id)
              )
            )
          | _ -> DoChildren)
      | Stmt_TCall (f, tys, args, throws, loc) -> (
          match Utils.flatten_map_option const_int_expr tys with
          | None ->
              (match self#collect_params args tys with
              | [] -> DoChildren
              | params' ->
                  (match self#build_case_stmt loc s params' with
                  | None -> DoChildren
                  | Some case_stmt ->
                    let env = Xform_constprop.mkEnv genv [] in
                    let case_stmts' = Xform_constprop.xform_stmts env [ case_stmt ] in
                    (* Now monomorphize the calls in each 'when' *)
                    ChangeDoChildrenPost (case_stmts', Fun.id)
                  )
              )
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value
                (Option.bind (IdentTable.find_opt decl_lookup_table f) (fun d ->
                     Option.bind (self#monomorphize_fun genv f d sizes args)
                       (fun (f', args') ->
                         Some
                           (ChangeDoChildrenPost
                              ([AST.Stmt_TCall (f', [], args', throws, loc)], Fun.id)))))
                ~default:DoChildren
          )
      | Stmt_Assign (LExpr_Var i, e, loc) -> (
          match self#collect_params [e] [] with
          | [] -> DoChildren
          | params -> (
              match self#build_case_stmt loc s params with
              | None -> DoChildren
              | Some case_stmt ->
                let env = Xform_constprop.mkEnv genv [] in
                let case_stmts' = Xform_constprop.xform_stmts env [ case_stmt ] in
                (* Now monomorphize the calls in each 'when' *)
                ChangeDoChildrenPost (case_stmts', Fun.id)
            )
          )
      | _ -> DoChildren

    method! vdecl d =
      (* Clear type info for each new declaration being processed *)
      self#clear_local_type_info ();

      (* If declaration is a function, add argument type info, then regardless
         of declaration process it *)
      match d with
      | Decl_BuiltinFunction (_, _, args, _, _)
      | Decl_FunType (_, _, args, _, _)
      | Decl_FunDefn (_, _, args, _, _, _)
      | Decl_ProcType (_, _, args, _)
      | Decl_ProcDefn (_, _, args, _, _)
      | Decl_ArrayGetterType (_, _, args, _, _)
      | Decl_ArrayGetterDefn (_, _, args, _, _, _)
      | Decl_ArraySetterType (_, _, args, _, _, _)
      | Decl_ArraySetterDefn (_, _, args, _, _, _, _) ->
          List.iter (fun (i, ty) -> self#update_local_type_info i ty) args;
          DoChildren
      | Decl_VarSetterType (_, _, v, ty, _)
      | Decl_VarSetterDefn (_, _, v, ty, _, _) ->
          List.iter (fun (i, ty) -> self#update_local_type_info i ty) [ (v, ty) ];
          DoChildren
      | _ -> DoChildren
  end

(* Add all global variables that has a type to the global type info map which
   is passed to the mono class *)
let build_global_type_info (ds : AST.declaration list) =
  let add_type_info (map : AST.ty Bindings.t) (d : AST.declaration) =
    match d with
    | Decl_Var (i, ty, _)
    | Decl_Const (i, Some ty, _, _) -> Bindings.add i ty map
    | _ -> map
  in
  List.fold_left add_type_info Bindings.empty ds

let monomorphize (ds : AST.declaration list) : AST.declaration list =
  let genv = Eval.build_constant_environment ds in
  let global_type_info = build_global_type_info ds in
  let mono = new monoClass genv global_type_info ds in
  let ds' = List.map (visit_decl (mono :> aslVisitor)) ds in
  ds' @ mono#getInstances

(****************************************************************
 * Command: :xform_monomorphize
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := monomorphize !Commands.declarations;
    true
  in
  let flags = Arg.align [
        ("--auto-case-split",    Arg.Set enable_auto_case_split,   " Generate case split code automatically");
        ("--no-auto-case-split", Arg.Clear enable_auto_case_split,   " Do not generate case split code automatically");
      ]
  in
  Commands.registerCommand "xform_monomorphize" flags [] [] "Monomorphize function calls" cmd

(****************************************************************
 * End
 ****************************************************************)
