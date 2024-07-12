(****************************************************************
 * Detect functions that have not been monomorphized
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Identset

let is_polymorphic_function (fty : AST.function_type) : bool =
  not (Utils.is_empty fty.parameters)

let is_polymorphic_decl (d : AST.declaration) : bool =
  ( match d with
  | Decl_Record (_, ps, _, _) -> not (Utils.is_empty ps)
  | Decl_FunType (_, fty, _) -> is_polymorphic_function fty
  | Decl_FunDefn (_, fty, _, _) -> is_polymorphic_function fty
  | _ -> false
  )

let show_dependency_graph (ds : AST.declaration list) (names : IdentSet.t) : unit =

  (* build callgraph and reverse callgraph *)
  let callees = ref Bindings.empty in
  let callers = ref Bindings.empty in
  List.iter
    (fun d ->
       let x = Option.get (decl_name d) in
       let ys = IdentSet.inter (calls_of_decl d) names in
       let old = Bindings.find_opt x !callees |> Option.value ~default:IdentSet.empty in
       callees := Bindings.add x (IdentSet.union old ys) !callees;
       IdentSet.iter (fun y -> callers := addToBindingSet y x !callers) ys;
    )
    ds;

  (* Find the root of each chain of monomorphization failures *)
  let roots = ref IdentSet.empty in
  let rec find_roots (x : Ident.t) : unit =
    ( match Bindings.find_opt x !callers with
    | None -> (* no caller: this is a root *)
        roots := IdentSet.add x !roots
    | Some ys when IdentSet.is_empty ys -> (* no caller: this is a root *)
        roots := IdentSet.add x !roots
    | Some ys ->
        IdentSet.iter find_roots ys
    )
  in
  IdentSet.iter find_roots names;

  (* Display the monomorphization failure trees *)
  let rec display_tree (depth : int) (x : Ident.t) : unit =
    for _ = 1 to 2*depth do
      print_char ' '
    done;
    Printf.printf "%s\n" (Ident.to_string x);
    let ys = Bindings.find_opt x !callees |> Option.value ~default:IdentSet.empty in
    IdentSet.iter (display_tree (depth+1)) ys
  in
  IdentSet.iter (display_tree 2) !roots


(****************************************************************
 * Command: :check_monomorphization
 ****************************************************************)

let _ =
  let fatal = ref false in
  let verbose = ref false in

  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let polymorphic = List.filter is_polymorphic_decl !Commands.declarations in
    if not (Utils.is_empty polymorphic) then (
      let seriousness = if !fatal then "ERROR" else "WARNING" in
      Printf.printf "%s: Monomorphization failed for the following definitions.\n"
        seriousness;
      Printf.printf "This will cause the C backend to fail.";

      let names = List.filter_map decl_name polymorphic |> IdentSet.of_list in
      (if !verbose then
        show_dependency_graph polymorphic names
      else
        IdentSet.iter (fun x -> Printf.printf "%s\n" (Ident.to_string x)) names
      );
      not !fatal
    ) else (
      true
    )
  in

  let flags = Arg.align [
      ("--fatal", Arg.Set fatal, " Abort build if incompletely monomorphized");
      ("--verbose", Arg.Set verbose, " Show detailed dependency chains");
    ]
  in
  Commands.registerCommand "check_monomorphization" flags [] [] "Check for monomorphization failures" cmd

(****************************************************************
 * End
 ****************************************************************)
