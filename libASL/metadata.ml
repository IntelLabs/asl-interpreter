(****************************************************************
 * Metadata generator
 *
 * Copyright Intel (c) 2023-2024
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
open Asl_utils

let generate_callgraph (filename : string) (ds : Asl_ast.declaration list): unit =
  let cg = ref Bindings.empty in
  List.iter (fun d ->
      ( match decl_name d with
      | None -> ()
      | Some nm ->
          let callees = calls_of_decl d in
          let old = Bindings.find_opt nm !cg |> Option.value ~default:IdentSet.empty in
          cg := Bindings.add nm (IdentSet.union callees old) !cg
      ))
      ds;
  let t = Bindings.bindings !cg
        |> List.map (fun (caller, callees) ->
              let callee_names = IdentSet.elements callees |> List.map Ident.pprint in
              (Ident.pprint caller, `List (List.map (fun s -> `String(s)) callee_names)))
        |> (fun xs -> `Assoc xs)
  in
  let chan = open_out filename in
  Yojson.pretty_to_channel chan t

(****************************************************************
 * Command: :callgraph
 ****************************************************************)

let cmd_callgraph (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) (args : string list) : bool =
  ( match args with
  | [ file ] ->
    Printf.printf "Generating callgraph metadata file %s.\n" file;
    generate_callgraph file !Commands.declarations;
    true
  | _ ->
    false
  )

let _ = Commands.registerCommand "callgraph" "<json file>" "Generate json file containing callgraph" cmd_callgraph

(****************************************************************
 * End
 ****************************************************************)
