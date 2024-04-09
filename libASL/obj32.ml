(****************************************************************
 * OBJ32 loader
 *
 * Currently limited to well-formatted OBJ32 files
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL

(****************************************************************)
(** {2 OBJ32 loader}                                            *)
(****************************************************************)

let rec read_blocks (write : Int64.t -> Int32.t -> unit) (chan : in_channel) (origin : Int64.t) : unit =
  let line = input_line chan in
  let line = String.trim line in
  match String.split_on_char ' ' line with
  | [] -> ()
  | [""] -> ()
  | ["/origin"; addr] ->
      let addr = Scanf.sscanf addr "%Lx" Fun.id in
      read_blocks write chan (Int64.mul addr (Int64.of_int 4))
  | bits ->
    List.iteri (fun i bit ->
        let addr = Int64.add origin (Int64.of_int (i * 4)) in
        let data = Scanf.sscanf bit "%lx" Fun.id in
        write addr data
    ) bits;
    let len = List.length bits * 4 in
    read_blocks write chan (Int64.add origin (Int64.of_int len))

(** load OBJ32 file *)
let load_file (name : string) (write : Int64.t -> Int32.t -> unit) : unit =
  let chan = open_in name in
  try
    read_blocks write chan Int64.zero
  with e ->
    close_in_noerr chan

(****************************************************************
 * Command: :obj
 ****************************************************************)

let _ =
  let file = ref "" in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Printf.printf "Loading OBJ32 file %s.\n" !file;
    load_file !file cpu.elfwrite32;
    true
  in
  let args = [
    (file, "file");
  ]
  in
  Commands.registerCommand "obj" [] args [] "Load an OBJ32 file" cmd

(****************************************************************
 * End
 ****************************************************************)
