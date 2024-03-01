(****************************************************************
 * Binary file loader
 *
 * Simplistic binary file loader
 *
 * Copyright (C) 2023-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

 let byte (b : bytes) (o : int) : char = Bytes.get b o

(****************************************************************)
(** {2 Binary file loader}                                      *)
(****************************************************************)

(** load binary file *)
let load_file (name : string) (write : Int64.t -> char -> unit) (address : Int64.t) : unit =
  let buffer = Utils.read_file name in
  let file_size filename =
    let channel = open_in_bin filename in
    let size = Int64.of_int (in_channel_length channel) in
    close_in channel;
    size
  in
  let fsz = file_size name in
  let rec copy (i : Int64.t) : unit =
    if i < fsz then begin
      write (Int64.add address i) (byte buffer (Int64.to_int i));
      copy (Int64.succ i)
    end
  in
  copy (Int64.of_int 0);

(****************************************************************
 * End
 ****************************************************************)
