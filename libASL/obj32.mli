(****************************************************************
 * OBJ32 loader
 *
 * Currently limited to well-formatted OBJ32 files
 *
 * Copyright Intel (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** load OBJ32 file *)
val load_file : string -> (Int64.t -> Int32.t -> unit) -> unit

(****************************************************************
 * End
 ****************************************************************)
