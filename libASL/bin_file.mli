(****************************************************************
 * Binary file loader
 *
 * Simplistic binary file loader
 *
 * Copyright Intel (c) 2023
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** load binary file *)
val load_file : string -> (Int64.t -> char -> unit) -> Int64.t -> unit

(****************************************************************
 * End
 ****************************************************************)
