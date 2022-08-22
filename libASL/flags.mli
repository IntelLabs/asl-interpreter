(****************************************************************
 * Control flag registry
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module FlagMap : Map.S with type key = string

val flags : (bool ref * string) FlagMap.t ref

val registerFlag : string -> bool ref -> string -> unit

(****************************************************************
 * End
 ****************************************************************)
