(****************************************************************
 * Commands registry
 *
 * Copyright Intel Inc (c) 2024
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type command = Tcheck.Env.t -> Cpu.cpu -> string list -> bool

module CommandMap : Map.S with type key = string

val commands : (command * string * string) CommandMap.t ref

(** [registerCommand name args description cmd] registers a command
    @param name        Command name
    @param args        Argument list (used for :help command)
    @param description Short description (used for :help command)
    @param cmd         Function to parse the arguments and execute the command
 *)
val registerCommand : string -> string -> string -> command -> unit

(** List of all declarations.
 *  This list can be accessed and updated by commands
 *)
val declarations : Asl_ast.declaration list ref

(****************************************************************
 * End
 ****************************************************************)
