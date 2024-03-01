(****************************************************************
 * Command registry
 *
 * Copyright (C) 2024-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type command = Tcheck.Env.t -> Cpu.cpu -> string list -> bool

module CommandMap = Map.Make(String)

let commands : (command * string * string) CommandMap.t ref = ref CommandMap.empty

let registerCommand (name : string) (args : string) (description : string) (cmd : command) : unit =
  commands := CommandMap.add name (cmd, args, description) !commands

let declarations : Asl_ast.declaration list ref = ref []

(****************************************************************
 * End
 ****************************************************************)
