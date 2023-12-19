(****************************************************************
 * Error
 *
 * Copyright Intel Inc (c) 2021-2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

exception Unimplemented of (Asl_ast.l * string * (Format.formatter -> unit))

val print_exception : exn -> unit
