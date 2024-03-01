(****************************************************************
 * Error
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

exception Unimplemented of (Asl_ast.l * string * (Format.formatter -> unit))

val print_exception : exn -> unit

(****************************************************************
 * End
 ****************************************************************)
