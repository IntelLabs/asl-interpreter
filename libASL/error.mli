(****************************************************************
 * Error
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

exception Unimplemented of (Asl_ast.l * string * (Format.formatter -> unit))
exception UnknownObject of (Asl_ast.l * string * string)
exception DoesNotMatch of (Asl_ast.l * string * string * string)
exception IsNotA of (Asl_ast.l * string * string)
exception Ambiguous of (Asl_ast.l * string * string)
exception TypeError of (Asl_ast.l * string)
exception ParseError of Asl_ast.l

val print_exception : exn -> unit

(****************************************************************
 * End
 ****************************************************************)
