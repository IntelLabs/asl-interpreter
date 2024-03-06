(****************************************************************
 * Tracing facilities
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Create Tracer module that produces Chekhov trace to file *)
val chekhovTextTracer : (string * string) list -> string -> string option -> out_channel -> (module LibASL.Value.Tracer)

(*****************************************
 * End
 *****************************************)
