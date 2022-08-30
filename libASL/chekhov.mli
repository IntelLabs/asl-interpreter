(****************************************************************
 * Tracing facilities
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Create Tracer module that produces Chekhov trace to file *)
val chekhovTextTracer : (string * string) list -> string -> string option -> out_channel -> (module Value.Tracer)

(*****************************************
 * End
 *****************************************)
