(****************************************************************
 * ASL to C backend
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C backend *)

module AST = Asl_ast
module FMTAST = Asl_fmt
module PP = Format
open Format_utils

exception Unimplemented of (AST.l * string * (Format.formatter -> unit))

let declaration (fmt : PP.formatter) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      match x with
      | _ ->
          raise
            (Unimplemented
               (AST.Unknown, "declaration", fun fmt -> FMTAST.declaration fmt x)))

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ -> map fmt (declaration fmt) xs)

(****************************************************************
 * End
 ****************************************************************)
