(****************************************************************
 * Generic utility functions
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Generic utility functions *)

(****************************************************************
 * Pretty-printer related
 ****************************************************************)

let to_string2 (pp : Format.formatter -> unit) : string =
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let to_file (filename : string) (pp : Format.formatter -> unit) : unit =
  let chan = open_out filename in
  let fmt = Format.formatter_of_out_channel chan in
  pp fmt;
  Format.pp_print_flush fmt ();
  close_out chan

(****************************************************************
 * List related
 ****************************************************************)

let is_empty (xs : 'a list) : bool = match xs with [] -> true | _ -> false

let nub (xs : 'a list) : 'a list =
  let rec nub_aux seen xs =
    match xs with
    | [] -> seen
    | y :: ys ->
        if List.mem y seen then nub_aux seen ys else nub_aux (y :: seen) ys
  in
  nub_aux [] xs

(** init [x1; ... xn] = [x1; ... xn-1] *)
let init (xs : 'a list) : 'a list =
  (* tail recursive helper function *)
  let rec aux r xs =
    ( match xs with
    | [x] -> List.rev r
    | (x :: xs') -> aux (x :: r) xs'
    | [] -> failwith "init []"
    )
  in
  aux [] xs

(** last [x1; ... xn] = xn *)
let rec last (xs : 'a list) : 'a =
  ( match xs with
  | [x] -> x
  | (_ :: xs') -> last xs'
  | [] -> failwith "last []"
  )

(** split3 [(x1, y1, z1); ... (xn, yn, zn)] = ([x1; ... xn], [y1; ... yn], [z1; ... zn]) *)
let split3 (xyzs : ('x * 'y * 'z) list) : ('x list * 'y list * 'z list) =
  List.fold_right
    (fun (x, y, z) (xs, ys, zs) -> (x :: xs, y :: ys, z :: zs))
    xyzs
    ([], [], [])


(****************************************************************
 * Option related
 ****************************************************************)

let from_option (ox : 'a option) (d : unit -> 'a) : 'a =
  match ox with None -> d () | Some x -> x

let orelse_option (ox : 'a option) (f : unit -> 'a option) : 'a option =
  match ox with None -> f () | Some _ -> ox

let rec concat_option (oss : 'a list option list) : 'a list option =
  match oss with
  | [] -> Some []
  | None :: _ -> None
  | Some xs :: xss -> Option.map (List.append xs) (concat_option xss)

(* extract all non-None elements from a list *)
let flatten_option (os : 'a option list) : 'a list =
  let rec aux r os =
    match os with
    | [] -> List.rev r
    | Some o :: os' -> aux (o :: r) os'
    | None :: os' -> aux r os'
  in
  aux [] os

(* Like Option.filter_map for binary operations
 *
 * `filter_map2 f xs ys` applies `f` to every element of `xs` and `ys`,
 * filters out the None elements and returns the list of the arguments
 * of the Some elements.
 * *)
let filter_map2 (f : 'a -> 'b -> 'c option) (xs : 'a list) (ys : 'b list) : 'c list =
  List.map2 f xs ys |> flatten_option

(* todo: give this a better name *)
let flatten_map_option (f : 'a -> 'b option) (xs : 'a list) : 'b list option =
  let rec aux r xs =
    match xs with
    | [] -> Some (List.rev r)
    | x :: xs' -> ( match f x with Some b -> aux (b :: r) xs' | None -> None)
  in
  aux [] xs

(* find first non-None result from function 'f' on list 'xs' *)
let rec first_option (f : 'a -> 'b option) (xs : 'a list) : 'b option =
  match xs with
  | [] -> None
  | x :: xs' -> (
      match f x with Some b -> Some b | None -> first_option f xs')

(****************************************************************
 * String related
 ****************************************************************)

(** Drop first n characters from string *)
let string_drop (n : int) (s : string) : string =
  let l = String.length s in
  if n > l then "" else String.sub s n (l - n)

(****************************************************************
 * Compare related
 ****************************************************************)

(** combine the result of two compare functions *)
let ( <?> ) c (cmp, x, y) = if c = 0 then cmp x y else c

(****************************************************************
 * End
 ****************************************************************)
