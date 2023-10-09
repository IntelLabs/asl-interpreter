type t =
    | Ident of string
    | FIdent of string * int

let pprint (x : t) : string =
    match x with
    | Ident s       -> s
    | FIdent (s, t) -> s ^ "." ^ string_of_int t

let add_tag (x : t) ~(tag: int): t =
    match x with
    | Ident s  -> FIdent (s, tag)
    | FIdent _ -> failwith "add_tag"

let name_of_FIdent (x : t) : string =
    match x with
    | Ident _       -> failwith "name_of_FIdent"
    | FIdent (s, _) -> s

let add_prefix (x : t) ~(prefix: string) : t =
    match x with
    | Ident q       -> Ident (prefix ^ "_" ^ q)
    | FIdent (q, t) -> FIdent (prefix ^ "_" ^ q, t)

let add_suffix (x : t) ~(suffix: string) : t =
    match x with
    | Ident p       -> Ident (p ^ "_" ^ suffix)
    | FIdent (p, t) -> FIdent (p ^ "_" ^ suffix, t)

let compare (x : t) (y : t) : int =
    match (x, y) with
    | (Ident xs, Ident ys) -> String.compare xs ys
    | (FIdent (x, i), FIdent (y, j)) ->
        let cx = String.compare x y in
        if cx <> 0 then cx else compare i j
    | (Ident _, FIdent _) -> -1
    | (FIdent _, Ident _) -> 1

let matches (x : t) ~(name : string) : bool =
    match x with
    | Ident s       -> s = name
    | FIdent (s, _) -> s = name

let is_function (x : t) : bool =
    match x with
    | FIdent _ -> true
    | Ident _  -> false
