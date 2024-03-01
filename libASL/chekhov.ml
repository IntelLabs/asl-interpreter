(****************************************************************
 * Tracing facilities
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
module RegMap = Map.Make(String)

module type ChekhovSI = sig
  type vmi_pid = int32
  type time = int64
  type regid = int32

  val setenv : string -> string -> unit
  val create_state : unit -> unit
  val load_memory : string -> unit
  val step : vmi_pid -> time -> unit
  val visibility : vmi_pid -> time -> int64 -> int32 -> bool -> bool -> int option -> int64 -> int64 -> Primops.bitvector -> unit
  val write_memory : vmi_pid -> int64 -> Primops.bitvector -> unit
  val write_register : vmi_pid -> string -> regid -> Primops.bitvector -> unit
  val add_access : vmi_pid -> int64 -> int32 -> bool -> bool -> int64 -> int64 -> int -> unit
  val comment : string -> unit
end


let chekhovText (trace_chan : out_channel) : (module ChekhovSI) = (module struct
  type vmi_pid = int32
  type time = int64
  type regid = int32

  let trace (kind : string) (params : (string * string) list) (comment : string) =
    Printf.fprintf trace_chan "%s" kind;
    List.iter (function (p, v) -> Printf.fprintf trace_chan " %s:%s" p v) params;
    Printf.fprintf trace_chan " // %s\n" comment

  let mkRecord (fields : (string * string) list) : string =
    "(" ^ String.concat "," (List.map (function (f, v) -> f ^":"^ v) fields) ^ ")"

  let mkArray (b : Primops.bitvector) =
    let bytes = List.init (b.n / 8) (fun i -> Z.to_int (Z.extract b.v (i*8) 8)) in
    String.concat "," (List.map (fun xx -> Printf.sprintf "%02x" xx) bytes)

  let mkFuzz (n : int) =
    let bytes = List.init (n / 8) (fun _ -> "ff") in
    String.concat "," (List.map (fun xx -> Printf.sprintf "%s" xx) bytes)

  let setenv (name : string) (value : string) : unit =
    trace "ChekhovSetEnvVariable" [("name", name); ("value", value)] ""

  let create_state _ : unit =
    trace "ChekhovCreateState" [("argc", "0"); ("argv", "NULL")] ""

  let load_memory (ami : string) : unit =
    trace "ChekhovLoadMemory" [("name", "\"" ^ ami ^ "\"")] ""

  let step (agent : vmi_pid) (t : time) : unit =
    trace "ChekhovStepArchsim"
      [ ("agent", Int32.to_string agent)
      ; ("time", Int64.to_string t)
      ; ("numsteps", "1")
      ; ("check", "1")
      ]
      ""

  let visibility (agent : vmi_pid) (t : time) (uid : int64) (pwid : int32) (is_read : bool) (is_data : bool) (ptw : int option) (virt_addr : int64) (phys_addr : int64) (data : Primops.bitvector) : unit =
      let ptw = match ptw with
                | None -> "0"
                | Some level -> if level == 0 then "b" else (string_of_int level) ^ "b"
      in
      let vis = mkRecord
        [ ("agent", Int32.to_string agent)
        ; ("address", Printf.sprintf "%08Lx" phys_addr)
        ; ("linear", Printf.sprintf "%08Lx" virt_addr)
        ; ("mask", "ffffffffffffffff")
        ; ("size", string_of_int (data.n / 8))
        ; ("lip", "0")
        ; ("time", Int64.to_string t)
        ; ("endtime", "0") (* end of precise time window *)
        ; ("iup", "0") (* false *)
        ; ("uid", Int64.to_string uid)
        ; ("context", "0") (* TLB context *)
        ; ("memtype", "0") (* writeback? *)
        ; ("sv", if is_read then "0" else "1")
        ; ("io", "0") (* false *)
        ; ("ptw", ptw) (* 0|3b|2b|1b|b *)
        ; ("adassist", "0") (* false *)
        ; ("faststring", "0") (* false - is it part of a fast string operation *)
        ; ("fetch", if is_data then "0" else "1")
        ; ("vmcs", "0") (* false  VMCS access? *)
        ; ("force", "0") (* false - deprecated *)
        ; ("fwd", "0") (* false  store forwarding hint? *)
        ; ("pwid", Int32.to_string pwid) (* page walk ID *)
        ; ("ept", "0") (* false - is it an EPT load?  *)
        ; ("spp", "0") (* false - *)
        ; ("nonnuking", "0") (* false *)
        ; ("idp", "1") (* true - store valid? *)
        ; ("data", mkArray data)
        ]
      in
      trace "ChekhovSendVisibilityMessage" [("vis", vis)] ""

  let write_memory (agent : vmi_pid) (phys_addr : int64) (data : Primops.bitvector) : unit =
      trace "ChekhovUpdateSIMemory"
        [ ("agent", Int32.to_string agent)
        ; ("address", Printf.sprintf "%08Lx" phys_addr)
        ; ("size", string_of_int (data.n / 8))
        ; ("value", mkArray data)
        ; ("fuzz", mkFuzz data.n)
        ; ("inject", "NULL")
        ; ("notes", "NULL")
        ]
        ""

  let write_register (agent : vmi_pid) (name : string) (ix : regid) (b : Primops.bitvector) : unit =
      trace "ChekhovUpdateSIRegister"
        [ ("agent", Int32.to_string agent)
        ; ("id", Int32.to_string ix)
        ; ("value", mkArray b)
        ; ("fuzz", mkFuzz b.n)
        ; ("notes", "NULL")
        ]
        name

  let add_access (agent : vmi_pid) (uid : int64) (ptw_id : int32) (is_read : bool) (is_data : bool) (virt_addr : int64) (phys_addr : int64) (size_in_bytes : int) : unit =
      trace "ChekhovAddAccessUID"
        [ ("agent", Int32.to_string agent)
        ; ("linear", Printf.sprintf "%08Lx" virt_addr)
        ; ("physical", Printf.sprintf "%08Lx" phys_addr)
        ; ("size", string_of_int size_in_bytes)
        ; ("uid", Int64.to_string uid)
        ; ("store", if is_read then "0" else "1")
        ; ("pwid", Int32.to_string ptw_id)
        ; ("fetch", if is_data then "0" else "1")
        ; ("tickle", "0")
        ]
        ""

  let comment (msg : string) : unit =
    trace "//" [ ("msg", msg) ] ""
end)


(** Create Tracer module that produces Chekhov trace through 'out' module *)
let chekhovTracer
    (env : (string * string) list)
    (regmap : int32 RegMap.t)
    (out : (module ChekhovSI))
    (o_ami : string option)
  : (module Value.Tracer) =
  let module Out = (val out : ChekhovSI) in
  (module struct
  let agent = 0l
  let cycle = ref 0L
  let page_walk_id = ref 0l
  let next_access_id = ref 0L
  (* let context = Value.from_bitsLit "0000 0000" *)

  let trace_next (_ : unit) : unit =
    cycle := Int64.add !cycle 1L;
    Out.step agent !cycle

  let trace_physical_memory ~(is_read : bool) ~(is_data : bool) ~(phys_addr : Z.t) ~(data : Primops.bitvector) : unit = ()

  let trace_virtual_memory ~(is_read : bool) ~(is_data : bool) ~(context : Z.t) ~(virt_addr : Z.t) ~(phys_addr : Z.t) ~(data : Primops.bitvector) : unit =
    Out.visibility agent !cycle !next_access_id !page_walk_id is_read is_data None (Z.to_int64 virt_addr) (Z.to_int64 phys_addr) data;
    if not is_read then Out.write_memory agent (Z.to_int64 phys_addr) data;
    Out.add_access agent !next_access_id !page_walk_id is_read is_data (Z.to_int64 virt_addr) (Z.to_int64 phys_addr) (data.n / 8);
    next_access_id := Int64.add !next_access_id 1L

  let trace_memory_pte ~(context : Z.t) ~(level : Z.t) ~(phys_addr : Z.t) ~(data : Primops.bitvector) : unit =
      let ptw = Some (Z.to_int level) in
      Out.visibility agent !cycle !next_access_id !page_walk_id true true ptw 0L (Z.to_int64 phys_addr) data;
      Out.add_access agent !next_access_id !page_walk_id true true 0L (Z.to_int64 phys_addr) (data.n / 8);
      page_walk_id := Int32.add !page_walk_id 1l

  let trace_var ~(is_local : bool) ~(is_read : bool) (name : Ident.t) (v : Value.value) : unit =
    let name = Ident.pprint name in
    if not is_local && not is_read then
      ( match RegMap.find_opt name regmap with
      | Some ix ->
        let b = match v with
            | VBits b -> b
            | _ -> failwith ("chekhov trace: bits expected. Got " ^ Value.string_of_value v)
        in
        Out.write_register agent name ix b
     | None ->
       ()
     )

  let trace_error ~(kind : string) (vs : string list) : unit =
    Out.comment ("ERROR " ^ kind ^ " " ^ String.concat " " vs)
  let trace_event ~(kind : string) (vs : string list) : unit =
    Out.comment ("EVENT " ^ kind ^ " " ^ String.concat " " vs)

  let trace_function ~(is_prim : bool) ~(is_return : bool) (name : Ident.t) (tvs : Value.value list) (vs : Value.value list) : unit = ()

  let _ = begin
    List.iter (fun (name, value) -> Out.setenv name value) env;
    Out.create_state ();
    Option.iter Out.load_memory o_ami;
  end

end)

(** Load register numbering from file *)
let loadRegmap (filename : string) : int32 RegMap.t =
  let regmap : int32 RegMap.t ref = ref RegMap.empty in
  let json = Yojson.Safe.from_file filename in
  (* json is expected to be of the form "`Assoc [(r1, `Int ix1); (r2, `Int ix2); ...]"
   * where r1 is a register name and ix1 is a unique number that will be used
   * as its identifier to Chekhov.
   * This reader ignores any other kind of information in the json.
   *)
  ( match json with
  | `Assoc assocs ->
      List.iter (fun (x : (string * Yojson.Safe.t)) -> match x with
        | (name, `Int i) ->
          regmap := RegMap.add name (Int32.of_int i) !regmap
        | _ -> ()
      ) assocs
  | _ -> ()
  );
  !regmap

(** Create Tracer module that produces Chekhov trace to file *)
let chekhovTextTracer (env : (string * string) list) (regfile : string) (o_ami : string option) (trace_chan : out_channel) : (module Value.Tracer) =
  let regmap = loadRegmap regfile in
  let si = chekhovText trace_chan in
  chekhovTracer env regmap si o_ami


(*****************************************
 * End
 *****************************************)
