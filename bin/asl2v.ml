(****************************************************************
 * ASL interactive frontend
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL interactive frontend *)

open LibASL
module AST = Asl_ast
module CP = Xform_constprop
module ASL_FMT = Asl_fmt
module PP = Format

open Yojson

(****************************************************************
 * C backend support
 *
 * The C backend generates separate files containing
 * - types, constants and function prototypes
 * - variable definitions
 * - function definitions
 * These functions split the list of ASL declarations into
 * these separate groups of declarations.
 ****************************************************************)

let rec type_decls (xs : AST.declaration list) : AST.declaration list =
  let mk_type_decl (x : AST.declaration) : AST.declaration option =
    ( match x with
    | Decl_Const _
    | Decl_Enum _
    | Decl_Record _
    | Decl_Typedef _
    | Decl_FunType _
    | Decl_ProcType _ -> Some x
    | Decl_FunDefn (f, ps, args, t, _, loc) -> Some (Decl_FunType (f, ps, args, t, loc))
    | Decl_ProcDefn (f, ps, args, _, loc) -> Some (Decl_ProcType (f, ps, args, loc))

    | Decl_Exception _
    | Decl_Var _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_VarGetterType _
    | Decl_VarGetterDefn _
    | Decl_ArrayGetterType _
    | Decl_ArrayGetterDefn _
    | Decl_VarSetterType _
    | Decl_VarSetterDefn _
    | Decl_ArraySetterType _
    | Decl_ArraySetterDefn _
    | Decl_Operator1 _
    | Decl_Operator2 _
    | Decl_NewEventDefn _
    | Decl_EventClause _
    | Decl_NewMapDefn _
    | Decl_MapClause _
    | Decl_Config _
      -> None
    )
  in
  List.filter_map mk_type_decl xs

let rec var_decls (xs : AST.declaration list) : AST.declaration list =
  let is_var_decl (x : AST.declaration) : bool =
    ( match x with
    | Decl_Var _
      -> true

    | Decl_Const _
    | Decl_Enum _
    | Decl_Record _
    | Decl_Exception _
    | Decl_Typedef _
    | Decl_FunType _
    | Decl_ProcType _
    | Decl_FunDefn _
    | Decl_ProcDefn _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_VarGetterType _
    | Decl_VarGetterDefn _
    | Decl_ArrayGetterType _
    | Decl_ArrayGetterDefn _
    | Decl_VarSetterType _
    | Decl_VarSetterDefn _
    | Decl_ArraySetterType _
    | Decl_ArraySetterDefn _
    | Decl_Operator1 _
    | Decl_Operator2 _
    | Decl_NewEventDefn _
    | Decl_EventClause _
    | Decl_NewMapDefn _
    | Decl_MapClause _
    | Decl_Config _
      -> false
    )
  in
  List.filter is_var_decl xs

let rec fun_decls (xs : AST.declaration list) : AST.declaration list =
  let is_fun_decl (x : AST.declaration) : bool =
    ( match x with
    | Decl_FunDefn _
    | Decl_ProcDefn _
      -> true

    | Decl_Var _
    | Decl_Const _
    | Decl_Enum _
    | Decl_Record _
    | Decl_Exception _
    | Decl_Typedef _
    | Decl_FunType _
    | Decl_ProcType _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_VarGetterType _
    | Decl_VarGetterDefn _
    | Decl_ArrayGetterType _
    | Decl_ArrayGetterDefn _
    | Decl_VarSetterType _
    | Decl_VarSetterDefn _
    | Decl_ArraySetterType _
    | Decl_ArraySetterDefn _
    | Decl_Operator1 _
    | Decl_Operator2 _
    | Decl_NewEventDefn _
    | Decl_EventClause _
    | Decl_NewMapDefn _
    | Decl_MapClause _
    | Decl_Config _
      -> false
    )
  in
  List.filter is_fun_decl xs

let fprinf_sys_includes (fmt : PP.formatter) (filenames : string list) : unit =
  List.iter (PP.fprintf fmt "#include <%s>@.") filenames;
  PP.pp_print_newline fmt ()

let fprinf_includes (fmt : PP.formatter) (filenames : string list) : unit =
  List.iter (PP.fprintf fmt "#include \"%s\"@.") filenames;
  PP.pp_print_newline fmt ()

let emit_c_header (filename : string) (sys_h_filenames : string list)
    (h_filenames : string list) (f : PP.formatter -> unit) : unit =
  let macro =
    String.uppercase_ascii filename
    |> String.map (fun c -> if List.mem c [ '.'; '/'; '-' ] then '_' else c)
  in
  Utils.to_file filename (fun fmt ->
      PP.fprintf fmt "#ifndef %s@." macro;
      PP.fprintf fmt "#define %s@,@." macro;
      fprinf_sys_includes fmt sys_h_filenames;
      fprinf_includes fmt h_filenames;
      f fmt;
      PP.fprintf fmt "#endif  // %s@." macro
  )

let emit_c_source (filename : string) (h_filenames : string list)
    (f : PP.formatter -> unit) : unit =
  Utils.to_file filename (fun fmt ->
      fprinf_includes fmt h_filenames;
      f fmt
  )

(****************************************************************
 * Small transformations
 ****************************************************************)

let xform_reachable (roots : AST.ident list) (ds : AST.declaration list) : AST.declaration list =
  let ds' = Asl_utils.reachable_decls roots ds in
  (* A minimal sanity check on code generation is that the result
   * should contain at least one definition.
   *)
  if ds' = [] then failwith "Couldn't find any roots";
  ds'

(****************************************************************
 * JSON file reading support
 ****************************************************************)

(* Attempt to get a Json string *)
let get_string (tree : Safe.t) : string option =
  ( match tree with
  | `String s -> Some s
  | _ -> None
  )

(* Attempt to get a Json list *)
let get_list (tree : Safe.t) : Safe.t list option =
  ( match tree with
  | `List s -> Some s
  | _ -> None
  )

(* Attempt to get a Json association list entry by key *)
let get_entry (key : string) (tree : Safe.t) : Safe.t option =
  ( match tree with
  | `Assoc kvs -> List.assoc_opt key kvs
  | _ -> None
  )

(* Read list of identifiers from Json *)
let get_idents (key : string) (transforms : Safe.t list) : AST.ident list =
  let nms = List.concat_map (fun json ->
      Option.bind (get_entry key json) (fun e ->
      Option.bind (get_list e) (fun es ->
      Some (List.filter_map get_string es)
      ))
      |> Option.value ~default:[]
    )
    transforms
  in
  (* The names could be either functions or variables/types
   * so treat them as both.
   *)
  let xs = List.map (fun f -> AST.FIdent (f, 0)) nms in
  let ys = List.map (fun f -> AST.Ident f) nms in
  xs @ ys

(****************************************************************
 * Callgraph surgery
 ****************************************************************)

(* Replace a function definition with a function declaration
 * (i.e., delete the function body) if it occurs in the list
 * of functions to be deleted.
 *)
let delete_function (discard : AST.ident list) (x : AST.declaration) =
  ( match x with
  | AST.Decl_FunDefn (f, ps, args, ty, b, loc) when List.mem f discard ->
    AST.Decl_FunType (f, ps, args, ty, loc)
  | AST.Decl_ProcDefn (f, ps, args, b, loc) when List.mem f discard ->
    AST.Decl_ProcType (f, ps, args, loc)
  | _ -> x
  )

let delete_functions (discard : AST.ident list) (ds : AST.declaration list) : AST.declaration list =
  List.map (delete_function discard) ds

(****************************************************************
 * Application
 ****************************************************************)

type backend = Backend_C | Backend_Verilog

let opt_filenames : string list ref = ref []
let opt_verbose = ref false
let opt_backend = ref Backend_Verilog
let transforms : Yojson.Safe.t list ref = ref []
let output_file : string ref = ref ""
let backend_pairs = [ ("c", Backend_C); ("verilog", Backend_Verilog) ]
let backend_symbols = List.map fst backend_pairs

let match_backend (symb : string) : unit =
  opt_backend := List.assoc symb backend_pairs

let options =
  Arg.align
    [
      ("-v", Arg.Set opt_verbose, "       Verbose output");
      ( "--backend",
        Arg.Symbol (backend_symbols, match_backend),
        "       Backend type (default: verilog)" );
      ( "--transforms",
        Arg.String (fun s ->
            let ops = Yojson.Safe.from_file s in
            transforms := !transforms @ [ops]),
        "       Apply transformations");
      ("-o", Arg.Set_string output_file, "       Output file");
      ("--no-unroll", Arg.Clear CP.unroll_loops, "       Do not unroll loops");
      ("--max-verilog-width", Arg.Int (fun i -> Backend_verilog.int_width := i), "       Maximum width used in Verilog");
    ]

let version = "ASL 0.2.0 alpha"
let usage_msg = version ^ "\nusage: asl2v <options> <file1> ... <fileN>\n"

let _ =
  Arg.parse options (fun s -> opt_filenames := !opt_filenames @ [ s ]) usage_msg

type 'a xform = 'a -> 'a

let  transform_count = ref 0

let transform
    (name : string)
    (f : AST.declaration list xform)
    (ds : AST.declaration list)
  : AST.declaration list =
  transform_count := !transform_count + 1;
  let filename = Printf.sprintf "tmp.%02d.%s.asl" !transform_count name in
  if !opt_verbose then Printf.printf "Applying transformation %02d.%s\n%!" !transform_count name;
  let ds' = f ds in
  if !opt_verbose then Utils.to_file filename (fun fmt -> ASL_FMT.declarations fmt ds');
  ds'

let main () =
  let paths = Option.value (Sys.getenv_opt "ASL_PATH") ~default:"." in
  let paths = String.split_on_char ':' paths in

  if !output_file = "" then
    failwith "Output file not specified (use -o foo to specify)";

  let imports = get_idents "imports" !transforms in
  let exports = get_idents "exports" !transforms in

  try
    let t = LoadASL.read_file paths "prelude.asl" true !opt_verbose in
    let ts = LoadASL.read_files paths !opt_filenames !opt_verbose in
    let ds = Global_checks.check_decls t @ ts
    |> transform "init0" Fun.id
    |> transform "keep_exports" (xform_reachable exports)
    |> transform "desugar" Xform_desugar.xform_decls
    |> transform "bittuples" Xform_bittuples.xform_decls
    |> transform "lower" Xform_lower.xform_decls
    |> transform "case" Xform_case.xform_decls in

    let genv = Eval.build_constant_environment ds in
    let ds = transform "constprop" (CP.xform_decls genv) ds
    |> transform "mono" Xform_mono.monomorphize
    |> transform "keep_mono" (xform_reachable exports)
    |> transform "mono2" Xform_mono.monomorphize
    |> transform "bitslices" Xform_bitslices.xform_decls
    |> transform "tuples" Xform_tuples.xform_decls
    |> transform "getset" Xform_getset.xform_decls
    |> transform "rmw" Xform_rmw.xform_decls
    |> transform "delete_imports" (delete_functions imports)
    |> transform "keep_exports2" (xform_reachable exports)
    in

    match !opt_backend with
    | Backend_C ->
        let sys_h_filenames = [ "stdbool.h" ] in
        let h_filenames = [ "asl/runtime.h" ] in

        let filename_t = !output_file ^ "_types.h" in
        emit_c_header filename_t sys_h_filenames h_filenames (fun fmt ->
            type_decls ds |> Asl_utils.topological_sort |> List.rev |> Backend_c.declarations fmt
        );
        let filename_e = !output_file ^ "_exceptions.h" in
        emit_c_header filename_e sys_h_filenames h_filenames (fun fmt ->
            Backend_c.exceptions fmt ds
        );
        let filename_v = !output_file ^ "_vars.h" in
        emit_c_header filename_v sys_h_filenames h_filenames (fun fmt ->
            Backend_c.declarations fmt (var_decls ds)
        );
        emit_c_source (!output_file ^ "_funs.c")
          ([ filename_t; filename_e; filename_v ] @ h_filenames)
          (fun fmt -> Backend_c.declarations fmt (fun_decls ds))
    | Backend_Verilog ->
        Utils.to_file !output_file (fun fmt ->
            Backend_verilog.declarations fmt (List.rev ds))
  with e -> Error.print_exception e; exit 1

let _ = ignore (main ())

(****************************************************************
 * End
 ****************************************************************)
