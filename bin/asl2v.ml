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
  Utils.flatmap_option mk_type_decl xs

let rec var_decls (xs : AST.declaration list) : AST.declaration list =
  let is_var_decl (x : AST.declaration) : bool =
    ( match x with
    | Decl_Var _
      -> true

    | Decl_Const _
    | Decl_Enum _
    | Decl_Record _
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

(* Generate code for declarations *)
let emit_c_code (filename : string) (ds : AST.declaration list) : unit =
  Utils.to_file filename (fun fmt ->
    Backend_c.declarations fmt ds
  )

(****************************************************************
 * Application
 ****************************************************************)

type backend = Backend_C | Backend_Verilog

let opt_filenames : string list ref = ref []
let opt_verbose = ref false
let opt_backend = ref Backend_Verilog
let roots : string list ref = ref []
let keeps : string list ref = ref []
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
      ( "--root",
        Arg.String (fun s -> roots := !roots @ [ s ]),
        "       Add function to convert" );
      ( "--keep",
        Arg.String (fun s -> keeps := !keeps @ [ s ]),
        "       Add variable to keep" );
      ("-o", Arg.Set_string output_file, "       Output file");
      ("--no-unroll", Arg.Clear CP.unroll_loops, "       Do not unroll loops");
    ]

let version = "ASL 0.2.0 alpha"
let usage_msg = version ^ "\nusage: asl2v <options> <file1> ... <fileN>\n"

let _ =
  Arg.parse options (fun s -> opt_filenames := !opt_filenames @ [ s ]) usage_msg

let main () =
  let paths = Option.value (Sys.getenv_opt "ASL_PATH") ~default:"." in
  let paths = String.split_on_char ':' paths in

  try
    let t = LoadASL.read_file paths "prelude.asl" true !opt_verbose in
    let ts = LoadASL.read_files paths !opt_filenames !opt_verbose in
    let ds = t @ ts in
    let roots = List.map (fun f -> AST.FIdent (f, 0)) !roots in
    let keeps = List.map (fun r -> AST.Ident r) !keeps in

    if true then
      Utils.to_file "tmp.00.init0.asl" (fun fmt -> ASL_FMT.declarations fmt ds);
    let ds = Asl_utils.reachable_decls (List.append roots keeps) ds in
    if ds = [] then failwith "Couldn't find any roots";

    if !output_file = "" then
      failwith "Output file not specified (use -o foo.v to specify)";

    if true then
      Utils.to_file "tmp.10.init.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Xform_bittuples.xform_decls ds in
    if true then
      Utils.to_file "tmp.15.bittuples.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Xform_case.xform_decls ds in
    if true then
      Utils.to_file "tmp.17.case.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let genv = Eval.build_constant_environment ds in
    let ds = CP.xform_decls genv ds in

    if true then
      Utils.to_file "tmp.20.constprop.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Xform_mono.monomorphize ds in
    if true then
      Utils.to_file "tmp.30.mono.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Asl_utils.reachable_decls (List.append roots keeps) ds in
    if ds = [] then failwith "Couldn't find any roots";

    let ds = Xform_mono.monomorphize ds in
    if true then
      Utils.to_file "tmp.40.mono2.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Xform_tuples.xform_decls ds in
    if true then
      Utils.to_file "tmp.45.tuples.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Xform_getset.xform_decls ds in
    if true then
      Utils.to_file "tmp.50.getset.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    let ds = Xform_rmw.xform_decls ds in
    if true then
      Utils.to_file "tmp.60.rmw.asl" (fun fmt -> ASL_FMT.declarations fmt ds);

    Utils.to_file !output_file (fun fmt ->
        match !opt_backend with
        | Backend_C ->
            type_decls ds |> Asl_utils.topological_sort |> emit_c_code (!output_file ^ "_types.h");
            var_decls ds |> emit_c_code (!output_file ^ "_vars.h");
            fun_decls ds |> emit_c_code (!output_file ^ "_funs.c")
        | Backend_Verilog ->
            Format.pp_print_string fmt "/* verilator lint_off WIDTH */";
            Format.pp_print_cut fmt ();
            Format.pp_print_string fmt "/* verilator lint_off UNPACKED */";
            Format.pp_print_cut fmt ();
            Backend_verilog.declarations fmt ds)
  with e -> Error.print_exception e; exit 1

let _ = ignore (main ())

(****************************************************************
 * End
 ****************************************************************)
