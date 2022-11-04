(****************************************************************
 * Test ASL utils module
 *
 * Copyright Intel Inc (c) 2022
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module AST = Asl_ast
module TC = Tcheck

(****************************************************************
 * Test functions
 ****************************************************************)

let format_identSet (fmt : Format.formatter) (s : IdentSet.t) : unit =
    Format.fprintf fmt "{ ";
    IdentSet.iter (fun f -> Format.fprintf fmt "%a" Asl_fmt.varname f) s;
    Format.fprintf fmt "}"

let identSet = Alcotest.testable format_identSet IdentSet.equal

let varNames_to_identSet (vs : string list) : IdentSet.t =
  IdentSet.of_list (List.map (fun f -> AST.Ident f) vs)

let funNames_to_identSet (fs : string list) : IdentSet.t =
  IdentSet.of_list (List.map (fun f -> AST.FIdent (f, 0)) fs)

let in_identSet (s : IdentSet.t) (f : string) : bool =
  IdentSet.mem (FIdent (f, 0)) s

(****************************************************************
 * Test side_effects_of_decl
 ****************************************************************)

(* Test that side_effects_of_decl returns exactly the correct result for
 * a function
 *)
let test_side_effects (globals : TC.GlobalEnv.t) (prelude : AST.declaration list)
    (decls : string) (f : string)
    (expected : (string list * string list * string list * bool))
    () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let ds = List.append prelude ds in
  (* to find the definition called 'f', we extract all the declarations called 'f'
   * and take the last element (because any function prototype will be listed first)
   *)
  let ds = List.filter (fun x -> Option.value (Option.map (AST.Id.matches f) (decl_name x)) ~default:false) ds in
  let d = ( match List.rev ds with
          | (d::_) -> d
          | []     -> Alcotest.fail ("Can't find declaration " ^ f)
          )
  in

  let (ex_reads, ex_writes, ex_callees, ex_throws) = expected in
  let (reads, writes, callees, throws) = side_effects_of_decl d in

  Alcotest.check identSet "read_set"    (varNames_to_identSet ex_reads) reads;
  Alcotest.check identSet "write_set"   (varNames_to_identSet ex_writes) writes;
  Alcotest.check identSet "callees"     (funNames_to_identSet ex_callees) callees;
  Alcotest.check Alcotest.bool "throws" ex_throws throws

let side_effect_tests : unit Alcotest.test_case list =
  let paths = [ "../../.." ] in
  let prelude = LoadASL.read_file paths "prelude.asl" true false in
  let globals = TC.env0 in
  [
    ("empty function", `Quick, test_side_effects globals prelude
       "func T() return; end" "T" ([], [], [], false));
    ("identity function", `Quick, test_side_effects globals prelude
       "func T(x :: integer) => integer return x; end" "T" ([], [], [], false));
    ("length function", `Quick, test_side_effects globals prelude
       "func T(x :: bits(N)) => integer return N; end" "T" ([], [], [], false));
    ("increment function", `Quick, test_side_effects globals prelude
       "func T(x :: integer) => integer return x + 1; end" "T" ([], [], ["add_int"], false));
    ("destructive increment function", `Quick, test_side_effects globals prelude
       "func T(x :: integer) => integer x = x + 1; return x; end" "T" ([], [], ["add_int"], false));
    ("global read function", `Quick, test_side_effects globals prelude
       "var X :: integer; func T() => integer return X; end" "T" (["X"], [], [], false));
    ("global write function", `Quick, test_side_effects globals prelude
       "var X :: integer; func T() X = 1; return; end" "T" ([], ["X"], [], false));
  ]

(****************************************************************
 * Test identify_impure_funs
 ****************************************************************)

(* Test that identify_impure_funs is correctly classifying functions
 * as pure or impure.
 *)
let test_impure_functions (globals : TC.GlobalEnv.t) (prelude : AST.declaration list)
    (decls : string) (ex_pure : string list) (ex_impure : string list) () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let ds = List.append prelude ds in

  (* for testing purposes, we treat any variable whose name starts with K as a constant *)
  let isConstant (v : AST.ident) : bool = String.get (AST.pprint_ident v) 0 = 'K' in

  let isImpurePrim (v : AST.ident) : bool = List.exists (fun p -> AST.Id.matches p v) Value.impure_prims in
  let impure = identify_impure_funs isConstant isImpurePrim ds in

  List.iter (fun f -> if in_identSet impure f then Alcotest.fail ("Function " ^ f ^ " incorrectly marked impure")) ex_pure;
  List.iter (fun f -> if not (in_identSet impure f) then Alcotest.fail ("Function " ^ f ^ " incorrectly marked pure")) ex_impure

let impure_function_tests : unit Alcotest.test_case list =
  let paths = [ "../../.." ] in
  let prelude = LoadASL.read_file paths "prelude.asl" true false in
  let globals = TC.env0 in
  [
    ("prelude functions", `Quick, test_impure_functions globals prelude
       ""
       [ "UInt"; "SInt"; "Align"; "Min"; "Max"; "Abs";
         "SignedSat"; "UnsignedSat"; "BitCount"; "LowestSetBit"; "HighestSetBit";
         "add_int"; "add_real"; "add_bits"; "add_bits_int";
       ]
       [ "ram_init"; "ram_read"; "ram_write"; "__InitRAM"; "__ReadRAM"; "__WriteRAM";
       ]
    );
    ("user-defined functions", `Quick, test_impure_functions globals prelude
       "
       func Null() return; end
       func Id(x :: integer) => integer return x; end
       func Len2(x :: bits(N)) => integer return N; end
       func Inc(x :: integer) => integer return x + 1; end
       func Inc2(x :: integer) => integer x = x + 1; return x; end
       let K42 :: integer = 42;
       var X :: integer;
       func ReadConst() => integer return K42; end
       func Read() => integer return X; end
       func Write() X = 1; return; end
       func IndirectRead() => integer return Read(); end
       func IndirectWrite() Write(); end
       "
       [ "Null"; "Id"; "Len2"; "Inc"; "Inc2"; "ReadConst" ]
       [ "Read"; "Write";
         "IndirectRead"; "IndirectWrite" ]
    );
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "asl_utils" [
    ("side_effects", side_effect_tests);
    ("impure_functions", impure_function_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
