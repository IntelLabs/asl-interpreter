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
  let ds = List.filter (fun x -> Option.value (Option.map (AST.Ident.matches f) (decl_name x)) ~default:false) ds in
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
  let prelude = load_test_libraries () in
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

  let isImpurePrim (v : AST.ident) : bool = List.exists (fun p -> AST.Ident.matches p v) Value.impure_prims in
  let impure = identify_impure_funs isConstant isImpurePrim ds in

  List.iter (fun f -> if in_identSet impure f then Alcotest.fail ("Function " ^ f ^ " incorrectly marked impure")) ex_pure;
  List.iter (fun f -> if not (in_identSet impure f) then Alcotest.fail ("Function " ^ f ^ " incorrectly marked pure")) ex_impure

let impure_function_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
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
 * Test topological sort function `reach`
 ****************************************************************)

let test_reach
    (check_order : bool)
    (graph : (string * string list) list)
    (roots : string list)
    (expected : string list)
    () : unit =
  let to_ident (x : string) : AST.ident = Ident x in
  let of_ident (x : AST.ident) : string = AST.pprint_ident x in

  (* generate dependencies of a node *)
  let next (x : AST.ident) : IdentSet.t =
    let ys = List.assoc (of_ident x) graph in
    List.map to_ident ys |> IdentSet.of_list
  in
  let result =
    List.map to_ident roots
    |> reach next
    |> List.map of_ident
  in

  if check_order then
    Alcotest.check (Alcotest.list Alcotest.string) "sorted output" expected result
  else
    Alcotest.check (Alcotest.slist Alcotest.string String.compare)
      "unsorted output" expected result

let toposort_tests : unit Alcotest.test_case list =
  (* example used by several tests *)
  let total_order =
    [ ("A", ["B"; "C"; "D"; "E"]);
      ("B", ["C"; "D"; "E"]);
      ("C", ["D"; "E"]);
      ("D", ["E"]);
      ("E", []);
    ]
  in
  let diamond =
    [ ("A", ["B"; "C"]);
      ("B", ["D"]);
      ("C", ["D"]);
      ("D", []);
    ]
  in
  let cycle =
    [ ("A", ["B"]);
      ("B", ["C"]);
      ("C", ["B"; "D"]);
      ("D", []);
    ]
  in
  [ ("linear1", `Quick, test_reach true total_order ["A"] ["A"; "B"; "C"; "D"; "E"]);
    ("linear2", `Quick, test_reach true total_order ["C"] ["C"; "D"; "E"]);
    ("linear3", `Quick, test_reach true total_order ["C"; "D"] ["C"; "D"; "E"]);
    ("linear4", `Quick, test_reach true total_order ["D"; "C"] ["C"; "D"; "E"]);

    ("diamond1", `Quick, test_reach true diamond ["B"] ["B"; "D"]);
    (* Note that ABCD would also be correct in the following tests *)
    ("diamond2", `Quick, test_reach true diamond ["A"] ["A"; "C"; "B"; "D"]);
    ("diamond3", `Quick, test_reach true diamond ["A"; "B"] ["A"; "C"; "B"; "D"]);
    ("diamond4", `Quick, test_reach true diamond ["B"; "A"] ["A"; "C"; "B"; "D"]);

    ("cycle1", `Quick, test_reach false cycle ["A"] ["A"; "B"; "C"; "D"]);
    ("cycle2", `Quick, test_reach false cycle ["B"] ["B"; "C"; "D"]);
    ("cycle3", `Quick, test_reach false cycle ["C"] ["C"; "B"; "D"]);
  ]

(****************************************************************
 * Test reachable_decls
 ****************************************************************)

let test_reachable_decls (globals : TC.GlobalEnv.t)
    (prelude : AST.declaration list) (decls : string) (roots : string list)
    (expected : string list) () : unit =
  let roots = List.map (fun f -> AST.FIdent (f, 0)) roots in
  let tcenv, ds = extend_tcenv globals decls in
  let ds = List.append prelude ds in
  let reachable : AST.declaration list = reachable_decls roots ds in
  let reachable : string list =
    List.map AST.pprint_ident (List.filter_map decl_name reachable)
  in

  Alcotest.(check (list string)) "sorted declarations" expected reachable

let reachable_decls_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  [
    ("diamond graph", `Quick, test_reachable_decls globals prelude
       "var X :: integer;
        func Read() => integer return X; end
        func Write(x :: integer) X = x; end
        func T() var x = Read(); Write(x); end"
       ["T"] ["T.0"; "Write.0"; "Read.0"; "X"]
    );
    ("lexpr write", `Quick, test_reachable_decls globals prelude
       "getter F => bits(1);
        setter F = value :: bits(1);
        func T() F = '0'; end"
       ["T"] ["T.0"; "F_write.0"]
    );
    ("lexpr read-write", `Quick, test_reachable_decls globals prelude
       "getter F => bits(1);
        setter F = x :: bits(1);
        getter G => bits(1);
        setter G = x :: bits(1);
        func T() [F, G] = '00'; end"
       ["T"] ["T.0"; "G_write.0"; "G_read.0"; "F_write.0"; "F_read.0"]
    );
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "asl_utils" [
    ("side_effects", side_effect_tests);
    ("impure_functions", impure_function_tests);
    ("topological_sort", toposort_tests);
    ("reachable_decls", reachable_decls_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
