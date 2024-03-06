(lang dune 3.11)
(using dune_site 0.1)

(name asli)
(version 0.2.0)

; Copyright Arm Limited (c) 2017-2019
; Copyright (C) 2022-2024 Intel Corporation
; SPDX-Licence-Identifier: BSD-3-Clause

(using menhir 2.0)

(package
  (name asli)
  (synopsis "Interpreter for Arm's Architecture Specification Language (ASL)")
  (description "\| Interpreter for the language ASL that Arm uses to write their ISA specifications.
               "\|
               "\| Currently supports reading/typechecking the specification,
               "\| interactive execution of ASL statements and expressions,
               "\| executing opcodes one at a time,
               "\| loading ELF files and executing Arm binaries.
               "\|
               "\| Experimentally includes partial support for generating Verilog
               "\| that can be used as a reference model for hardware verification.
  )
  (sites (lib plugins)
         (lib runtime)
         (lib runtime_include)
         (share stdlib))
  (depends
    ("alcotest"  :with-test)
    "dune-site"
    "linenoise"
    ("menhir"    :build)
    ("ocaml"     (>= "4.14"))
    ("ocolor"    (>= "1.2.2"))
    "odoc"
    "yojson"
    ("z3"        (>= "4.8.7"))
    "zarith"
  )
)

(license BSD-3-Clause)
(authors "Alastair Reid")
(maintainers "Alastair Reid <alastair.d.reid@gmail.com>")
(source (github alastairreid/asl-interpreter))
(bug_reports "https://github.com/alastairreid/asl-interpreter/issues")
(homepage "https://github.com/alastairreid/asl-interpreter")
; (documentation ...)

(generate_opam_files true)
