(lang dune 2.5)
(name asli)
(version 0.2.0)

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
  )
  (depends
    ("ocaml"     (>= "4.09"))
    ("menhir"    :build)
    ("ott"       :build)
    "linenoise"
    ("ocolor"    (>= "1.2.2"))
    "zarith"
    ("z3"        (>= "4.8.7"))
    ("alcotest"  :with-test)
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
