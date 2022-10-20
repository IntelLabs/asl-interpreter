---
title: "ASLi project documentation"
---

- Internal implementation details

  - [Visitor class](internal/visitor.md): one of the most important design patterns in the
    ASLi codebase.

  - Coding guidelines

  - [OCaml coding tips](ocaml/tips.md): OCaml tips and tricks

- ASL language discussion

  - [Typechecking and synthesis](asl/typechecking.md): how we can specify the ASL
    typesystem and how that relates to the current implementation.

- Unofficial ASL extensions

  - [Foreign function interface](asl/ffi.md): how we connect generated C or Verilog code to
    external codebases such as simulators and formal verification harnesses.
    (Not implemented yet)

  - [Events and maps](asl/events_and_maps.md): a "hook" mechanism that makes it easier to
    write reset code and register access code.
    (Implemented but not currently used and may be removed)

  - [Modules and spec files](asl/modules.md): how we expect to evolve ASL to having a module
    system.

  - [Subtypes in ASL](asl/subtypes.md): use of subtypes in ASL.
    (Also describes two different ways of handling MSRs in IA-Spec.)

  - [Constants: evaluation phases in ASL](asl/constants.md): the multiple
    different flavours of constants in ASL code.

- Intel specific documentation

  (These are kept separate to make it easy to not publish this section)
