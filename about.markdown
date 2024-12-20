---
layout: page
title: About
permalink: /about/
---

Architecture Specification Language (ASL) is an executable language for writing
clear, precise specifications of Instruction Set Architectures (ISAs).

The ASL interpreter (ASLi) is an implementation of ASL that can execute ASL
specifications either in an interpreter or by compiling via C code.  We include
a small demonstration of how to use ASLi to build simulators for a toy
architecture specification.

This tool is based on Arm's open source
[asl-interpreter](https://github.com/ARM-software/asl-interpreter) release with
extensive modifications to

* Change ASLi to support most of ASL version 1.0 requiring significant
  changes to the AST, lexer, parser and typechecker.
* Cleanup the "CPU" API that ASLi expects an ISA to implement.
* Add support for compiling ASL specifications to C by adding
  many transformations and multiple runtimes.
* Add a demo ISA to illustrate how to generate simulators from
  an ASL specification.
* Split libASL out from ASLi to make it easier to reuse
  parts of ASLi in other tools.
* Add support for loading ELF files and executing binaries.
