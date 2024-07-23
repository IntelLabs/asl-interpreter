# ASL Interpreter

## Introduction

Example implementation of Arm's Architecture Specification Language (ASL).

The ASL interpreter is a collection of resources to help you to
understand and make use of Arm's architecture specifications.
It consists of lexer, parser, typechecker and interpreter for the ASL language
and an interactive interface for evaluating ASL statements and expressions.

## Requirements

To build and run the ASL interpreter, you will need:

  * OCaml version 4.14.1 (older versions may work, 5.x untested)
  * OPAM OCaml version 2.1.2 (other versions may work)
  * The following OPAM packages
      * ocaml     - OCaml compiler
      * ocolor    - adds color support to formatted output
      * odoc      - OCaml documentation generator (optional)
      * dune      - OCaml build system
      * dune-site - OCaml plugin support
      * menhir    - parser generator tool
      * linenoise - OCaml line editing library
      * yojson    - OCaml JSON library
      * z3        - OCaml bindings for the Z3 SMT solver
      * zarith    - OCaml multiprecision arithmetic library

To develop the ASL interpreter, you will also need the LLVM lit and filecheck testing tools
and googletest (git submodule dependency).

## License and contribution

The software is provided under the [BSD-3-Clause licence](https://spdx.org/licenses/BSD-3-Clause.html).
Contributions to this project are accepted under the same licence.

This software includes code from one other open source projects

 * The [CIL project](https://people.eecs.berkeley.edu/~necula/cil/)
   defines a useful
   [visitor class](https://github.com/cil-project/cil/blob/936b04103eb573f320c6badf280e8bb17f6e7b26/src/cil.ml#L931)
   for traversing C ASTs.
   The file `visitor.ml` is a modified copy of this class that generalizes
   the type to work with an arbitrary AST.

   CIL is distributed under a [BSD-3-Clause licence](https://github.com/cil-project/cil/blob/develop/LICENSE).


## Building and development

### Cloning the repository

Since the repository contains submodules, be sure to recursively clone it:

```
    git clone --recursive https://github.com/intel-innersource/libraries.isa.asl.asl-interpreter
```

### Directory structure

This interpreter consists of a single directory organized as follows

  * Metadata, documentation, etc:
      * `LICENCE`             - Software licence
      * `README.md`           - This file
      * `Makefile`            - build system file
  * Source code consisting of
      * Lexer
          * `libASL/lexer.mll`       - ASL lexer (ocamllex file)
          * `libASL/lexersupport.ml` - indentation-based parsing support
      * Grammar and Parser
          * `libASL/asl_visitor.ml`  - code to traverse abstract syntax tree
          * `libASL/asl_utils.ml`    - code to transform abstract syntax tree
      * Typechecker
          * `libASL/tcheck.ml`       - typechecker
      * Interpreter
          * `libASL/primops.ml`      - implementation of ASL builtin types and operations
          * `libASL/value.ml`        - interpreter support code
          * `libASL/eval.ml`         - evaluator for ASL language
      * ASL standard library
          * `libASL/prelude.asl`     - builtin types and functions
      * Programs
          * `bin/asli.ml`         - interactive ASL tool
          * `bin/asl2v.ml`        - Verilog generation tool
          * `bin/testlexer.ml`    - test program that converts ASL code to list of tokens
      * Misc
          * `libASL/utils.ml`        - utility code
  * Code copied from other open source projects
      * `libASL/visitor.ml`


### Installing dependencies

Platform specific instructions:
```
    MacOS:
        brew install opam
        brew install gmp mpir
    Ubuntu:
        sudo apt-get install opam
```

Platform independent instructions:

```
    opam install ocaml.4.14.1
    opam install . --yes --deps-only --with-doc --with-test

    # On OSX, you may need to use this command to install zarith
    env CFLAGS="-I$HOME/homebrew/include/" LDFLAGS="-L$HOME/homebrew/lib/" opam install zarith

    eval `opam config env`

    pip3 install lit filecheck # only needed for testing
```

You also need to execute this command

```
    MacOS: export DYLD_LIBRARY_PATH=`opam config var z3:lib`
    Linux: export LD_LIBRARY_PATH=`opam config var z3:lib`
```


### Building

To build the ASL lexer and ASL interpreter, execute this command.

```
    make install
```

If you get a lot of linker errors involving Z3, double-check that you installed
the right version.

### Editor plugins

For VIM:
You can copy `editors/asl.vim` to `~/.vim/syntax/asl.vim` and use `:set
syntax=asl` to enable syntax highlighting of ASL code.
And you can add the line `au BufRead,BufNewFile *.asl set filetype=asl` to `~/.vimrc` to
automatically use this for all asl files.


### Using ASL lexer

This displays a list of tokens in an ASL file.

```
    $ dune exec bin/testlexer.exe prelude.asl
```

### Using ASL interpreter

This reads ASL files specified on the command line and
provides an interactive environment for executing ASL
statements and expressions.

```
    $ asli
                _____  _       _
        /\     / ____|| |     (_)   ASL interpreter
       /  \   | (___  | |      _    Copyright Arm Limited (c) 2017-2019
      / /\ \   \___ \ | |     | |   Copyright (C) 2022-2024 Intel Corporation
     / ____ \  ____) || |____ | |
    /_/    \_\|_____/ |______||_|   Asli 0.2.0 alpha

    Type :? for help
    ASLi> 1+1
    2
    ASLi> ZeroExtend('11', 32)
    32'x3
    ASLi> let x : bits(32) = ZeroExtend('11', 32);
    ASLi> x
    32'x3
    ASLi> :quit
```

The ASL interpreter needs `prelude.asl` which is part of this repository. You
either run the ASL interpreter from a directory containing `prelude.asl` or run
the ASL interpreter from anywhere by setting `ASL_PATH` to point to a
directory containing `prelude.asl`.

Enjoy!
