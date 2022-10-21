---
title: OCaml tips and tricks
---

# Gotchas

OCaml is mostly a reliable programming language with little surprising behavior.
But, there are a few gotchas

## Pointer equality

OCaml has two forms of equality/inequality tests:

- "="/"<>" (structural equality)
  Recursively compares two data structures.

  99.9% of the time, this is the correct equality test to use.

- "=="/"!=" (pointer equality)
  Checks whether two objects are the same object.

  This is useful for a small number of performance optimizations
  but can lead to surprising, brittle results if accidentally used.

Unfortunately, most other languages use "==" for equality
so we will accidentally use "==" when we really mean "=".

### Correct uses of pointer equality

There are only two places that use pointer equality in the codebase at the moment.

1) The visitor pattern uses a pointer equality check as an optimization
   to avoid building a new AST node if all fields of the node are
   equal. Using pointer equality makes this efficient whereas using
   structural equality would impose a quadratic cost because of repeatedly
   revisiting each node during the recursive tree walk.

2) The subexpression visitor class uses a pointer equality check to
   detect whether it is visiting the root node.
   Again, use of structural equality would impose a quadratic cost.

### Protecting against errors

We can protect against accidentally using pointer equality instead of
structural equality by periodically scanning the codebase for use of `==`.

The following checks should only find code that is obviously part of comments.

```bash
git grep '==' | grep -v '"=="' | grep -v '" == "' | grep '\.ml:' | grep -v visitor
git grep '!=' | grep -v '"!="' | grep -v '" != "' | grep '\.ml:' | grep -v visitor
```

If this finds new uses of pointer equality, they should either be changed to structural
equality or a comment added to explain why pointer equality is correct and
important to use, it should be added to the above list of [correct uses of pointer equality]
and it should be carefully reviewed.

# Debugging tips

## Stack traces

To run with stack traces, set the environment variable `OCAMLRUNPARAM=b`.
(We have debugging enabled by default so this is enough to get stack dumps)

## Stack overflow

When a stack overflow occurs, you do not get a stack trace (at least, you don't
get one on WSL).  To get a stack dump on overflow, build with the bytecode
compiler instead of the native compiler.

e.g., to diagnose a problem in bin/asl2v.exe:

1. In bin/dune, change `(modes exe)` to `(modes exe byte)` for asl2v
2. Run `dune build bin/asl2v.bc`    The critical part here is the suffix ".bc"
3. Request a backtrace with `export OCAMLPARAM=b`
4. Run `$ASLI_DIR/_build/default/bin/asl2v.bc`  (Again, note the change of suffix)

# Project management

## Installing ASLi locally using OPAM

Installing ASLi in a central location could be useful for teams that are
using ASLi but not developing it.

There are three ways to install ASLi in a central location for a team to use.

1. Just copy over the .exe and prelude.asl files to your install directory.
   This is not obviously wrong

2. Push a new release of ASLi to opam.

   This is fine for occasional releases that we want to make public - but
   probably not right if we want to do a new release every work week and if
   there is anything not appropriate for public release or if the public
   release process needs much review.

3. Use opam to install ASli locally.

   This puts asli.exe and the prelude.asl file somewhere in `$OPAMROOT` - the
   same place that `opam env` puts on your PATH

   (But note that this is about how to install it for others to use - if you
   are an ASLi developer, you should probably continue using `dune build`,
   `dune exec` or `_build/default/bin/asli.exe`)

   To install ASLi locally, what you need to do is

   - Bump the version number in bin/asli.ml, bin/asl2v.ml and dune-project.
     (This is optional - but you probably want to do it)
   - Run dune-build (this causes the version number in asli.opam to be updated)
   - `git checkout -b opam_test`    # branch name can be anything you like the critical thing is that it has to be a branch of its own
   - `git commit -a -m`bump version number``
   - `opam pin add .`


   After doing this `which asli` and `asli --version` show that it is installed.

   I don't fully understand why I needed to create a new branch to be allowed
   to install it locally - but it seemed unwilling to do it otherwise.  If you
   don't want to bump the version number, you may have to `opam remove asli` if
   you have previously installed it.
