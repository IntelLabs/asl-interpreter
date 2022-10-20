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

