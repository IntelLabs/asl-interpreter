---
title: Foreign function interface (planned extension)
---

At the moment, if there is any function that we want to implement in C, the
only way to do it is to add a new builtin (aka primop).  But, suppose what we
want to do is just call a function that is provided by a simulator such as
Simics? We don't want to add it as a new builtin because it is too simics
specific but we need to do something...

The way that Haskell, OCaml, Rust (and probably others) solve this is with a
foreign function interface where you writes something like the following

```
__foreign_import func SimicsTrace(s :: string);
```

Code generators might respond to this by compiling invocations of this function
just the same as for native functions and it is then up to the user to link the
generated code against the Simics library that it refers to.

Code generators might also use a different calling convention for foreign
calls.

And we would probably want to

- restrict the set of types that can be used as function arguments and results

- be very clear about how ownership changes on function call/return.

- whereas the C code generation currently mangles function names to things
  like `SimicsTrace_0`, this would need to be avoided on foreign imports. (The
  mangling doesn't actually happen in C code generation - but that is where it
  becomes most visible to users)

Note that I called it "foreign import" - there is also "foreign export" where
we make an ASL function available to be called from outside.
