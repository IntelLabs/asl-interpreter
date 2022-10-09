---
title: "Constants: evaluation phases in ASL"
---

The high level intuition for the different types of constants is:

- Some global variables/constants are 100% fixed (can only be changed by
  editing the code) and we want the compiler to be able to optimize based on
  them (eg to do dead code elimination).

- Some global variables/constants are varied at "link time". eg if we had a
  module system, we might have two instances of the IA spec in a single
  platform: one instance configured like a Xeon-like core, the other instance
  configured for an Atom-like core. After link time, they cannot be changed.
  (This case may not exist in ASL 1.0 ???)

- Some global variables/constants can be varied from one run of the simulator
  to another but they remain fixed during any individual execution of the
  simulator. eg we might want to be able to change the maximum physical address
  from one run of the simulator to another without having to recompile or
  relink. (And we don't want to have to build a separate version for every
  possible configuration because the configuration space is too large)

- Local variables/constants might have the same value throughout the entire
  lifetime of a function call - but each function call could give it a
  different value

- Some variables have different values within a single function invocation. eg
  loop counters, etc.

In other words, the code has many different "phases" and different types of
constant are associated with each of these phases.
