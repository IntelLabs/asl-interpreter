---
title: Steps towards a module system for ASL
---

At the moment, ASLi supports a "spec file" which is just a file containing a list of the files that make up
a specification.
The ASL standard doesn't require them and they are not used at the moment.

I expect these to evolve into something that we want to add to the ASL language.

1. At the moment, they could be used as a form of library mechanism so that, for
   example, the floating point spec can be maintained semi-separately from the
   rest of the spec.

   (This would be similar in power to the C "module system". That is, it would
   provide no information hiding and loading all the files in the spec files
   would be roughly equivalent to linking.)

2. This could then evolve into a weak module system by extending the list of
   files with a list of all the imports and exports from a module so that
   we have a limited form of information hiding.

   Optionally, we would not just list the imports and exports but we would
   also write function signatures for the imports and exports so that
   some degree of separate compilation is possible.

3. Finally we could add a mechanism to explicitly construct a larger
   system out of modules. This requires

   - a syntax for composing / connecting modules 

   - treat ASL configuration variables such as the maximum physical
     address size as parameters of the modules

   - the ability to make multiple instances of each module so that we can do
     things like instantiate 2 x86 specs plus a memory system spec plus a
     collection of I/O devices and build a platform spec.

So the "spec file" is just a very, very small step on that path.

Notes:

1. Simics already has lots of those I/O devices and a way to combine them into
   a platform... so the module system should be designed and implemented in a
   way that those device emulators can be used in that combination.

2. Verilog has a way to instantiate Verilog modules and combine those into a
   platform... so the module system also needs to be a good match to that.

3. Instead of generating "C" code, we might want to generate C++ code and
   convert modules into classes.
(
