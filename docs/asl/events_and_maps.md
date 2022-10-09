---
title: Events and maps (unofficial extension)
---

Events and maps are unofficial extensions of the ASL language.  They are a bit
of syntactic sugar that have been used in other ISA specs.

At present, it's not clear if events and maps are needed.

# Events

Events are a way of defining a procedure that does something like resetting the
processor.  This code mostly just assigns a value to every global variable.
But, awkwardly, the variables are scattered all over the specification while
the assignments all occur in just one place (the reset procedure).

An event lets you define a new event in one place and then in many other places
you can add extra statements to that event.  In particular, each time you
define a new register, you extend the event with initialization code for that
register.  Calling an event is just like calling a procedure: when it is
called, it executes all the statements in some unspecified order.

Although I used reset as an example, there are often multiple events. In
particular cold reset (power on reset), warm reset, various sleep/wakeup
events, etc. so a hardcoded "reset" feature would not be sufficient.

Events use the keywords `__newevent`, `__event`, `__endevent`.

# Maps

Maps have a similar motivation and also allow you to scatter the full
definition of a function (or procedure) across the entire spec.  Maps generate
case statements.

Each map entry defines a pattern and some side conditions and an action to
perform.  Executing a map selects a map entry that matches the values of the
inputs and satisfies the side conditions and executes the action.

Maps can be used for things like address decoding, MSR decoding, etc.   (They
were not quite powerful enough to express Arm instruction decoding - so
probably not powerful enough for x86 either.)

Maps use the keywords `__map`, `__newmap`.

# Do we need events and maps?

The reason that we may not want them is that, in practice, we tend to build
large databases of registers that define their addresses, what modes can access
them, their cold and warm reset behaviour, etc. and it is easy to generate a
single procedure from them.

That is, we can achieve the same effect in the scripts that turn the databases
into ASL code.  So they are probably not as important as they seemed to be when
I added them to ASL.  But... although their major use case is not as important
as it was when they were added to ASL, they are still sometimes useful so
it is not clear yet whether they should be made an official part of ASL.

The obvious problem with both maps and events is that things execute in an
arbitrary order so overlapping patterns or conflicting assignments are bad.

In theory we can check for overlaps. In practice, it was not a problem.  But...
as you get a bigger team working on specs, it is yet another thing that they
need to be told and have to be careful about - so not ideal having them without
having checks.


# Implementation details

Events and maps are currently implemented in ASLi's frontend and in the interpreter.

The easiest way to implement them in backends is: just after typechecking, a
lowering pass gathers all the parts of an event or a map and transforms it into
a procedure/function.

