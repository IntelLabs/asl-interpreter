---
layout: post
title: "Language extension: named parameters"
tags: extension
---

When writing a specification, we want to make it easy to
see at a glance what the specification means and to avoid
potential confusion in readers.
As we have been reviewing specifications, one of the problems
we often run into concerns boolean function arguments.

Quick, without looking up the answer, what does the 'TRUE'
argument mean in the following code

```asl
let result = Saturate(sum, 8, TRUE);
```

The reader might know or guess that this function saturates
an integer: converting it to a bitvector.
It is easy enough to guess that the bitvector size is 8.
And the reader might guess that the final argument
determines whether it is a signed saturation (clamping
values to be in the range -128 to +127) or an unsigned
saturation (clamping values to be in the range 0 to 255).
But does 'TRUE' mean signed or unsigned?

To deal with this kind of problem, we often write the above code like this

```asl
let is_unsigned = TRUE;
let result = Saturate(sum, 8, is_unsigned);
```

Now the code is clear but... it is a bit clunky.

## The solution: named arguments extension

A neater way of solving this problem is to support named arguments.
When calling a function, you can specify exactly which formal argument
a given argument matches:

```asl
let result = Saturate(sum, 8, is_unsigned=TRUE);
```

This makes it clear what the final argument is, and it lets the compiler check
that the name used actually matches the definition.

In principle, it also lets you specify the arguments in any order
that you want. For example, you could provide them in reverse order
by writing this

```asl
let result = Saturate(is_unsigned=TRUE, N=8, x=sum);
```

We recommend that you resist the urge to do this and that reviewers
should complain if the named argument order does not match
the formal argument order.
(And we are thinking of extending ASLi to give a warning if the order does not match.)
Using a different order each time you call a function has the
potential for confusion and has no obvious benefit.

## The default arguments extension

Another problem that we saw in our specification was that
some functions are normally used one way but very, very occasionally,
they are used in a different way.

An example is the 'PushStack' function.
In almost all cases, this takes an N-bit argument, it decrements the
stack pointer by 'N DIV 8' and then writes the N-bit argument to the
stack.
But, in a small number of cases, it decrements the stack by a different
amount --- leaving a hole in the stack.

We could handle this in two ways.

1. We could define two functions to handle the two cases --- but this duplication
   makes it challenging to check that the only difference is that the stack
   pointer is decremented by different amounts.

2. Alternatively, we could add an extra argument to the function specifying how
   much to decrement the stack pointer by. And then we have to add an extra
   'decrement_by' argument at every single call site. Apart from making most calls
   to 'PushStack' a bit longer and a bit more confusing,
   this makes it quite hard to find the special cases
   where the decrement amount is not just 'N DIV 8'
   and it makes it easy to accidentally pass the decrement amount.

As you might have guessed, the solution is to allow the definition of the
function to specify the default value of an argument if a call to the function
does not specify the amount.

```asl
func PushStack(value : bits(N), decrement_by : integer = N DIV 8)
begin
    assert decrement_by >= (N DIV 8);
    RSP = RSP - decrement_by;
    WriteMemory(RSP, value);
end
```

Now most of the specification can call the function with just one argument. For example,

```asl
PushStack(EAX);
```

which the typechecker expands to

```asl
PushStack(EAX, 32 DIV 8);
```

And any parts of the specification that need special treatment can
pass the additional argument either by writing

```asl
PushStack(EAX, 8); // push EAX in an 8-byte stack slot
```

or, it can be combined with named arguments like this

```asl
PushStack(EAX, decrement_by = 8);
```

Consistently using named arguments makes it easier to find the
unusual cases.

## Fine detail

In the above, we are using a combination of positional arguments (e.g., the first argument
to 'PushStack' and named arguments (e.g., the second argument to 'PushStack').
The way this works is hopefully not too surprising:

- All positional arguments must occur before the first named argument.
  That is, you cannot write 'PushStack(decrement_by=8, EAX)'.

- After matching the positional arguments against the first n formal arguments
  matching the named arguments against the remaining arguments
  we check that no formal argument matches multiple actual arguments
  and that every formal argument is either specified explicitly
  or has a default value expression.

- Default value expressions are allowed to refer to any argument that comes
  before it (i.e., further to the left). This makes it possible for the
  default value of 'decrement_by' to refer to the implicit type parameter
  'N' that is introduced in the type of 'value'.

## This is a breaking change

We think that this is a useful extension to ASL but, unfortunately, it comes at
a small price: a very small amount of existing ASL code no longer works.
(In our specification of the Intel Architecture, this affected two functions.)

Until now, ASL has allowed a very small amount of overloading:
if two functions have different numbers of arguments, then they are allowed
to have the same name.
When the typechecker sees a call to a function with this overloaded name, it
disambiguates the call by counting the number of arguments.

Obviously, this disambiguation rule is incompatible with the concept of default
arguments because calls to a function with default arguments could have
different numbers of explicit arguments.

You can probably imagine some ways that we could handle this. For example, we could
continue to allow overloading of functions that do not have default arguments.
However, we want the language to be simple and easy to understand. Specifically, if
you need to consult a "Language Lawyer" (i.e., an expert in the language) to understand
the specification, then we have failed in our primary design goal of making the
language understandable to readers who have only skimmed a few pages of description
of ASL --- and then forgot half of what they read!

Overall, we decided that it was better to simplify the language by dropping
overloading in order to enable the use of default arguments.

In any case, we were not sure that overloading with respect to the number of
arguments was a good idea. Imagine leafing through a 5,000 page PDF ISA
specification and coming across a call to a function "SaveState". You search
the rest of the PDF for a definition of "SaveState" and find a definition a few
hundred pages later. You read the function and form an impression of what the function
does but, unfortunately, you did not notice that you are reading a 3-argument version
of "SaveState" but the original call was to a 4-argument version of "SaveState" --- a
completely different function that is defined 1,000 pages later in the PDF.

In other words, we cannot (or, rather, choose not to) rely on the reader to
spot fine distinctions such as the number of arguments when they occur in large
specifications in large PDF documents.

## Design process

If you are curious about the design process, you can read the original descriptions
of [named arguments](https://github.com/IntelLabs/asl-interpreter/issues/29)
and [default arguments](https://github.com/IntelLabs/asl-interpreter/issues/68).
These differ slightly from the implementation that we describe above
but they add some more detail about the design choices and alternatives.
