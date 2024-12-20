---
layout: page
title: Writing ASL
---

*[Note: This document should be read in conjunction with the document [Reading ASL specifications]({{site.baseurl}}/asl_reading.html).
We expect that the vast majority of ASL users only read ASL and do not need to read this document.]*

Architecture Specification Language (ASL) is a specification language
that is designed to make it easy to write clear, easily understood
ISA specifications.
To achieve this goal, ASL is different from many programming languages
in the following ways

- Support for bitvectors of arbitrary size (e.g., 32, 64, 5, 12, etc.).

- Support for bitslice operations that manipulate part of a bitvector.

- Support for other common bitvector operations: concatenating 
  bitvectors to form a longer bitvector,
  replicating a bitvector to form a longer bitvector,
  zero-extension, sign-extension.

- A single integer type `integer` that does not overflow or wrap around.
  Variables of this type are "as big as they need to be" to represent
  the value that they contain.

- Encouraging a "single assignment" style of programming where most
  variables have just one assignment to that variable.

- Not supporting silent default behavior on case statements.
  Unlike C, it is an error if no branch of a case-statement matches.

- Static detection of confusing, subtle, dangerous, or incorrect programs
  including

  - Rejecting expressions where the evaluation order can affect behavior.

  - Requiring that two bitvectors have the same length if they are
    added, subtracted, ANDed, ORed, etc.

  - Making the silent branches associated with exception propagation
    explicit by requiring that calls to functions that can throw
    exceptions look different from calls to functions that cannot throw
    exceptions.

    Marking calls to functions that could throw exceptions is important
    in code that changes state (e.g., changes a register or pushes a
    value onto the stack) and also calls a function that could throw
    an exception because the order in which those happen matters.

  The goal of these restrictions is to make the language easier for readers
  because they will never have to deal with programs where
  evaluation order matters, they will never have to understand
  type promotion rules that determine what happens when a 'char' is
  added to a 'long int', etc.

  This is important because specifications differ from programs in
  that they have orders of magnitude more readers than writers and the
  readers tend to be focused on understanding some small corner of
  the architecture and cannot be expected to understand all of the
  intricacies of the architecture (such as which functions throw exceptions).

These rules and restrictions are intended to make it easier to read the code
and understand it correctly.
If you want to use the specification for formal verification or you need
the additional performance of compiling the ASL specification
to C/C++ or Verilog, then there are some additional restrictions because
the ASL compiler does not support all the features that are supported
by the ASL interpreter.
This guide explains both types of restriction and how to work with them.

## Type checking

### Type conversions

ASL is strongly typed (that is, every expression has a type and
mismatched types are not allowed).

Most significantly, ASL does not have anything like the C/C++ rules
for type promotion and therefore does not allow things like

- "truthiness": treating zero values as FALSE and non-zero values as TRUE
- mismatched sizes: adding, comparing, etc. bitvectors of different lengths
- mismatched types: mixing bitvectors and integers
  (There are a few exceptions for the common case of expressions
  like `RSP + 8`.)

When writing ASL code, it is necessary to explicitly convert values from one
type to another. The common conversions are as follows

- integer to bitvector: the bitslice operation written `i[0 +: N]` converts an integer `i` to `bits(N)`
- bitvector to integer (unsigned): `UInt(b)` converts a bitvector to an integer interpreting the bits as unsigned.
- bitvector to integer (signed): `SInt(b)` converts a bitvector to an integer interpreting the bits as signed.
- from bit `x` to boolean (positive logic): `x == '1'`
- from bit `x` to boolean (negative logic): `x == '0'`
- from boolean `b` to bit (positive logic): `if x then '1' else '0'`
- from boolean `b` to bit (negative logic): `if x then '0' else '1'`

### Sized bitvectors

The bitvector type includes an explicit size. For example `bits(8)` is the
type of 8-bit values.
The ASL compiler checks the length of bitvectors at compile time and rejects
code with mismatched lengths.
For example, the following assignment of a 4-bit value to an 8-bit variable
is incorrect: the architect needs to explicitly indicate what should happen
to the other 4 bits.

``` asl
var x : bits(8);  // declaration of a variable
var y : bits(4);  // declaration of a different size of variable
...
x = y; // INCORRECT: assigning a 4-bit value to x
```

Some of the common ways of correcting bitsize mismatches like this are

- zero-extend the value on the right hand side: `x = ZeroExtend(y, 8);`
- sign-extend the value on the right hand side: `x = SignExtend(y, 8);`
- assign to a part of the left-hand side using a bitslice assignment:
  `x[3:0] = y;`

These three options behave very differently so it is important to be
explicit about which one is intended.

### Variable sized bitvectors

The size of a bitvector does not need to be a literal constant.
It can be useful for the size to be a variable or expression
containing a variable.
For example, in the following function, the variables `src1`, `src2`
and `result` are all of width `N` and `N` can be any non-negative value.

``` asl
func ADD(src1 : bits(N), src2 : bits(N)) => bits(N)
begin
    let result = src1 + src2;
    return result;
end
```

The ASL compiler reasons about bitwidth expressions to check that bitvectors
have matching sizes.

These checks happen at compile time, not when the code is executed and
it is possible that the ASL compiler will reject some code that would not
have hit a problem at runtime.  Any such issues have to be fixed
before the compiler will accept the code.

The three main places where you might encounter problems are

1. A function is only called with 'safe' values but there would be problems
   if it was called with other inputs.
   For example, if we change the function `ADD` above to take inputs of
   different sizes, then, the ASL compiler will report an error because,
   even though the programmer knows the sizes will always match, the
   compiler does not know that.

    ``` asl
    func ADD(src1 : bits(M), src2 : bits(N)) => bits(N)
    begin
        let result = src1 + src2; // error: mismatched sizes
        return result;
    end
    ```

   [Actual examples tend to be more subtle than this example.
   For example, they might depend on an assumption that `M >= N`.]

2. An expression is not always safe but it is impossible to reach
   that expression with unsafe values because it is inside
   an if-statement or case-statement or it occurs after an
   assert-statement.
   For example, the ADD function could try to cope with mismatched
   sizes as follows

    ``` asl
    func ADD(src1 : bits(M), src2 : bits(N)) => bits(N)
    begin
        if M == N then
            let result = src1 + src2; // error: mismatched sizes
            return result;
        else
            return Zeros(N);
        end
    end
    ```

   Even though this code would not have mismatched sizes at runtime,
   the compiler is not able to check that and the code is rejected.

   A possible fix would be to apply a size-conversion (zero-extend, sign-extend
   or bitslice) even though it will have no effect.  For example,

   ``` asl
   let result = ZeroExtend(src1, N) + src2;
   ```

   Doing this is not ideal because, even though the code means the same,
   it adds clutter into the specification.
   Reviewers should try to suggest ways to avoid such artifacts.

3. The expression depends on the relationship between some variables.

   For example, in floating point code, we might calculate the
   size of the exponent (E) and mantissa (M) like this

   ``` asl
   let E = if N == 32 then 8 else 11;
   let M = N - (E + 1);
   ```

   And then, further down the function, we might concatenate the sign bit,
   the exponent (of type `bits(E)`,
   and the mantissa (of type `bits(M)`)
   to produce the final floating point result like this

   ``` asl
   let result : bits(N) = [sign, exponent, mantissa];
   ```

   To confirm that this is correct, the ASL compiler has to
   check that `N == 1 + E + M`.
   To do this, it needs to use the fact that `M == N - (E + 1)`
   which comes from the second assignment statement above.

   To help the ASL compiler recognize that it should use this fact,
   we change the second assignment statement as follows

   ``` asl
   constant M = N - (E + 1);
   ```

   (It is very, very rare that this needs to be done. We have
   only ever seen it in floating point specifications.)

### Constraint checks

Integer variables can be "constrained" by specifying a set of legal values
or a range of legal values.
For example, `integer {8, 16, 32, 64}` or `integer {0..255}`.
(It is also possible to combine the two in a type like `integer {0, 3, 5..9}`
but we have not found a place where that is useful.)

These constraints are checked by ASLi, they can improve readability of the
code, and they can help the ASLi compiler generate better C/C++/Verilog code.

Most constraints are used on function arguments, function results and on
mutable variables.

#### Adding constraints on function results

It is often useful to add constraints on the results of functions
that return integers because many of these integers will be used
as bitwidths, as array indexes, as bit indexes, etc. and it is
useful to the reader and to the compiler to know that it is in range.

For example, suppose there is a function that calculates the
size of floating point values, we can add the set of possible
results as a constraint on the result type.

``` asl
func DecodeFPSize(instruction : bits(32)) => integer {32, 64}
```

Adding constraints like this is almost always a good idea
because it provides information that is needed for checking
other constraints.


#### Adding constraints on function arguments

Many constraints are applied to size parameters. For example, we might
want to constrain a floating point addition operation `FPAdd` to
require a bitwidth of 32 or 64.

If the bitwidth is an explicit argument of FPAdd, this is done by adding
the constraint as port of the function type like this

``` asl
func FPAdd(x : bits(N), y : bits(N), N : integer {32, 64}) => bits(N)
```

if, instead, the bitwidth is an implicit parameter  of the function
(i.e., it is not listed as an argument), then it is necessary to 
add it as a parameter like this

``` asl
func FPAdd{N : {32, 64}}(x : bits(N), y : bits(N)) => bits(N)
```

When first adding constraints to a specification, you might generate a lot of
error messages because adding a constraint to a function argument will require
that the ASL compiler can show that every call to that function satisfies that
constraint.
It is common that resolving one constraint error
requires the addition of a constraint to some of the
callers of the constrained function.
This has a reverse cascade effect: you need to keep adding constraints all
the way up the call tree until you reach the point where the constrained
value originates.

It is often easier to start at the top of the call tree and work your way down
the tree to the leaves (where the constraints are needed).

#### Adding constraints on immutable variables

If you have added constraints on function arguments and parameters, then
the ASL compiler will infer most of the constraints on immutable variables
(i.e., variables declared with the `let` keyword) and there is
usually no need to explicitly add a constraint.

For example, given that `x : integer {0..15}` and a let-assignment
`let y = x * 2 + 3;`, the ASL compiler infers that `y : integer {3..33}`.

This inference usually works but will fail if you assign the result of a
user-defined function that has an unconstrained result type or if the
expression is especially complicated.

Note that if you add an explicit type to a variable declaration, then
the variable is unconstrained *even if ASL was able to infer a constraint*.
For example, if you write

``` asl
let y : integer = x * 2 + 3;
```

then the addition of ": integer" disables inference of a constraint on "y".

#### Adding constraints on mutable variables

It is usually possible to infer the constraint on an immutable integer variable
because it has a single assignment.
It is usually not possible to infer a useful constraint on a mutable integer variable because
that would require examination of all assignments to that variable.

In particular, given this statement

``` asl
var count = 0;
```

we would not want ASL to infer that `count` has the constraint `{0}` since that
would prevent us from setting count to any other value --- probably not what is
intended.
Instead, we should write something like this

``` asl
var count : integer {0..N};
```

A further complication with using mutable variables is that they are commonly
used inside a loop. For example

``` asl
var count : integer {0..N} = 0;
for i = 0 to N-1 do
    if x[i] == '1' then
        count = count + 1; // error reported on this line
    end
end
```

In code like this, ASL will report an error on the line that increments `count`
because it cannot see that count will always be in the range 0..N.

In this case, you need to add a "checked type conversion".

``` asl
count = (count + 1) as {0..N};
```

The notation `... as {0..N}`
acts like a "type cast" in C/C++ except that it inserts a runtime
check that checks that the expression satisfies the constraint.

As you have probably realized, adding constraints on *mutable* variables
can take a bit of effort. (This is one reason that we recommend using
*immutable* variables as much as possible.)


## Problems generating C code

The ASLi tool includes an interpreter that can execute any correct ASL program.
However, most specifications written in ASL are compiled to C/C++ so that they
can be linked to other tools and to improve performance.
The ASL compiler is not able to compile all ASL code to C/C++ and it is
important to understand what can cause these problems and how to fix them.

### Unsupported features

The ASL compiler does not support any language feature that would require
dynamic memory allocation.
This affects support for `integer`, `real` and `string`.

- Although the ASL language says that `integer` values are unbounded
  (i.e., they can be as large as they need to be),
  the ASL compiler imposes a maximum size on
  unconstrained integers. This size can be controlled at compile time.
  At present, overflowing an integer does not generate an error message.

  We are in the process of improving this for "constrained integer" types such as
  `var x : integer {0..2^1000-1};`.
  Once finished and deployed, a constraint like this
  will cause the compiler to represent the variable `x` using the smallest size
  that can hold the variable (in this case 1000 bits)
  even if that size is larger than the default integer size.

- The ASL language has a type `real` that represents unbounded rational
  numbers.
  These are not supported because we do not use them in our
  specifications and they would require dynamic memory allocation.

- The only form of strings supported are string literals like `"Hello, World!"`
  and the only operation supported on strings is to print them.

  In particular, string concatenation and formatting numbers as strings
  is not supported.

### Bitwidth polymorphism

We refer to a function like `func F(x : bits(8)) => bits(16)` (where all bitvectors
have concrete size) as "monomorphic"
and a function like `func G(x : bits(N)) => bits(2*N)` (where some bitvector
has a variable size) as "polymorphic".
And we refer to a function call like "G(y)" as "monomorphic if 'y' has a concrete
size and "polymorphic" if 'y' has a variable size.

As the ASL compiler generates C code, it attempts to transform polymorphic
functions into monomorphic functions.  This process (called "monomorphization")
works by finding monomorphic calls to polymorphic functions and creating
"monomorphic instances" of those functions.
For example, a polymorphic function like this

``` asl
func G(x : bits(N)) => bits(2 * N)
begin
    return ZeroExtend(x, 2*N);
end
```

can be transformed into a monomorphic function like this

``` asl
func G8(x : bits(8)) => bits(16)
begin
    return ZeroExtend(x, 16);
end
```

that can be used in place of any monomorphic call to G with bitwidth 8.

Note that, in function "G", the call to "ZeroExtend" was polymorphic but, in
the function "G8", the call to "ZeroExtend" is monomorphic. As a result, it
would now be possible to create a monomorphic instance of "ZeroExtend".  It is
common for monomorphization to have a "cascade effect" where monomorphizing one
function "F" can enable the monomorphization of several other functions "G1",
"G2", "G3" (say) and monomorphization of each "Gi" can enable monomorphization
of even more functions "H1", "H2", ...

Most of the time, the monomorphization process works smoothly and you need
not think about it. However, sometimes monomorphization fails and you need
to help the ASL compiler.

(Note: similar issues apply when using C++ templates and you may be
able to adapt techniques that you would use with C++ code.)

There are two steps to solving monomorphization issues: diagnosis and cure.
There are two main techniques to cure monomorphization issues

#### Diagnosing monomorphization issues

Diagnosis can be challenging because of the cascade effect.
For example, if the ASL compiler is unable to monomorphize functions "F",
"G2" and "H3", the root cause of this problem may be that it
problems monomorphizing "F" prevented the usual cascade that would have
enabled it to monomorphize "G2" and "H3".
In this example, the wrong thing to do would be to try to fix "H3" or "G2": we need
to identify the topmost function that was not monomorphized.

To aid in diagnosis, the ASL compiler groups monomorphization failures
according to the function call tree. You should always focus your attention
on functions nearer the top of the call tree since that will almost always
resolve problems with functions further down the call tree.

#### Curing monomorphization: Help the compiler recognize bitwidth parameters.

In a function with a type like "func G(x : bits(M)) => bits(N)",
it is clear to the compiler that both "M" and "N" are bitwidths and that
finding concrete values of "M" and "N" such as "8" and "32"
would enable "G" to be monomorphized.

But, sometimes a function has an argument that will be
used as a bitwidth parameter but that is not used as a bitwidth in
the function's arguments or results.
For example, consider a function like this that is monomorphic
but contains a polymorphic call to "Write_Memory".

``` asl
func Push(address_size : integer, x : bits(32))
begin
    let address = RSP[0 +: address_size] - 4;
    Write_Memory(address, x);
    RSP[0 +: address_size] = address;
end
```

If the ASL compiler knew that `address_size` was a bitwidth
parameter, then a call to `Push(16, y)` would be monomorphized
and this would enable a cascade effect that would monomorphize
the call to `Write_Memory`.

We can hint that `address_size` is a bitwidth parameter by
listing all the bitwidth parameters to the function like this

``` asl
func Push{address_size}(address_size : integer, x : bits(32))
begin
    // remainder of function is unchanged from above
end
```

#### Curing monomorphization: Use 'case splitting'.

Some bitwidth parameters are calculated dynamically.
For example, they are determined by the value in a register
or the width of a value is determined by a field or prefix
on an instruction.

In this case, it is necessary to split the code into several
different cases each of which involves constant bitwidths.

For example, some floating point instruction sets use 1 bit
in the instruction encoding to choose between single and
double precision floating point.
The code might look like this (where "D" contains the value
of this instruction field).

``` asl
...
integer N = if D == '1' then 64 else 32;
let src1 = FP[x][0 +: N]; // read first operand
let src2 = FP[y][0 +: N]; // read second operand
let result = FPAdd(src1, src2);
FP[z] = ZeroExtend(result, 64); // write result
```

To enable this code to be monomorphized, we must duplicate
the last 4 lines and move them inside the if-then-else
like this.

``` asl
...
if D == '1' then
    let src1 = FP[x][0 +: 64]; // read first operand
    let src2 = FP[y][0 +: 64]; // read second operand
    let result = FPAdd(src1, src2);
    FP[z] = ZeroExtend(result, 64); // write result
else
    let src1 = FP[x][0 +: 32]; // read first operand
    let src2 = FP[y][0 +: 32]; // read second operand
    let result = FPAdd(src1, src2);
    FP[z] = ZeroExtend(result, 64); // write result
end
```

This case splitting is unfortunate because it hides
the similarities between the two cases and makes the
specification longer.
With a little forethought, we can mitigate this by

1. Moving the code that needs to be duplicated into
   a function so that there is less duplication

2. The primary cause of this problem in most ISAs
   is instruction decode where we read an instruction
   from memory and the instruction encoding determines
   data sizes.
   Fortunately, we usually generate the instruction decode tree
   automatically from a list of instruction
   encodings so it is often possible to modify the
   decode tree generator to also insert case-splitting code.
   This avoids the costs of hand-writing the decoder and
   most people will not be reading machine-generated code like
   a decode tree since the real specification of the encodings
   is the list of encodings.

## Runtime errors in ASL specifications

In ASL, the following conditions are errors

- Bitslice operations that go beyond the start or end of a bitslice.
  For example `x[-1]` or `x[64]` if `x` is a 64-bit bitvector.

- Array operations that go beyond the start or end of an array.
  For example `a[-1]` or `a[32]` if `a` is a 32-entry array.

- Assigning a constrained integer a value that is not in the constraint set.
  For example, `x = 16;` if `x` has type `integer {32, 64}`.

- Division by zero.

- Calling `Log2` on a number that is not a power of two.

- Reading a variable or part of a variable that has not been initialized or
  assigned to.

- Infinite loops.

- Case statements where no case-alternative matches the
  value of the discriminating expression.

- Failing the condition on an `assert` statement.

- Executing the function `Unreachable`.

- Throwing an ASL exception that is not caught.

If any of these can occur in the specification, it indicates
an error in the specification and it is not possible to reason
about what the processor will do.

At the moment, all of these (except infinite loops and reading uninitialized
variables) are detected by the interpreter.
However, the only errors detected by the C/C++ compiler are

- case statements where no case-alternative matches
- failing an assertion
- executing the function `Unreachable`
- throwing an ASL exception that is not caught

Until this is fixed, we recommend testing the specification with the interpreter and exercising additional care during code review.

## Assertions

> Failing an assertion indicates a bug in the specification.

Because failing an assertion indicates a bug in the specification, assertions should only
be used to check for things that should not be able to happen even in the
presence of buggy or malicious code.
Assertions should not be used to check for things that are technically possible
but indicate a software issue.

## Exception markers

Exception markers are used to indicate whether a function call
always throws an exception, can throw an exception or (when there is no marker),
cannot throw an exception.

Function definitions must be marked with an accurate marker and function calls
must have the same marker as the function definition.

If a function is changed in a way that changes the exception marker, all call sites
need to be updated with the new exception marker.
This is one of the primary design goals of exception markers: helping ASL authors to
find all the parts of the specification where a function now behaves differently.
In particular, you should be alert for changes that create the following code patterns.

- Exception thrown after modifying processor state (or memory)

    ```asl
    RAX = RAX + 4;
    F?(x);
    ```

  or

    ```asl
    RAX = RAX + 4;
    F!(x);
    ```

  This pattern is often problematic because it may not be possible to restart
  the current instruction after dealing with the cause of the exception.

- Exception generating dead code

    ```asl
    if condition then
        F!(x);
        RAX = Zeros(64);  // this code cannot execute
    end
    // this code cannot execute if `condition` is TRUE
    RBX = Zeros(64);
    return result;
    ```

You might also have naming conventions associated with certain kinds of
exception marker (e.g., a function whose primary job is to check for
a problem and throw an exception if it exists might have a name that starts
with `Check`). In this case, you might want to rename a function if the marker changes.

## Exception markers and incomplete specifications

When adding a new feature to the specification, it is common to work top down: adding an instruction
that calls a number of helper functions; then adding the helper functions; etc. until the
feature has been fully specified.

If one of the helper functions is expected to throw exceptions, you should add the expected
exception marker to the helper function even before you have defined the helper function.
But doing so may result in code that does not compile because the function definition does not match
the marker.

There are two alternatives for dealing with this. The first is somewhat ad hoc and can be used
if you rarely hit this problem; the second is recommended if you often hit this issue.

1. Add a conditional or unconditional 'throw' statement at the start of the function

   Add this for functions with exception marker '?'

    ```asl
    if FALSE then throw ExceptionTaken; end // dummy throw until this function is completed
    ```

   or add this for functions with exception marker '!'

    ```asl
    throw ExceptionTaken; // dummy throw until this function is completed
    ```

2. Define three helper functions for use when adding new features

   Call this function at the start of stub functions that never throw exceptions

    ```asl
    func UnimplementedFeature(message : string)
    begin
        print("Unimplemented feature : ");
        print(message);
        println();
        assert FALSE;
    end
    ```

   Call this function at the start of stub functions that sometimes throw exceptions

    ```asl
    func UnimplementedFeatureWithCheck?(message : string)
    begin
        print("Unimplemented feature : ");
        print(message);
        println();
        if FALSE then throw ExceptionTaken; end
        assert FALSE;
    end
    ```

   Call this function at the start of stub functions that sometimes throw exceptions

    ```asl
    func UnimplementedException!(message : string)
    begin
        print("Unimplemented feature : ");
        print(message);
        println();
        assert FALSE;
        throw ExceptionTaken;
    end
    ```

