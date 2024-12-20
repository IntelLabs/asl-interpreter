---
layout: page
title: Reading ASL
---

Instruction specifications are written in Architecture Specification Language (ASL).


## Limitations of the ASL Specification

The ASL specification should be read in combination with the natural language
parts of the specification.  In particular, memory accesses may be performed in
a different order from that shown in the ASL and exception prioritization may
differ from that shown in the ASL.

## Basic ASL Concepts

ASL provides the following types

- The type `integer` representing mathematical (unbounded) integers.
- The type `bits(N)` representing bitvectors of length `N`.
  The type `bit` is short for `bits(1)`.
- The type `boolean` with values `TRUE` and `FALSE`.
- Tuple types such as `(bit(32), bits(6))` representing a pair of bitvectors of different lengths.

You can also define record types with named fields and enumeration types with named constant values.

Arithmetic values include

- Decimal integers `1000`. These can optionally include underscores to make
  large numbers such as `1_000_000_000` easier to understand.
- Hexadecimal integers `0xFFFF`. These can optionally include underscores to make
  large numbers such as `0xFFFF_FFFF_0000_0000` easier to understand.
- Binary bitvectors such as `'101'`. These can optionally include spaces to make
  large bitvectors such as `'1110 1010'` easier to understand.

Bitslice operations are used to extract part of a bitvector.  There are several
different notations that can be used to extract a single bit or multiple bits.

- `x[63]` - bitslice (single bit)
- `x[31 : 0]` - bitslice (high : low)
- `x[16 +: 8]` - bitslice (low +: width)

The notation `x[16 +: 8]` is equivalent to `x[23:16]` (both extract bits 23 down
to 16 of the variable `x`). The choice of notation does not affect the meaning
but is used to textually emphasize either the width of the value being extracted
or the topmost bit being extracted.

The notation `[x, y]` is used to concatenate bitvectors `x` and `y` into a
single bitvector.  The top bits of the result are the bits from `x`; and the
bottom bits of the result are the bits from `y`.

- `[top, middle, bottom]` - concatenate list of bitvectors into a single bitvector
- `x[31:0, 63:32]` - concatenate slices (this example swaps the top/bottom of a value)
- `x.[CF,OF,PF]` - select fields of record and concatenate result (equivalent to `[x.CF, x.OF, x.PF]`


The following arithmetic operators are used

- The usual equality operators - `==` and `!=` on all types.
- The usual ordering operators - `<`, `<=`, `>=` and `>` on the type integer.
- The usual arithmetic operators - `+`, `-`, `*`.
- Integer division and remainder (rounding down to -infinity) - `DIV` and `MOD`.
- Integer division and remainder (rounding to zero) - `QUOT` and `REM`.
- Integer exponentiation - `^`. This is almost always used in the form `2 ^ x`.
- Bitvector operations - `AND`, `OR`, `XOR` and `NOT`.
- Boolean shortcircuiting operations - `&&` and `||`.
  (These are 'shortcircuiting' operators - the second operand is not evaluated unless needed.)

The usual expression syntax and statement syntax is used

- Conditional expression - `if x < y then x else y`
- Tuple expressions - `(1, TRUE)` - create a tuple value
- Test if a value matches a pattern - `p IN '11xx'` (This example tests whether the top two bits of `p` are both `'1'`.)
- Record field access - `r.address`
- Array subscript - `x[i]` (Note that the same syntax is used for bitslicing.)
- Record values - `MyRecord{ address = Zeros(64), valid = FALSE }` - create a record value
- Unknown values - `UNKNOWN : bits(64)` (Used when the architecture allows several choices of behavior.)

## ASL statements

### Comments

Comments start with the symbols `//` and continue until the end of the line.

### Variables and assignments

Variables in ASL can be "immutable" (their value cannot be changed) or "mutable"
(their value can be changed by assigning to them).

- `let x = <expression>;` --- define an immutable variable
- `var x = <expression>;` --- declare a mutable variable with initializer
- `var x : <type>;` --- declare a mutable variable with no initializer
- `x = <expression>;` --- assign to a variable
- `x[7 : 0] = Zeros(8);` --- assign to a bitslice
- `(x, y) = <expression>;` --- assign a tuple to a pair of variables
- `[x, y] = <expression>;` --- assign a bitvector to a list of bitvectors (splitting value based on size of x and y)

These forms can be combined. For example,

- `let (max, min) = if a >= b then (a, b) else (b, a);` --- initializing several
  variables
- `let [hi : bits(32), lo : bits(32)] = RAX;` --- initializing variables `hi` and
  `lo` with the top and bottom (respectively) of `RAX`.

### Conditional statements

- `if <condition> then <statements> { elsif <condition> then <statements> } [else <statements>] end`
- `case <expression> of { when <pattern> [where <condition>] => <statements> } [otherwise => <statements>] end`

### Loop statements

- `for x = <expression> to <expression> do <statements> end`
- `for x = <expression> downto <expression> do <statements> end`
- `while <condition> do <statements> end`
- `repeat <statements> until <condition>;`

### Function return

- `return <expression>;` --- return from a function with a return type
- `return;` --- return from a function with no return type

### Exceptions

Exceptions can be thrown and caught.

- `throw <expression>;`
- `try <statements> catch x {when <identifier> : <type> => <statements>} [otherwise => <statements>] end`

Calls to a function `F` that can throw an exception are marked with either `?` or `!`.

- `F!(x)` indicates a call to a function that always throws an exception.
  Calls to this function cannot return.

- `F?(x)` indicates a call to a function that could throw an exception.
  Calls to this function may not return.

- `F(x)` indicates a call to a function that cannot throw an exception.
  Calls to this function will always return.

Function definitions are also marked with exception markers. For example

```asl
func AbortExecution!()
begin
    throw EndOfInstruction;
end
```

*[Exception markers are an experimental extension of ASL that are intended
to make it easier to understand the impact of exceptions on the meaning of
the specification.
We welcome feedback on whether the markers are helpful or distracting and
on how they can be improved.
This will help determine whether they are adopted into the ASL language or whether
we abandon the experiment and remove all exception markers.]*


### Other forms of statement

- `assert <expression>;`

   Note that failing an assertion indicates that there is a bug in the
   specification.
   Failing an assertion is not the same as throwing an exception and
   functions that contain assertions need not have exception markers.

## Function definitions

### Functions that return a value

Functions that return a value can be used in expressions.

```
func Concat(x : bits(32), y : bits(32)) => bits(64)
begin
    return [y, x];
end
```

### Functions that do not return a value

Functions that do not return a value can be called from statements.

```
func BranchTo(x : bits(32))
begin
    RIP = x;
end
```

### Getters and setter functions

Getter and setter functions provide an alternative syntax for functions
that allows the functions to be called using the same syntax as if
the functions were variables or arrays.
This has no impact on the behavior of the functions but is used
to textually simplify the appearance of the specification.

Getter functions are functions that return a value.
There are two forms of getter function.

- Variable-like getter functions are defined like this

  ```
  getter RAX => bits(64)
  begin ... end
  ```

  These functions can be used like variables in expressions.
  For example, `RAX + RBX`.

- Array-like getter functions are defined like this

  ```
  getter XMM[i : integer] => bits(128)
  begin ... end
  ```

  These functions can be used like arrays in expressions.
  For example, `let src1 = XMM[i];`.

Setter functions are functions that do not return a value.
There are two forms of setter function.

- Variable-like setter functions are defined like this

  ```
  setter RAX = val : bits(64)
  begin ... end
  ```

  These functions can be used like variables in assignment statements.
  For example, `RAX = Zeros(64);`

- Array-like setter functions are defined like this

  ```
  setter XMM[i : integer] = val : bits(128)
  begin ... end
  ```

  These functions can be used like arrays in assignment statements.
  For example, `XMM[i] = result;`.

