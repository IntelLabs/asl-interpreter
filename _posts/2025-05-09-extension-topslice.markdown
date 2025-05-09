---
layout: post
title: "Language extensions: topslice and with-expressions"
tags: extension
---

An important part of the ASL language is its support for manipulating bitvectors and,
in particular, manipulating parts of bitvectors.
ASL has 4 standard ways of manipulating part of a bitvector;
in this article I will describe two new ways.

## ASL's existing slice support

### Individual bits

If you read hardware documentation from several sources, you will have noticed that they usually write 'x[7]' or 'x<7>' to refer to a single bit of a bitvector called 'x'.
And, 99% of the documentation treats 'x[0]' as the least significant bit and 'x[31]' as the most significant bit (assuming that 'x' is 32 bits in size).
A really, really early version of ASL used 'x<7>' but, after a while the syntax was changed to 'x[7]'.
This is a pretty good syntax to use because it is the same syntax that System Verilog uses and it looks like an array access --- which is a good mental model to have.

### Multiple bits --- emphasizing the position

Hardware documentation also commonly talks about sequences of adjacent bits and uses syntax like 'x[15:8]' to refer to the 8 bits in the second byte of a variable 'x'.
Again, this is a pretty good syntax that most people understand and that matches what System Verilog uses and so ASL adopts the same syntax.

Sometimes, parts of a register that logically make up a single field are not all adjacent.
For example, a register might start with a 4-bit priority field in bits '3:0' but this is later extended to 8 bits by using bits '11:8' (because bits 7:4 are already in use).
So it is useful to support slices made up of several parts of the register.
The syntax for this is in System Verilog and ASL is 'x[11:8, 3:0]'.

### Multiple bits --- emphasizing the width

The syntax introduced so far works great in many cases but it gets a bit awkward when we want to extract 'N' bits from position 'i' of a bitvector.
To do that, you have to write 'x[i+N-1 : i]'.
This expression gets the job done but System Verilog has a better notation for when you want
to emphasize the width of a bitvector: 'x[i +: N]'.
This means exactly the same but it is simpler to write and it makes it easier to see that the width is 'N'.

### Element access

When we are writing specifications of vector instructions, a really common
pattern is to think of the vector as being divided into some number of elements
of width W and to iterate over all those elements.
The specification often looks something like this

```asl
let elements = VL DIV W;
for i = 0 to elements - 1 do
    let op1 = src1[i * W +: W];
    let op2 = src2[i * W +: W];
    let r = op1 * op2;
    result[i * W +: W] = r;
end
```

It turns out that the pattern 'x[i * W +: W]' is really common in vector instructions
so ASL has a special syntax for this to help emphasize the fact that the bitvector
is being treated as a collection of elements of width W.
The syntax is 'x[i *: W]' and it is equivalent to 'x[i * W +: W]' or 'x[i*W + W-1 : i*W]'.

With this syntax, the above example looks like this

```asl
let elements = VL DIV W;
for i = 0 to elements - 1 do
    let op1 = src1[i *: W];
    let op2 = src2[i *: W];
    let r = op1 * op2;
    result[i *: W] = r;
end
```

### Named fields

Status and control registers are usually defined as having a number of named
fields that occupy specific bit positions.  For example, in the Intel
Architecture®, the RFLAGS register is a 64-bit register with around 14 named
fields.

In ASL, we can define these fields like this.

```
type RFLAGS_Type of bits(64) {
    [0]     CF
    [2]     PF
    [4]     AF
    [6]     ZF
    [7]     SF
    [8]     TF
    [9]     IF
    [10]    DF
    [11]    OF
    [13:12] IOPL
    [14]    NT
    [16]    RF
    [17]    VM
    [18]    AC
    [19]    VIF
    [20]    VIP
    [21]    ID
    [1]     RESERVED_ONE
    [31:22,15,5,3] RESERVED_ZERO
    [63:22, 15:15, 5:5, 3:3, 1:1] RESERVED
};

var RFLAGS : RFLAGS_Type;
```

With this definition, we can write 'RFLAGS.ZF' instead of writing 'RFLAGS[6]'.
For example, we can write "RFLAGS.ZF = '1';" to set the ZF field.

Note: For our convenience, we have defined some additional fields 'RESERVED_ONE',
'RESERVED_ZERO', and 'RESERVED'. These names have no special meaning in ASL.


## New extensions

Now that we know what the ASL1 language provides, we can move onto some recent extensions to this syntax.

### Top slices

The syntax 'x[i +: w]' is great for accessing 'w' bits starting from position 'i'.
It is especially useful where 'w' is not a literal constant (like '8')
and it is my preferred syntax in most cases.

However, this syntax is a bit awkward if you want to refer to the top bits of a register.
We don't do this very often but there are a few places where we want to check that an address is 'canonical' (i.e., the top 'w' bits are all the same) or check that the top bits of a register are all zero where we do want to do this.

If we want to access the top 'w' bits of a 64-bit register in ASL1, then we
have a choice of writing 'x[64-w +: w]' or 'x[63 : 64-w]'. I don't think that
either of these is ideal.

Fortunately, System Verilog has exactly the syntax we need: 'x[63 -: w]' which extracts the 'w' bits up to and including bit 63.
We don't need this syntax often but, when we do, it is a perfect match.

### With expressions

The final extension is motivated by the observation that, in almost all cases,
the bitslice and named field syntax above is easier to understand and maintain
than the typical code you would write in C.
Which is easier to understand "RFLAGS.ZF = '1';" or "RFLAGS = RFLAGS OR 0x40;"?

But there is one case where using AND/OR/NOT is more convenient: passing a register
to a function and clearing/setting a bit at the same time.
For example, if you want to pass the RFLAGS register to a function 'Update' but mask out the NT flag (bit 14), you could write

```asl
var flags = RFLAGS;
flags.NT = '0';
Update(flags);
```

which is clear but introduces an intermediate variable (which it is often hard
to find a good name for) and is quite long.

We could use 'AND' to mask out the bits like this.

```asl
Update(RFLAGS AND 0xffff_ffff_bfff_ffff);
```

This is hard to understand because we can't use the named fields but it is
short because we don't have to explicitly copy RFLAGS into an intermediate
variable and then change it.

Our solution is to create a new operator 'with' that lets you copy a value
but change some fields in the process.

```asl
Update(RFLAGS with { ZF = '1' });
```

This is equivalent to both of the above versions. It combines the readability of
using named fields with the brevity of being an expression.

Just to be absolutely clear, this new syntax does not modify the value of
RFLAGS: it makes a copy and modifies the copy. Exactly the same as the two
other ways illustrated.

## Wrapping up

ASL1 already had 4 useful ways of accessing parts of a bitvector: 3 slice syntaxes and named fields.
This article describes 2 new ways of accessing parts of a bitvector: topslices and with-expressions.

## Design notes

The "topslice syntax" 'x[i -: w]' has been on my mind since I first saw it in
System Verilog but it is only recently that I started to see enough places that
could use it to make it worthwhile adding to ASL.  Beyond the obvious cost of
implementing and testing a new language feature, there is a hidden cost on
every reader: there is more potential for confusion, more to look up, more to
remember. Adding new syntax that is only used in a few places can cause more
problems than it solves and we will monitor how useful this new syntax is and
remove it if it does not pay for itself.

As we are writing the Intel Architecture® specification, we often have to consult
the source code of internal simulators written in C++.
Since C/C++ have poor support for manipulating bitvectors, a lot of the
code seems quite opaque. It took me a good 10-15 minutes to figure out what flags
are being checked in the following code.

```C
RFLAGS & FFFFFFFF_FFC2802AH != 2
```

However, there were a few places where C/C++ was more concise. After a bit of
staring at the examples, it became clear that the problem was that the only way
to change a field in ASL was in a *statement* but that C/C++ was able to achieve
the same effect in an *expression* by AND/OR-ing with a mask.

The keyword 'with' was inspired by the with statements in Pascal and by
the syntax for copying a record while changing some fields used in OCaml:
'{r with f1 = e1; ...; fn = en }'.
