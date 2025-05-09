---
layout: post
title: "Language extension: exception markers"
tags: extension
---

It can be hard to think about code that contains exceptions
because exceptions introduce invisible control flow.
Consider this function (from [this article]({% post_url 2025-05-08-extension-named-parameters %})).

```asl
func PushStack(value : bits(N), decrement_by : integer = N DIV 8)
begin
    assert decrement_by >= (N DIV 8);
    RSP = RSP - decrement_by;
    WriteMemory(RSP, value);
end
```

If you think carefully about this code, you will realize that it has a subtle
but important bug.

The problem is that I forgot that the 'WriteMemory' function can
generate a memory protection exception such as a page fault.
If this function does generate a page fault, we want to be able to fix the page fault
(e.g., by adding another page below the stack) and then to re-execute the
instruction that triggered the exception. This should try the same memory write again
and, hopefully, it succeeds this time.
The bug is that re-executing the above function after a page fault would not try the same
memory write again because we have already decremented the stack pointer 'RSP' so we would perform a write further down the stack.

This is a well known problem in any system that supports exceptions
and there is a well known solution: delay any state changes until after all the
exception checks have been performed.
For example, we might write this instead

```asl
func PushStack(value : bits(N), decrement_by : integer = N DIV 8)
begin
    assert decrement_by >= (N DIV 8);
    let new_rsp = RSP - decrement_by;
    WriteMemory(new_rsp, value);
    RSP = new_rsp;
end
```

This kind of issue happens a lot in ISA specifications and we have found that
we can only really understand code like this
if we know which functions can throw exceptions.
It _might_ be realistic to expect people writing the specifications to know
all the exception throwing functions but this is not a reasonable assumption to
make of _all_ people reading the specification.

## How can we help readers understand this code?

Our answer is to make the invisible control flow visible by introducing
"exception markers" that are added to function definitions and to function
calls.
The exception markers are written after the function name and come in three
different flavors.

- `F!(x)` is used if a function always throws an exception. That is, it
  never returns.

- `F?(x)` is used if a function can either return successfully or if it
  can throw an exception.

- `F(x)` is used if a function cannot throw an exception.

Here's how the 'PushStack' function looks with these markers added.

```asl
00  func PushStack?(value : bits(N), decrement_by : integer = N DIV 8)
01  begin
02      assert decrement_by >= (N DIV 8);
03      let new_rsp = RSP - decrement_by;
04      WriteMemory?(new_rsp, value);
05      RSP = new_rsp;
06  end
```

We have added a `?` (can-throw) marker to the call to 'WriteMemory' on line 04.
This serves as a hint to readers that an exception could occur on that line
and so all previous lines could be re-executed if the current instruction is
restarted after the exception.

And we have added a `?` (can-throw) marker to the 'PushStack' function
prototype on line 01.  This serves as an overall summary of the exception
behavior.  Any call to the function 'PushStack' will require a matching marker.

## Design process

ASL is not the first language to try to help programmers understand exceptions.
For example, Java functions have to be annotated with a list of all the exceptions
that they can raise.
There is value in that but it is also quite noisy and, more importantly, it does not
help you spot a call to a function that can raise exceptions.

Swift requires that functions that can throw exceptions explicitly declare it with the 'throws' keyword. (See [the Swift book](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/errorhandling/).)
For example,

```swift
func canThrowErrors() throws -> String
```

And it requires that a call to a function that can throw an exception is marked with the 'try' keyword (or is inside a 'do-catch' block).

```swift
try canThrowErrors();
```

ASL's exception markers are clearly similar to Swift.
We chose single-letter markers to make the markers a little less noisy.

Our original design for this feature did not distinguish between functions that
can throw an exception and those that always throw an exception but one of the
reviewers of our specifications noted that it was very helpful to know where
control flow cannot reach so we added the `!` marker.

The initial design also put the marker after the function arguments like this

```asl
WriteMemory(new_rsp, value)?;
```

This made the marks even quieter (which we liked) but in long lines it gets
harder to figure out which function the marker belongs to.

```asl
WriteMemory(new_rsp, ZeroExtend(value, 64))?;
```

And [Brian Campbell at the University of
Edinburgh](https://homepages.inf.ed.ac.uk/bcampbe2/) noted that putting
the marker after the function name would allow people to think of the marker as
part of the function name. For example, they might talk about the function
'PushStack?' or 'ReportPageFault!'.

## Exception catching

ISA specifications tend to have just a few places where they catch exceptions.
In our specification of the Intel Architecture (aka "the x86"), all exception
catching is performed in a function called 'Step' that is responsible
for advancing the processor by fetching and executing an instruction or
dealing with a pending interrupt.

This function contains a number of 'try-catch' blocks that catch all
exceptions and trigger the appropriate response.
Since these try-catch blocks catch all of the exceptions, the 'Step' function
itself cannot generate exceptions: a simple thing for the ASLi tool to
check.

## Some tricky design questions

One tricky question that might occur to you is what to do with an assert statement.
In the ASL language, a failing assertion indicates an error in the specification.
It does not throw an exception and you cannot catch the exception and restart.
So, in ASL, we do not add an exception marker to 'assert' or to functions that
contain assertions.

Similarly, ASL has a function 'Unreachable()' that can be used in places that
we believe cannot be reached. For example, in a case-statement, we might explicitly add

```asl
    case operand_size of
        ...
        when 64 => Unreachable();
    end
```

if we think that a size of 64 is impossible in this bit of code.
This is like an assertion: if the function is ever called, it indicates an
error in the specification.
Like an assertion then, it does not require an exception marker.

Finally, as we are writing the specification, we sometimes use a special
function 'UnspecifiedFeature("...")' to mark part of the ISA that we are not yet ready
to write a specification for.
This placeholder is helpful for reviewers but it is purely
internal, temporary feature that will not appear anywhere in the complete
specification.
When this function is called, it prints a message and halts execution so,
again, you might expect that it would have a '!' (always-throws) marker but
this would be very inconvenient and confusing because exception markers are
contagious: if a function contains an exception marker then that function
_and all functions that directly or indirectly call it_ require exception
markers.
This could be a lot of annotation and it might be incorrect if the missing
piece of code cannot raise an exception.
Since, this function is not raising an exception with the expectation
that execution can restart successfully after that exception so we do
not add an exception marker to this function.
