---
title: Subtypes in ASL
---

ASL 0.x had subtypes.
ASL 1.0 greatly extends the notion of subtypes.
This note is mostly about their use in ASL 0.x
and whether that use case is still relevant.

# MSRs: the motivation for subtypes

MSRs and similar configuration/status registers are accessed in two ways:

1. As a collection of named fields accessed via record field syntax "MXSCR.RC"

2. As a 32 or 64-bit bitvector accessed either as a complete variable or using slice notation.

One aspect of "subtyping" was to support these two views. (The "scare quotes"
is because this aspect of "subtyping" was not really subtyping because the two
views are more or less symmetric.)

The other aspect of "subtyping" - that really was subtyping was that it is
quite common to find several MSRs that have fields in common. In particular,
sometimes fields are repeated between related MSRs: the same fields at the same
location occur in MSRs that are used in different modes.  But, in practice, not
all of the fields are the same: in some modes, there is an extra field not
needed in other modes.

This is problematic because it makes it harder to share
code between modes. Ideally, we want to be able to write code that could use
any of the MSRs but they all have different sets of fields so they don't have
exactly the same type.

But, the different MSRs have a common intersection type consisting of only the
fields that they have in common (ie same name and same bitslice) and, most of
the time, we only wanted to access the fields that they have in common - so
subtypes provide a way of letting us write code that could work the same at
any privilege level.

(Note that this is really just syntactic sugar though - we can always have
written the code with bitslice syntax instead - but the spec would be harder to
read.)

# Two alternative ways of handling MSRs

There are two approaches to handling MSRs.
The motivation for subtypes above is only relevant to one of those ways
so it is useful to consider both approaches when thinking about subtyping.

## Directly representing MSRs in the spec

Within the IA Spec, we could choose to treat MSRs in a very different way
though - which would largely eliminate these motivations.

The obvious way to deal with MSRs is what I was assuming above:

- For each MSR, you define a 32/64-bit bitvector variable

- When you want to refer to a field of the MSR, you use field notation to select the appropriate slice. eg MXSCR.RC

- Instructions that read/write the MSR access the 32/64-bit bitvector variable - what could be simpler?

    There are two complications

    1. They need to mask out bits that have reserved values, that are not
       writable/readable at the current privilege level, etc. They might even
       generate exceptions when some values are written to MSRs.

    2. It is probably true that some MSRs alias each other. That is, writing to a
       field of one MSR changes a field of another MSR to the same value and vice
       versa. (This is probably not common - but it probably happens enough to be
       an issue.


Having every access to an MSR be of the form "MSR.FIELD" has costs: we need to
do a bitslice every time we access that field; and any analysis to understand
where a given field of an MSR is read/written is complicated by all accesses
ultimately being bitslices.  The complications in reading/writing MSRs are also
quite significant - far outweighing the simplicity of being able to just
read/write the whole MSR in one go.


## Splitting MSRs into component fields

An alternative approach to handling MSRs is this

- For every *field* of an MSR, we define a variable

- The type of the variable will often not be a bitvector: it will usually be
  boolean or an enumeration. The type is chosen to capture the information as
  clearly as possible.

- Within the spec, we only refer to these variables - with no mention of the
  MSR (except perhaps in naming conventions)

  HTML renderings of the spec probably link to the MSR in a mouseover or whatever.
  (todo: but what about the SDM?)

- Instructions that read/write an entire MSR, call functions that

    - On reads: assemble the value of the MSR from all the component parts
      (i.e., all those variables we introduced) and insert 0s, etc. in between
      depending on privilege level, features enabled, etc.

    - On writes: take the 32/64-bit value being assigned and, for each field,
      copy the relevant bits over into the corresponding variable applying the
      conversion to boolean/enumeration at the same time.

This approach makes subtyping pretty much unnecessary and I think it is mostly
better: clearer specs, faster execution, more precise dependency analysis.
Where it is worse is that the connection between the variables and MSRs will be
less clear.

But, this approach is largely untested so, at the moment, it is not clear which
approach is better.  It is also not clear whether subtypes are needed
elsewhere.
