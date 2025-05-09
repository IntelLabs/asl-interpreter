---
layout: post
title: "Language extension: parameterized records"
tags: extension
---

Our specification of floating point operations starts by
unpacking N-bit floating point values into a record that
looks something like this

```asl
00  record UnpackedFloat {
01      // numerical data
02      is_negative : boolean;
03      exponent    : integer;
04      significand : bits(???);
05
06      // classification
07      is_nan : boolean;
08      is_inf : boolean;
09
10      ...
11  };
```

The only problem here is how big to make the fraction.
On 64-bit floats, it should be 53 bits; on 32-bit floats, it should be 24 bits.

We could handle this by making the significand the maximum required size.
This would be equivalent to representing every floating point value by the
equivalent 64-bit float.
This would work but it would lead to awkward code for rounding results
to the correct precision.

A better solution is to make the significand size depend on the type being represented.
To do this, we need to parameterize the 'UnpackedFloat' record over the data size ('N')
and the exponent size ('E').

```asl
00  record UnpackedFloat(N, E) {
01      // numerical data
02      is_negative : boolean;
03      exponent    : integer;
04      significand : bits(N-E);
05
06      // classification
07      is_nan : boolean;
08      is_inf : boolean;
09
10      ...
11  };
```

Now we unpack 64-bit floats into records with type 'UnpackedFloat(64, 11)';
we unpack 32-bit floats into records with type 'UnpackedFloat(32, 8)';
and we unpack 16-bit floats into records with type 'UnpackedFloat(16, 5)'.

## Design process

The design process for this extension was fairly simple.
The only minor challenge was that ASL has a syntax for creating and initializing records that looks like this

```asl
let origin = Point{x = 0, y = 0};
```

When we added parameterized records, it became possible to write code like this

```asl
let one = UnpackedFloat(16, 5){
             is_negative = FALSE,
             exponent = 0,
             significand = '1 00 0000 0000',
             ...
          };
```

This has a minor parsing issue that, until the '{' character is seen, it
is not possible to tell whether this is a call to a function called 'UnpackedFloat'
or a parameterized record constructor.

