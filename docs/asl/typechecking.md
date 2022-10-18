---
title: "Type checking and synthesis"
---

This is a long overview of how type systems are defined and implemented in general and
how ASLi in particular performs type checking and synthesis.

- How rules are used to define type systems
- Type checking: Using the rules to check a fully annotated AST
- Type inference part 1: Changing the rules to be "bidirectional"
- Type inference part 2: Implementing a bidirectional type system
- Handling overloading of operators, functions and procedures
- Checking constraints
- Unification

# How rules are used to define type systems

Usually you have one or two rules per syntactic construct. (eg one for assignments, another one for if expressions, ...)
In this section, I will describe the conventional approach where there is one
rule per construct; the section on "bidirectional" type systems
changes this to have up to two rules per construct where the rules are a bit more precise / restrictive
about what information can and cannot be inferred and also a bit easier to implement.

The rules are all a form of implication: "if properties A and B and C hold then property X holds" and they are normally written like this

```
A
B
C
----------------- name-of-rule
X
```

Usually A, B and C are properties of the children of the node and X is a property of the node.
eg some really simple rules could be

```
e :: integer
------------------------ negation_integer
-e :: integer
```

```
C is an integer literal
----------------------- literals_integer
C :: integer
```

from which you can conclude "1 :: integer" and "-1 :: integer" (and "- (-1) :: integer" and ...)

In practice though, most expressions don't have a type in isolation, they
depend on the variables and functions that are in scope.

So, instead of saying "e has type integer", you want to say 'In environment
"env", "e" has type "integer"'. This is normally written with the "turnstile"
symbol "|-" like this "env |- e :: integer".

With this, we can add rules for variables and functions.

```
env(x) == ty
------------------- variable-read
env |- x :: ty
```

and now, if we assume that env(count) == integer, we can use the above rules to
conclude that "env |- -count :: integer".

And we can add rules for functions too (this is not the rule for ASL)

```
env(f) == "(t1, ... tm) => rty"
env |- e1 :: t1
...
env |- em :: tm
--------------------------------------------------------------------------------- function-call
env |- f(e1, ... em) :: rty
```

and for procedure call (also not ASL)

```
env(f) == "(t1, ... tm) => unit"
env |- e1 :: t1
...
env |- em :: tm
--------------------------------------------------------------------------------- procedure-call
env |- "f(e1, ... em)" :: statement
```

Here I'm using "env |- s :: statement" to mean "'s' is a legal statement in environment 'env'".

Many languages have some form of subtyping that says when it is legal to assign
a value to a variable.  If we write "t1 <= t2" to mean that t1 is a subtype of
t2 (by which I mean that any value of type t1 is also a value of type t2), then
we can write an assignment rule

```
env(x) :: t2
env |- e :: t1
t1 <= t2
----------------------------- assignment
env |- "x = e;" :: statement
```

And we could go on and define some more rules for if-statements, binary
operators, arrays, etc. - hopefully it is clear how we would do that based on
the above examples.


# Type checking: Using the rules to check a fully annotated AST

There are two things that you might do with rules like the above:


- infer the types in a program. That is, we take a program that has partial
  type information and we annotate every expression, statement, etc. with its
  type inforrmation.

- check the types in a program. That is, given an expression where every type
  is explicitly annotated, check that each AST node is correct according to one
  of the rules in the type system.

Despite the name, "tcheck.ml" infers types. In particular, it infers the type parameters in function/procedure calls.

From rules like the ones above, typecheckers are fairly simple to implement.
If every node in your AST is annotated with the type of that node, you can
visit the nodes in almost any order you like and perform a local check that
everything is ok. The only constraint on the order you visit the nodes is that
you need to know what is in scope at any time so that you can update the
environment appropriately.

In practice, your AST representation probably doesn't explicitly annotate every
node with its type because, most of the time, you can easily calculate the type
of a node from the type of its children.  For example, we know that the type of
"add_int(p, q)" is "integer" because the return type of "add_int" is "integer".

For this kind of AST, you have to perform typechecking in a bottom-up sweep
that checks the type of the children of a node before checking the type of the node itself.

- for each subexpression:

    - check that it is correctly typed
    - return the type of that sub-expression

- use the types of all the sub-expressions to figure out the type of the current expression according to the relevant rule

For example, if "c :: boolean", "t :: integer" and "e :: integer" then
"if c then t else e" is correctly typed and has type integer.


# Type inference part 1: Changing the rules to be "bidirectional"

If our starting point does not have enough type annotations in it that we can
just check that the code is correctly typed, then we need to fill in the gaps
by inferring the types.

Different languages (OCaml, Haskell, Scala, ...) use different ways to do this.
The ASL approach is close to what ML (an ancestor of OCaml) does:
"bidirectional typing" - because it is simple. ASLi's implementation is
strongly influenced by this tutorial paper

    Bidirectional Typing,
    Jana Dunfield, Neel Krishnaswami,
    2020
    [https://arxiv.org/pdf/1908.05839.pdf]

In bidirectional typing, the input is a partially typed program and we have to
infer the missing types and check that the whole thing is legal. For this, we
have change the type system to distinguish cases where we have enough information
that we are just typechecking from cases where we have to fill in some missing type
information. Specifically, we will have two kinds of rules:

- Checking rules: if we have all the type information we need, do the check. (ie what I sketched above)

- Synthesis rules: if we don't have all type information we need, infer a correct type based on what information we have

The rules will look similar to the rules seen above but we replace the syntax
"env |- e :: t" with two variants

- "env |- e <== t": in environment 'env', expression 'e' checks against type 't'

- "env |- e ==> t": in environment 'env', expression 'e' synthesizes type 't'

The notation "<==" (checking) and "==>" (synthesizing) hints at the direction
of information flow in the typechecker. I don't think it is explictly said in
the paper but I think of it as "<==" (checking) consumes information we already
have while "==>" generates new information.  (But, to be honest, I tend to
forget the meaning of the syntax fairly quickly and have to look it up each
time)

In bidirection form, some of the rules above are:


- A synthesis rule for variables. Since the environment can tell us the type of
  any variable it contains, we just look it up in the environment and return
  it.

    ```
    env(x) = t
    ---------------   Var_==>
    env |- x ==> t
    ```


- A checking rule for assignments. Here we want to synthesize/infer the type of
  the right hand side and the left hand side and then we want to check that the
  two types are compatible.

    ```
    env |- e ==> t1
    env |- v ==> t2
    t1 <= t2
    ---------------- Assign-<==
    env |- "v = e;" <== statement
    ```

- A synthesis rule for let-statements with no explicit type annotation. Here,
  we need to synthesize the type of the right hand side and, from that, we can
  synthesize the type of the variable we introduce. (Incidentally, note that
  this rule omits something really, really important: we need to add "v :: t"
  to the environment. To describe that properly, we would need to change the
  notation to say that we synthesize both a type and a new environment.)

    ```
    env |- e ==> t
    v is not in env
    -------------------------- Let-assign-implicit-==>
    env |- "let v = e;" ==> statement
    ```

- A synthesis rule for if-expressions. Here we want to synthesize the overall
  type of the if-expression but we want to check that the condition is boolean

   ```
   env |- c <== boolean
   env |- t ==> A
   env |- e ==> B
   A == B
   --------------------------- IfExpr-==>
   env | "if c then t else e" ==> A
   ```

Here, the rule for each syntactic construct is either checking or synthesizing.
Sometimes you need two rules though: one to use in a synthesis context and one
to use in a checking context.


Unlike the original form of the rules, bidirectional rules make explicit
what type information must be present in the original program and
what type information can be filled in by the typesystem.
If we were to define the type system using rules, we would want to use
bidirectional rules to make this information clear.


# Type inference part 2: Implementing a bidirectional type system

Turning bidirectional typing rules into code is not completely automatic
(although sections 3 and 4 of the paper describe some techniques and
requirements to do it)

One approach (that ASLi does not use for historical reasons) is to have a function
to check expression types and another function to synthesize expression types.

The approach that ASLi does use is that we have a function

```
    expr (env : Env.t) (x : expr) : type
```

that does both checking and synthesis and returns the type of the expression.

The function does a giant case split on the different syntactic shapes and then
visits each subexpression and either

- checks the type of a subexpression (eg check that the condition in an
  if-expression is boolean)

- or synthesizes the type of a subexpression (eg synthesize the types of the
  "then" and "else" branches of an if-expression)

If there is a side-condition in the rule like "type A == type B" or "variable
'v' is not in environment 'env'" then we perform those checks.

If we are using a synthesis rule, we then use the types of the subexpressions
to calculate the type of the expression.


# Handling overloading of operators, functions and procedures

ASL has several forms of overloading that need to be resolved.
These are done before checking constraints.
(In theory, overloading resolution could be done in a separate pass but, instead,
they are done at each node in the AST just before invoking the constraint
checker for that node.)

## Resolving operator overloading

When you write "x + y", it could mean "add_int(x, y)" or "add_bits(x, y)" or
"add_real(x, y)".  The typechecker decides which one it means based on the
types of x and y. That is, it has a list of all the different overloadings that
are allowed (defined in the prelude.asl using __operator1 (unary operators) or
__operator2 (binary operators)) and it checks all the overloadings that "+"
supports. If there is a unique choice, we use that; if not, we report an error.

The result is a transformed AST where, for example, `Expr_BinOp "+" x y` has
been replaced with `Expr_TApply "add_int" [x; y]`.

## Resolving function overloading

When you write "F(x, y)", there might be several definitions of "F" that are
being called.  In ASL, the rule is that you look for the definition with the
same number of arguments and where each formal argument has the same "base
type" as the corresponding actual argument. If there is a unique function with
matching arguments, select it, otherwise error.  (Note that the result type is
not used in the selection process even if you know what type you want it to
return . This means that the selection process is entirely bottom-up from the
leaves of the AST tree to the root.)

[The "base type" has a different name in the ASL spec - I can't find what it is
at the moment. A base type is a type without the width parameter. eg the base
type of "bits(4)" is "bits(-)". So bits(4) and bits(N) have the same base
type.]

Within the typechecked function environment, a function name such as "F" is
given a unique suffix such as "0" or "1" to distinguish it from other functions
with the same name (but different argument types).  The result of resolving
function overloading is a transformed AST where a function name such as "F" has
been replaced by the unique name "F_0".

## Resolving array, bitslice and getter/setter syntax

We also disambiguate syntax like X[i] based on the type of X. The rule is something like

- if X is an array variable, this is is an array index expression and i should
  be integer (with a dynamic check that i is in range)

- if X is a bitvector, this is a bitslice expression and i should be integer
  (with a dynamic check that i is in range)

- if X is a getter function, then it is a function call

- (in L-expressions): if X is a setter function, then it is either a call to
  the setter function or, in some contexts, it is a call to the getter
  function, then a modification of the result then a call to the setter
  function.

Note: the actual rule used is a bit complex and fairly delicate: consult the source
or the ASL spec for the precise rule.

# Checking constraints

We have already seen constraints like requiring two types to be the same (in an if-expression).
Constraints are also used to check that bitwidths match.

If all our bitwidths are concrete like "bits(4)" or "bits(32)", then checking
constraints is easy: just check the base types and ("bits" == "bits"?) and then
check the bitwidths ("4 == 32"?)

If our bitwidths are symbolic like "bits(M)" and "bits(N)", then checking
constraints still consists of checking the base types ("bits" == "bits") and
the bitwidths ("M == N"). What changes is that we need a symbolic reasoning
engine of some form to check "M == N".  The components of this are:

1. We have a set of facts that we know about M and N and their relation to other constants and variables.

    eg we might know that M == T and N == 32 and T == 31 + 1 - from which we can conclude that "M == N"

    or we might know that "M == T" and "N == 32" - from which we cannot conclude that "M == N"

   These facts come from statements that we have previously processed.

2. We have the property that we want to check which might be something like "M == N" or "N + 1 = 32" or whatever

   This is usually a type that we have just synthesized.

To check whether the facts imply that the property holds, we need a symbolic checker.

## Checking by reduction to a normal form

One way we could do this (that ASLi does not use) is to try to simplify all
properties down to some "normal form" that can be directly compared.
Polynomials where all the coefficients are constants work pretty well for this.
eg `2*M - (N - 1)` could be represented by `(2 * M) + (-1 * N) + 1` and we
could compare two polynomials just by comparing corresponding coefficients.

Note that not everything can be reduced to polynomials so some parts of the
check need to be represented as "uninterpreted expressions".
For example, if we need to check that "M == Log2(N)", we cannot represent "Log2(N)"
as a polynomial so we just treat "Log2(N)" as if it was a variable name and we hope
that we don't need to know what "Log2" does to typecheck the code.

## Checking using an SMT solver

Another way (that ASLi does use) is to use a general purpose solver library such as an SMT solver.

When using an SMT solver, we construct queries such as

```
P: (implies
     (and
        (== M T)
        (== N 32)
        (== T (+ 31 1))
     )
     (== M N)
   )
```

and then we ask the SMT solver to prove that P always holds for any value of M, N and T.

We do this by asking the SMT solver to find all solutions of (not P). If it
cannot find any solutions, then we can conclude that P is always true.

An important performance optimization takes advantage of the fact that many
of the checks are of the form "32 == 32" or "M == N ==> M == N".
These checks are so trivial that we can easily answer them directly without having
to invoke the SMT checker.
(Time spent in the SMT checker can be as high as 90% of the total parse+typecheck
time so we should periodically check that we are not generating too many trivial checks.)

SMT solvers are much more powerful than the polynomial solver sketched above
but there are still limitations and we will still have to treat some
subexpressions as "uninterpreted expressions".


# Unification

Unification is a way of inferring type information that results in a type T and
a substitution s. As we perform type inference, any type information that we
obtain is collected in the substitution s.

Notes: 

- ASLi uses unification internally to implement the typesystem.
  For an earlier version of ASL, this was essential but it is not clear whether it is still necessary.

- Unification is complex and there are extensive comments in the codebase.
  I will not repeat all that information here.

## Overview

An example substitution is of the form

```
    $A = integer
    $B = bits(32)
```

which maps type variables $A and $B to some types.

The final result of typechecking is the result of applying the substitution s to T.
For example, if the type "T" is the tuple type `(boolean, $A, $B)`, then the final type would be
`(boolean, integer, bits(32))`.

## The unification algorithm

Unification of two types T1 and T2 creates a substitution and works as follows
(for a simple type system that only has tuples, integers and bitvectors)

- if T1 and T2 are both N-tuples, then unify corresponding fields of the tuples.
  (If they are different lengths, that is an error)
- if T1 and T2 are both integers, then they unify
- if T1 == "bits(M)" and T2 == "bits(N)" then add the constraint "M == N"
- if T1 is a type variable "$v" then add "$v = T2" to the substitution
- if T2 is a type variable "$v" then add "$v = T1" to the substitution
- (all other cases do not unify: report an error)

The result of unification is a substitution
and a set of constraints.

As can be seen, unification combines aspects of checking and synthesis.
If there is a structural mismatch, then checking fails.
If part of one of the types is a variable `$v`, we synthesize a binding of `$v`
to the corresponding part of the other type.


## Usage in the type system

The way that unification is used is that typechecking an expression constructs a type that
contains type variables together with a substitution and a set of integer
constraints (to be checked with the SMT solver).
If the constraints are all satisfied, we apply the substitution to the
type.

### Types and concepts used in unification

The main types/objects used in unification are

- "generic type variables".

  These are type variables that are distinct from type variables that occur in source code
  and have names like `$3` or `$9`.
  These are introduced when instantiating a function with a type like `{M,N}(M :: integer, bits(N)) => bits(M*N)`
  as part of the "book-keeping" required when figuring out the values of `M` and `N`.

- "generic types", "generic expressions", etc.

  These are types, expressions, etc. that may contain generic type variables.
  For example, the type `bits($3 + $9)` is a generic type.

- "substitutions".

  These map type variables to expressions.
  For example, a substitution might map `$3` to `24` and `$9` to `8`: `{ $3 -> 24, $9 -> 8 }`.

  Substitutions are produced at the end of unification and
  constraint solving when we figure out the bitwidths occuring in the code being typechecked.

  Substitutions can be applied to generic types such as `bits($3 + $9)` to get the
  non-generic type `bits(24 + 8)`.

- "unifiers".

  These collect together type information gathered during the typechecking process.

  These are fairly complex objects that collect together

  - an equivalence relation saying that two type variables are known to be equal
  - the substitution produced so far during unification
  - the set of constraints to be checked

  And whose methods include

  - creating generic type variables
  - adding equalities to the equivalence relation and substitution
  - checking constraints

### The unification "API"

The main functions that perform unification in the typechecker are

- `unify_subst_e s x` apply a substition `s` to a generic expression `x`
  resulting in a non-generic expression.

  The functions `unify_subst_le`, `unify_subst_ty` and `unify_subst_di` are similar
  but for L-expressions, types and initializer declarations.

- `check_type env u loc ty1 ty2` checks that `ty2` is a subtype of `ty1`.

  For example

  - checking `bits(M)` with `bits(N+1)` adds the constraint that `M == N+1`
  - checking `bits(M)` with `boolean` reports a type mismatch

- `with_unify env loc f` creates a unifier `u`, applies the typechecking function `f` to `u` and
  checks the constraints accumulated during typechecking.
  It returns a substitition `s` and the result `r` of `f u`.

  (`f u` is typically a generic type, generic expression, etc. and `unify_subst s r` represents
  the final result of typechecking that object.)

- `tc_expr env u loc x` typechecks expression `x` using local environment `env` and unifier `u`
  and returns the generic type of `x`.

  For example, it might typecheck `[p, q]` and produce the type `bits($3 + $9)`.


- `check_expr env loc ty x` typechecks an expression with a fresh unifier (using `with_unify`),
  and returns a non-generic type such as `bits(24 + 8)`.


- `instantiate_fun env u loc fty es tys`
  takes a function type `fty` like `{M,N}(bits(M), bits(N)) => bits(M+N)`
  and a list of argument types and instantiates the function type
  with the argument types.

  Returns a generic function type like `($3 :: integer, bits($9)) => bits($3 + $9)`

  In addition, the actual arguments `es` are used to handle any formal
  arguments of the function are also type parameters.
  e.g., in a function with type `{M, N}(x :: bits(N), M :: integer) => bits(M)`,
  the second argument of the function is used as the value for `M` in the
  type.



## Do we need to use unification?

Unification was used because an earlier version of ASL allowed bitwidth constraints to
be driven from the result type of an expression and functions with types like this were legal

```
func Zeros{M :: integer}() => bits(M)
```

This was challenging because the only way to infer the value of `M` is to look
at how the result of a call to `Zeros()` is used.
For example, if you write

```
let z :: bits(64) = Zeros(); // this is not legal in ASL 1.0
```

then (in an older version of ASL), you could infer the type parameter "64" to
the call to Zeros.
This required bitwidth information to be propagated from left-to-right and down the
abstract syntax tree: the sort of gymnastics that unification is ideal for.

Since ASLi was first written, ASL has been simplified by forbidding functions
where type parameters only occur in the result type. This allows all bitwidths
to be synthesized in a bottom-up manner based on the types of sub-expressions.
It is likely that this makes unification unnecessary but we have not done the
analysis needed to be certain that we can eliminate unification or how much benefit
(if any) this would bring.
