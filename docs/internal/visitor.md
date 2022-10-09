---
title: The visitor class
---

Classes are one of the defining features of OCaml (O is for Object) - but a lot
of OCaml code and a lot of ASLi does not use classes much except for one
particular use: visitor patterns.

The visitor pattern provides a very concise  way of implementing the many, many
recursive walks over the AST that a compiler needs.

It is particularly good for functions that need to do something with only a
small fraction of all the different types of AST nodes. eg just visiting
function calls or just visiting assignments, etc.

It is not so useful for functions that need to do something with almost every
one of the AST nodes. eg typechecking, evaluating and code generation.

# Overview of the visitor pattern.

The visitor pattern is defined in visitor.ml and instantiated for the ASL AST
in `asl_visitor.ml`. If you look at `asl_visitor.ml`, you will see lots of
recursive functions that do nothing but call functions to visit all the child
nodes - that is, the usual pattern of code that you find in compilers.  What
makes this powerful is

- on each node, it calls a method specific to that type of node (eg "vexpr" to
  visit nodes with type "Expr"). This method can do one of four things
  (see visitAction in visitor.ml)

    - recursively visit the children and reconstruct the node based on the
      values returned from visiting the children.  This is the common case and
      `asl_visitor` implements an important optimization for the case the the
      value returned is identical (ie "the same pointer") to the original

    - stop recursing and just return the node.  (This is often done as an
      optimization)

    - replace the node with some other node. eg in an optimizer, we might
      replace "x + 0" with "x"

    - change the node to some other node, then recursively visit the children,
      then apply a function to the result. (This is very, very rarely used -
      but when it is needed, it is really useful. There are only 4 uses in the
      xforms at the moment)

- The class mechanism makes it easy to

    - define a visitor (nopVisitor) that just recursively visits every node doing nothing

    - override some methods of that visitor to do something to/with the nodes as they are visited.


# Usage

There are two main patterns of use (and some hybrid patterns)

1) Collect information about a piece of AST(usually without changing the AST)

   Most of the 12 classes in `asl_utils.ml` are of this type

2) Transform a piece of AST

   Most of the classes in the xform files are of this type

## Collecting information (analyses)

An example of collecting information is producing the set of all functions used
inside a function, expression or statement. (callsClass in `asl_utils.ml`.)

There are maybe 50 or more different kinds of node that can occur in a
function: many types of statement, many types of expression, many types of
types, ... But the only nodes we care about are function calls, procedure
calls, getter calls and setter calls. After typechecking, these are one of
`Expr_TApply`, `Stmt_TCall`, `LExpr_Write` or `LExpr_ReadWrite` - the other 50 or more
nodes are uninteresting.

If we take `Expr_TApply`, the minimum code we can imagine having to write is a
pattern match followed by an action (adding the function name to the set we are
collecting).

```
| Expr_TApply (f, _, _) ->
          calls <- IdentSet.add f calls;
```

If you look at callsClass in `asl_utils.ml`, you will see that the entire handler for expressions is the following code:

```
method! vexpr =
   function
   | Expr_TApply (f, _, _) ->
       calls <- IdentSet.add f calls;
       DoChildren
   | _ -> DoChildren
```

This overrides the default method in nopAslVisitor with

- a case to handle the one kind of expression that we are interested in and
  then recurse into the function parameters and arguments

- a default case to recurse on every other kind of expression

The other three interesting kinds of statement and L-expression are handled by
similar code. A total of 30 lines. If we had written the more conventional nest
of recursive functions, we would have at least 500 lines in which it is really
hard to find the interesting cases that actually do something, which almost
certainly contain bugs and which are painful to maintain as we modify the AST
nodes.
