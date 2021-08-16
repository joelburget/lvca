## Introduction

Algebraic datatypes (ADTs) are core to typed functional programming. In their most basic form, ADTs are *sums of products*. For example:

```
data These a b = This a | That b | These a b
```

A type (`These`) is a sum of constructors (`This`, `That`, `These`), each of which contains a (possibly empty) product of types.

Today I'll briefly cover several familiar extensions to ADTs, as well as an extension I haven't seen talked about before.

## GADTs, Inductive Types, and Higher Inductive Types

Generalized algebraic datatypes (GADTs) allow one to explicitly write the types of data constructors. The example above would be written:

```
data These a b where
  This :: a -> These a b
  That :: b -> These a b
  These :: a -> b -> These a b
```

Allowing explicit type annotations allows TODO do we really want to cover all this?

```
data Expr a where
    EBool  :: Bool     -> Expr Bool
    EInt   :: Int      -> Expr Int
    EEqual :: Expr Int -> Expr Int  -> Expr Bool
```

TODO: Inductive, higher inductive types
https://cstheory.stackexchange.com/questions/10594/whats-the-difference-between-adts-gadts-and-inductive-types
https://en.wikipedia.org/wiki/Inductive_type

## Binding Datatypes

In Practical Foundations For Programming Languages (PFPL), Robert Harper uses *Abstract Binding Trees* TODO

Let's see an example.

```
expr :=
  | lam(expr. expr) // lambda: \x -> ...
  | app(expr; expr) // function application
```

The first constructor, `lam`, represents a lambda, and has binding structure. Its definition says it holds a single `expr` which binds a single `expr` variable (the `.` represents binding).

The second constructor, `app`, represents function application. Its definition says it holds two `expr`s (the `;` separates children). This constructor has no binding so it could be represented by in a regular ADT.

Let's see a slightly more involved example (this is the language `E` from PFPL)

```
exp :=
  | num(int)
  | str(string)
  | plus(exp; exp)
  | times(exp; exp)
  | cat(exp; exp)
  | len(exp)
  | let(exp; exp. exp)
```

Everything here could be written

## Binding-Aware Patterns
