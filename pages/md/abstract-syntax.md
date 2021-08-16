## Motivation

The goal of LVCA is to build tools that work for any programming language. To do so, we need a way to way to manipulate abstract syntax trees, regardless of the concrete syntax that end-users write. For example, say we have two languages: (1) the lambda calculus (`(\x -> x) (\y -> y)`), and (2) arithmetic (`x + y * z`). For the purpose of LVCA tools, we could write these terms with the same abstract syntax notation as `ap(lam(x. x); lam(y. y))` and `add(x; mul(y; z))`. I'll explain the details in a moment, for now the point is that this syntax is meant to be universal -- to be capable of representing the abstract syntax for any language -- and that's what makes LVCA tooling possible.

## Introduction

The first step when defining a language with LVCA is to declare the structure of its abstract syntax trees. This is the most foundational part of LVCA. Thankfully it's also the simplest part, and we should be able to cover everything in one post.

Let's start with an example.

```Figure 1: Declaration of a language with functions and booleans
tm :=
  | true()
  | false()
  | ite(tm; tm; tm)
  | annot(tm; ty)
  | app(tm; tm)
  | fun(tm. tm)\n
ty :=
  | bool()
  | arrow(ty; ty)
```

This language introduces two *sorts*, `tm` and `ty`. Each sort defines a set of operators. For `tm`, these are `true`, `false`, etc. We can construct a *term* using these operators. For example, `true()`, `ite(true(); false(); true())`, or `annot(true(); bool())` are all valid ASTs of sort `tm`. Notice that we use a semicolon to separate subterms.

However, in the declaration of the `fun` operator, the two sorts are separated by a dot instead of a semicolon. This means that instead of two subterms, we have one subterm (`tm`), that binds a `tm` variable. For example, valid functions include `fun(x. x)` and `fun(x. ite(x; false(); true()))`. In both examples we're binding the variable `x` (of sort `tm`), though it could be named anything, and using it in the term body.

For another example, `case(tm; tm. tm; tm. tm)` has three subterms. The first is just a `tm` but the second and third both bind a `tm` and are theselves of type `tm`. An example AST using this operator is `case(true(); x. x; y. y))`.

One important note is that we never explicitly include variables in the declaration of the language. That is, we don't have to say "a term is one of `true()`, `false()`, ..., *or a variable*". All sorts implicitly allow for variables. However, it's not always possible to construct a valid AST using a variable of some sort. For example, we can bind a variable of sort `tm` (`fun(x. x)`), but it's impossible to build a valid AST with a variable of sort `ty`, since `ty` has no binding sites in the language declaration.

## Declaring and using ASTs

I want to pause for a moment to make an important point. We've talked about two closely related, but different languages in this post. First, the language for declaring the abstract syntax of a language (`tm := true() | ...`), and second, the language for writing abstract syntax trees (`fun(x. x)`). ASTs (the second language) are made of the operators declared in the first language.

Typically, users of a language want to use a nicer syntax than the one we've shown for ASTs. We prefer to write `((\x -> if x then false else true) : bool -> bool) true` rather than `app(annot(fun(x. ite(x; false(); true())); arrow(bool(); bool())); true())`. LVCA includes a tool for defining concrete syntax, but you also get this abstract syntax notation for free. You can think of it as a default to use before you've defined concrete syntax, or as an unambiguous notation that can be used for tooling and debugging.

## Arity / Valence

We've defined a set of operators, and each has a sort of schema of subterms which it can hold. We call this the *arity* of an operator. An arity is a (possibly-empty) list of *valences*, where each valence is 0 or more sorts, representing the sorts of bound variables, plus one sort representing an argument to the operator. Let's look at some examples.

* `tm` is a valence representing a `tm` argument
* `tm. tm` is a valence where the argument binds a `tm` variable
* `tm. ty. tm` is a valence where the argument binds both a `tm` and `ty` variable
* `(tm)` is an arity for an operator with a single `tm` argument
* `(tm; tm. tm; tm. tm)` is an arity for an operator with three arguments. The second and third both bind a variable.

## Variable Arity / Valence

It's sometimes the case that an operator can have a variable number of arguments. The main motivating example is branches in a pattern match. We want a programmer to be able to write as many of these lines as they like:

```Figure 2: Pattern matching
match x with
  | [a] -> ...
  | [a; b] -> ...
  | [a; b; c] -> ...
  | [a; b; c; d] -> ...
  | _ -> ...
```

To allow for this we introduce the notion of *variable arity*. Similarly, each of those branches binds a different number of variables! To allow for this we need to introduce the notion of *variable valence*. With both variable arity and variable valence it's possible to have a variable number of subterms and a variable number of... variables. Which means we write languages with pattern matching. Let's see a fragment of a language using these features (TODO: cite pfpl).

```Figure 3: Variable arity / valence rules
rules := rules(rule*)
rule  := rule(pattern; expr*. expr)`}
```

There can be zero or more rules, where each rule contains a pattern and an expression with zero or more bound variables, coming from the pattern.

## Imports Primitives, and Sequences

That's almost everything there is to know about abstract syntax. Unfortunately the language as stated so far would be awfully cumbersome to use, since it doesn't allow for, eg, string and integer literals.

This is kind of a tricky point, as all terms so far are inductively defined, and we'd like to keep this property as far as possible. However, I want users to be able to type `"Hello 🌎"` or `1729` rather than the inductive version of each. So we compromise, and allow these two common types to be imported:

Like integers and strings, sequence types (like lists or arrays) are ubiquitous, so we build in special syntax for them as well. You can write, for example, `[1, 2, 3]`.

```Figure 4: Imports and primitives
expr :=
  | lit-int(integer)
  | lit-string(string)
  | add(expr; expr)
  | concat(expr; expr)
```

Of course, `"builtin"` isn't the only thing you can import, but we'll talk about that another day. We'll also cover how you can create a nice concrete syntax (`1 + 2 + 3`) to go with your abstract syntax (`add(add(1; 2); 3)`), and how to typecheck and evaluate a language.
