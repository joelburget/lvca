# Language Verification, Construction, and Analysis

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fjoelburget%2Flvca%2Fmain&logo=ocaml&style=for-the-badge)](https://ci.ocamllabs.io/github/joelburget/lvca)

## Introduction

LVCA is a toolkit for building programming languages. There are two main activities in LVCA -- (1) defining the abstract syntax for a language and (2) mapping between languages.

### Abstract syntax

We can, for example define the abstract syntax for the lambda calculus.

```
term := Lam(term. term) | App(term; term)
```

This says that a term is either a lambda abstraction or an application of two terms.

That's very simple. Let's define something slightly different.

```
string : *
primitive : *
list : * -> *

term :=
  | Operator(string; list term)
  | Primitive(primitive)
```

In this language a term is either an operator or a primitive.

### Mapping between languages

Now that we've defined syntax, the real fun is mapping between languages. For
example, say we have a language which combines the lambda calculus with
real-valued expressions.

```
real : *

term :=
  | Lam(term. term)
  | App(term; term)
  | Real_expr(real)
```

Now we can define a mapping to reals:

```
\(term: term) -> match term with {
  | Lam(_) -> tm
  | App(f; arg) -> match reduce f with {
    | Lam(x. body) -> body[f := reduce arg]
    | f' -> {App(f'; {reduce arg})}
  }
  | Real_expr(expr) -> expr
}
```

This is a function of type `term -> real`, meaning it interprets the term
language in terms of reals. This function defines the _semantics_ of terms, by
translation to another language.

## About the name

1. LVCA is an acronym for Language Verification, Construction, and Automation

2. In biology, LUCA stands for Last Universal Common Ancestor -- the most recent common ancestor of all life on earth. LVCA occupies a somewhat analogous position (maybe that's a stretch) as it can be used to implement any programming language.

3. I pronounce it "Luca".

# Build

LVCA is written in OCaml and built with [dune](https://dune.build/). So first
you need `opam` and `dune` installed, then:

```
make install-deps
dune build
```

To produce JS files small enough to put online, run in release mode. Optionally, also compress with [terser](https://terser.org/):

```
dune build --profile=release
terser -c toplevel,sequences=false,drop_console=true --mangle -- _build/default/pages/0x-huttons-razor/main.bc.js > out.js
```

# Test

From the top level or any subpackage (`syntax`, `core`, etc):

```
dune runtest
```

From the top level:

```
make lint
```
