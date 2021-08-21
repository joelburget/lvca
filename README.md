# Language Verification, Construction, and Analysis

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fjoelburget%2Flvca%2Fmain&logo=ocaml&style=for-the-badge)](https://ci.ocamllabs.io/github/joelburget/lvca)

## Introduction

LVCA is a toolkit for building programming languages. There are two main activities in LVCA -- (1) defining the abstract syntax for a language and (2) mapping between languages.

### Abstract syntax

We can, for example define the abstract syntax for the lambda calculus.

```
term := Lam(term. term) | App(term; term)
```

This language definition defines a new _sort_, `term`. It says that a `term` is either a `Lam` or an `App`.

A `Lam` (lambda abstraction) binds a variable of sort `term` within an
expression of sort `term`. For example, the classic identity function (`\x ->
x`) looks like `Lam(x. x)`. An `App` (function application) holds two subterms
of sort `term`. We can apply the identity function to itself: `App(Lam(x. x);
Lam(x. x))`.

Aside: If you're familiar with a language with algebraic datatypes (like
Haskell, OCaml, Rust, etc), then this ought to look familiar. We've just
defined a sums-of-products-style datatype. We can work with these as you'd work
with algebraic datatypes: by constructing them and pattern-matching against
them. However, sort declarations generalize algebraic datatypes because they
have a notion of binding structure.

Let's try a different example.

```
string : *
primitive : *
list : * -> *

term :=
  | Operator(string; list term)
  | Primitive(primitive)
```

This definition says that a `term` is either an `Operator` (which holds both a
`string` and `list term`) or a `Primitive` (which holds a single `primitive`).

We've also declared three _external sorts_: `string`, `primitive`, and `list`.
These are sorts that are assumed to exist but will not be defined in our
language.

Note that in each case we've just defined the _abstract syntax_ of the language (not the _concrete_ syntax). We can also define the concrete syntax via a parser and pretty-printer, but for now, we'll work with just the abstract syntax.

With a language definition like either of the above, LVCA can provide some nice tools:

* We can [view the binding structure](https://lvca.dev/binding-viewer/) of a term.
* We can write a query for a given pattern over a codebase. For example, we could search for all lambda abstractions with the pattern `Lam(_)`. Or we could search for all identity functions with `Lam(x. x)`. Important note: this pattern will match `Lam(y. y)` or any other variable name.
* Similarly, we can even rewrite parts of our codebase.

### Mapping between languages

Once we've defined syntax, the real fun is mapping between languages. For
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

This is a function of type `term -> real`, meaning it interprets terms as
reals. This function defines the _semantics_ of terms by translation to another
language.

Now, if we can evaluate `real` expressions (and we can evaluate the translation from `term` to `real`), then we can evaluate `term`s.

One final thing we might want to do is lift `real`s back to `term`:

```
\(real: real) -> Real_expr(real)
```

Now, since we have a `term -> real` and a `real -> term`, we can compose them (with the real evaluator `real -> real` in the middle) to get a term evaluator of type `term -> term`.

## About the name

1. LVCA is an acronym for Language Verification, Construction, and Automation

2. In biology, LUCA stands for Last Universal Common Ancestor -- the most recent common ancestor of all life on earth. LVCA occupies a somewhat analogous position (maybe that's a stretch) as it can be used to implement any programming language.

3. I pronounce it "Luca".

## Subpackages

LVCA is composed of several subpackages. Topologically sorted by dependencies:

* `util`: A few utilities used in the rest of the packages. Mostly extensions to Jane Street's [base](https://ocaml.janestreet.com/ocaml-core/latest/doc/base/index.html).
* `provenance`: Types to represent _provenance_, ie where did a term come from.
* `parsing`: Extensions to [Angstrom](https://github.com/inhabitedtype/angstrom), used in the rest of the packages for parsing.
* `syntax`: The most important package -- contains representations for the core LVCA data types.
* `core`: Definition of a "core" language.
* `bidirectional`: Experimental library for defining bidirectional typechecking schemes.
* `constructive-real`: Constructive / computable real numbers.
* `crowbar`: Defines a binary for property checking.
* `syntax_quoter`: Utilities used by both `ppx_lvca` and `ppx_lvca_core`.
* `ppx_lvca`: An OCaml ppx for easily defining languages and terms.
* `ppx_lvca_core`: An OCaml ppx for easily defining _core_ language terms.
* `languages`: Example languages built with LVCA.
* `pages`: Web pages (many available at lvca.dev).

## Build

LVCA is written in OCaml and built with [dune](https://dune.build/). So first
you need `opam` and `dune` installed, then:

```
make install-deps
dune build
```

The `make` commands are available only from the project root.`dune build` can be run from the project root or any of the subpackages.

### `pages`-specific:

To produce JS files small enough to put online, run in release mode. Optionally, also compress with [terser](https://terser.org/):

```
dune build --profile=release
terser -c toplevel,sequences=false,drop_console=true --mangle -- _build/default/pages/0x-huttons-razor/main.bc.js > out.js
```

## Test

From the top level or any subpackage (`syntax`, `core`, etc):

```
dune runtest
```

From the top level:

```
make lint
```
