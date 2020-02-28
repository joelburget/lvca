# Language Verification, Construction, and Analysis

## Introduction

LVCA is a tool for building programming languages. It has an intentionally
small core. You create a language by specifying (1) its syntax, (2) its statics (ie typechecking rules), (3) its dynamics (ie how it evaluates), and (4) its concrete syntax.

LVCA then provides tools:
* parser
* pretty-printer
* interpreter
* typechecker
* Automatic serialization (to JSON or cbor)
* Content-identification of programs / expressions

Things that don't yet exist but can and should:

* A debugger
* Language-server protocol implementation
* Syntax highlighting / editor tools

## Example

First we define the abstract syntax of the language. This simple language only
has booleans and functions.

```
tm :=
  // a term can be a simple boolean literal
  | true()
  | false()

  // ... or a type-annotated term (holding a tm and a ty)
  | annot(tm; ty)

  // ... an if-then-else
  | ite(tm; tm; tm)

  // ... or a function or function application. Note the `tm. tm` syntax means
  // that we bind a `tm` in the body of the function (also a `tm`). Contrast
  // with `tm; tm` which means there are two `tm` children.
  | lam(tm. tm)
  | app(tm; tm)

ty :=
  // a type is either a bool
  | bool()
  // or an arrow between two types
  | arr(ty; ty)
```

The syntax we're using here comes from Robert Harper's [Practical Foundations
for Programming Languages](http://www.cs.cmu.edu/~rwh/pfpl/).

Next we define the typechecking rules for this language. We support expressing
typing rules in a
[bidirectional](http://davidchristiansen.dk/tutorials/bidirectional.pdf) style.
I'd like to add support for unification in the future.

```

----------------------- (bool intro 1)
ctx |- true() => bool()

------------------------ (bool intro 2)
ctx |- false() => bool()

      ctx |- tm <= ty
-------------------------- (annot)
ctx |- annot(tm; ty) => ty

ctx |- t1 <= bool()  ctx |- t2 <= ty  ctx |- t3 <= ty
----------------------------------------------------- (bool elim)
           ctx |- ite(t1; t2; t3) <= ty

    ctx, x : ty1 |- tm <= ty2
---------------------------------- (lam intro)
ctx |- lam(x. tm) <= arr(ty1; ty2)

ctx |- tm1 => arr(ty1; ty2)  ctx |- tm2 <= ty1
---------------------------------------------- (lam elim)
        ctx |- app(tm1; tm2) => ty2

// important: this rule must go last or else it will subsume all others
ctx |- tm => ty
--------------- (switch)
ctx |- tm <= ty
```

Lastly, we define the denotational semantics of the language.

```
meaning = \(tm : tm) -> match tm with {
  | true()          -> true()
  | false()         -> false()
  | annot(tm; ty)   -> meaning tm
  | ite(t1; t2; t3) -> case(meaning t1; true() -> meaning t2; false() -> meaning t3)
  | lam(x. body)    -> lam(x. meaning body)
  | app(fun; arg)   -> app(meaning fun; meaning arg)
}
```

Given all of these pieces, we can automatically produce an interpreter that
typechecks and evaluates expressions.

## About the name

1. LVCA is an acronym for Language Verification, Construction, and Automation

2. In biology, LUCA stands for Last Universal Common Ancestor -- the most recent common ancestor of all life on earth. LVCA occupies a somewhat analogous position as it can be used to implement any programming language.

3. I pronounce it "Luca".

# Build

LVCA is written in OCaml and built with [dune](https://dune.build/). So first
you need `opam` and `dune` installed, then:

```
opam install base bignum cbor core_kernel digestif ppx_jane re
dune build
```

# Test
```
dune runtest
```
