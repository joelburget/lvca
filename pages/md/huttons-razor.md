Now that we’ve talked a bit on what this site is about, and how hard it is to build a language that people want to use, I think it’s time to write our first (simple) language.
The language we'll use is Hutton's Razor, a minimal language that has exactly two types of expressions: integer literals and addition.
There are four parts to our definition of the language: abstract syntax, concrete syntax, statics, and dynamics. I'll explain exactly what I mean by these terms one-by-one.

### Abstract syntax

We need to first describe the abstract syntax of the language. As we said,
Hutton's Razor has two types of expressions: integer literals and addition
(which holds two sub-expressions).

```
import {integer} from "lvca/builtin/integer"

expr :=
  | lit(integer())      // an expression can be a literal integer
  | add(expr(); expr()) // or the addition of two expressions

type := int() // there's only one type in the language
```

A few notes:
* We're defining the *sorts* `expr` and `type`, as well as the *operators*
  `lit`, `add`, and `int`. `integer` is predefined.
* In LVCA it's conventional to define at least the `expr` (or `term`) and `type` sorts. In more complicated languages there will be more sorts, but we almost always want at least expressions (terms) and types.
* You may wonder about the syntax. We're using the syntax that Robert Harper uses for abstract binding structure in his book [Practical Foundations for Programming Languages](http://www.cs.cmu.edu/~rwh/pfpl/). It's described in more detail in the [introduction](/introduction).

### Concrete syntax

Just by defining the abstract syntax, you can write `add(add(lit(1); lit(2)); lit(3))` to define a term. This is a standardized syntax that works for any language (great for debugging).

But it's much more natural for humans to write `1 + 2 + 3`. This means that we have to define a *parser*.

```
import integer from "lvca/builtin/integer/parser"
import foldl from "lvca/prelude/foldl"

// an expression can be parsed as one of:
expr : expr parser
  // an integer literal
  = integer-literal
  // or a sequence of "+"-separated expressions
  | (e=expr es=("+" e2=expr -> e2)* -> {foldl e es (\l r -> add(l; r))})
  // or a parenthesized expression
  | ("(" e=expr ")" -> e)

type : type parser
  = "int"
```

We're writing this concrete syntax in a language built for parsers that I defined in LVCA. More on this later.

### Statics

Now we define that static semantics of the language. These are the typechecking rules used to check that expressions are well-formed.
In this case the rules are extremely simple: everything (both integer literals and addition) is an `int`.

```
--------------------------
ctx >> lit(_) => int()

-----------------------------
ctx >> add(_; _) => int()
```

We're using an algorithm called bidirectional typechecking (which I'm not going to talk about in this article), but the way to read these rules is that both a `lit` and `add` always have type `int`. Bidirectional typechecking requires us to supply a rule for each operator in the language (that's the simplified story -- the details will have to wait). Since we've done that, LVCA can construct a typechecker automatically.

### Dynamics

```
dynamics = \(expr : expr()) -> match expr with {
  | add(a; b) -> #add(dynamics a; dynamics b)
  | lit(i) -> i
}
```

TODO: this is outdated:

Finally, we get to the piece that makes programs actually run. Here we define a function, `dynamics`, mapping our terms to another language. In this case, the codomain (target language) is _core_ (which also happens to be the language `dynamics` is defined in).

Dynamics allow us to express the meaning of our language in terms of another. Typically the second language is well-understood or simpler than the language we're defining. Integers definitely qualify as well-understood. More typically, we might want to define semantics in terms of something like assembly language (or JavaScript).

## Writing an interpreter

Now we've done a lot of work, what do we have to show for it?

It's possible to build a lot of tools that can function given just the abstract syntax:

* A term viewer, which displays the abstract syntax of a term with collapsible
  subtrees.
* Given a set of terms, we can query for any that match a pattern. For example,
  `add(_, lit(_))` would match the term we wrote earlier, `add(add(lit(1);
  lit(2)); lit(3))`. It's not very interesting in this language, but this kind
  of querying becomes much more interesting for writing linters and refactoring
  tools in a more fully-featured language.
* Content-addressed storage. LVCA has built-in capabilities for hashing a term
  to a SHA-256 sum, which uniquely identifies it. Any term built using LVCA has
  a globally unique identifier which can be used to store and retrieve it.

By additionally defining concrete syntax, LVCA is able to produce both a parser
and pretty-printer. This opens the door to some very interesting possibilities,
which I'll only tease here. You don't necessarily need to use the same concrete
syntax as other people you collaborate with. I can use one language while you
use another, both for the same abstract syntax. Finally that guy on your team
who always wants to use lisp can have his parens.

Statics give us a typechecker. This is important for checking our terms are
well-formed before evaluating them.

Finally, dynamics allow for evaluation. If dynamics map your language to LVCA
core, then you can step through evaluation one step at a time (ie LVCA produces
a debugger). Otherwise, you can take the resulting program and evaluate it in
your language of choice.

Defining all four (abstract syntax, concrete syntax, statics, and dynamics)
allow us to build a compiler and interpreter.

## A language less monolithic

I've been talking about some consequences of a concept that I haven't
explicitly said yet. This is important so I want to make this blindingly clear.
In LVCA, a language is not monolithic. It's composed of pieces. We can swap out
one concrete syntax for another. We can opt to not define any dynamics. Or we
can define multiple different dynamic semantics mapping to different languages.

The point is, we have the vocabulary to understand languages, to change them,
to relate two languages, or to evolve one.
