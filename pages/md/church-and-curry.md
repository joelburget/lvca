When defining a programming language and its typing rules there are two main
approaches:

1. The *intrinsic* approach, also called Church-style (named after [Alonzo
   Church](https://en.wikipedia.org/wiki/Alonzo_Church), discoverer of the
   lambda calculus). In this approach, every term in a language *intrinsically*
   has a type which we can check. Some languages which exemplify this approach
   are Haskell, OCaml, Agda, and Coq. Branching out from functional languages,
   C / C++ and Java are also Church-style because terms have built-in types.

2. The *extrinsic* approach, also called Curry-style (named after
   [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry), founder of
   combinatory logic). In this style, a term in a language don't necessarily
   have a built-in type, though you may still be able to typecheck it. That is,
   types classify (untyped) terms.  Most lisps are good examples of this
   approach. So is [NuPRL](https://www.nuprl.org/).

## In LVCA

The reason I bring up this distinction is to clarify my thoughts on the two
approaches as they relate to LVCA.

A design decision that's been nagging at me for a while is, how are terms
stored in LVCA? Is it Church-style, with statics attached? Or is it
Curry-style, just the term without reference to its statics?

I recently came to a conclusion that I'm happy with, realizing that allowing
terms to be stored independently of their statics is strictly more general. For
example, suppose we have some term `t`, typed by some statics `s`.  Now, if we
want to operate à la Curry, we can simply store the term `t`. Then `t` exists
as an essentially untyped term in our store. Yet if we have a statics `s`, it's
possible to check `t` against `s`.  However, if we prefer to operate à la
Church, we can define `typed(term; statics) = typed(term; statics)` and store
the term `typed(t; s)`. Now our term is defined with reference to its statics,
and we can check that it's a valid term if it typechecks.

## References

* [Eric Normand, Church vs Curry Types](https://lispcast.com/church-vs-curry-types/)
* [intrinsic and extrinsic views of typing (nLab)](https://ncatlab.org/nlab/show/intrinsic+and+extrinsic+views+of+typing)
