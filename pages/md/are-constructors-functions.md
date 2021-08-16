## Haskell

In Haskell, constructors appear to be functions.

```
Prelude> :t Just
Just :: a -> Maybe a
Prelude> :t (:)
(:) :: a -> [a] -> [a]
Prelude> foldr (:) [] [1,2,3]
[1,2,3]
```

But are they? Functions can do arbitrary computation. Constructors just build a cell in memory [1]. Functions can't be pattern-matched, but constructors can. Functions have lower-case names, but constructors are upper-case (ignoring operators).

```
foo (Just x) = x
foo Nothing = 0
```

## OCaml

In OCaml, patterns are not functions.

```
utop # (+);;
- : int -> int -> int = <fun>

utop # None;;
- : 'a option = None

utop # Some;;
Line 1, characters 0-4:
Error: The constructor Some expects 1 argument(s),
       but is applied here to 0 argument(s)

utop # List.fold_right [1;2;3] ~init:[] ~f:(fun x xs -> x::xs);;
- : int list = [1; 2; 3]

utop # List.fold_right [1;2;3] ~init:[] ~f:(::);;
Line 1, characters 36-40:
Error: The constructor :: expects 2 argument(s),
       but is applied here to 0 argument(s)
```

We have to explicitly write out a function which takes two arguments and applies a constructor (`fun x xs -> x::xs`). We can't use the same punning as in Haskell (`foldr (:)`).

## Tradeoffs

Haskell is surely more convenient -- I wish I could write `~f:(::)` in OCaml instead of the verbose `~f:(fun x xs -> x::xs)` (there are [OCaml extensions](https://github.com/janestreet/ppx_variants_conv) to derive "constructor functions" for you). It's nice to be able to partially-apply constructors.

OCaml is also inconsistent in some ways:

* `x |> f` is equivalent to `f x`, but only for functions. `1 |> Some` doesn't compile, but `Some 1` does.
* Constructor arguments can sometimes appear as a single value (I can pattern match on `(::) _`), but sometimes not (I can't apply `(::)` to a tuple).

```
utop # (::)(1, []);;
- : int list = [1]

utop # let pair = (1, []);;
val pair : int * 'a list = (1, [])

utop # (::)pair;;
Line 1, characters 0-8:
Error: The constructor :: expects 2 argument(s),
       but is applied here to 1 argument(s)

utop # (function | (::) _ -> "::" | [] -> "[]") [1; 2; 3];;
- : string = "::"
```

Meanwhile Haskell is inconsistent in others: function and construction application look the same, but functions do _computation_, while constructors (_simple_ constructors, ignoring extensions) can only construct a cell.

## The Point

My point is that, even in something as simple and seemingly well understood as functions vs constructors, there are viable systems which have made both choices. Both are valid -- each has pros and cons.

To my mind, Haskell is no doubt more convenient (this extends to other aspects of data types as well -- GHC's deriving is built-in, while OCaml requires a [ppx](https://github.com/ocaml-ppx/ppx_deriving) to do the same). But OCaml's inconsistencies have a distinctly shallow, syntactic flavor. Haskell's inconsistency is deeper -- conflating _constructors_ and _computations_.

1: Like everything in GHC, the story is actually more complicated, since view patterns and pattern synonyms can do arbitrary computation.
