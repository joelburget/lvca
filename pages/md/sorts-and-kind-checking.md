I recently made two related changes to the sort / kind system in LVCA.

## Sorts

Previously the syntax for sorts required `()` for even unary sorts. For example, the integer sort, taking no arguments, was written `integer()`. Similarly `string()`, etc. This was motivated by consistency with the syntax we use for terms, where an operator is always constructed with `()`. Likewise, for consistency with terms, variables could be bound, but these were not constructed with `()`. For example (old syntax):

```
example(a) :=
  // An operator, first, taking an a
  | first(a)
  // An operator, second, taking an integer
  | second(integer())
```

However, this has two problems -- one large and one small.

The first problem is purely ergonomic. It's a pain to always write `integer()` and I found myself often forgetting the parens, to be reminded by the parser.

The second problem is more substantial -- it's inconsistent. Sometimes unary sorts are written with parens but sometimes they're not. It just depends on whether the sort is bound (as in `example(a)`, binding `a`). But a unary sort should just be a unary sort.

Because of these two considerations I've changed the sort syntax. There are variables, application (via juxtaposition), and parens for grouping. Here are three sorts for example.

```
integer // a unary sort
list integer // list applied to integer
foo (a b) string // foo applied to (a b) and string
```

And this is the new syntax for the previous example:

```
example a :=
  | first(a)
  | second(integer)
```

## Kind Checking

This leads me to kind checking. We want to make sure that sorts have a consistent *kind* everywhere. That is, a given sort's arity should always be the same. For example, if we use `a` by itself, the kind checker will infer that it's unary -- `a : *`. If we apply it to an argument, `a integer`, the kind checker infers that it has kind `* -> *`. If we use it both ways in the same program, the kind checker will complain.

This is implemented today. One thing I'd like to add in the future is optional kind annotations, so you could write:

```
integer : *
list : * -> *

foo (a : *) :=
  | list(list integer)
  | a(a)
```

Here I've declared the types of two external dependencies, free variables which need to be linked in. The `foo` sort we're declaring also takes an `a` argument, which has kind `*`. If we don't use annotations the kind checker will infer them, but I think it would be nice to have the option to make kinds fully explicit.

Finally, I'll note that users of Haskell will notice a similarity to GHC's [kind signatures](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/kind-polymorphism.html), but LVCA doesn't support kind polymorphism or dependent types for that matter. Each sort has a fixed kind and sorts are not *indexed*. I don't anticipate the kind system ever becoming more complicated.


