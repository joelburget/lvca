I've been working on two projects I'd like to give an update about.

## `ppx_lvca`

The purpose of this ppx is to translate LVCA-stuff into OCaml. For example

```
let test_nominal = [%lvca.nominal "foo(x. x)"]
```

[becomes](https://github.com/joelburget/lvca/blob/8d1282163623b3541eef021cdff92865890b0563/ppx_lvca/test/test.expected.ml)

```
let test_nominal =
  Nominal.Term.Operator
    ((Some (let open Lvca_provenance.Range in { start = 0; finish = 9 })),
      "foo",
      [Nominal.Scope.Scope
         ([Pattern.Var
             ((Some
                 ((let open Lvca_provenance.Range in
                     { start = 4; finish = 5 }))), "x")],
           (Nominal.Term.Var
              ((Some
                  ((let open Lvca_provenance.Range in
                      { start = 7; finish = 8 }))), "x")))])
```

That's a simple example of a term. It gets a bit more complicated when talking about a language definition:

```
module Lang =
[%lvca.abstract_syntax_module
{|
integer : *
string : *

foo :=
  | Foo(integer)
  | Bar(foo[foo]. foo. foo)
|}
```

This [outputs](https://github.com/joelburget/lvca/blob/8d1282163623b3541eef021cdff92865890b0563/ppx_lvca/test/test.expected.ml) a bunch more code. I'm not going to link the whole thing here (it's long), but the result is a module, with an OCaml type definition:

```
type 'info foo =
  | Foo of 'info * 'info Integer.t
  | Bar of 'info * ('info Pattern.t * string * 'info foo)
```

We also include a simpler `Plain` module without the `info` parameter.

```
module Plain = struct
  type foo =
    | Foo of Integer.Plain.t
    | Bar of (Pattern.Plain.t * string * foo)
end
```

But most of the code is defining functions like `info`, `map_info`, `equal`, `to_nominal`, and `of_nominal`.

This is all now working, except for `to_nominal` and `of_nominal`. Defining those two functions gives a bunch of other stuff for free -- (de)serialization, parsing, pretty-printing, etc.

## Core language

The purpose of the core language is mostly mapping between languages. For example, translating a higher-level language to a lower-level one.

My near-term goal is to use it to translate terms (in any language, but the simplest example is the lambda calculus) to the document language.

```
let rec to_doc \(tm : lc) ->
  let children =
    match tm with {
      | Lam(x. body) -> Cons(name x; Cons(to_doc body; Nil()))
      Concat(
      | App(t1; t2) -> Cons(to_doc t1; Cons(to_doc t2; Nil()))
    }
  in
  let desc = Concat(children) in
  Inline(desc; Nil())
```

As an aside, this shows something interesting -- it's not good to translate directly from a language (or at least one that can nest) to a document -- we need a pretty-printing step, otherwise we're going to end up with deeply nested `Inline`s but no structure.

Now both the abstract syntax and core languages have notions of externals, they ought to both have a notion of linking. Is this the same thing? For core, it should definitely be called *linking*. For abstract syntax, I'm not sure. Union? But it's not symmetric -- there's a notion of dependency.
