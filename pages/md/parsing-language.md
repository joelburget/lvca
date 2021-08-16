Back in my [September 23 update](/progress-september-23-2020/), I mentioned that I was working on a new language for parsing. Today I'm excited to show an initial (rough) version.

For now my goal is just to build a parsing library that _I_ want to use to build languages for this site. There are three ways in which I think I can improve on the alternatives.

### 1. Error communication

Writing good parser error messages is something of an art. A _very, very_ difficult art. I'm as guilty of writing bad error messages as anyone. The [Concrete / Abstract demo](/lambda-concrete-and-abstract/) just says ": end_of_input" if you forget a paren. The problem is that I can't figure out how to create a better message with the library I used.

Parser error messages seems to be an active research area, but I haven't seen any tools that make good messages easy (Menhir is perhaps the [closest I've seen](https://baturin.org/blog/declarative-parse-error-reporting-with-menhir/)). My approach, somewhat different from others I've seen, is to give up on writing nice messages and instead create a parser debugger, so you can interactively examine everything the parser tried. This is demoed below.

### 2. Readability

Let me just quote my example from September.

What I had (don't worry about understanding this -- the details don't matter):

```
(* Parse a lambda, eg "\x -> x" *)
let lam : (OptRange.t, Primitive.t) Nominal.term Parsers.t =
  pos
  >>= fun start ->
  lift4
    (fun _lam var _arr body ->
      let range = OptRange.extend_to (location body) start in
      let tm = Nominal.Operator (range, "lam", [ Scope ([ var ], [ body ]) ]) in
      tm)
    (char '\\')
    p_var
    (string "->")
    t
```

What I wanted:

```
let lam = '\\' var:p_var "->" body:t -> lam(var. body)
```

The second one is easier to read, right?

### 3. Provenance tracking

It's important for compilers to track where expressions came from. If there's a type error in an expression, like `1 + 'a'`, the compiler needs to be able to give a good message:

```
1 + 'a'
    ^^^
    Type error here!
```

To do this, we need each expression to hold metadata about where it came from, and it turns out this is usually a lot of work, but I want my parser library to do it for free.

## The structure of parsers

Now that I've given some reasons for building a new parsing library, let's explore it.

Before we start, a quick disclaimer. I _just_ wrote this, it's had no code review and no users other than myself. Don't be surprised if you find a bug (if you do, please [open an issue](https://github.com/joelburget/lvca/issues)).

Each of the tables you see below is an interactive demo, where you can test some input against a parser (try it!).

```demo
parser
```

## Conclusion

This parser language, while still a bit rough around the edges, (I think) shows promise.

1. It allows us to use an intuitive, regex-inspired syntax, but with all the power of parser combinator libraries like Parsec or Angstrom. I'm particularly fond of the syntax for parsing a sequence, naming some of the subparses.

2. Rather than attempt the Sisyphean task of writing good error messages for every possible case, we circumvent the problem by providing a (hopefully intuitive) debugger, so the use can explore the parser's actions and see what it was "thinking".

3. Terms parsed using this language have location information associated to them automatically. This frees the language designer from painstaking work.

This language is not the end goal, just a step in service of the larger project to explore how simple we can make it to build new (small) languages. That purpose in mind, I'm excited to both continue exploring extensions to this language, and using it as a tool to build languages for other purposes.

### Big Picture

One topic I'd like to address is how this fits in with other tools you'd use to implement a language. This library is written in OCaml, so in theory you could use it as the parsing frontend for a language also written in OCaml. But, to be frank, I wouldn't recommend it. This is a prototype and it has to mature before it would be a great experience to use for real work.

Besides the parser, there are a handful of other tools that are commonly needed for new languages. For example, a typechecker, interpreter (and debugger), etc. I'm actively working on more of these tools for LVCA. This is where I think the parser will really shine, as part of this set. I'm excited to share them with you.
