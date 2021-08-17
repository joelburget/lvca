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

### Fixed character and string parsers

Let's start with the simplest class of parsers, which accept a single character or a fixed string.

#### `.`

The parser `.` accepts any single character.

<Demo any-char />

#### `'c'`

A single-quoted character accepts exactly that character. Note that this example, like many of the others is (intentionally) failing initially. Try changing the input so it's accepted.

<Demo char />

#### `"str"`

Similarly, a double-quoted string accepts exactly that string.

<Demo string />

#### Debugging

You've probably noticed the *debugger* rows below each parse result. By toggling this row you can see the steps the parser took to consume an input (or not). For the parsers we've seen so far, it's always exactly one step, but as soon as we get to *repetition* below, that will change. But this tool will really become useful when we get to `choice` and `fix`.

### satisfy

Fixed characters are awfully limiting. `satisfy` parses a single character that satisfies some predicate. The available predicates are `is_digit`, `is_lowercase`, `is_uppercase`, `is_alpha`, `is_alphanum`, and `is_whitespace`.

<Demo satisfy1 />

<Demo satisfy-is-alpha />

<Demo satisfy-is-digit />

For convenience, I'll leave the last two parsers in scope as `alpha` and `digit`, so we can use them later on.`

You might wonder, what's the syntax inside the `satisfy` expression? It's a language I'm calling *core*, which can be used for manipulating syntax trees. It's not what this post is about, but I'll have more to say about it in the future.

### Repetition

The next class of operators accepts some number of repetitions of another parser.

#### `*`

The star operator can be used to accept any number of repetitions of the previous parser. For example `'c'*` accepts any number of `'c'`s, including 0.

<Demo star />

#### `+`

The plus operator can be used to accept one or more repetitions of the previous parser. For example `'c'` accepts one or more `'c'`s

<Demo plus />

#### count

A parser followed by a number accepts a fixed number of repetitions of that parser.

<Demo count />

### Sequence

Concatenating a sequence of parsers accepts when they all parse successfully in sequence. A parser must return something, which goes to the right of the arrow. For example <Demo sequence1 /> parses a simple addition expression where the operands, `a` and `b`, are both one character (any character).

<Demo sequence1 />

Of course, it would be more useful to return something we parsed. That's why you can name the result of any parsers you'd like to use in the result."

<Demo sequence2 />

This is a good time to revisit the *Debugger* tool. If you look at the debugger for the sequence parser, you'll see that it calls five subparsers. You can click the *view* button to inspect the details of any subparser, then *return here* to return to a caller anywhere up the stack.

### Choice

The `choice` construct can be used to accept one of several parsers. For example `choice ("c" | "foo")` accepts `"c"` or `"foo"`.

<Demo choice1 />

`choice` can accept any number of choices, and you can start each line with `|`. Note that choice always chooses the first matching branch, so in this example, `"abcd"` will never match (`abc` will match, leaving `d` unconsumed).

<Demo choice2 />

An empty choice always fails.

<Demo choice3 />

### Language constructs

So far all of our parsers have looked a lot like regular expressions. Let's introduce a construct that will make this look more like a real language. Let-binding allows us to name parsers and use them later, for example <Demo let />.

<Demo let />

Parsers can also fail with a message, like <Demo fail /> <Demo fail />. This example as written is of course not very useful, but this can be quite useful as part of a larger parser.
    ]

<Demo fail />

### Fix

Our parsers to this point have been limited: we can parse regular languages but not context-free languages. `fix` extends the language in the same way as [recursive regular expressions](https://catonmat.net/recursive-regular-expressions) to give it more power.

Let's say you want to parse addition expressions like "1 + 2", "1 + 2 + 3", "1 + 2 + 3 + 4", etc. We need a way to recursively use the parser we're defining. It's a little mind-bending, so let's look at an example.

Note: For clarity I've pre-defined two parsers: `name = (chars=alpha+ -> {var(string_of_chars chars)})`  and `literal = (chars=digit+ -> {literal(string_of_chars chars)})`.

<Demo fix />

`fix` computes the [fixed-point](https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node24.html#sec:proc-general-methods) of our parser. It takes a function which receives the parser being defined... and uses it to define itself.

### Playground

Finally, here is a playground where you can write and test your own parsers.

<Demo playground />

## Conclusion

This parser language, while still a bit rough around the edges, (I think) shows promise.

1. It allows us to use an intuitive, regex-inspired syntax, but with all the power of parser combinator libraries like Parsec or Angstrom. I'm particularly fond of the syntax for parsing a sequence, naming some of the subparses.

2. Rather than attempt the Sisyphean task of writing good error messages for every possible case, we circumvent the problem by providing a (hopefully intuitive) debugger, so the use can explore the parser's actions and see what it was "thinking".

3. Terms parsed using this language have location information associated to them automatically. This frees the language designer from painstaking work.

This language is not the end goal, just a step in service of the larger project to explore how simple we can make it to build new (small) languages. That purpose in mind, I'm excited to both continue exploring extensions to this language, and using it as a tool to build languages for other purposes.

### Big Picture

One topic I'd like to address is how this fits in with other tools you'd use to implement a language. This library is written in OCaml, so in theory you could use it as the parsing frontend for a language also written in OCaml. But, to be frank, I wouldn't recommend it. This is a prototype and it has to mature before it would be a great experience to use for real work.

Besides the parser, there are a handful of other tools that are commonly needed for new languages. For example, a typechecker, interpreter (and debugger), etc. I'm actively working on more of these tools for LVCA. This is where I think the parser will really shine, as part of this set. I'm excited to share them with you.
