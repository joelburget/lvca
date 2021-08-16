### Parser Language

Since last time I've been focusing on implementing a language for writing parsers.

This is important for a couple of reasons:

* I'm embarrassed by the quality of the error messages in the constructive real calculator [demo](/constructive-real-calculator).

* My goal with this project is basically to build a lot of languages. Better parser tooling should make that much easier.

My starting point has been to look at existing parser combinator libraries, for example [Parsec](https://hackage.haskell.org/package/parsec) or [Angstrom](https://github.com/inhabitedtype/angstrom). The language has the same primitives you'd see in either of those libraries, but it has a few differences in focus:

1. It's its own language, not a Haskell or OCaml library. This means that we can use some terser syntax, which I prefer. For example, in Angstrom you can use the `char` or `string` functions to build a simple parser: `char 'c' | string "str"`. The syntax in the language I'm building is simply `'c' | "str"`. I don't want to say too much more because there are still a few places I haven't decided exactly what syntax I want to use.

2. Parsers built using this language can attach locations to terms for free. For example, say we're parsing the string `x + 1`. The result is a term with locations attached (this is essential for error messages and tooling):

```
add( // 0:5
  x; // 0:1
  1  // 4:5
)
```

3. Since this language runs in the browser I'm taking the opportunity to build a debugger which allows one to step through the state as the parser (and subparsers) try to consume the input string.

### Design Language

Part of building the debugger has been creating a set of reusable components that I can use to build it (and future demos). Until now (for the previous demos), I've just used [TyXML](https://github.com/ocsigen/tyxml) to build HTML. This has been fine, except I've been writing custom CSS for every component. This means that I've been thinking at the level of "div with flexbox (flex-direction column) and 2px black border", but I should be thinking "rows". I'm trying to build a simple, consistent framework for everything on lvca.dev. At its most grandiose you might call this a design language.

So far my primitives include: input, button, table, rows, cols, etc. This list is evolving as I work on the parser debugger and I'm sure it will continue to evolve with every new demo.

One of my goals is that things should look good without too much effort. In fact, I prefer customizability to be as limited as possible. Today I was looking for styling ideas and discovered the fantastic [moderncss.dev](https://moderncss.dev). I'm currently exploring their [CSS Button Styling Guide](https://moderncss.dev/css-button-styling-guide/) and [Custom CSS Styles for Form Inputs and Textareas](https://moderncss.dev/custom-css-styles-for-form-inputs-and-textareas/).

There are a few unorthodox design choices that I'd like to preview:

* I'm building tools for programming. There should be minimal flourishes and customizability. Elements should be simple, even bare.
* Interactivity should be intentionally limited. For example, if I'm trying to demonstrate a function, there should be clear input (A) and output (B) areas. Information flows from A to B, not vice-versa.
* We're not just working with functions, but with data structures. There's an emphasis on tables, rows, and columns.

That's all for now! I plan to have a parser demo ready in the next week or two.
