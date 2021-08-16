import HlDemo1Webm from "../../assets/hl-demo1.webm"
import HlDemo2Webm from "../../assets/hl-demo2.webm"

import HlDemo1Mp4 from "../../assets/hl-demo1.mp4"
import HlDemo2Mp4 from "../../assets/hl-demo2.mp4"

### Parser Language

Recently my work has been focused on the parser language and its [demo page](/parsing-language/). This has been by far the most effort I've put into a single page for LVCA, because (a) the language is complicated and (b) there is a lot of cool stuff to demo! It's getting close enough to done that I'm happy to talk about it here, though I haven't published it on the homepage yet.

One bit I've put a lot of effort into is showing off the location information parsers get for free. For example, we can highlight bits of the input to see where they end up in the parsed result:

<video controls>
  <source src={HlDemo1Webm} type="video/webm" />
  <source src={HlDemo1Mp4} type="video/mp4" />
</video>

We can also highlight bits of the parsed result to see where they came from:

<video controls>
  <source src={HlDemo2Webm} type="video/webm" />
  <source src={HlDemo2Mp4} type="video/mp4" />
</video>

These controls are still a bit wonky (the videos shown here are how it works in the best case). Overall I've found this quite hard to get right, mostly because browsers don't support this kind of thing especially well, for example:

* It's possible to set an input selection, but what I'm trying to do is not actually a selection, just a... highlight. Which inputs have no concept of. So, to make my own, each input in the demo actually has a transparent background and a `<div>` behind it. The `<div>` contains the same text as the input, but the text is hidden, and we highlight spans within the div as the text they live behind is selected.

* The JavaScript select event only fires for selections in an `<input>` or `<textarea>`. The _Result_ box is a `<div>` with `<span>`s, so we need to work with `mousedown` and `mouseup` events instead. So for every `mousedown` within the result box, we track its location and then try to highlight on `mouseup`. Bleh.

You can probably imagine how both of those controls are hard to get right and why I'm still polishing them.

#### Core / Quoting

One thing I'm still working on is a tricky language design issue. This requires a little bit of backstory. In a few places the parser language embeds terms from a "core" language. This shows up in the `satisfy`, `fail`, `count`, and `sequence` constructs.

The core language is meant primarily for transforming syntax trees. You could, for example, map from Peano-style natural numbers to binary nats:

```
let rec to_binary = \(x: peano()) -> match x with {
  | Z() -> {Zero()}
  | S(n) -> binary_succ (to_binary n)
}
```

This is just an example, but it's sufficient to illustrate the problem. The notation I use for syntax trees is, for example, `S(n)`. But that could just as easily be function application, and I want them to be clearly distinguished in core. That lead me to decide we should quote terms in expression position, like `{Zero()}`. I think this is reasonable.

However, I'm also using curly braces to denote the transition from the parser language to core. So it's very easy to end up in an unfortunate double-braces situation. Say you want two repetitions of a parser: `'c'{{2}}`. Or, returning a term `"foo" -> {{foo()}}`. Uh-oh.

So, the language is principled but ugly. I'm considering adding sugar so you can write `'c'{2}` (or even `'c'2`, wow!) but haven't decided yet.

### Upcoming Work

Besides finishing the parser demo, I have a few more projects in the planning / early implementation stage:

1. Typechecker debugger. One of the main insights of the parser language is that error messages for parsers are hard. At some point it's more useful to just have a debugger. I think the same applies to typechecking. It's really hard (harder?) to write good typechecker error messages. Better would be to punt on writing good messages and just expose a debugger. I'm hoping to reuse a lot of the tooling that went into the language debugger to produce this tool a little more quickly.
2. Syntax tree viewer (/ editor?). One piece of feedback I received on the parser demo was that there should be a structured view of the parsed ASTs, that we only display "flat" structures. I don't think this is quite right (the parsed ASTs are structured), so I'm adding a sentence or two to clear up any possible confusion, but it is a great point:
    * The AST view doesn't show location information because it's noisy, but this wouldn't be a problem in a tree viewer.
    * Structured tree viewers are useful in their own right for exploring large structures.
    * Plus, I've been meaning to explore structured editing in this project anyway.
3. Pretty-printing. Once you've done parsing you're almost obligated to do pretty-printing next. This is something I've been thinking about (in the context of LVCA) for [a while](https://github.com/joelburget/lvca/issues/5).
4. Scope viewing. The parser language is a first example of an LVCA-based language with binding. I'd like to build tooling to explore the scope of variables. For example, I'd like to be able to select a variable and immediately see where it's defined and where it's used. It would also be useful to see where it's shadowed (replaced by another variable of the same name). This would probably be an enhancement to the existing term viewer / structured tree viewer.

### Site Improvements

I'd like to add a couple small site usability improvements: A dark mode selector (currently we select light or dark based on [`prefers-color-scheme`](https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme) but I'd like it to be explicitly selectable) and sidenotes. See [Gwern](https://www.gwern.net/Sidenotes) for examples of both.

That's all for now. Back to work!
