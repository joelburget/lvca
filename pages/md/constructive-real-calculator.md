[Last time](/progress-october-8-2020) I mentioned building a calculator for constructive reals. Today I have a demo of the initial version.

## What is this?

In one sense this is just a calculator like you can find on any computer or phone (in fact, in some sense this is the same calculator you'd find on Android, since I ported [their algorithm](https://dl.acm.org/doi/abs/10.1145/3385412.3386037) to OCaml). The interesting part here is the implementation: the entire calculator is running in the browser. That includes parsing and evaluating to as many digits as you ask for.

This seems mundane, and it should be. But if you open up your JavaScript console and enter `0.2 + 0.1`, it will respond `0.30000000000000004`. This is because floating-point is ubiquitous and easy, but wrong in subtle, confusing ways (It's not just issues like the one I showed -- did you know that floating point numbers include positive and negative infinity? Positive and negative _zero_? There's even a number literally called [Not a Number](https://en.wikipedia.org/wiki/NaN)). Because of these problems, it's usually not acceptable to expose floating-point calculations directly to end users. Which means we can't use the browser's built in `Math.PI`, `Math.cos`, etc. This is a real, difficult limitation. Instead, this calculator uses arbitrary-precision integers as a base, and heavily uses Taylor series approximations for everything beyond simple arithmetic. [Take a look at the code](https://github.com/joelburget/lvca/blob/main/constructive-real/ConstructiveReal.ml) if you're interested.

This means calculations aren't nearly as fast as the corresponding floating-point operations, but every digit you see is exact and the calculator can produce as many digits as you ask for.

The other fun thing about this demo is all the network requests it's _not_ making. [Wolfram Alpha](https://www.wolframalpha.com/input/?i=1+%2B+1) or Google's [built-in calculator](https://www.google.com/search?q=1+%2B+1&oq=1+%2B+1) (for example) do their work on the server, meaning queries can never return faster than it takes to round-trip to their servers. My hope is that this tool feels snappier to use. It's satisfying for me to peek at the network console and see absolutely nothing. It's nontrivial to do parsing and sophisticated numerical calculations in the browser. I'm excited both that I got it working and to extend this work to more interesting problems.

## Demo

Be careful of asking for too many digits (more than 5000 digits of `pi` for example). It could cause your browser to hang since I didn't implement any safeguards. Also, see the language chart below for a comprehensive list of expressions the calculator understands. Note that the syntax for function application doesn't require parens, so `sin 1` is valid, though `sin(1)` also works if you prefer, and parens are required in `sin (1 + 1)`.

```demo
calculator
```

## Future work

Today this makes for a decent calculator, but an impoverished language. There's a lot more one might want to do with a calculator language. The first things that come to mind are variables and user-defined functions. But real numbers also seem valuable to embed in other languages. I can imagine several domains where this functionality might be useful. So I'll be using this language as a base to explore and build on.

Note that this language has only one type, real numbers. But one of the interesting parts of the implementation is that we have a decision procedure for telling if two numbers are equal. I'd like to extend the language to have comparisons as well (this implies the extended language will have booleans).

Today the parser is really bad. Enter a malformed expression like `1 +` or `cos` and the error message is... less than helpful. I'm actively working on fixing this. I'm guessing there's also some low hanging performance optimization fruit, but I need to run some profiling to find out what's slow. Finally, it would be a lot of fun to have a debugger of sorts to understand what's happening under the hood, how many iterations it takes for the underlying sums to converge, etc.
