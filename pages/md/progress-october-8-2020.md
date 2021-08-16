This week I have another quick progress update. I was originally planning to present a demo of the parser language I mentioned [last time](/progress-september-23-2020), but two things happened. First, I got stuck on one or two aspects of the implementation. Also, I discovered a fascinating paper that I started to implement instead. That's what I'd like to talk about today.

## Real numbers

Real numbers are fantastically useful, but at the same time hideously complicated. We often use floating point as a pragmatic approximation to real arithmetic, but that introduces its own set of complexities (just ask Python what `0.1 + 0.2` is).

In general, it's much nicer to use exact arithmetic when possible, but that introduces a complicated set of tradeoffs. Until now I've never considered implementing exact real arithmetic myself, but last week I came across the paper [Towards an API for the Real Numbers](https://dl.acm.org/doi/pdf/10.1145/3385412.3386037) by Hans-J. Boehm. This paper provides a set of primitives that can be computed exactly (to arbitrary precison), and an implementation in Java. The primitives are:

* literals (eg `1.2`)
* addition, subtraction, multiplication, division
* exponentiation
* "select" (ie `if`)
* `cos`, `atan`, `ln`, `asin`, `sqrt`

From these we can compute many more derived operators, like other trig operators, `pi`, logarithms of other bases, etc. This is a nice set of primitives / derived operators, good for 95% of cases.

## Implementation

The implementation is quite difficult, but tons of fun. Each of `exp`, `cos`, `atan`, `ln`, `asin`, and `sqrt` involve evaluating a series until the error is less than the last digit requested. The final API is intuitive, but the code behind it is not. It's also been a challenge to port it from Java to [OCaml](https://github.com/joelburget/lvca/blob/f65952d0a4dd96719c8f6e7ff3ab619a59cee9be/languages/Calculator.ml).

But I now have a fairly robust set of tests, and am starting to gain confidence in the implementation. There is a lot of cleaning up to do, but I think it shows a lot of promise. It's extremely satisfying to watch the tests run, evaluating all of the examples in the blink of an eye.

## Plans

My primary reason for implementing this is to create a calculator for myself. I'll be able to run it both in the console and online. And in the style of LVCA languages it'll be extensible. I should be able to add variables, user-defined functions, etc.

While implementing it I realized that this might be worth uploading to opam in some form. I can imagine that others in the OCaml community would enjoy using it as a library, so sometime this week I'd like to extract it from LVCA and start to shape it into a library.

That's all for now. I'm looking forward to next time when I can share a demo of the calculator in action.
