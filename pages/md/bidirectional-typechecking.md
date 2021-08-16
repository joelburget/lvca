Bidirectional typechecking is the one typechecking algorithm LVCA has built-in
support for. In this post I'd like to cover the basics of what bidirectional
typechecking is and how it's supported in LVCA.

## What is Bidirectional Typechecking?

In order of increasing formality / length (ie the order you should read them), the best references I know of are:
* David Raymond Christiansen's [Bidirectional Typing Rules: A Tutorial](http://davidchristiansen.dk/tutorials/bidirectional.pdf)
* Frank Pfenning's [Lecture Notes on Bidirectional Type Checking](https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf).
* Joshua Dunfield and Neel Krishnawami's [Bidirectional Typing](https://arxiv.org/pdf/1908.05839.pdf)
