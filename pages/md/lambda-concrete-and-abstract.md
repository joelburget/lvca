import Demo from 'components/Demo'

With LVCA I've been working on building a set of reusable language tools. This is the very first (modest) demo of what these tools can do.

What is it? There are two panes: on the top is the input and on the bottom is the output. There are also two languages: the lambda calculus (concrete syntax) and LVCA's internal representation of its abstract syntax. Initially the lambda calculus is used as the input language and LVCA's term representation is the output. Using the "switch input languages" button you can make the term representation input and lambda calculus output. If lambda calculus is on the top, the function from input to output is parsing. If LVCA terms are on the top, the function is pretty-printing.

The main novelty of this demo is that when you highlight a section of input, the corresponding output is highlighted as well. This is due to the fact that LVCA tools track provenance, in this case provenance is a range of characters in the input pane where the value was parsed from. Every time you highlight a range in the input, we simply highlight the terms in the output which came from (a subrange of) that range.

I believe every good compiler eventually needs to implement provenance tracking, but it's mostly incidental complexity (a lot of plumbing). LVCA promises to do this work for free. The main use of provenance tracking is good error messaging, but this demo shows that there might be other interesting uses as well.

Besides highlight tracking, the other novelty is the fact that this demo runs in the browser. This is unusual among language tools, but something I've always designed LVCA for.

In future demos I'd like to show the stretch goals I mentioned last time (and didn't hit today): visualization of variable scope and a tool for renaming variables. I'd also like to show what it takes to build a language like the lambda calculus. But first (next time), I plan to write about the making of this demo. I think some of the code is very interesting and I'm using a rather unique technology stack that I've found very productive and would like to share.

<Demo demo_name="term-and-concrete" />
