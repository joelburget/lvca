<Tweet tweetLink="paf31/status/1246167749962784768" />

## Introduction

What important problems does LVCA solve? What's new about LVCA that doesn't already exist? What's the point of all this?

Today I'd like to discuss why I find LVCA interesting to work on.

## Experimentation

One goal of mine is to decrease the effort required to create a language by at
least 10x (if you've been in software a while you may find this number wildly
implausible, but I think it's justified by the amount of work LVCA can save --
more on this later in this post and in future posts).

When language creation becomes 10x easier, the calculus of when it's
appropriate to create a new language really changes. Today it's almost never a
good idea to write a new language. If LVCA succeeds, it might, sometimes, be a
good idea.

## Custom Language Extension

As I mentioned in the [introduction](/introduction), a typical Haskell file has thousands of language extensions enabled. This situation is due to Haskell's original purpose as a playground for language experimentation, which is in tension with modern Haskell's use as an actual language that people sometimes do work in. As a language playground, it's totally reasonable to have 1000 knobs to tweak, and people should be encouraged to add new knobs! However, engineers mostly just want to write in _one_ language without any knobs. That's not to say there should be no features! But features might need to be tailored to the needs of a particular team. Right now it's hard to write custom extensions. For example:

* [HLint](https://github.com/ndmitchell/hlint) lints Haskell programs for common antipatterns. It's run as a standalone program and uses the [haskell-src-exts](https://github.com/haskell-suite/haskell-src-exts) package to parse Haskell code (this is not the same parser that the compiler uses).
* [SHE preprocessor](https://hackage.haskell.org/package/she), the Strathclyde Haskell Enhancement. An experimental preprocessor that adds features to Haskell. SHE uses its own custom minimal Haskell parser.
* [Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell-blog/) is a static verifier for Haskell. It enriches Haskell's type system with _refinement types_. They avoid changing Haskell's syntax by using comments marked with `@` (`{-@ ... @-}`) as liquid typing annotations. The liquid typechecker is then run as a standalone program apart from the compiler.

In each of these cases the author had to write a standalone program not integrated with the compiler and find a way to work around the fact that the compiler has a fixed syntax (it's non-extensible). LVCA provides allowances for more tightly integrated language changes.

## Language Evolution

Language extensions are one example of Haskell's evolution as a language.
However, there are three big problems with the way Haskell and other languages
evolve today.

### 1. Change is slow.

When a language is first getting started things happen quickly, but users force
a language to slow down. Backwards compatibility constrains _every_ decision.

### 2. There's no way to formally reason about language changes.

The current best practice for language changes is to write a changelog
announcing everything that has changed, with upgrade instructions. Better would
be to mechanize the upgrade process. Every language change comes with a
function converting terms in the old language to terms in the new. A language
upgrade then involves running a program to upgrade your code. I'll write about
this in its own post.

### 3. Changes are add-only

It's hard to impossible to remove features from a language once they're added.
This is still done in rare cases, but only after months-to-years long period of
deprecation.

As a result, a careful language designer will be conservative about adding new
features. This is a main cause of evolution slowness. But no matter how
conservatively new features are added, it's inevitable that there will be
misfeatures which cause pain.

## Smaller Languages

Instead of one big Haskell that seeks to serve everyone's needs, we might instead have thousands of languages, all evolving from the same source, but in ways that perfectly suit their creators' needs.

You might find this prospect terrifying, but hopefully it's also exciting. When one language invents a new feature, other related languages can simply merge it in. The way I see this playing out is that in the LVCA ecosystem there will still be a handful of languages analogous to Haskell, or C, or JavaScript of today. Stable languages with a large community. These languages move relatively slowly, but they have clusters of smaller languages orbiting around them. LVCAHaskell would have languages like Haskell+FRP and HighPerformanceHaskell in its orbit. When LVCAHaskell adds new features, most languages in its orbit merge those features in. But the languages in its orbit are constantly exploring the idea space, and when they hit on something that would be useful to everyone who appreciates LVCAHaskell, it's relatively easy to merge it back in. TODO: this is better for the language evolution section

## Run Anywhere

When Java was a new language it had a slogan "Write once, run anywhere".

I have a goal that it's simple to port LVCA to any platform. This is certainly
not yet the case, but my ultimate goal is that it's possible to implement LVCA
for most new platforms in less than a day.

This would immediately enable running many programs written on top of LVCA. I
can't say _all_ programs, because a program might still call a primitive not
available on your platform. However, it will be possible to query an LVCA
program for all primitives it calls. If they're all supported on your platform
then it's runnable. So, instead of Java's promise, we have "Write once, run
anywhere that supports all required primitives". Not quite as pithy, but I like
the more nuanced message.

## Formally Reason About Languages

TODO

## `n * m` problems

Today, every language has to implement their own _everything_. Parser, typechecker, and interpreter / code generator to start. But there's also a lot more if you want the language to be successful. You probably want syntax highlighting for editors, a debugger, a language server protocol implementation, package management. This is just scratching the surface -- there's infinite work in making a good, usable language.

The good news is, effort on most of these tools can be shared among languages using the same infrastructure. As soon as you write an abstract syntax declaration LVCA tools can store and serialize your programs, or query for programs matching some pattern. As soon as you write a concrete syntax declaration you get a parser, pretty-printer, and syntax highlighting. Statics and dynamics unlock other tools.
