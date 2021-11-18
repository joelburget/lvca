My favorite programming language is Haskell. It’s not my favorite because it’s perfect. In fact, there are some obvious things that need to be fixed — they’re a problem for me and everyone else who uses Haskell, but the language implementers have to balance fixing things against concerns like (a) not breaking everyone’s existing Haskell code and (b) making sure that anything they add to the language works with everything that’s already there. Haskell is actually pretty good when it comes to evolution — languages like C, C++, and Java are forced into evolving much more slowly for various reasons. It’s all very reasonable. As a language user you get all the benefits of stability (like not having to rewrite your code or relearn how to use it). The downside is that you better not mind the annoying thorns of the language because they’ll probably never go away.

I’d like to give a few examples of what I’m talking about in Haskell. I chose Haskell because it’s the language I know best, though we could repeat this exercise with any language (I think Haskell is one of the best).


## Language Pragmas

A typical Haskell file begins like this:

```Caption: Figure 1: Language extensions
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
```

The designers of Haskell didn’t have this in mind in 1987 when they first started designing the language. But at some point they came up with a new feature that was experimental, or broke backwards compatibility, or something. So they invented the first language pragma and it was a great idea. But then 32 years passed and before you know it there are (by my count) 106 language pragmas and if you want to write Haskell well you need to at least be familiar with most of them so you can use 21 pragmas at the start of a single source file.

Here’s an incomplete list of problems with this situation:

- The obvious tedium of actually writing all those pragmas.
- With pragmas at the beginning of every file, there can be a huge amount of duplication. I just grepped a medium-size codebase with 137 Haskell files and… 99 of them had `{-# LANGUAGE OverloadedStrings #-}`.
- Sometimes these features [interact in weird ways](https://twitter.com/totbwf/status/1455684517117583370) that you should maybe be aware of if you’re using them. [Ahem](https://gitlab.haskell.org/ghc/ghc/issues/1496). If this complexity is daunting as a user, it’s surely far harder for GHC implementers trying to do things correctly.

## Strings

Another idea that was good when Haskell was first defined, but hasn’t aged well. Haskell’s `String` type is defined as an alias for a linked list of characters. This is not what you want, for reasons I’ll go into in just a moment. But here’s the thing. We know how to do strings the right way. Or at least a much better way. In Haskell it’s a type called `Text` and you have to require a package, `text`, to use it. This is not too much work, and most libraries that people rely on today use `Text`. But people still use `String`  because it’s there and it’s easy and since it’s built in it seems like the right thing to use. Even if we wanted to change `String` to be better, there’s really no way to do it without breaking millions of lines of code, which I guess the creators of Haskell don’t want to do.

I just looked up a [page](https://jship.github.io/posts/2017-08-10-writing-performant-haskell-part-2.html) where someone compares the performance of `String`. I’ll quote.


> Representing `"0x0000001f"` as a `String` means we are using 51 machine words for a 10-character string. Oh, the memory overhead!!!

To be honest, I’ve found heaps of pages of people saying to use `Text`  instead of `String`, but I’m struggling to find, you know, *benchmarks* that back this up.

- https://github.com/haskell-perf/checklist#are-you-using-text-or-bytestring-instead-of-string
- https://www.reddit.com/r/haskell/comments/120h6i/why_is_this_simple_text_processing_program_so/


## `:` vs `::`

This is a trivial syntactic thing, so of course there’s heated debate about it. In Haskell, `::` means “has type” and `:` means “list cons”. Some people prefer for it to be the other way around, [including Digital Asset Holdings](https://medium.com/daml-driven/four-tweaks-to-improve-haskell-b1de9c87f816):


> If Haskell was designed today, `:` would mean "type" and Hackage would be [1Mb smaller](https://neilmitchell.blogspot.com/2018/11/counting-cost-of-colons-in-haskell.html).

Obviously this is not a huge problem. Sometimes I need to write two colons instead of one. So I’m not really complaining about this so much as I’m just mentioning it to illustrate that sometimes it’s impossible to change a language decision after the fact, [no matter how trivial](https://github.com/ghc-proposals/ghc-proposals/pull/118).

## The point

These are all relatively small issues. I’m not too worried about any of them in isolation. What seems problematic to me is that this class of problem can’t really be fixed. Languages have no built-in facility for evolution.

I have two related ideas I want to leave you with, for exploration in later posts.

First, what if language updates included facilities for updating code from the old version? For example an early version of this was Python’s [2to3](https://docs.python.org/2/library/2to3.html). I think there are others who’ve tried this as well.

Second, what if languages were hackable by design? I’m picturing an environment where I could fork my language to, say, enable all 21 pragmas I need by default. That’s kind of a boring change though. I could also write language extensions of my own. One could even imagine a community of people, all extending the language, making it their own, borrowing ideas from one another.

This is possible today, in principle, but it doesn’t happen because (i) updating compilers is hard and (ii) nobody is able to run your code unless they’re also willing to first build a custom version of the compiler and then compile your code with it. It would be much better if other developers could just run your code without the whole *rebuild the compiler* step.

So that's what I'm excited about and want to explore with this site. Building, understanding, and evolving languages.
