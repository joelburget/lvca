Last time I promised to write a bit more about abstract syntax. I plan to do so today, but indirectly. I think that last time I sensed the project floundering a bit, noticing that some pieces weren't fitting together how I'd planned, and abstract syntax was the bit on my mind.

When I notice that happening, I think it's best to take a step back, stop working on it for a week or two, walk a lot, and let my mind wander. I've been doing a bit of that and it certainly helped, but the thing that really helped most was that I actually implemented a small language, in OCaml, by hand (not using LVCA). This simple exercise helped me to refocus on what motivated me to start this project in the first place by highlighting the original pain points that I wanted to address.

I think that in my approach to this project I was trying to do too much all at once. Some version of the [second system effect](https://wiki.c2.com/?SecondSystemEffect). You can see it in the (half-baked) Hutton's Razor I was [working on](https://github.com/joelburget/lvca/blob/9d6db7e4960e2aa53781c98a5ea0054909d11a1e/languages/LanguageHuttonsRazor.ml#L21-L61), where I was trying to use three half-built languages. Trying to build the perfect system all at once.

What I need to start with is a system that makes it easier for me to build languages today (or at least this week). With that in mind, my most immediate goal is to write an *OCaml package* (not a language) that helps with parsing and pretty-printing. I started working on it earlier this week and was hoping to have a demo by today, but it didn't happen quite in time. So, my goal for next update is to have a demo of the following:

* Two editor panes:
  - On the left is the concrete syntax of a language (probably the lambda calculus)
  - On the right is abstract syntax
* Editing the left side should re-parse and cause the right to update. Similarly for editing the right.
* Selecting a term on either side should show the corresponding term on the other side.
* Stretch goals:
  - When you select a variable binding site we can highlight the scope where it's visible.
  - A tool to rename variables (the simplest refactoring tool I can think of)

The two things I'm trying to solve here are:

1. A nice, general, reusable representation for abstract syntax that I can use for basically any language I want.
1. Automatic provenance handling. A good compiler will show you exactly where in the source an error originated. It's (a) boilerplate to plumb this information everywhere and (b) significant work to turn that provenance information into good error messages. My goal is to automate some of this.

That's where things stand today. I can't wait to share the demo next time.
