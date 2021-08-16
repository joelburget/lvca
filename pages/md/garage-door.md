Despite the fact that I've been hard at work on LVCA, things have been very quiet here because I've been hesitant to share things before they're right. But I now think that's the wrong approach and it would be better for me to share the process of what I'm thinking and what I'm doing in all its imperfection. I think it's just as valuable to share things I got wrong and dead ends as it is to share the final product.

So, to borrow a phrase from [Andy Matuschak](https://notes.andymatuschak.org/Work_with_the_garage_door_up) and [Jason Crawford](https://rootsofprogress.org/agriculture-with-the-garage-door-up), I'm planning to work with the garage door up, meaning I'll share what I'm working on at least biweekly. And to keep myself accountable, I've set up a [Beeminder](https://www.beeminder.com/joelburget/garage-door), so that as I write I have 3d 2h 09m (update: 1d 3h 26m) to post this before I owe $5.

I have a lot to share, so let's get started.

## Concrete Syntax

The biggest dead end I explored was to do with parsing / concrete syntax. For a long time I was hoping to declare concrete syntax once, deriving both a parser and pretty-printer from a single declaration. If you've written a few languages you may have noticed the strong similarity between parsers and pretty-printers. It _feels_ like enough duplication that you should be able to write just one.

Anyway, here's a simple example of the type of parser / pretty-printer LVCA handled:

```
ARR    := "->"
ADD    := "+"
SUB    := "-"
MUL    := "*"
DIV    := "/"
LPAREN := "("
RPAREN := ")"
FUN    := "fun"
NAME   := /[a-z][a-zA-Z0-9]*/

arith :=
  | name = NAME                                  { var(name) }
  | [ LPAREN tm = arith_1 RPAREN ]               { tm        }
  > [ a = arith_4 _ b = arith_5 ]                { app(a; b) }
  > [ a = arith_3 _ MUL _ b = arith_4 ]          { mul(a; b) }
  | [ a = arith_3 _ DIV _ b = arith_4 ]          { div(a; b) }
  > [ a = arith_2 _ ADD _ b = arith_3 ]          { add(a; b) }
  | [ a = arith_2 _ SUB _ b = arith_3 ]          { sub(a; b) }
  > [ FUN _ name = NAME _ ARR _ body = arith_1 ] { fun(var(name). body) }
```

If you're familiar with other parser generator tools then this syntax probably looks familiar. Without going into detail, an `arith` term can be constructed from any of the forms under `arith :=`. On the left is the concrete syntax and on the right, in braces, is the abstract syntax. We distinguish between different precedence levels by putting `>` at the start of a line instead of `|`.

Five reasons I decided this path is a dead end:

### 1. It requires a complicated transformation to turn it into something an LALR parser can handle

LALR parsers don't have any built-in notion of precedence, so we need to desugar the above into something with explicit precedence levels.

```
ARR    := "->"
ADD    := "+"
SUB    := "-"
MUL    := "*"
DIV    := "/"
LPAREN := "("
RPAREN := ")"
FUN    := "fun"
NAME   := /[a-z][a-zA-Z0-9]*/

arith := tm = arith_1 { tm }

arith_1 :=
  | [ FUN _ name = NAME _ ARR _ body = arith_1 ] { fun(var(name). body) }
  | tm = arith_2 { tm }

arith_2 :=
  | [ a = arith_2 _ ADD _ b = arith_3 ] { add(a; b) }
  | [ a = arith_2 _ SUB _ b = arith_3 ] { sub(a; b) }
  | tm = arith_3 { tm }

arith_3 :=
  | [ a = arith_3 _ MUL _ b = arith_4 ] { mul(a; b) }
  | [ a = arith_3 _ DIV _ b = arith_4 ] { div(a; b) }
  | tm = arith_4 { tm }

arith_4 :=
  | [ a = arith_4 _ b = arith_5 ] { app(a; b) }
  | tm = arith_5 { tm }

arith_5 :=
  | name = NAME                    { var(name) }
  | [ LPAREN tm = arith_1 RPAREN ] { tm        }
```

I eventually decided this transformation was a little too complex and decided to drop it for the purpose of finishing LVCA.

### 2. LALR parsers are complex!

Speaking of complexity, LALR parsers are very complex. I couldn't find an LALR parser that worked exactly how I wanted so I ended up having to write my own. The [LALR parser](https://github.com/joelburget/lvca/blob/caf5f1d673ffb5ab5036985c0478ca12cce58235/src-old/LalrParsing.ml) came out to 644 lines and the [LR parser](https://github.com/joelburget/lvca/blob/caf5f1d673ffb5ab5036985c0478ca12cce58235/src-old/LrParsing.ml) it depends on 1108 lines. That's not to mention test or the cool debugger / visualizer I wrote (very similar to the [JSMachines tool](http://jsmachines.sourceforge.net/machines/lalr1.html)).

This is unfortunate for a number of reasons. I intended for LVCA to be easy for others to implement, so needing to implement your own LALR parser is obviously a huge impediment.

But it's not just the implementation that's complex. LALR parsers are complex and confusing in general. They tend to expose a bunch of implementation details to the user: action table, goto table, shift vs reduce and shift/reduce conflicts (or reduce/reduce conflicts), parser states, lookahead. It was my experience that to use an LALR parser well you need to become somewhat of an expert in LALR parsers.

### 3. Limited to parsers with no computation

Because I wanted to do parsing and pretty-printing at the same time, every rule needs to be bidirectional, meaning no functions / computation. It's possible to work around this with a [theory of lenses](https://www.cis.upenn.edu/~bcpierce/papers/index.shtml#Lenses), but again, trying to keep things simple, this is a non-starter.

This style of parser is limited in other ways too. Want to parse a language with significant whitespace? Good luck!

### 4. Parsers and pretty-printers are just different enough to be confusing

For example, see the brackets and underscores in the rules above? They're meant to signal to the pretty-printer where to open [boxes](https://ocaml.org/learn/tutorials/format.html#Boxes) and print spaces, but they're ignored by the parser. The parser also needs to know about spaces, but it needs to know what's acceptable as a space. What about parentheses? The pretty-printer needs to figure out where to use them.

### 5. Non-compositional (most important)

One of my main goals with LVCA is to explore combining languages. For example, in the web dev world, JSX allows one to write interspersed HTML and JavaScript:

```javascript
const name = 'Josh Perez';
const element = <h1>Hello, {name}</h1>;
```

Imagine doing the same but with a language other than JavaScript.

Here's the problem. LR algorithms are non-compositional. For example (from private correspondence with [Laurence Tratt](https://tratt.net/laurie/)):

```
A: "IF"  "("  cond  ")" A;
B: "IF"  "("  cond  ")" stat "ELSE" stat;
C: A | B;
```

Then `C` isn't LR / LALR(1)!

LR parsers are generally monolithic. They need to be analyzed all at once -- there are absolutely no guarantees when combining two parser fragments `A` and `B` whether their composition is a valid parser. I see this as a huge problem.

Similarly, LR parsers really need a tokenization step. But if you're composing two languages they surely have different tokens. Now you need to figure out how to write a single tokenizer (with a different mode for each language?). At this point you've written a parser.

## Conclusion

That's why I abandoned that approach for dealing with concrete syntax. For now, I'm back to using parser combinators (I have a newfound appreciation for them) and conventional pretty-printing.

I still find the idea of declaring a parser and pretty-printer at the same time tantalizing. I hope someone figures out how to do it well.

This post is getting long so I'll leave it at that. Next time I hope to get into some of what I'm working on now, instead of just talking about what didn't work.
