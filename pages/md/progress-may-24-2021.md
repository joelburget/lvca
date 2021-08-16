Long time no see! I've been working hard on LVCA, more focused on implementation than writing about it. I'm still focused on implementation, but I want to at least restart these progress updates. Today I have an example of the kind of thing I've been working on. Warning: this is fairly low-level, but I think still interesting.

## Parsing and Comments

Say we're parsing something with a comment: `cons(1; cons(2; nil())) // the list [1, 2]`. What happens to the comment? The easy answer is that we throw it away in parsing (lexing). I think a better answer is that we should record it as metadata, so the parser is responsible for attaching the body of the comment to the term it's annotating.

## Implementation

Before my latest change, parsers were implemented as the following (OCaml) [signature](https://github.com/joelburget/lvca/blob/7cbe48e1720475da1b336148adb0bcdac16944f3/syntax/Pattern.mli#L86-L89):

```
module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : OptRange.t t ParseUtil.t
  val whitespace_t : OptRange.t t ParseUtil.t
end
```

A parser for some language would take in a comment parser. This is neat because it means that (a) we can use the syntax for comments we prefer, and (b) when composing languages you can enforce that they all use the same comment syntax.

Now they're implemented as this [signature](https://github.com/joelburget/lvca/blob/193d5b9fe65d0db88c130c77cb9a699282ee47bc/syntax/Pattern.mli#L86):

```
module Parse : sig
  val t : Lvca_provenance.OptRange.t t ParseUtil.t
  val whitespace_t : Lvca_provenance.OptRange.t t ParseUtil.t
end
```

This has a number of advantages:

* It's simpler. This is the biggest advantage. It's made implementation much easier.
* A given language always uses the same comment syntax. This is more consistent in some ways (though it negates the previous advantage (a)). Also, previously there was a chance the given comment syntax could interfere with the language being defined. This is bad.

Lastly, the thing I'm most interested for the purposes of this post, comments aren't necessarily just thrown away. We already record the locations of parsed terms:

```
cons(1; cons(2; nil())) // the list [1, 2]
<--------------------->
     ^  <------------>
             ^  <--->
```

Now we might also record comments:

```
cons(1; cons(2; nil())) // the list [1, 2]
<---------------------> "the list [1, 2]"
     ^  <------------>
             ^  <--->
```

## Closing Thoughts

This agrees nicely with the design principle of not unnecessarily throwing anything away.

Broadening scope a bit, I'm thinking about the status of comments. Perhaps comments should have affordances for referring to other bits of code, using math notation or LaTeX, etc. Perhaps they should have affordances for mentioning preconditions, invariants, and other laws.
