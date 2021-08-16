As part of [working with the garage door up](/garage-door), I've promised to
share what I've been working on. I last updated on June 23. Unfortunately,
progress has been limited, primarily since Tessa and I went on a half-vacation
/ half-working trip, which spanned most of two weeks. Then, I got back and
started engaging with buying a house, so I've been a bit distracted.

## What's in a Term?

Despite that, I did manage to get substantial work done on a large-ish task, [refactoring the structure of terms to be a bit more general](https://github.com/joelburget/lvca/pull/12). I haven't quite decided whether this is a good idea or not (leaning yes), hopefully this post helps to clarify my thoughts.

The core of the change is this diff in AbstractSyntax.mli:

```diff
--- a/src/AbstractSyntax.mli
+++ b/src/AbstractSyntax.mli
@@ -14,21 +14,18 @@ type sort =
   | SortAp of sort_name * sort list (** A higher-kinded sort can be applied *)
   | SortVar of string

+(** A sort can be starred to indicate it's repeated, or not. *)
+type starred = Starred | Unstarred
+
+(** Represents a place where a sort can go in a valence. *)
+type sort_slot = sort * starred
+
 (** A valence represents the sort of an argument (to an operator), as well as the number
     and sorts of the variables bound within it *)
-type valence =
-  | FixedValence of sort list * sort (** A fixed valence is known a priori *)
-  | VariableValence of sort * sort
-      (** A variable valence binds a number of variables not known a priori. All must be
-          of the same sort. *)
+type valence = Valence of sort_slot list * sort_slot

 (** An arity specifies the arguments to an operator *)
-type arity =
-  | FixedArity of valence list
-  (** A fixed arity operator always has the same number of children *)
-  | VariableArity of sort
-  (** A variable arity operator has a variable number of children (all of the same
-      sort (non-binding valence)) *)
+type arity = valence list

 type operator_def = OperatorDef of string * arity
   (** An operator is defined by its tag and arity *)
```

Note that the resulting code is simpler than before. We're generalizing two
things here, which I'll show by example.

### The old way

First, let's talk about the old way of doing things. It was the case that you
had two choices when it comes to valence: fixed or variable valence.

```
example :=
  | fixed(a. b. a)  // fixed valence
  | variable(a*. a) // variable valence
```

An operator with fixed valence always binds the same number of variables, for
example `fixed(a. b. a)` (this binds `a` and `b` then uses `a` as the body). An
operator with variable valence binds, well, a variable number of... variables.
For example, `variable(pattern(a1; a2). a1)` binds two variables, then uses one
of them as the body. In a fixed binding position, you introduce exactly one
variable for every binding position. But in a variable binding position, you
use a pattern instead (patterns, by their nature, can bind any number of
variables).

A variable valence operator can only have _one_ binding position, meaning I
couldn't have written `variable(a*. b*. a)`, though `variable(a*. a)` or
`variable(b*.a )` would be okay. I'm not sure what my justification was for
this restriction. I think this is something I was confused about.

It was also the case that you had two choices when it comes to arity: fixed or
variable arity.

```
example :=
  | fixed(a; b; c) // fixed arity
  | variable(a*)   // variable arity
```

A fixed arity operator always has the same number of children, for example
`fixed(a; b; c)`. A variable arity operator has a variable number of
children, for example `variable(a1; a2)`.

A variable arity operator can only have one child sort, meaning I'm not allowed
to write `variable(a*; b*)`. Why? Because we use `;` to separate subterms, so
there's no way to tell when the `a`s end and the `b`s begin.

### The new way

That probably seemed a bit more complicated than necessary. In my change I
removed both of the restrictions, so now you can write:

```
example :=
  | variable_valence(a*. b*. a) // variable valence with multiple binding positions
  | variable_arity(a*; b*)      // variable arity in multiple slots
  | both(a*. b*. a*)
```

A few example terms:

```
variable_valence(a_pattern(a1; a2). b_pattern(). a1)
variable_arity(a1, a2; b1, b2)
both(a_pattern(a1; a2). b_pattern(). a1, a2)
```

Notice that we've now introduced commas (before the only separators were `.`
and `;`). Previously you had to use `;` to separate children in a
variable-arity operator, but now we use `;`s to separate "slots" (each of which
holds repeated terms of the same sort) and `,`s to separate terms within a
slot.

Note that `variable_arity(;)` is now a valid term. This raises the question in
my mind of whether we should specify 1-or-more repetition (`+`) in addition to
the 0-or-more repetition (`*`) that we currently have. Happily, this is
strictly a generalization, so we can add it later.

### Conclusion

Overall I think I'm happy with this new system. My main hesitation is this
nagging feeling that people will find this system overly complicated but in a
new way. I think you'd be justified in asking what the deal is with `.`, `,`,
and `;`.

However, as I'm in the mood for rethinking things, I've begun to think about
the purpose of the abstract syntax system. That's what I plan to talk about
next time.
