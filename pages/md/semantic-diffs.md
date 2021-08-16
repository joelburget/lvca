In this post I'd like to explore an unorthodox view of how we should think
about software diffs. I'll propose a simple unconventional model for diffs and
explore how it compares to conventional diffs.

## An unconventional model for diffs

The conventional model for diffs is lexical (operating on the level of characters):
* insert a line
* delete a line
* change the contents of a line

But programmers rarely consider lines and characters directly in their work. We
think about variables, functions, modules, and the like. The work
of a programmer consists of tasks like:
* add a comment
* add / remove an import
* rename a variable
* call a function
* etc

We're really doing structured code transformations. We can think of programming
tasks as functions of type `code -> code` (here `code` is intended to be an
[abstract syntax](/abstract-syntax) tree in the language we're writing). For
example:

```
// some helpers I won't define right now
rename         : string -> string -> code -> code
add_definition : code -> code -> code
add_comment    : string -> code -> code

update1, update2, update3 : code -> code

update1 = rename "old_name" "new_name"
update2 = add_definition function_definition
update3 = add_comment "this code has several important invariants ..."
```

Instead of a textual diff, we're writing pure functions of type `code -> code`.

## Less brittle

The great thing about functions is that they can be _composed_. Not only that,
these can be composed in any order, so the equivalent of a rebase is easy --
it's reordering function application:

```
oritinal, rebased : code
oritinal = update3 (update2 (update1 origin))
rebased  = update1 (update2 (update3 origin))
```

In reality, our functions are not `code -> code`, but `code -> maybe code`.
That is, they allow for the possibility of failure. This means that we can
still compose two functions in any order that results in success.

You know the git interactive rebase interface?

```
pick f7f3f6d changed my name a bit
pick 310154e updated README formatting and added blame
pick a5f4a0d added cat-file

# Rebase 710f0f8..a5f4a0d onto 710f0f8
#
# Commands:
# p, pick <commit> = use commit
# r, reword <commit> = use commit, but edit the commit message
# e, edit <commit> = use commit, but stop for amending
# s, squash <commit> = use commit, but meld into previous commit
# f, fixup <commit> = like "squash", but discard this commit's log message
# x, exec <command> = run command (the rest of the line) using shell
# b, break = stop here (continue rebase later with 'git rebase --continue')
# d, drop <commit> = remove commit
# l, label <label> = label current HEAD with a name
# t, reset <label> = reset HEAD to a label
# m, merge [-C <commit> | -c <commit>] <label> [# <oneline>]
# .       create a merge commit using the original merge commit's
# .       message (or the oneline, if no original merge commit was
# .       specified). Use -c <commit> to reword the commit message.
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# If you remove a line here THAT COMMIT WILL BE LOST.
#
# However, if you remove everything, the rebase will be aborted.
#
# Note that empty commits are commented out
```

Does it fill you with dread like it does me? It's impossible to rebase
substantial changes that touch the same code -- you have to help the compiler
apply every single change. Often it's not worth it.

Let's look at an example:

#### Original code

```
def greet(x):
  print("Hello, " + x)
```

#### Rename variable

```
@@ -1,2 +1,2 @@
-def greet(x):
-    print("Hello, " + x)
+def greet(name):
+    print("Hello, " + name)
```

#### Uppercase name

```
@@ -1,2 +1,4 @@
+import string
+
 def greet(x):
-    print("Hello, " + x)
+    print("Hello, " + string.capwords(x))
```

#### Add docstring

```
@@ -1,2 +1,5 @@
 def greet(x):
+    """
+    Greet the user!
+    """
     print("Hello, " + x)
```

#### Say goodbye

```
@@ -1,2 +1,2 @@
-def greet(x):
-    print("Hello, " + x)
+def goodbye(x):
+    print("Goodbye, " + x)
```


All plausible changes. But they would not rebase well _at all_ (in git). But I
claim that these transformations, properly written, could be applied in any
order in our semantic update system, with no conflict.

## Intent communication

Have you ever had a change where you, say, rename a symbol that's used in
hundreds of places? Conventional version control / diffs handle this extremely
poorly. Compare `rename "old_name" "new_name"` to the hundreds or thousands of
lines diff you could get with a conventional system. Nobody is going to closely
inspect every line in the conventional diff, making it extremely error prone.

That's a particularly bad case for conventional diffs, but in general it's a
fantastic property of semantic diffs that they clearly communicate intent.

With semantic diffs you never waste time trying to understand how some diff was
created because the diff _is_ the exact procedure. Semantic diffs are
inherently reproducible and verifiable.

Aside: I like to think of the difference between semantic and textual diffs as
analogous to intensional vs extensional logic (definitions from Wikipedia):

* an intensional definition gives the meaning of a term by specifying necessary and sufficient conditions for when the term should be used
* this is the opposite approach to the extensional definition, which defines by listing everything that falls under that definition

## Structuring changes

One problem that's always bothered me with version control is that changes only
come in one size: the commit.

There are two roles I would like my version control to play, but the model of a repository as just a sequence of commits doesn't allow for both:

1. A clean sequence of understandable feature additions, removals, or bug fixes. This parallels the changelog.
2. A log of everything I tried, _especially_ if it didn't work out. When my collaborators ask "Why didn't you do it this way?" I want to be able to say "Here is the micro-commit where I tried that".

Today I have to choose between the clean and messy model because there's just
one notion of commit.

What I really want is have a hierarchy of changes, evaluated from top to
bottom:

```
project:
+ v1
- v2
  - add important feature
    + add dependency
    + add module that does the work
    - integrate with the rest of the system
      + update main
      - update module a
        - add import
        - rename variable "a" "b"
        ...
      + update module b
      + update module c
  + remove flag
  + update design
+ v3
```

We only hit atomic changes (rename variable, etc) five levels in (in this
example -- my intent is that you can use as many or as few levels as makes
sense). This is nice because it solves
* It allows me to have both understandable changes and a messy log of
  everything I tried.
* And this is roughly how we think about software projects (this would make it
  easier both to understand how a project is evolving and also to find old
  changes).

## Conventional diffs are just a special case

You might wonder, could it happen that sometimes conventional diffs express
intent more clearly? The good news is that conventional diffs are a special
case of functional diffs. We could simply write a function:

```
update : code -> maybe code
update = execute_diff
  """
  --- a/describe.c
  +++ b/describe.c
  @@@ -98,20 -98,12 +98,20 @@@
  	return (a_date > b_date) ? -1 : (a_date == b_date) ? 0 : 1;
    }

  - static void describe(char *arg)
   -static void describe(struct commit *cmit, int last_one)
  ++static void describe(char *arg, int last_one)
    {
   +	unsigned char sha1[20];
   +	struct commit *cmit;
  	struct commit_list *list;
  	static int initialized = 0;
  	struct commit_name *n;

   +	if (get_sha1(arg, sha1) < 0)
   +		usage(describe_usage);
   +	cmit = lookup_commit_reference(sha1);
   +	if (!cmit)
   +		usage(describe_usage);
   +
  	if (!initialized) {
  		initialized = 1;
  		for_each_ref(get_name);
  """
```

This just matches the lexical structure (rather than abstract structure) of our code. It it doesn't match then the result is `Nothing`. Otherwise the result is the diff applied.

Being humans, we don't necessarily always want to write a function for every change. I think it's important that we can continue to edit code directly. This system allows it.

This style of fuctional diff has one immediate win, even when writing a direct-style update. We can match any incoming code with the same *abstract* structure, meaning formatting differences never trip us up.

For example, whether you prefer curly braces to open on the same line:

```C
if (b) {
  ...
}
```

or (god forbid) the next:

```C
if (b)
{
  ...
}
```

it's all the same abstract syntax, `if(b; ...)`. Conventional diffs see different characters, but semantic diffs don't see any difference.

## Maintaining invariants

Has this ever happened to you?

Your change:

```
  grocery_list = [
    "butter",
    "salt",
    "apples",
    "spinach",
+   "cheese",
    ]
```

Your coworker's change:

```
  grocery_list = [
+   "cheese",
    "butter",
    "salt",
    "apples",
    "spinach",
    ]
```

Both of these changes get applied independently and you end up with

```
grocery_list = [
  "cheese",
  "butter",
  "salt",
  "apples",
  "spinach",
  "cheese",
  ]
```

And now your `grocery_list` is broken! How does this look in our model of
software change?

```
add_cheese : code -> maybe code
add_cheese = fun grocery_list ->
  if has_cheese grocery_list
    then Nothing
    else Just (add_cheese grocery_list)
```

This will only successfully apply if the list doesn't already have cheese!
Obviously this is a somewhat silly example, but the point is:
* You can explicitly document your assumptions in the change
* They're machine-checked, including during rebases and merges

## Merging

What about merges? Say we have two updates, where this property should hold:
`update1 (update2 origin) = update2 (update1 origin)`. Then we're in luck, we
can just define `merge update1 update2 origin = update2 (update1 origin)`. What
happens when that's not the case? TODO

## A bit more formally, what is an edit?

Here's a very rough outline for the [abstract syntax](/abstract-syntax) of an
edit.

```
// An edit is either:
edit(lang) :=
  // A simple, atomic edit (source -> source)
  | atomic(core(lang; lang))
  // Or an edit with a message attached
  | labeled(edit(); string())
  // Or a sequence of edits.
  | sequence(list(edit()))
```

Note that I'm completely avoiding `source_function`, or the language of how we
actually make changes. This is extremely important, but I think it should be
the topic of its own post.

## References

* [A Categorical Theory of Patches](https://arxiv.org/abs/1311.3903)
* [Homotopical Patch Theory](http://dlicata.web.wesleyan.edu/pubs/amlh14patch/amlh14patch.pdf)
