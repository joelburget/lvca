# How to make code review much easier

## 1. Intentional diffs

It's often best to express diffs [intentionally, rather than extensionally](/semantic-diffs). What do I mean by that? For the whole story, do read the linked post. But here's the short version. I'll start with two examples.

### Renaming

Here's (part of) a change I'm working on right now. (Please ignore the Capitalized_snake_case if it's ugly to you -- it's [an OCaml thing](https://opensource.janestreet.com/standards/#naming), okay). Imagine this but spread across 50 locations in 10-20 files. A pretty typical rename operation.

```diff
 module Action = struct
   type t =
-    | UpdateLanguage of string
-    | UpdateTerm of string
+    | Update_language of string
+    | Update_term of string
 end

 module Controller = struct
   let update (action : Action.t) model =
     match action with
-    | UpdateLanguage language_str ->
+    | Update_language language_str ->
       { model with Model.language_str; language_parsed = parse_lang language_str }
-    | UpdateTerm term_str -> { model with term_str; term_parsed = parse_term term_str }
+    | Update_term term_str -> { model with term_str; term_parsed = parse_term term_str }
   ;;
 end
```

But there's a much better (clearer) way to express the same thing

```edit
[rename "UpdateLanguage" "Update_language"; rename "UpdateTerm" "Update_term"]
```

This expresses the _intention_ behind the diff (the extension). Note that any sane environment using intensional diffs will show you the extension if you ask for it.

### Boilerplate, or the choice between repetitive code and awkward abstraction

Sometimes you need to perform a similar action to some existing code, but _tweaked_. You end up dumping 20 lines (or a lot more) where you need it and modifying it to suit the new situation. This is boilerplate. Let me propose an alternative.

```edit
[ generate_boilerplate "function_name";
  rename "boilerplate_name" "more_expressive_name";
  etc...
]
```

Here, rather than appear from thin air, it's clear where our boilerplate came from.

### 1b. Changes can be hierarchical

In my opinion this is somewhat less crucial (and therefore a sub-point). It should be possible to structure your changes to form a narrative. Rather than throwing a big mess of changes at reviewers, it ought to look like this:

```
1. major change
  A. sub-change
  B. sub-change
2. major change dependent on 1
3. major change dependent on 2
```

## 2. Invariants

A very common pattern in software is to change the implementation of some function, expecting the result to be the same. A few examples:

* The simplest cases are mechanical tasks like variable renaming
* Only slightly more complicated are tasks like exchanging a loop for recursion, ie writing the same fundamental algorithm in a different style
* There are other tasks which change the algorithm but expect the same result. Optimizations are a good example. A somewhat canonical example is [O(n^2) vs O(n) list reverse](https://blog.poisson.chat/posts/2019-09-13-reverse.html).
* Other tasks which invariants are useful for include generalizing a function (eg adding parameters) or specializing (removing parameters).

These tasks are often done in preparation for some change in functionality (especially the last case -- generalizing a function). Let me give an example. I have a function to help with testing parsers and pretty-printers. What it does is to parse some input and then print it. I'll elide the implementation:

```ocaml
let parse_print : string -> unit = ...

let%expect_test _ = parse_print "test input"; [%expect{|test input|}]
```

Now, I would like to test the pretty-printer at multiple widths. My first change is to update parse_print to accept a width:

```ocaml
let parse_print : ?width:int -> string -> unit = ...
```

This is the point at which the implementation of the function has changed (been generalized), but the result should be the same. Now, we can add the test cases using the added generality:

```ocaml
let%expect_test _ = parse_print ~width:20 "test input"; [%expect{|test input|}]
let%expect_test _ = parse_print ~width:80 "test input"; [%expect{|test input|}]
let%expect_test _ = parse_print ~width:100 "test input"; [%expect{|test input|}]
```

Invariants to assert:

* Say "this computes exactly the same thing as before"
* Or, for these inputs, "this computes ..."

## 3. Reviewers can test their inputs right there

Have you ever reviewed code and wondered "what does this evaluate to for input [...]?", but it was too much work to patch the change in to your client and test it yourself? Oh boy you bet I have. In fact I would argue this is the case for almost every change I review. _This should be extremely easy_. It should be built in. You shouldn't have to run any special commands -- _you should be able to poke around with the code right there in your browser_.

Table of input, output pairs

### 3b. Visuals are... visible

Code is just... code. Aren't you nearly always visualizing what it does? You ought to see the change. Examples:

* CSS
* image rendering
* the extensional counterpart to the intentional diff
