Today I'd like to talk about the making of the [last post](/lambda-concrete-and-abstract/). This is a collection of observations and choices that I found interesting.

## Using OCaml and `js_of_ocaml`

The most unique technology choice I made on this project was the choice to use OCaml and `js_of_ocaml`. OCaml is a niche language, especially for web programming ([Reason](https://reasonml.github.io/) is much more common, but still niche).

I've had a lot of experience trying to do PL-related projects that work online. At different times I've used vanilla JS, Haskell (GHCJS), and Reason. OCaml (with `js_of_ocaml`) has been easily my most positive experience:

* The compiler is *fast*
* The language itself is amazing
* My experience with libraries has been very positive
* [dune](https://dune.readthedocs.io/) is the nicest build system I've used
* Jane Street's [expect](https://github.com/janestreet/ppx_expect) and [inline](https://github.com/janestreet/ppx_inline_test) tests make testing a pleasure

The most problematic part of this stack has been [TyXML](https://ocsigen.org/tyxml/4.4.0/manual/intro), the library used to build DOM trees from OCaml. The problem is that in my experience it's very easy to trigger type errors and they're often inscrutable. The TyXML docs give this as an example error (it's saying that `b` is not allowed in `title`):

```
let mytitle = title (b [txt "A Bold Web Page"])
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type ([> Html_types.b ] as 'a) elt
       but an expression was expected of type
         ([< Html_types.title_content_fun ] as 'b) elt = 'b elt
       Type 'a = [> `B ] is not compatible with type 'b = [< `PCDATA ]
       The second variant type does not allow tag(s) `B
```

But let me tell you, this is very tame compared to a lot of the errors I encountered. I've found that using TyXML requires you to know html pretty well, know OCaml pretty well, spend time with the [docs](https://ocsigen.org/tyxml/4.4.0/api/Html_sigs.T), and look at examples.

`js_of_ocaml` build size is not great, but not horrible either. My latest build was 877K. Far smaller than what GHCJS would produce, but far larger than writing this code by hand. There's a [post on the OCaml forums](https://discuss.ocaml.org/t/reducing-the-size-of-js-of-ocaml-output/2538) discussing build sizes, the main takeaways that I can see are to use the `profile=release` flag and avoid libraries you don't need. This has been the biggest reason I've stuck with the [Base](https://ocaml.janestreet.com/ocaml-core/latest/doc/base/index.html) library instead of using [Core_kernel](https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/index.html). Like the author of that post, I've gotten the build to 800-900k and decided that it's good enough for me, but I'd love to hear if there's an easy way to make my builds significantly smaller.

## Reactive Programming

I've been using the [react](https://erratique.ch/software/react) library by Daniel Bünzli for reactive programming (This is not the same as Facebook's React, by the way. OCaml's react is apparently [several years older](https://www.reddit.com/r/ocaml/comments/gt2ncz/todo_app_in_js_of_ocaml_react_and_reactivedata/fsaqk5e/)). I've very much enjoyed the experience and haven't had any problems (though my application is quite simple). One issue on my radar is that [React can leak memory](https://github.com/ocsigen/eliom/issues/262) (when run in the browser) due to the lack of weak references in JavaScript. My application is far from needing to worry about this, but when I get to that point I plan to try Daniel's new [note](https://github.com/dbuenzli/note/) library, which is meant as a successor to React (mostly to avoid this problem, as I understand it).

There are three examples that have been especially useful to me in understanding how `js_of_ocaml` and `react` work:

* [CueKeeper Internals](https://roscidus.com/blog/blog/2015/06/22/cuekeeper-internals-irmin/)
* [TodoMVC: a reactive version](https://ocsigen.org/blog/2015/10/07/react-example-todomvc/)
* Reddit: [Todo app in `Js_of_ocaml`, react and reactiveData](https://www.reddit.com/r/ocaml/comments/gt2ncz/todo_app_in_js_of_ocaml_react_and_reactivedata/)

## Range

The demo uses my own [`Range`](https://github.com/joelburget/lvca/blob/fa4981a04b0cdc91c10eb1ebdd81338e7adcfda1/syntax/Range.mli) module to model input selections. The core type is simply two positions in the input buffer:

```ml
type t =
  { start : int
  ; finish : int
  }
```

There are a few operations we want to support, like merging ranges, checking for intersection, checking for subranges, [etc](https://github.com/joelburget/lvca/blob/fa4981a04b0cdc91c10eb1ebdd81338e7adcfda1/syntax/Range.mli). This is the kind of data type which is extremely simple, yet I think demonstrates the level of care which has to be taken to make really good software. A simple and clear model with a small set of primitives, this is the kind of software I really enjoy writing.

## Parsing, Pretty-Printing

The demo is all about parsing and pretty-printing, and (again) this is an area where OCaml's libraries really help.

For parsing I use the [Angstrom](https://github.com/inhabitedtype/angstrom) library. Angstrom focuses on async and concurrent applications, but it also seems to be the standard parser combinator library in OCaml. I'm also working on a library built (for now) on top of Angstrom, which automates some of the location tracking I had to write by hand for the demo.

Pretty-printing uses the built-in OCaml [Format](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html) library, as well as Daniel Bünzli's [fmt](https://erratique.ch/software/fmt) library (which wraps Format with some nice helpers).

## Formatter

The OCaml formatter interface is [complicated and difficult](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html). It's also quite powerful. In particular, I made use of *semantic tags*: user-defined annotations associated to printed entities.

I won't try to explain semantic tags here, but the [format unraveled](https://rbonichon.github.io/papers/format-unraveled.pdf) paper explores Format in depth. In particular, it gives two examples of using semantics tags which are simpler than what I'm going to show here:

1. Coloring terminal output
2. Producing either HTML or Markdown from formatted text (by interpreting semantic tags differently)

I created a formatter that produces Html elements (that also react to a range signal to highlight when selected). I'll give the abbreviated version here.


```ocaml
(** The incoming signal holds the currently selected range. We return both a
    Dom element (<code>) to be inserted in the Dom and the formatter which is
    used to write formatted text to this element. Note that the returned Dom
    element is empty until the formatter is used and flushed. *)
let mk_range_formatter
  : Range.t React.signal
  -> [> `Code ] Js_of_ocaml_tyxml.Tyxml_js.Html5.elt * Stdlib.Format.formatter
  = fun rng_signal ->
  let br, span, txt = Html.(br, span, txt) in

  let top_level_elems, top_level_handle = RList.create [] in

  (* - Push a new range and queue on the stack every time we enter a semantic tag
     - Pop every time we exit a tag
     - The queue is used to add elements (text and nested children) under this element
     - The range is the extent of this element, used to update the style when text is
       selected
   *)
  let stack : (Range.t * [> `Span ] Html5.elt Queue.t) Stack.t
    = Stack.create ()
  in

  (* Add a Dom element at the current level. *)
  let add_at_current_level elem = ... in
  let add_text str = add_at_current_level (span [txt str]) in

  Stdlib.Format.pp_set_formatter_stag_functions fmt
    (* We open a new span for every range tag we encounter. All children until we
       encounter the matching close tag will be nested under it (by enqueuing). *)
    { mark_open_stag = (function
      | Range.Stag rng -> Stack.push stack (rng, Queue.create ()); ""
      | _ -> ""
    )
    (* Closing a range; create the span holding all of the enqueued children. *)
    ; mark_close_stag = (fun _ ->
      begin
      match Stack.pop stack with
        | None -> ()
        | Some (_, q) -> q
          |> Queue.to_list
          |> span
          |> add_at_current_level
      end;
      ""
    )
    ; print_open_stag = Fn.const ()
    ; print_close_stag = Fn.const ()
    };

  R.Html5.code top_level_elems, fmt
```

Summarized, it's possible to output HTML DOM elements instead of text when pretty-printing. We use semantic tags to associate to the DOM elements a range, representing a provenance. We also use reactive programming to highlight these DOM elements if their provenance is a subrange of the current input selection.

## Summary

Overall I've had a very positive experience using OCaml and its libraries. I hope to extend the demo with a couple new features for next time.
