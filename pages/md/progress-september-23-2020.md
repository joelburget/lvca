This week I'd like to give a progress update instead of a demo. I'm busy moving so I don't quite have time to put together a demo. Also, I have progress to report on several fronts!

## Parametrized term

Previously all term representations hardcoded [`Primitive.t`](https://github.com/joelburget/lvca/blob/2438176912b90fdb61e798bf6fc8e5e6e10ff76f/syntax/Primitive.ml#L3) as their notion of _primitive_. I wanted to make the notion of primitive more flexible, motivated by these use-cases:

* A language whose primitives include ints but not strings, chars, etc (eg Hutton's Razor)
* A language without primitives (eg lambda calculus)
* A language which _embeds_ another language. In this case the language may not really have "primitives" (ints, chars, and the like), but the embedded terms will still be primitive to it, in the sense that they're opaque. This is the direction I'm most excited to explore.

So, I [made primitives a type parameter](https://github.com/joelburget/lvca/compare/202fd12980db50a11002dcf3288267fc8ad8997f..b3cd59d24e2828091cf2c5c9482a4034d5c7b09a) of the term types. This change affected `Pattern.t`, `Nominal.t`, `DeBruijn.t`, `DeBruijn2d.t`, and `NonBinding.t`.

`Primitive.t` is a holdover from an earlier time when I expected LVCA to have a fixed set of primitives that stay the same forever. Back then, my goal was to build in the smallest set of primitives needed to build others on top. However, my thinking on this has changed, and I no longer think `Primitive.t` makes much sense. It's an arbitrary bundling of (big) integers, floats, strings, and chars. I expect it will go away soon.

## Parser language

For the demos I've produced so far, parsers have been written in [Angstrom](https://github.com/inhabitedtype/angstrom) (which is a fantastic library), but creating a new language for writing parsers gives some clear advantages.

### 1. Better error messages

Angstrom seems to be focused on efficiency, not so much on producing nice errors. Take for example the error message the [Concrete / Abstract demo](http://localhost:8000/lambda-concrete-and-abstract) produces right now if you forget a paren: `: end_of_input`. Not ideal (this reminds me that I ought to spend a bit of time to make the messaging less embarrassingly bad).

The point is that:

* Be default, parser error messages aren't good right now
* It's probably possible to make them nicer (with Angstrom) but it takes real work
* There's a relatively low ceiling to how nice you can make Angstrom error messages

My goal (you might say this is the goal of LVCA but just applied to parsing) is to make the _default_ nice. IE you ought to be happy with the results you get by just straightforwardly writing a parser. The process shouldn't be fraught with gotchas.

### 2. Nice syntax

Writing a parser with Angstrom is fine, I guess. Let me paste a short example:

```
(* Parse a lambda, eg "\x -> x" *)
let lam : (OptRange.t, Primitive.t) Nominal.term Parsers.t =
  pos
  >>= fun start ->
  lift4
    (fun _lam var _arr body ->
      let range = OptRange.extend_to (location body) start in
      let tm = Nominal.Operator (range, "lam", [ Scope ([ var ], [ body ]) ]) in
      tm)
    (char '\\')
    p_var
    (string "->")
    t
```

It doesn't take a lot of imagination to think we could make this better. I'm
still working on the syntax, but I have something like this in mind:

```
let lam = '\\' var:p_var "->" body:t -> lam(var. body)
```

We're capturing to variables on the left of the arrow and producing a term on
the right. Provenance is automatically tracked. 12 lines becomes one.

### 3. No compile necessary

I want people to be able to produce languages quickly, in the browser. I can't
do this with Angstrom (or any OCaml library).

### 4. Extensibility

Part of my philosophy with LVCA is that languages should be extensible. Say you don't like my lambda calculus and want to use a different syntax. It should be possible to easily modify it to exchange '\\' for "fun":

```
let lam = "fun" var:p_var "->" body:t -> lam(var. body)
```

Again, this is not possible with Angstrom. In the future I'll talk a bit about how it will be possible with LVCA.

The first two points (error messages and syntax) would be really nice to have, but the last two (no compile and extensibility) are the really big ones.

## Fuzz-testing

Ocaml has an amazing library called [Crowbar](https://github.com/stedolan/crowbar) for fuzz-testing code. The interface to crowbar is very similar to QuickCheck or any other property-testing library: you write properties that should always hold and the library tries to find inputs that falsify them. The novelty with Crowbar is that it uses [American fuzzy lop](https://lcamtuf.coredump.cx/afl/) to explore all execution paths in your code to find ones that fail:

> American fuzzy lop is a security-oriented fuzzer that employs a novel type of compile-time instrumentation and genetic algorithms to automatically discover clean, interesting test cases that trigger new internal states in the targeted binary. This substantially improves the functional coverage for the fuzzed code.

The reason you don't often see this in other languages is that the OCaml compiler has special built-in support for AFL (which AFL uses to explore execution paths). To use it you have to install a specialized version of the compiler, eg 4.09.0+afl, instead of 4.09.0.

This has forced me to consider a lot of weird edge cases. A few examples, all taken from properties I wrote should hold for primitives:

(1) I naively wrote a property that `prim_str |> parse |> to_string = prim_str`. This should hold right? Well, it turns out that my parser was willing to parse the input "+0", but that value prints as just "0". I decided this is acceptable, but that `parse |> to_string` must be idempotent, ie it must reach a fixed-point after one application.

(2) Another property: `t |> jsonify |> unjsonify = t`. This should _definitely_ hold, right? Well, it turns out that `nan <> nan` (`<>` means not-equal in OCaml).

(3) How about one more. For context, I was trying to keep things simple with floats: I'd prefer not to parse scientific notation ("2.99e8"), so the parser I wrote can only parse the form "x.y". I wrote the property `prim |> to_string |> parse = prim`. Crowbar / AFL produced the input `2.9384442618974733e-233` (which is apparently a valid float in OCaml). Let's try an experiment:

```
let f = 2.9384442618974733e-233 in
Format.printf "%f %F %e %E %g %G %s\n" f f f f f f (Base.Float.to_string f);;
```

Output:

```
0.000000 2.9384442619e-233 2.938444e-233 2.938444E-233 2.93844e-233 2.93844E-233 2.9384442618974733e-233
```

What's the point? The first of these doesn't represent the number accurately (Is this a bug in the standard library? I don't know.), while the rest output some form of scientific notation. My choices seem to be: write a new float formatter, write a scientific notation parser, or get rid of floats. I haven't decided which is the best choice yet.

## Build size

When reinstalling dependencies on 4.09.0+afl, I noticed that installing the bignum package also caused core_kernel to be installed. If you [recall](/making-concrete-and-abstract/), I've been working with base specifically to avoid including all of core_kernel. So it's really bad news if I'm accidentally including it anyway! I ran a quick experiment, removing `Bigint` from LVCA, then rebuilt the "binary" I'm using for my demos. The only difference between these two is the inclusion of big integers:

### before:

```
> ls -lh _build/default/pages/main.bc.js
rw-rw-r-- 1 joel joel 884K Sep 19 12:59 _build/default/pages/main.bc.js
```

### after:

```
> ls -lh _build/default/pages/main.bc.js
rw-rw-r-- 1 joel joel 513K Sep 19 13:03 _build/default/pages/main.bc.js
```

This is huge. It suggests that 42% of LVCA is bignum + core_kernel.

I could have saved myself some time if I had just read [bignum's description](https://github.com/janestreet/bignum): "Core-flavoured wrapper around zarith's arbitrary-precision rationals". This obviously suggests just using [zarith](https://github.com/ocaml/Zarith) instead (I haven't decided what to do yet, see `Primitive.t` discussion above). It seems like [js_of_ocaml separate compilation](https://ocsigen.org/js_of_ocaml/3.5.1/manual/separate-compilation) is a helpful tool for this: I can change my mindset from having one "binary" incorporating all of LVCA to multiple binaries each including exactly what it needs (then choosing to include big ints or not isn't such a big decision).

This also raises the question of what else is contributing to the final build size? I found some [helpful](https://discuss.ocaml.org/t/reducing-mirageos-image-size-using-ocamlclean/2481) [links](https://discuss.ocaml.org/t/large-binaries-break-down-the-size-by-library/1098/3) on the [OCaml Discourse](https://discuss.ocaml.org/) that I haven't had time to investigate yet (this isn't the most pressing issue).

## Next time

That's all for now! I was also hoping to write a bit about my experiences so far with _not_ using React, but writing UI in a reactive style. Perhaps in a future progress update. Next time I'll have another demo!
