Recently I've been working on a tool to translate abstract syntax declarations into OCaml modules ([some examples](https://github.com/joelburget/lvca/blob/main/ppx_lvca/test/test.ml)). This has me thinking about the fractal nature of software and syntax trees.

## The Fractal Nature of Software

My terminal goal with LVCA is to propose a rethinking of some aspects of software development. For our purposes, we can simply say "build LVCA". But the task I've been working on lately is translating LVCA abstract syntax declarations into OCaml modules (`Module_builder` for short). Depending on your viewpoint, it likely seems that I've been focused on a task which is fairly far removed from the terminal goal. I don't disagree. I want to note how fractal in nature software is. An example of my work context stack from one point recently:

* Build LVCA (top-level goal)
* Build demos for the website
* Build demo programming languages
* Build evaluators for demo languages (this was very cumbersome without the OCaml translation)
* Build `Module_builder`
* Allow for importing externally-defined (OCaml) modules
* Make sure sorts of kind `* -> *` (as opposed to simply `*`) can be imported
* Refactor all generated functions to take an extra argument for each sort they're applied to
* Generate a unique name for each argument
* Build a unique name generator
* Allow the name generator to generate names with a set prefix (eg `info_`)
* Refactor name generator to efficiently generate long names rather than searching all one-character names, then two characters, etc.

This, to me, is one of the truly daunting things about software. I often find myself working in a context stack roughly this deep (this is 12 items, but could easily be presented as more or fewer).

My first observation is that it's hard to remember the whole context. It sometimes happens that some observation at depth 12 should cause you to rethink the plan at depth anywhere between 11 and 1! But it's really hard to keep all those depths in mind at once.

More interestingly, I could easily spend a week just at depth 10, building a really great unique name generator. I might add a bunch of features, make it fast, add unit and property-based-tests, create an opam package for it, etc. It's possible to spend unbounded time at nearly any level, besides the lowest two or three. It's nearly impossible to make any nontrivial piece of code perfect. Software approaches perfection asymptotically.

Note: this is an observation about software, and I'm most familiar with this phenomenon in the context of software, but it can certainly be applied more broadly (to any task?).

## Syntax Tree Complexity

I think syntax trees are generally too complex to easily work with. Complex syntax trees make all tools (for example: parsers, simplifiers, code generators, etc) more difficult to create. Let's take OCaml as an example.

### OCaml

The OCaml parse tree ([github](https://github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli), with comments; [clickable version](https://ocaml.org/api/compilerlibref/Parsetree.html), without comments) has:

* `core_type`, with 12 different constructors
* `pattern`, with 18 constructors
* `expression`, with 37 (!) constructors
* `module_type`, with 8 constructors
* `signature_item`, with 15 constructors
* `module_expr`, with 7 constructors
* `structure_item`, with 15 constructors

In total (by my count), there are 72 types defined in `parsetree.mli`. Plus other types imported from the `Asttypes`, `Longident`, and `Location` modules.

When generating code it's generally far easier to use [ppxlib / metaquot](https://ppxlib.readthedocs.io/en/latest/ppx-for-plugin-authors.html) to generate code than it is to build a parsetree directly. Compare `[%expr 1 + 1]` to

```
{
  pexp_desc =
    (Pexp_apply
       ({
          pexp_desc = (Pexp_ident { txt = (Lident "+"); loc });
          pexp_loc = loc;
          pexp_attributes = []
        },
         [(Nolabel,
            {
              pexp_desc = (Pexp_constant (Pconst_integer ("1", None)));
              pexp_loc = loc;
              pexp_attributes = []
            });
         (Nolabel,
           {
             pexp_desc = (Pexp_constant (Pconst_integer ("1", None)));
             pexp_loc = loc;
             pexp_attributes = []
           })]));
  pexp_loc = loc;
  pexp_attributes = []
}
```

`metaquot` provides seven extensions for the seven main parsetree types (quoted from the ppxlib homepage):

* `expr` for `Parsetree.expression`: `[%expr 1 + 1]`
* `pat` for `Parsetree.pattern`: `[%pat? ("", _)]`
* `type` for `Parsetree.core_type`: `[%type: int -> string]`
* `stri` for `Parsetree.structure_item`: `[%stri let a = 1]`
* `sigi` for `Parsetree.signature_item`: `[%sigi: val i : int]`
* `str` and `sig` respectively for `Parsetree.structure` and `Parsetree.signature`. They use similar syntax to the `_item` extensions above as they are just a list of such items.

Of course, there are also five anti-quotation extensions (quoted again):

* `e` to anti-quote values of type `Parsetree.expression`: `[%expr 1 + [%e some_expr_node]]`
* `p` to anti-quote values of type `Parsetree.pattern`: `[%pat? (1, [%p some_pat_node]]`
* `t` to anti-quote values of type `Parsetree.core_type`: `[%type: int -> [%t some_core_type_node]]`
* `m` to anti-quote values of type `Parsetree.module_expr` or `module_type`: `[%expr let module M = [%m some_module_expr_node]] or [%sigi: module M : [%m some_module_type_node]]`
* `i` to anti-quote values of type `Parsetree.structure_item` or `signature_item`: `[%str let a = 1 [%%i some_structure_item_node]] or [%sig: val a : int [%%i some_signature_item_node]]`

Finally, one example of several extensions working in concert:

```
let structure_item =
  [%stri let [%p some_pat] : [%t some_type] = [%e some_expr]]
```

By the way, check out how many functions [`Ppxlib.Ast_builder`](https://ocaml-ppx.github.io/ppxlib/ppxlib/Ppxlib/Ast_builder/Default/index.html) provides.

One, this is a lot to remember. But I also often find myself wanting to antiquote something which is not one of the seven sorts listed above (most often a name, of which the compiler uses at least three representations).

That's not to say that the situation in OCaml is bad. It's actually great compared to most languages. But I think it could be easier to use.

### The other extreme

Lisp is notorious for its use of s-expressions as both data structure representation *and* its programmer-visible syntax (See [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity)). So (depending on the dialect), everything is a list (slash dotted pair), number, symbol, or string. That's it. And the tools for manipulating syntax trees are simple -- they're the same tools you use for manipulating any other data structure.

This makes the barrier to entry (into metaprogramming) much lower in lisps. Every seasoned lisp hacker is always on the lookout for an opportunity to use macros.
