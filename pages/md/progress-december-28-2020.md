import TreeViewWebm from "../../assets/treeview.webm"
import TreeViewMp4 from "../../assets/treeview.mp4"

Happily it's only been five days since the last update. I'm on "vacation" so I have a lot more time than usual to work on LVCA. This is a quick update on a new feature.

### Tree Viewer

I added a tree viewer to the [parsing language](/parsing-language/) demo page. It's helpful for seeing the location data attached to terms, but I'm even more excited to use it as a base for the binding / scope viewer I mentioned in the [last update](/progress-december-23-2020/).

<video controls>
  <source src={TreeViewWebm} type="video/webm" />
  <source src={TreeViewMp4} type="video/mp4" />
</video>

### Core / Quoting

I settled on a compromise I'm fairly happy with. The parser language uses braces (`{...}`) to embed core terms and the core language also uses braces to embed quoted terms. This does mean we sometimes end up with double braces, eg `a=. ' '* '+' ' '* b=. -> {{add(a; b)}}`. As a compromise I added sugar for two common cases:

* Counts: `'c'2` is short for `'c'{{2}}`
* `fail`: `fail "message"` is short for `fail {{"message"}}`

### Upcoming work

My next small project will be to enhance this tree viewer with scope information:

* View the scope of a variable definition
* View the use-sites of a variable definition
* View the definition site of a variable
* View places where a variable is shadowed

I have three larger projects coming up, but haven't decided yet in which order they'll appear:

* A typechecker debugger
* Pretty-printing terms
* A language of edits

I'm excited to say more about these soon!
