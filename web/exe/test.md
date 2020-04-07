# Hutton's Razor

Hutton's Razor is a minimal language with exactly two types of expressions:
integer literals and addition.

```
{define arith : abstract_syntax<abstract_syntax_concrete>}
import {integer} from "builtins"

tm :=
  | add(tm(); tm())
  | lit(integer())
```

```
{define arith_concrete : concrete_syntax<concrete_syntax_concrete>}
PLUS := "+"
NUM := /[0-9]+/

tm :=
  | a = tm PLUS b = tm { add(a; b) }
  | n = NUM { lit(integer(n)) }
```

```
{define arith_statics : statics<statics_concrete>}
--- (infer int)
ctx >> tm => int()
```

```
{define arith_dynamics : dynamics<dynamics_concrete>}
meaning = \(tm : int()) -> match tm with {
  | add(x; y) -> #add(meaning x; meaning y)
  | lit(i) -> i
}
```

```
{define test : arith<arith_concrete>}
1 + 1
```
