# test definitions page

This page executes from top to bottom.

---

```lvca
define arith : abstract_syntax<abstract_syntax_concrete> := {{
import {integer} from "builtins"

tm :=
  | add(tm(); tm())
  | lit(integer())
}}
```

```
define arith_concrete : concrete_syntax<concrete_syntax_concrete> := {{
PLUS := "+"
NUM := /[0-9]+/

tm :=
  | a = tm PLUS b = tm { add(a; b) }
  | n = NUM { lit(integer(n)) }
}}
```

```lvca
define test : arith<arith_concrete> := {{
1 + 1
}}
```
