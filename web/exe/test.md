# test definitions page

This page executes from top to bottom.

---

```lvca
define arith : abstract_syntax := {{
import {integer} from "builtins";

tm :=
  | add(tm(); tm())
  | lit(integer())
}}
```

```
define arith_concrete : concrete_syntax := {{
PLUS := "+"
NUM := /[0-9]+/

tm :=
  | tm PLUS tm { add(tm; tm) }
  | NUM { lit(integer($1)) }
}}
```

```lvca
define test : language(arith; some(arith_concrete)) := {{
1 + 1
}}
```
