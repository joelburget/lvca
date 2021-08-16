## Introduction

Having declared an abstract syntax, we want to be able to check that terms are well-formed. For example, say we've defined a sort of natural numbers.

```
nat := zero() | succ(nat)
```

Examples of well-formed terms include `zero()` and `succ(zero())`. Non-well-formed terms include `zero(zero())` and `succ()`. These demonstrate one way a term can be incorrect -- by having the wrong number of children, but there are many others. In this post I'll enumerate the checks that LVCA currently does and their limitations.

## Term Checks

## Patterns

## Typechecking
