I've made an effort to keep LVCA small and easy to implement (correctly). This
focus means that there's a lot LVCA can't do. Today I'd like to explain what
LVCA does and doesn't do (and why).

## Parsing
## Typechecking
## Builtins

## Files

You should be able to use LVCA locally with files on your own computer. And maybe with version control integration. You should be able to use it on the web pointing to http urls. And I have plans to write a service which uses content-addressing to point to LVCA programs. The point is, these are all valid uses, and they're all work to implement. I don't want to require an implementation of LVCA to implement all of them. And I don't want to choose some arbitrary subset. Point is, LVCA is storage agnostic. It's up to different implementations to decide.
