One of the most important concerns when developing a new language is how it interoperates with existing code. It's always tempting to ignore interop in the design phase, but the reality is that in most cases a language will only find adoption if it can play nice with existing systems, for three reasons:

* Using the new language from existing infrastructure
* Using existing infrastructure from the new language
* Migrating pieces of a system without rewriting in one go

## How interop works in LVCA

LVCA is intentionally very simple. My goal is that you should be able to reimplement it in a day. This is hugely important for dealing with other hardware platforms and languages.

For example, by implementing LVCA in JavaScript, it's possible to:

1. Run LVCA programs in any browser or any platform that implements JavaScript
2. Call LVCA programs from JavaScript
3. Expose externals to LVCA implemented in JavaScript. This means that LVCA-in-JS could use libraries like D3 or React.
