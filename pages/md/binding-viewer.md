After building a structured tree viewer for the [parsing language demo](/parsing-language/), I decided to extend it a bit to demonstrate binding structure in an interactive way.

In the first image my cursor (which you can't see) is hovering over the first `x`. Highlighted in pink you can see where `x` is defined, in orange is a binding which shadows the outer `x`, and in blue is a use of the variable. The green background shows the scope of the variable. Notice the hole where it's being shadowed.

![Demo screenshot 1](/static/images/scopeview1.png)

In the second image I've selected the second `x`, highlighted in pink for a definition site. In yellow we can see that the outer `x` is shadowed by this variable, and in blue is the one use of this new `x`. Notice how the scope of this `x` perfectly fills the gap in the shadowed `x`'s scope.

![Demo screenshot 2](/static/images/scopeview2.png)

Third, I've selected `f`, which is a free variable, so its scope is (implicitly) global.

![Demo screenshot 3](/static/images/scopeview3.png)

In the last two images I've selected the two use-sites of the two different `x`es. We can see both the use-site in blue and the definition in pink.

![Demo screenshot 4](/static/images/scopeview4.png)
![Demo screenshot 5](/static/images/scopeview5.png)

Finally, a demo for you to try.

```demo
binding-viewer
```

## Conclusion

There is a dedicated page with more info about [abstract syntax in LVCA](/abstract-syntax/).

This tool is one example of a major theme in LVCA: building tools which are broadly applicable. Using this notation, which is meant to represent the abstract syntax for any language, this tool could in theory be used for any language.

The next major projects I have planned are additional tools for pretty-printing and for typechecking.
