let str = {|
== abstract syntax ==

tm :=
  | var(var)
  | annot(tm; ty)
  | ite(t1: tm; t2: tm; t3: tm)
  | app(t1: tm; t2: tm)
  | val(val)
  | binary-op(binary-op)

binary-op :=
  | or(t1: tm; t2: tm)
  | xor(t1: tm; t2: tm)
  | and(t1: tm; t2: tm)

val :=
  | true()
  | false()
  | lam(val. tm)

ty :=
  | bool()
  | arr(ty; ty)

== statics ==

// TODO:
// * should we use concrete syntax?
// * how to show var separate from context?
// * how to separate hypotheses?
// * should we make var rule explicit instead of implicit?

----------------------- (bool intro 1)
ctx >> true() => bool()

------------------------ (bool intro 2)
ctx >> false() => bool()

  ctx >> v => ty
------------------- (val)
ctx >> val(v) => ty

      ctx >> tm <= ty
-------------------------- (annot)
ctx >> annot(tm; ty) => ty

ctx >> t1 <= bool()  ctx >> t2 <= ty  ctx >> t3 <= ty
----------------------------------------------------- (bool elim)
           ctx >> ite(t1; t2; t3) <= ty

    ctx, x : ty1 >> tm <= ty2
---------------------------------- (lam intro)
ctx >> lam(x. tm) <= arr(ty1; ty2)

ctx >> tm1 => arr(ty1; ty2)  ctx >> tm2 <= ty1
---------------------------------------------- (lam elim)
        ctx >> app(tm1; tm2) => ty2

// important: this rule must go last or else it will subsume all others
ctx >> tm => ty
--------------- (switch)
ctx >> tm <= ty

== dynamics ==

// [[_]] : tm -> core(val)

[[ value(v)        ]] = v
[[ annot(tm; ty)   ]] = [[ tm ]]
[[ ite(t1; t2; t3) ]] = case([[ t1 ]]; true() -> [[ t2 ]]; false() -> [[ t3 ]])
[[ app(fun; arg)   ]] = app([[ fun ]]; [[ arg ]])

== dynamics ==
|}
