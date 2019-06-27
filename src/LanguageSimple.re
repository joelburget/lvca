let abstractSyntax = {|tm :=
  | var(var)
  | annot(tm; ty)
  | ite(tm; tm; tm)
  | app(tm; tm)
  | val(val)
  | binary-op(binary-op)

binary-op :=
  | or(tm; tm)
  | xor(tm; tm)
  | and(tm; tm)

val :=
  | true()
  | false()
  | lam(val. tm)

ty :=
  | bool()
  | arr(ty; ty)|}

// TODO:
// * should we use concrete syntax?
// * how to show var separate from context?
// * how to separate hypotheses?
// * should we make var rule explicit instead of implicit?
// important: this rule must go last or else it will subsume all others

/*
    ctx, x : ty1 >> tm <= ty2
----------------------------------
ctx >> lam(x. tm) <= arr(ty1; ty2)
*/

let statics = {|
-----------------------
ctx >> true() => bool()

------------------------
ctx >> false() => bool()

  ctx >> v => ty
-------------------
ctx >> val(v) => ty

      ctx >> tm <= ty
--------------------------
ctx >> annot(tm; ty) => ty

ctx >> t1 <= bool()  ctx >> t2 <= ty  ctx >> t3 <= ty
-----------------------------------------------------
           ctx >> ite(t1; t2; t3) <= ty

ctx >> tm1 => arr(ty1; ty2)  ctx >> tm2 <= ty1
----------------------------------------------
        ctx >> app(tm1; tm2) => ty2

ctx >> tm => ty
---------------
ctx >> tm <= ty|}

// [[_]] : tm -> core(val)
let dynamics = {|[[ val(v)          ]] = v
[[ annot(tm; ty)   ]] = [[ tm ]]
[[ ite(t1; t2; t3) ]] = case(
  [[ t1 ]];
  true() -> [[ t2 ]];
  false() -> [[ t3 ]]
)
[[ app(fun; arg)   ]] = app([[ fun ]]; [[ arg ]])|}