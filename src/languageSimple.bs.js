// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';


var abstractSyntax = "tm :=\n  | var(var)\n  | annot(tm; ty)\n  | ite(tm; tm; tm)\n  | app(tm; tm)\n  | val(val)\n  | binary-op(binary-op)\n\nbinary-op :=\n  | or(tm; tm)\n  | xor(tm; tm)\n  | and(tm; tm)\n\nval :=\n  | true()\n  | false()\n  | lam(val. tm)\n\nty :=\n  | bool()\n  | arr(ty; ty)";

var statics = "\n-----------------------\nctx >> true() => bool()\n\n------------------------\nctx >> false() => bool()\n\n  ctx >> v => ty\n-------------------\nctx >> val(v) => ty\n\n      ctx >> tm <= ty\n--------------------------\nctx >> annot(tm; ty) => ty\n\nctx >> t1 <= bool()  ctx >> t2 <= ty  ctx >> t3 <= ty\n-----------------------------------------------------\n           ctx >> ite(t1; t2; t3) <= ty\n\nctx >> tm1 => arr(ty1; ty2)  ctx >> tm2 <= ty1\n----------------------------------------------\n        ctx >> app(tm1; tm2) => ty2\n\nctx >> tm => ty\n---------------\nctx >> tm <= ty";

var dynamics = "[[ value(v)        ]] = v\n[[ annot(tm; ty)   ]] = [[ tm ]]\n[[ ite(t1; t2; t3) ]] = case([[ t1 ]]; true() -> [[ t2 ]]; false() -> [[ t3 ]])\n[[ app(fun; arg)   ]] = app([[ fun ]]; [[ arg ]])";

exports.abstractSyntax = abstractSyntax;
exports.statics = statics;
exports.dynamics = dynamics;
/* No side effect */
