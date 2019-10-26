// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';


var abstractSyntax = "\ntm :=\n  | true()\n  | false()\n  | ite(tm; tm; tm)\n  | annot(tm; ty)\n  | app(tm; tm)\n  | fun(tm. tm)\n\nty :=\n  | bool()\n  | arr(ty; ty)";

var concrete = "\nCOLON := \":\"\nIF    := \"if\"\nTHEN  := \"then\"\nELSE  := \"else\"\nFUN   := \"fun\"\nARROW := \"->\"\nTRUE  := \"true\"\nFALSE := \"false\"\nBOOL  := \"bool\"\nID    := [a-zA-Z][a-zA-Z0-9_]*\nSPACE := [ ]+\n\ntm :=\n  | ID                    { var($1)         }\n  | IF tm THEN tm ELSE tm { ite($2; $4; $6) }\n  | tm COLON ty           { annot($1; $3)   }\n  | FUN ID ARROW tm       { fun($2; $4)     }\n  | TRUE                  { true()          }\n  | FALSE                 { false()         }\n  > tm _ tm               { app($1; $2)     } // %right\n\nty :=\n  | BOOL         { bool()      }\n  | ty ARROW ty  { arr($1; $3) }\n";

var statics = "\n----------------------- (true)\nctx >> true() => bool()\n\n------------------------ (false)\nctx >> false() => bool()\n\n      ctx >> tm <= ty\n-------------------------- (annot)\nctx >> annot(tm; ty) => ty\n\nctx >> t1 <= bool()  ctx >> t2 <= ty  ctx >> t3 <= ty\n----------------------------------------------------- (ite)\n           ctx >> ite(t1; t2; t3) <= ty\n\n    ctx, x : ty1 >> body <= ty2\n------------------------------------ (fun)\nctx >> fun(x. body) <= arr(ty1; ty2)\n\nctx >> tm1 => arr(ty1; ty2)  ctx >> tm2 <= ty1\n---------------------------------------------- (application)\n        ctx >> app(tm1; tm2) => ty2\n\n// important: this rule must go last or else it will subsume all others\nctx >> tm => ty\n--------------- (reverse)\nctx >> tm <= ty";

var dynamics = "[[ true() ]] = true()\n[[ false() ]] = false()\n[[ ite(t1; t2; t3) ]] = case [[ t1 ]] of {\n  | true()  -> [[ t2 ]]\n  | false() -> [[ t3 ]]\n}\n[[ annot(tm; ty) ]] = [[ tm ]]\n[[ app(fun; arg) ]] = app([[ fun ]]; [[ arg ]])\n[[ fun(x. body)  ]] = \\(x : bool) -> [[ body ]]";

exports.abstractSyntax = abstractSyntax;
exports.concrete = concrete;
exports.statics = statics;
exports.dynamics = dynamics;
/* No side effect */
