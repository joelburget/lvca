let abstractSyntax = {|tm :=
  | true()
  | false()
  | ite(tm; tm; tm)
  | annot(tm; ty)
  | app(tm; tm)
  | fun(tm. tm)

ty :=
  | bool()
  | arr(ty; ty)|}

let concrete = {|
COLON := ":"
IF    := "if"
THEN  := "then"
ELSE  := "else"
FUN   := "fun"
ARROW := "->"
TRUE  := "true"
FALSE := "false"
BOOL  := "bool"
ID    := /[a-zA-Z][a-zA-Z0-9_]*/
// SPACE := / +/

tm :=
  | ID                    { var($1)         }
  | IF tm THEN tm ELSE tm { ite($2; $4; $6) }
  | tm COLON ty           { annot($1; $3)   }
  | FUN ID ARROW tm       { fun($2; $4)     }
  | TRUE                  { true()          }
  | FALSE                 { false()         }
  > tm SPACE tm           { app($1; $2)     } // %right

ty :=
  | BOOL         { bool()      }
  | ty ARROW ty  { arr($1; $3) }
|}

// TODO:
// * how to show var separate from context?
// * how to separate hypotheses?
// * should we make var rule explicit instead of implicit?

let statics = {|
----------------------- (true)
ctx >> true() => bool()

------------------------ (false)
ctx >> false() => bool()

      ctx >> tm <= ty
-------------------------- (annot)
ctx >> annot(tm; ty) => ty

ctx >> t1 <= bool()  ctx >> t2 <= ty  ctx >> t3 <= ty
----------------------------------------------------- (ite)
           ctx >> ite(t1; t2; t3) <= ty

    ctx, x : ty1 >> body <= ty2
------------------------------------ (fun)
ctx >> fun(x. body) <= arr(ty1; ty2)

ctx >> tm1 => arr(ty1; ty2)  ctx >> tm2 <= ty1
---------------------------------------------- (application)
        ctx >> app(tm1; tm2) => ty2

// important: this rule must go last or else it will subsume all others
ctx >> tm => ty
--------------- (reverse)
ctx >> tm <= ty|}

// [[_]] : tm -> core(val)
let dynamics = {|[[ true() ]] = true()
[[ false() ]] = false()
[[ ite(t1; t2; t3) ]] = case [[ t1 ]] of {
  | true()  -> [[ t2 ]]
  | false() -> [[ t3 ]]
}
[[ annot(tm; ty) ]] = [[ tm ]]
[[ app(fun; arg) ]] = app([[ fun ]]; [[ arg ]])
[[ fun(x. body)  ]] = \(x : bool) -> [[ body ]]|}
