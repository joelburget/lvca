module P_statics = Parsing.Incremental (Parsing.Parseable_statics)
module P_term = Parsing.Incremental (Parsing.Parseable_term)
open Bidirectional

let statics_str =
  {|

----------------------- (infer true)
ctx >> true() => bool()

------------------------ (infer false)
ctx >> false() => bool()

ctx >> tm1 => arr(ty1; ty2)   ctx >> tm2 <= ty1
----------------------------------------------- (infer app)
          ctx >> app(tm1; tm2) => ty2

    ctx, x : ty1 >> body <= ty2
------------------------------------ (check lam)
ctx >> lam(x. body) <= arr(ty1; ty2)

     ctx >> tm <= ty
-------------------------- (infer annot)
ctx >> annot(tm; ty) => ty

ctx >> t1 <= bool()  ctx >> t2 => ty  ctx >> t3 => ty
----------------------------------------------------- (infer ite)
           ctx >> ite(t1; t2; t3) => ty

ctx >> tm => ty
--------------- (reverse)
ctx >> tm <= ty
|}
;;

let statics =
  match P_statics.parse statics_str with
  | Ok statics -> statics
  | Error msg -> failwith msg
;;

let parse_cvt str =
  let tm =
    match P_term.parse str with
    | Ok tm -> tm
    | Error msg -> failwith msg
  in
  let tm' =
    match Binding.DeBruijn.from_nominal tm with
    | Ok tm -> tm
    | Error msg -> failwith msg
  in
  Statics.of_de_bruijn tm'
;;

let true_tm = parse_cvt "true()"
let bool_ty = parse_cvt "bool()"
let env = { rules = statics; var_types = Belt.Map.String.empty }
let ite = parse_cvt "ite(true(); false(); true())"
let annot_ite = parse_cvt "annot(ite(true(); false(); true()); bool())"
let lam_tm = parse_cvt "lam(x. true())"
let bool_to_bool = parse_cvt "arr(bool(); bool())"
let annot_lam = parse_cvt "annot(lam(x. true()); arr(bool(); bool()))"

let app_annot =
  Statics.(Operator ("app", [ Scope ([], annot_lam); Scope ([], true_tm) ]))
;;
