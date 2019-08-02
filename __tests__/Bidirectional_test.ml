open Jest
open Expect
open Bidirectional
open Belt.Result
module M = Belt.Map.String

let _ = describe "Bidirectional" (fun () ->
  let module P_statics = Parsing.Incremental(Parsing.Parseable_statics) in
  let module P_term    = Parsing.Incremental(Parsing.Parseable_term) in
  let statics_str = {|

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
  in

  let statics = match P_statics.parse statics_str with
    | Ok statics -> statics
    | Error msg  -> failwith msg
  in
  let parse_cvt str =
    let tm = match P_term.parse str with
      | Ok tm -> tm
      | Error msg -> failwith msg
    in
    let tm' = match Binding.DeBruijn.from_nominal tm with
      | Ok tm -> tm
      | Error msg -> failwith msg
    in
    Statics.of_de_bruijn tm'
  in
  let true_tm  = parse_cvt "true()" in
  let bool_ty  = parse_cvt "bool()" in
  let env      = { rules = statics; var_types = M.empty } in

  let ite = parse_cvt "ite(true(); false(); true())" in
  let annot_ite = parse_cvt "annot(ite(true(); false(); true()); bool())" in

  let lam_tm = parse_cvt "lam(x. true())" in
  let bool_to_bool = parse_cvt "arr(bool(); bool())" in

  let annot_lam = parse_cvt "annot(lam(x. true()); arr(bool(); bool()))" in


  let app_annot = Statics.(
    Operator ("app", [
      Scope ([], annot_lam);
      Scope ([], true_tm);
    ])
  ) in

  testAll "check / infer" [

    expect (check env (Typing (true_tm, bool_ty)))
      |> toBe ();
    expect (infer env true_tm)
      |> toEqual bool_ty;

    expect (check env (Typing (ite, bool_ty)))
      |> toBe ();
    expect (infer env ite)
      |> toEqual bool_ty;
    expect (infer env annot_ite)
      |> toEqual bool_ty;

    expect (check env (Typing (lam_tm, bool_to_bool)))
      |> toBe ();

    expect (infer env annot_lam)
      |> toEqual bool_to_bool;
    expect (check env (Typing (annot_lam, bool_to_bool)))
      |> toBe ();

    expect (infer env app_annot)
      |> toEqual bool_ty;
  ] Util.id;
)
