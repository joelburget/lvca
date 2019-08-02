open Jest
open Expect
open Bidirectional
module M = Belt.Map.String

let _ = describe "Bidirectional" (fun () ->
  let module P_statics = Parsing.Incremental(Parsing.Parseable_statics) in
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
  let Belt.Result.Ok statics = P_statics.parse statics_str in
  test "check / infer" (fun () ->
    let true_tm  = Statics.Term ("true", []) in
    let false_tm = Statics.Term ("true", []) in
    let bool_ty  = Statics.Term ("bool", []) in
    let env      = { rules = statics; var_types = M.empty } in

    expect (check env (Typing (true_tm, bool_ty)))
      |> toBe ();
    expect (infer env true_tm)
      |> toEqual bool_ty;

    let ite = Statics.(Term ("ite",
      [ Scope ([], true_tm);
        Scope ([], false_tm);
        Scope ([], true_tm);
      ]))
    in
    let annot_ite = Statics.(Term ("annot",
      [ Scope ([], ite);
        Scope ([], bool_ty);
      ]))
    in

    expect (check env (Typing (ite, bool_ty)))
      |> toBe ();
    expect (infer env ite)
      |> toEqual bool_ty;
    expect (infer env annot_ite)
      |> toEqual bool_ty;

    (* let Belt.Result.Ok tm = P_term.parse "annot(lam(x. true()); arr(bool; bool))" in *)
    (* let Belt.Result.Ok ty = P_term.parse "bool" in *)

    let lam_tm = Statics.(Term("lam", [ Scope (["x"], Term ("true", [])) ])) in
    let bool_to_bool = Statics.(Term("arr", [ Scope ([], bool_ty); Scope ([], bool_ty) ])) in

    let annot_lam = Statics.(
      Term ("annot", [
        Scope ([], lam_tm);
        Scope ([], bool_to_bool);
      ])
    ) in

    expect (check env (Typing (lam_tm, bool_to_bool)))
      |> toBe ();

    expect (infer env annot_lam)
      |> toEqual bool_to_bool;
    expect (check env (Typing (annot_lam, bool_to_bool)))
      |> toBe ();

    let app_annot = Statics.(
      Term ("app", [
        Scope ([], annot_lam);
        Scope ([], true_tm);
      ])
    ) in

    expect (infer env app_annot)
      |> toEqual bool_ty;
  );
)
