open Jest
open Expect
open Bidirectional
open Bidirectional_TestUtil

let _ = describe "Bidirectional" (fun () ->
  (* TODO: check these traces *)
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
