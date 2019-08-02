open Jest
open Expect
open Bidirectional
module M = Belt.Map.String

let _ = describe "Bidirectional" (fun () ->
  let module P_statics = Parsing.Incremental(Parsing.Parseable_statics) in
  let module P_term    = Parsing.Incremental(Parsing.Parseable_term) in
  let statics_str = {|

-------
ctx >> true() => bool

-------
ctx >> false() => bool

ctx >> tm1 => arr(ty1; ty2)   ctx >> tm2 <= ty1
-------
ctx >> app(tm1; tm2) => ty2

ctx, x : ty1 >> body <= ty2
-------
ctx >> lam(x. body) <= arr(ty1; ty2)

-------
ctx >> annot(tm; ty) => ty

ctx >> tm => ty
-------
ctx >> tm <= ty
  |}
  in
  let Belt.Result.Ok statics = P_statics.parse statics_str in
  test "check fun" (fun () ->
    (* let Belt.Result.Ok tm = P_term.parse "annot(lam(x. true()); arr(bool; bool))" in *)
    (* let Belt.Result.Ok ty = P_term.parse "bool" in *)
    let ty = Types.Statics.Term ("bool", []) in
    let tm = Types.Statics.(
      Term ("annot", [
        Scope ([], Term("lam", [ Scope (["x"], Term ("true", [])) ]));
        Scope ([], Term("arr", [ Scope ([], ty); Scope ([], ty) ]));
      ])
    ) in
    expect (check { rules = statics; var_types = M.empty } (Typing (tm, ty)))
      |> toBe ();
  );
)
