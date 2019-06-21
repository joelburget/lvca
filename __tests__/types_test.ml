open Jest
let _ =
  describe "Expect"
    (fun ()  ->
       let open Expect in
         test "toBe" (fun ()  -> (expect (1 + 2)) |> (toBe 3)))
let _ =
  describe "Expect.Operators"
    (fun ()  ->
       let open Expect in
         let open! Expect.Operators in
           test "==" (fun ()  -> (expect (1 + 2)) == 3))
