open Jest
open Expect
open Bitstring

let () = describe "Bitstring" (fun () ->
  let bs = alloc 10 false in
  testAll "operations" [
    expect (index 0) |> toEqual (0, 0);
    expect (index 7) |> toEqual (0, 7);
    expect (index 8) |> toEqual (1, 0);
    expect (index 9) |> toEqual (1, 1);
    expect (index 15) |> toEqual (1, 7);

    expect (bs |. get_exn 0) |> toEqual false;
    expect (bs |. set_exn 0 true) |> toEqual ();
    expect (bs |. get 0) |> toEqual (Some true);
    expect (bs |. set 0 false) |> toEqual (Some ());

    expect (bs |. get 9) |> toEqual (Some false);
    expect (bs |. get 10) |> toEqual None;
  ] Util.id;
)
