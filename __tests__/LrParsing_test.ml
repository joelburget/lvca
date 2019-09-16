open Jest
open Expect
open LrParsing
module M = Belt.Map.Int
module SI = Belt.Set.Int

module Grammar : GRAMMAR = struct
  let grammar = M.fromArray
    [|
       (* E' *)
       0, { productions = [[Nonterminal 1]] }; (* E' -> E *)
       (* E *)
       1, { productions = [
         [Nonterminal 1; Terminal 0; Nonterminal 2]; (* E -> E + T *)
         [Nonterminal 2];                            (* E -> T *)
         ]
       };
       (* T *)
       2, { productions = [
         [Nonterminal 2; Terminal 1; Nonterminal 3]; (* T -> T * F *)
         [Nonterminal 3]                             (* T -> F *)
         ]
       };
       (* F *)
       3, { productions = [
         [Terminal 2; Nonterminal 1; Terminal 3]; (* F -> (E) *)
         [Terminal 4];                            (* F -> id *)
         ]
       };
    |]
end

let () = describe "LrParsing" (fun () ->

  let module Lr0' = Lr0(Grammar) in

  testAll "mk_item / view_item" [
    expect (view_item @@ mk_item' 0 0)
      |> toEqual { production_num = 0; position = 0 };
    expect (view_item @@ mk_item' 0 1)
      |> toEqual { production_num = 0; position = 1 };
    expect (view_item @@ mk_item' 1 0)
      |> toEqual { production_num = 1; position = 0 };
    expect (view_item @@ mk_item' 1 1)
      |> toEqual { production_num = 1; position = 1 };
  ] Util.id;

  (* I0 *)
  let items0 = [| mk_item' 0 0 |] in
  let expected0 =
    { kernel_items = SI.fromArray items0;
      nonkernel_items = SI.fromArray
        [| mk_item' 1 0;
           mk_item' 2 0;
           mk_item' 3 0;
           mk_item' 4 0;
           mk_item' 5 0;
           mk_item' 6 0;
        |]
    }
  in

  (* I1 *)
  let items1 = [|
    mk_item' 0 1;
    mk_item' 1 1;
  |]
  in

  let expected1 =
    { kernel_items = SI.fromArray items1;
      nonkernel_items = SI.fromArray [||];
    }
  in

  (* I7 *)
  let items7 = [| mk_item' 3 2 |] in
  let expected7 =
    { kernel_items = SI.fromArray items7;
      nonkernel_items = SI.fromArray [| mk_item' 5 0; mk_item' 6 0 |];
    }
  in

  testAll "closure" [
    expect (Lr0'.closure @@ SI.fromArray items0)
      |> toEqual expected0;
    expect (Lr0'.closure @@ SI.fromArray items1)
      |> toEqual expected1;
    expect (Lr0'.closure @@ SI.fromArray items7)
      |> toEqual expected7;
  ] Util.id;

  let goto_kernel = SI.fromArray [| mk_item' 1 2 |] in

  let goto_nonkernel = SI.fromArray [|
    mk_item' 3 0;
    mk_item' 4 0;
    mk_item' 5 0;
    mk_item' 6 0;
  |]
  in

  testAll "goto" [
    expect (Lr0'.goto_kernel (SI.fromArray items1) 0)
      |> toEqual goto_kernel;
    expect (Lr0'.goto (SI.fromArray items1) 0)
      |> toEqual
      { kernel_items = goto_kernel; nonkernel_items = goto_nonkernel };
  ] Util.id;


  Js.log2 "M.get Lr0'.items' 0" (M.get Lr0'.items' 0);
  testAll "items" [
    expect (Lr0'.state_to_item_set 0 == SI.fromArray items0) |> toBe true;
    (*
    expect (M.get Lr0'.items' 1 == Some (SI.fromArray items1)) |> toBe true;
    expect (M.get Lr0'.items' 7 == Some (SI.fromArray items7)) |> toBe true;
    *)
  ] Util.id;
)
