open Jest
open Expect
open Types

(* expected serialized values via http://cbor.me/ *)

let _ = describe "Ast.(jsonify, serialize, hash)" (fun () ->
  let open Ast in

  let serialize tm = ArrayBuffer.to_hex
    (Uint8Array.to_array_buffer
    (Ast.serialize tm)) in

  let tm = Var "x" in
  testAll "(var) x"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["v", "x"] |}];
      expect (serialize tm)
      |> toBe "8261766178";
      expect (hash tm)
      |> toBe "bbc37ed1e26f8481398dc797bd0b3ec2e3ae954e1c3f69b461a487d8703ec3d6";
    ]
    (fun x -> x);

  let tm = Term ("Z", []) in
  testAll "Z()"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["t", "Z", []] |} ];
      expect (serialize tm)
      |> toBe "836174615a80";
      expect (hash tm)
      |> toBe "cc55b934e76de136a1664dc89c473b2fdc52948d8ba4394bfad5e1219841ffb3";
    ]
    (fun x -> x);

  let tm = Term ("S", [Scope (["x"], Var "x")]) in
  testAll "S(x. x)"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["t", "S", [[["x"], ["v", "x"]]]] |}];
      expect (serialize tm)
      |> toBe "836174615381828161788261766178";
      expect (hash tm)
      |> toBe "22e98205b448e5d79e3dd8fe46469288e9292c0a10eb1b6eb0b896d54e016661";
    ]
    (fun x -> x);

  let tm = Primitive (PrimBool true) in
  testAll "true"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["p", ["b", true]] |}];
      expect (serialize tm)
      |> toBe "826170826162f5";
      expect (hash tm)
      |> toBe "9550353a3a0ae660a7bf31bb592f795f31babd8ea8af329394baa3ae860cb759";
    ]
    (fun x -> x);

  let tm = Primitive (PrimInteger (Bigint.of_string "12345")) in
  testAll "12345"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["p", ["i", "12345"]] |}];
      expect (serialize tm)
      |> toBe "826170826169653132333435";
      expect (hash tm)
      |> toBe "e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5";
    ]
    (fun x -> x);

  let tm = Sequence [] in
  testAll "[]"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["s", []] |}];
      expect (serialize tm)
      |> toBe "82617380";
      expect (hash tm)
      |> toBe "8afbfb879b5a95214c4c483c401313235040663bbdc08220992a5841801a421e";
    ]
    (fun x -> x);

  let tm = Sequence [Var "x"] in
  testAll "[x]"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["s", [["v", "x"]]] |}];
      expect (serialize tm)
      |> toBe "826173818261766178";
      expect (hash tm)
      |> toBe "28b6e8f2124dd5931d69e1a5350f5c44ebdec7e0f6be9f98d2c717fcf09fa3d8";
    ]
    (fun x -> x);
)
