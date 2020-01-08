open Jest
open Expect
open External (* modules BitArray, Cbor, Sha256, Uint8Array *)
open Types
open Binding

(* expected serialized values via http://cbor.me/ *)

let _ = describe "Nominal.(jsonify, serialize, hash)" (fun () ->
  let open Nominal in

  let serialize tm = ArrayBuffer.to_hex
    (Uint8Array.to_array_buffer
    (Nominal.serialize tm)) in

  let tm = Var "x" in
  testAll "(var) x"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["v", "x"] |}];
      expect (serialize tm)
      |> toBe "8261766178";
      expect (hash tm)
      |> toBe "bbc37ed1e26f8481398dc797bd0b3ec2e3ae954e1c3f69b461a487d8703ec3d6";
    ]
    Util.id;

  let tm = Operator ("Z", []) in
  testAll "Z()"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["o", "Z", []] |} ];
      expect (serialize tm)
      |> toBe "83616f615a80";
      expect (hash tm)
      |> toBe "2380ed848a0c5ce3d0ad7420e841578e4068f394b37b9b11bd3c34cea391436c";
    ]
    Util.id;

  let tm = Operator ("S", [Scope ([Var "x"], Var "x")]) in
  testAll "S(x. x)"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["o", "S", [[[["v", "x"]], ["v", "x"]]]] |}];
      expect (serialize tm)
      |> toBe "83616f615381828182617661788261766178";
      expect (hash tm)
      |> toBe "391e4a6e3dc6964d60c642c52416d18b102dca357a3e4953834dfefc0e02dfbc";
    ]
    Util.id;

  let tm = Primitive (PrimInteger (Bigint.of_string "12345")) in
  testAll "12345"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["p", ["i", "12345"]] |}];
      expect (serialize tm)
      |> toBe "826170826169653132333435";
      expect (hash tm)
      |> toBe "e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5";
    ]
    Util.id;

  let tm = Sequence [] in
  testAll "[]"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["s", []] |}];
      expect (serialize tm)
      |> toBe "82617380";
      expect (hash tm)
      |> toBe "8afbfb879b5a95214c4c483c401313235040663bbdc08220992a5841801a421e";
    ]
    Util.id;

  let tm = Sequence [Var "x"] in
  testAll "[x]"
    [ expect (jsonify tm)
      |> toEqual [%raw {| ["s", [["v", "x"]]] |}];
      expect (serialize tm)
      |> toBe "826173818261766178";
      expect (hash tm)
      |> toBe "28b6e8f2124dd5931d69e1a5350f5c44ebdec7e0f6be9f98d2c717fcf09fa3d8";
    ]
    Util.id;
)
