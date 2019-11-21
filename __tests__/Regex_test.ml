open Jest
open Expect

let _ = describe "Regex.(accepts_empty, to_string)"
  (fun () ->
    let (accepts_empty, to_string) = Regex.(accepts_empty, to_string) in

    testAll "accepts_empty"
      [ expect (accepts_empty (ReString "foo"))
        |> toBe false;
        expect
          (accepts_empty (ReConcat
            [ReStar (ReString "foo"); ReOption (ReString "bar")]
          ))
        |> toBe true;
        expect
          (accepts_empty (ReConcat
            [ReStar (ReString "foo"); RePlus (ReString "bar")]
          ))
        |> toBe false;
        expect (accepts_empty (ReClass Boundary))
        |> toBe true;
        expect (accepts_empty (ReClass Digit))
        |> toBe false;
        expect (accepts_empty (ReSet "a-z"))
        |> toBe false;
        expect (accepts_empty (RePlus (ReString "")))
        |> toBe true;
      ]
      Util.id;

    testAll "to_string"
      [ expect (to_string (ReConcat [ReString "foo"; ReString "bar"]))
        |> toBe {|foobar|};
        expect (to_string (ReSet "a-z"))
        |> toBe "[a-z]";
        (*
        expect (to_string
          (ReConcat [ReClass Boundary; ReClass (Negate Boundary)]))
        |> toBe {|\b\B|};
        *)
        expect (to_string (ReConcat
          [ ReStar (ReString "foo");
            RePlus (ReString "foo");
            ReOption (ReString "foo");
          ]))
        |> toBe {|(foo)*(foo)+(foo)?|};
      ]
      Util.id;
);
