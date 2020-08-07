open Base

type 'a located = 'a * Range.t option
type 'a t = 'a located Angstrom.t

let (<>) r1 r2 =
  Range.{ start = min r1.start r2.start; finish = max r1.finish r2.finish }

let (</>) r1 r2 = match r1, r2 with
  | Some r1', Some r2' -> Some (r1' <> r2')
  | Some r, None
  | None, Some r -> Some r
  | None, None -> None

let list_range : Range.t list -> Range.t option
  = function
    | [] -> None
    | ps ->
      let start = ps
        |> List.min_elt ~compare:(fun r1 r2 -> compare r1.start r2.start)
        |> Option.value_exn
        |> fun r -> r.Range.start
      in
      let finish = ps
        |> List.max_elt ~compare:(fun r1 r2 -> compare r1.finish r2.finish)
        |> Option.value_exn
        |> fun r -> r.Range.finish
      in
      Some { start; finish }

      (*
let list_range' : ('a * Range.t) list -> 'a list located
  = fun lst ->
    let vs, rs = List.unzip lst in
    match vs with
      | [] -> [], None
      | _ ->
        begin
          match list_range rs with
            | Some range -> vs, Some range
            | None -> failwith "exn"
        end
*)

let char c =
  let open Angstrom in
  lift2 (fun p c -> (c, Some (Range.mk p (p + 1)))) pos (char c)

let take_while f =
  let open Angstrom in
  lift3 (fun p1 str p2 -> (str, Some (Range.mk p1 p2))) pos (take_while f) pos

let take_while1 f =
  let open Angstrom in
  lift3 (fun p1 str p2 -> (str, Some (Range.mk p1 p2))) pos (take_while1 f) pos

let string str =
  let open Angstrom in
  lift3 (fun p1 str p2 -> (str, Some (Range.mk p1 p2))) pos (string str) pos

let satisfy f =
  let open Angstrom in
  lift2 (fun p c -> (c, Some (Range.mk p (p + 1)))) pos (satisfy f)

let option p =
  let open Angstrom in
  option (None, None) (p >>| fun (a, range) -> Some a, range)

let count n p = Angstrom.(count n p >>| failwith "TODO")
let many p = Angstrom.(many p >>| failwith "TODO")
let many1 p = Angstrom.(many1 p >>| failwith "TODO")

let fix = Angstrom.fix
let (<|>) = Angstrom.(<|>)
let (<?>) = Angstrom.(<?>)
let fail = Angstrom.fail

let return = Angstrom.return

let (>>=) = Angstrom.(>>=)

let lift = Angstrom.lift
let lift2 = Angstrom.lift2
let lift3 = Angstrom.lift3
let lift4 = Angstrom.lift4

let expect_range = function
  | Some range -> range
  | None -> failwith "exn"

let var p =
  Angstrom.(p >>| fun (name, maybe_range) ->
    let range = expect_range maybe_range in
    Binding.Nominal.Var (range, name), Some range)

let primitive p =
  Angstrom.(p >>| fun (p, maybe_range) ->
    let range = expect_range maybe_range in
    Binding.Nominal.Primitive (range, p), Some range)

let scope_range : Range.t Binding.Nominal.scope -> Range.t
  = fun (Scope (patterns, body)) ->
    let patterns_range = patterns
      |> List.map ~f:Pattern.location
      |> list_range
    in
    let body_range = body
      |> List.map ~f:Binding.Nominal.location
      |> list_range
    in

    match patterns_range, body_range with
      | None, Some range
      -> range
      | Some r1, Some r2
      -> r1 <> r2
      | _, None
      -> failwith "exn"

let operator (name, name_range) scopes =
  let scopes_range = scopes
    |> List.map ~f:scope_range
    |> list_range
  in
  match name_range </> scopes_range with
    | Some range -> Angstrom.return (Binding.Nominal.Operator (range, name, scopes), Some range)
    | None -> failwith "exn"

let%test_module "Parsing Tests" = (module struct
  let print_parse parser str = match Angstrom.parse_string ~consume:All parser str with
    | Error msg -> Caml.Printf.printf "%s\n" msg
    | Ok (tm, _range) -> Binding.Nominal.pp_term_range Caml.Format.std_formatter tm
  ;;

  let%expect_test _ =
    print_parse (var (string "foo")) "foo";
    [%expect {| foo{0,3} |}]

  let%expect_test _ =
    print_parse (var (take_while1 Char.is_alpha)) "var";
    [%expect {| var{0,3} |}]

  let%expect_test _ =
    let p = primitive (lift3
      (fun _ (content, range) _ -> Primitive.PrimString content, range)
      (char '"')
      (take_while (fun c -> Char.(c <> '"')))
      (char '"'))
    in
    print_parse p {|"str"|};
    [%expect {| "str"{0,5} |}]
end);;
