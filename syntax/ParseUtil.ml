open Base
open Lvca_provenance
open ParseUtil_intf

type +'a t = latest_pos:int -> 'a ParseResult.t Angstrom.t

module ParseResult = ParseResult

module ParseString = struct
  let parse_string_pos p str = Angstrom.parse_string ~consume:All (p ~latest_pos:0) str

  let parse_string p str =
    parse_string_pos p str |> Result.map ~f:(fun ParseResult.{ value; _ } -> value)
  ;;
end

include ParseString

(** Parser combinators that *don't* handle trailing whitespace / comments. *)
module Junkless = struct
  include Angstrom

  type +'a t = ('a * OptRange.t) Angstrom.t

  let integer_lit_no_range = take_while1 Char.is_digit

  let integer_lit =
    lift3
      (fun start lit finish -> lit, Some Range.{ start; finish })
      pos
      integer_lit_no_range
      pos
  ;;

  let sign =
    peek_char
    >>= function
    | Some '-' -> advance 1 >>| fun () -> "-"
    | Some '+' -> advance 1 >>| fun () -> "+"
    | Some c when Char.is_digit c -> return ""
    | _ -> fail "Sign or digit expected"
  ;;

  let integer_or_float_lit =
    pos
    >>= fun start ->
    sign
    >>= fun sign ->
    integer_lit_no_range
    >>= fun whole ->
    choice
      [ (char '.'
        >>= fun _ ->
        option "" integer_lit_no_range
        >>= fun part ->
        return (Either.Second (Float.of_string (sign ^ whole ^ "." ^ part))))
      ; return (Either.First (sign ^ whole))
      ]
    >>= fun result -> pos >>| fun finish -> result, Some Range.{ start; finish }
  ;;

  let string_lit =
    lift3
      (fun start result finish -> result, Some Range.{ start; finish })
      pos
      (char '"' *> AngstromStr.str (Buffer.create 0x100))
      pos
  ;;

  let char_lit =
    lift3
      (fun start result finish -> result, Some Range.{ start; finish })
      pos
      (char '\'' *> any_char <* char '\'')
      pos
  ;;

  let identifier =
    lift4
      (fun start c cs finish -> String.(of_char c ^ cs), Some Range.{ start; finish })
      pos
      (satisfy Char.(fun c -> is_alpha c || c = '_'))
      (take_while Char.(fun c -> is_alpha c || is_digit c || c = '_' || c = '\''))
      pos
  ;;

  let char c =
    lift3
      (fun start result finish -> result, Some Range.{ start; finish })
      pos
      (char c)
      pos
  ;;

  let string str =
    lift3
      (fun start result finish -> result, Some Range.{ start; finish })
      pos
      (string str)
      pos
  ;;

  let mk_list_parser p =
    Angstrom.lift3
      (fun start x finish -> List.map ~f:fst x, Some Range.{ start; finish })
      Angstrom.pos
      p
      Angstrom.pos
  ;;

  let many p = mk_list_parser (Angstrom.many p)
  let many1 p = mk_list_parser (Angstrom.many1 p)
  let sep_by s p = mk_list_parser (Angstrom.sep_by s p)
  let sep_by1 s p = mk_list_parser (Angstrom.sep_by1 s p)

  let sep_end_by s p =
    let open Angstrom in
    let seb =
      fix (fun seb ->
          let seb1 =
            p >>= fun x -> choice [ lift2 (fun _ xs -> x :: xs) s seb; return [ x ] ]
          in
          choice [ seb1; return [] ])
    in
    mk_list_parser seb
  ;;

  let mk_bracket_parser open_c close_c p =
    Angstrom.lift3
      (fun (_, rng1) (v, _) (_, rng2) -> v, OptRange.union rng1 rng2)
      (char open_c)
      p
      (char close_c)
  ;;

  let parens p = mk_bracket_parser '(' ')' p
  let braces p = mk_bracket_parser '{' '}' p
  let brackets p = mk_bracket_parser '[' ']' p
  let ( >>== ) a f = Angstrom.(a >>= fun (a, pos) -> f ~pos a)
  let ( >>= ) a f = a >>== fun ~pos:_ a -> f a
  let ( >>|| ) a f = Angstrom.(a >>| fun (a, pos) -> f ~pos a)
  let ( >>| ) a f = a >>|| fun ~pos a -> f a, pos
  let ( <$> ) f a = a >>| f
  let ( <*> ) f a = f >>= fun f' -> a >>| f'
  let pos = Angstrom.(pos >>| fun p -> p, OptRange.mk p p)
  let option a = Angstrom.option (a, None)
  let return ?(pos = None) a = Angstrom.return (a, pos)
  let attach_pos p = p >>|| fun ~pos t -> (t, pos), pos

  let satisfy f =
    Angstrom.(pos >>= fun p -> satisfy f >>| fun c -> c, OptRange.mk p (p + 1))
  ;;

  let count n p =
    let open Angstrom in
    pos
    >>= fun p1 ->
    count n p
    >>= fun result ->
    pos >>= fun p2 -> return (result |> List.map ~f:fst, OptRange.mk p1 p2)
  ;;

  let lift f a = Angstrom.lift (fun (a, pos) -> f a, pos) a

  let lift2 f a b =
    Angstrom.lift2
      (fun (a, a_pos) (b, b_pos) -> f a b, OptRange.list_range [ a_pos; b_pos ])
      a
      b
  ;;

  let lift3 f a b c =
    Angstrom.lift3
      (fun (a, a_pos) (b, b_pos) (c, c_pos) ->
        f a b c, OptRange.list_range [ a_pos; b_pos; c_pos ])
      a
      b
      c
  ;;

  let lift4 f a b c d =
    Angstrom.lift4
      (fun (a, a_pos) (b, b_pos) (c, c_pos) (d, d_pos) ->
        f a b c d, OptRange.list_range [ a_pos; b_pos; c_pos; d_pos ])
      a
      b
      c
      d
  ;;

  let parse_string_pos p str = Angstrom.parse_string ~consume:All p str
  let parse_string p str = parse_string_pos p str |> Result.map ~f:fst
end

let whitespace = Angstrom.(take_while Char.is_whitespace *> return ())
let whitespace1 = Angstrom.(take_while1 Char.is_whitespace *> return ())

module NoComment = struct
  let comment = Angstrom.fail "no comment"
end

(** C-style comments *)
module CComment = struct
  open Angstrom

  let comment =
    string "//" >>= fun _ -> many (satisfy Char.(fun x -> x <> '\n')) >>| fun _ -> ()
  ;;
end

(** Parser combinators that clean up trailing whitespace / comments. *)
module Mk (Comment : Comment_s) = struct
  module ParseResult = ParseResult
  module JL = Junkless
  open Angstrom
  include ParseString

  type +'a t = latest_pos:int -> 'a ParseResult.t Angstrom.t

  let junk ~latest_pos =
    many (whitespace1 <|> Comment.comment)
    >>| fun _ -> ParseResult.{ latest_pos; value = (); range = None }
  ;;

  let ( <* ) p1 p2 ~latest_pos =
    p1 ~latest_pos
    >>= fun result -> p2 ~latest_pos:result.ParseResult.latest_pos >>| fun _ -> result
  ;;

  let ( *> ) p1 p2 ~latest_pos =
    p1 ~latest_pos >>= fun ParseResult.{ latest_pos; _ } -> p2 ~latest_pos
  ;;

  let ( <|> ) p1 p2 ~latest_pos = p1 ~latest_pos <|> p2 ~latest_pos

  let ( <*> ) p1 p2 ~latest_pos =
    p1 ~latest_pos
    >>= fun result1 ->
    p2 ~latest_pos:result1.ParseResult.latest_pos
    >>= fun result2 ->
    return
      ParseResult.
        { value = result1.value result2.value
        ; latest_pos = result2.latest_pos
        ; range = OptRange.union result1.range result2.range
        }
  ;;

  let ( >>|| ) p f ~latest_pos = p ~latest_pos >>| f

  let ( >>| ) p f ~latest_pos =
    p ~latest_pos >>| fun result -> ParseResult.{ result with value = f result.value }
  ;;

  let ( >>== ) p f ~latest_pos =
    p ~latest_pos
    >>= fun (ParseResult.{ latest_pos; _ } as parse_result) -> f parse_result ~latest_pos
  ;;

  let ( >>= ) p f ~latest_pos =
    p ~latest_pos >>= fun ParseResult.{ value; latest_pos; _ } -> f value ~latest_pos
  ;;

  let ( <?> ) p msg ~latest_pos = p ~latest_pos <?> msg

  let adapt (junkless_p : ('a * OptRange.t) Angstrom.t) : 'a t =
   fun ~latest_pos ->
    let f (value, range) =
      let latest_pos =
        match range with None -> latest_pos | Some Range.{ finish; _ } -> finish
      in
      Angstrom.return ParseResult.{ latest_pos; value; range }
    in
    Angstrom.(junkless_p >>= f)
 ;;

  let whitespace ~latest_pos =
    Angstrom.(
      whitespace >>| fun _ -> ParseResult.{ value = (); range = None; latest_pos })
  ;;

  let whitespace1 ~latest_pos =
    Angstrom.(
      whitespace1 >>| fun _ -> ParseResult.{ value = (); range = None; latest_pos })
  ;;

  let char_lit = adapt JL.char_lit <* junk
  let identifier = adapt JL.identifier <* junk
  let integer_lit = adapt JL.integer_lit <* junk
  let integer_or_float_lit = adapt JL.integer_or_float_lit <* junk
  let string_lit = adapt JL.string_lit <* junk
  let char c = adapt (JL.char c) <* junk
  let string str = adapt (JL.string str) <* junk
  let ( <$> ) f p = p >>| f
  let choice = failwith "TODO"
  let fail msg ~latest_pos:_ = fail msg

  let lift f a ~latest_pos =
    Angstrom.lift
      (fun (ParseResult.{ value; _ } as parse_result) ->
        { parse_result with value = f value })
      (a ~latest_pos)
  ;;

  let lift2 f a b ~latest_pos =
    Angstrom.lift2
      (fun ParseResult.{ value = a; range = a_range; _ }
           ParseResult.{ value = b; range = b_range; latest_pos } ->
        ParseResult.{ value = f a b; range = OptRange.union a_range b_range; latest_pos })
      (a ~latest_pos)
      (b ~latest_pos)
  ;;

  let lift3 f a b c ~latest_pos =
    Angstrom.lift3
      (fun ParseResult.{ value = a; range = a_range; _ }
           ParseResult.{ value = b; range = b_range; _ }
           ParseResult.{ value = c; range = c_range; latest_pos } ->
        ParseResult.
          { value = f a b c
          ; range = OptRange.list_range [ a_range; b_range; c_range ]
          ; latest_pos
          })
      (a ~latest_pos)
      (b ~latest_pos)
      (c ~latest_pos)
  ;;

  let lift4 f a b c d ~latest_pos =
    Angstrom.lift4
      (fun ParseResult.{ value = a; range = a_range; _ }
           ParseResult.{ value = b; range = b_range; _ }
           ParseResult.{ value = c; range = c_range; _ }
           ParseResult.{ value = d; range = d_range; latest_pos } ->
        ParseResult.
          { value = f a b c d
          ; range = OptRange.list_range [ a_range; b_range; c_range; d_range ]
          ; latest_pos
          })
      (a ~latest_pos)
      (b ~latest_pos)
      (c ~latest_pos)
      (d ~latest_pos)
  ;;

  let count n p ~latest_pos =
    let open Angstrom in
    pos
    >>= fun p1 ->
    count n (p ~latest_pos)
    >>= fun result ->
    pos >>= fun p2 -> return (result |> List.map ~f:fst, OptRange.mk p1 p2)
  ;;

  let satisfy f ~latest_pos:_ =
    Angstrom.(pos >>= fun p -> satisfy f >>| fun c -> c, OptRange.mk p (p + 1))
  ;;

  let attach_pos p ~latest_pos =
    Angstrom.(
      p ~latest_pos
      >>| fun (ParseResult.{ value; range; _ } as parse_result) ->
      { parse_result with value = value, range })
  ;;

  let return ?pos value ~latest_pos =
    Angstrom.return ParseResult.{ value; range = pos; latest_pos }
  ;;

  let option value p ~latest_pos =
    Angstrom.option ParseResult.{ value; range = None; latest_pos } p
  ;;

  let pos ~latest_pos = ParseResult.{ value = latest_pos; range = None; latest_pos }
  let fix f ~latest_pos:_ = Angstrom.fix f
  let many _p ~latest_pos:_ = failwith "TODO"
  let many1 _p ~latest_pos:_ = failwith "TODO"
  let sep_by _s _p ~latest_pos:_ = failwith "TODO"
  let sep_by1 _s _p ~latest_pos:_ = failwith "TODO"
  let sep_end_by _s _p ~latest_pos:_ = failwith "TODO"
  let parens _p ~latest_pos:_ = failwith "TODO"
  let braces _p ~latest_pos:_ = failwith "TODO"
  let brackets _p ~latest_pos:_ = failwith "TODO"
end

let%test_module "Parsing" =
  (module struct
    module Parse = Mk (CComment)

    let ( = ) = Result.equal (ParseResult.equal String.( = )) String.( = )
    let mk a b = Some (Range.mk a b)

    let%test _ =
      Parse.parse_string_pos Parse.string_lit {|"abc"|}
      = Ok ParseResult.{ value = "abc"; range = mk 0 5; latest_pos = 5 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.string_lit {|"\""|}
      = Ok ParseResult.{ value = {|"|}; range = mk 0 4; latest_pos = 4 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.string_lit {|"\\"|}
      = Ok ParseResult.{ value = {|\|}; range = mk 0 4; latest_pos = 4 }
    ;;

    let ( = ) =
      Result.equal
        (ParseResult.equal (Either.equal String.( = ) Float.( = )))
        String.( = )
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.integer_or_float_lit "123"
      = Ok ParseResult.{ value = First "123"; range = mk 0 3; latest_pos = 4 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.integer_or_float_lit "-123"
      = Ok ParseResult.{ value = First "-123"; range = mk 0 4; latest_pos = 4 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.integer_or_float_lit "+123"
      = Ok ParseResult.{ value = First "+123"; range = mk 0 4; latest_pos = 4 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.integer_or_float_lit "1.1"
      = Ok ParseResult.{ value = Second 1.1; range = mk 0 3; latest_pos = 4 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.integer_or_float_lit "-1.1"
      = Ok ParseResult.{ value = Second (-1.1); range = mk 0 4; latest_pos = 4 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.integer_or_float_lit "+1.1"
      = Ok ParseResult.{ value = Second 1.1; range = mk 0 4; latest_pos = 4 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.integer_or_float_lit "1."
      = Ok ParseResult.{ value = Second 1.; range = mk 0 2; latest_pos = 4 }
    ;;

    let ( = ) = Result.equal (ParseResult.equal (List.equal String.( = ))) String.( = )

    let%test _ =
      Parse.parse_string_pos Parse.(sep_end_by (char ';') string_lit) {|"abc"|}
      = Ok ParseResult.{ value = [ "abc" ]; range = mk 0 5; latest_pos = 4 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.(sep_end_by (char ';') string_lit) {|"abc"; "def"|}
      = Ok ParseResult.{ value = [ "abc"; "def" ]; range = mk 0 12; latest_pos = 4 }
    ;;

    let%test _ =
      Parse.parse_string_pos Parse.(sep_end_by (char ';') string_lit) {|"abc"; "def";|}
      = Ok ParseResult.{ value = [ "abc"; "def" ]; range = mk 0 13; latest_pos = 4 }
    ;;

    (*
    let%expect_test _ =
      let open Junkless in
      let junk = Parse.junk in
      let parse =
        parse_string_pos
          (sep_by1 (junk *> string "->") (junk *> char '*') <* junk <* string "foo")
      in
      let result1 = parse "* -> *\nfoo" in
      (*                   0123456 7890 *)
      let result2 = parse "* -> * // asjdflksajfklasjfsal\nfoo" in
      (*                   0123456789012345678901234567890 1234
                                     1         2         3
       *)
      (match result1, result2 with
      | Ok (_, rng1), Ok (_, rng2) -> Fmt.pr "%a, %a\n" OptRange.pp rng1 OptRange.pp rng2
      | _ -> Fmt.pr "not okay\n");
      [%expect {|{0,6}, {0,6}|}]
    ;;
    *)
  end)
;;
