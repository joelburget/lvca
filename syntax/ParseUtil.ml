open Base
open Lvca_provenance

type +'a t = ('a * OptRange.t) Angstrom.t

let parse_string_pos p str = Angstrom.parse_string ~consume:All p str
let parse_string p str = parse_string_pos p str |> Result.map ~f:fst

module type Comment_int = sig
  val comment : unit Angstrom.t
end

module type Junkless_int = sig
  type +'a t = ('a * OptRange.t) Angstrom.t

  val ( >>== ) : 'a t -> (pos:OptRange.t -> 'a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>|| ) : 'a t -> (pos:OptRange.t -> 'a -> 'b * OptRange.t) -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val char_lit : char t
  val identifier : string t
  val integer_lit : string t
  val integer_or_float_lit : (string, float) Either.t t
  val string_lit : string t
  val pos : int t
  val option : 'a -> 'a t -> 'a t
  val return : ?pos:OptRange.t -> 'a -> 'a t
  val attach_pos : 'a t -> ('a * OptRange.t) t
  val satisfy : (char -> bool) -> char t
  val count : int -> 'a t -> 'a list t
  val lift : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val char : char -> char t
  val string : string -> string t
  val many : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t
  val sep_by : _ t -> 'a t -> 'a list t
  val sep_by1 : _ t -> 'a t -> 'a list t
  val sep_end_by : _ t -> 'a t -> 'a list t
  val parens : 'a t -> 'a t
  val braces : 'a t -> 'a t
  val brackets : 'a t -> 'a t
  val fail : string -> 'a t
  val ( *> ) : 'a Angstrom.t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b Angstrom.t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val choice : ?failure_msg:string -> 'a t list -> 'a t
  val ( <?> ) : 'a t -> string -> 'a t
  val fix : ('a t -> 'a t) -> 'a t
end

module type Parsers_int = sig
  include Junkless_int

  val junk : unit t
end

(** Parser combinators that *don't* handle trailing whitespace / comments. *)
module Junkless : Junkless_int = struct
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
end

let whitespace = Angstrom.(take_while Char.is_whitespace *> return ())
let whitespace1 = Angstrom.(take_while1 Char.is_whitespace *> return ())

module NoComment : Comment_int = struct
  let comment = Angstrom.fail "no comment"
end

(** C-style comments *)
module CComment : Comment_int = struct
  open Angstrom

  let comment =
    string "//" >>= fun _ -> many (satisfy Char.(fun x -> x <> '\n')) >>| fun _ -> ()
  ;;
end

(** Parser combinators that clean up trailing whitespace / comments. *)
module Mk (Comment : Comment_int) : Parsers_int = struct
  include Junkless

  let junk = Angstrom.(many (whitespace1 <|> Comment.comment) >>| fun _ -> (), None)
  let char_lit = char_lit <* junk
  let identifier = identifier <* junk
  let integer_lit = integer_lit <* junk
  let integer_or_float_lit = integer_or_float_lit <* junk
  let string_lit = string_lit <* junk
  let char c = char c <* junk
  let string str = string str <* junk
  let many p = many p <* junk
  let many1 p = many1 p <* junk
  let sep_by s p = sep_by s p <* junk
  let sep_by1 s p = sep_by1 s p <* junk
  let parens p = parens (junk *> p) <* junk
  let braces p = braces (junk *> p) <* junk
  let brackets p = brackets (junk *> p) <* junk
end

let%test_module "Parsing" =
  (module struct
    let ( = ) =
      Result.equal (Lvca_util.Tuple2.equal String.( = ) OptRange.( = )) String.( = )
    ;;

    module Parse = Mk (CComment)

    let mk a b = Some (Range.mk a b)

    let%test _ = parse_string_pos Parse.string_lit {|"abc"|} = Ok ("abc", mk 0 5)
    let%test _ = parse_string_pos Parse.string_lit {|"\""|} = Ok ({|"|}, mk 0 4)
    let%test _ = parse_string_pos Parse.string_lit {|"\\"|} = Ok ({|\|}, mk 0 4)

    let ( = ) =
      Result.equal
        (Lvca_util.Tuple2.equal (Either.equal String.( = ) Float.( = )) OptRange.( = ))
        String.( = )
    ;;

    let%test _ =
      parse_string_pos Parse.integer_or_float_lit "123" = Ok (First "123", mk 0 3)
    ;;

    let%test _ =
      parse_string_pos Parse.integer_or_float_lit "-123" = Ok (First "-123", mk 0 4)
    ;;

    let%test _ =
      parse_string_pos Parse.integer_or_float_lit "+123" = Ok (First "+123", mk 0 4)
    ;;

    let%test _ =
      parse_string_pos Parse.integer_or_float_lit "1.1" = Ok (Second 1.1, mk 0 3)
    ;;

    let%test _ =
      parse_string_pos Parse.integer_or_float_lit "-1.1" = Ok (Second (-1.1), mk 0 4)
    ;;

    let%test _ =
      parse_string_pos Parse.integer_or_float_lit "+1.1" = Ok (Second 1.1, mk 0 4)
    ;;

    let%test _ = parse_string_pos Parse.integer_or_float_lit "1." = Ok (Second 1., mk 0 2)

    let ( = ) =
      Result.equal
        (Lvca_util.Tuple2.equal (List.equal String.( = )) OptRange.( = ))
        String.( = )
    ;;

    let%test _ =
      parse_string_pos Parse.(sep_end_by (char ';') string_lit) {|"abc"|}
      = Ok ([ "abc" ], mk 0 5)
    ;;

    let%test _ =
      parse_string_pos Parse.(sep_end_by (char ';') string_lit) {|"abc"; "def"|}
      = Ok ([ "abc"; "def" ], mk 0 12)
    ;;

    let%test _ =
      parse_string_pos Parse.(sep_end_by (char ';') string_lit) {|"abc"; "def";|}
      = Ok ([ "abc"; "def" ], mk 0 13)
    ;;

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
  end)
;;
