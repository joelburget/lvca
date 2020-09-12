open Base

type 'a t = ('a * OptRange.t) Angstrom.t

let parse_string_pos p str = Angstrom.parse_string ~consume:All p str
let parse_string p str = parse_string_pos p str |> Result.map ~f:fst

module type Comment_int = sig
  val comment : unit Angstrom.t
end

module type Parsers = sig
  type +'a t = ('a * OptRange.t) Angstrom.t

  val return : ?pos:OptRange.t -> 'a -> 'a t
  val fail : string -> 'a t
  val ( *> ) : 'a Angstrom.t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b Angstrom.t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val choice : ?failure_msg:string -> 'a t list -> 'a t
  val ( <?> ) : 'a t -> string -> 'a t
  val fix : ('a t -> 'a t) -> 'a t
  val many : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t
  val sep_by : _ t -> 'a t -> 'a list t
  val sep_by1 : _ t -> 'a t -> 'a list t
  val option : 'a -> 'a t -> 'a t
  val ( >>== ) : 'a t -> (pos:OptRange.t -> 'a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>|| ) : 'a t -> (pos:OptRange.t -> 'a -> 'b * OptRange.t) -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val lift : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val identifier : string t
  val junk : unit t
  val char : char -> char t
  val parens : 'a t -> 'a t
  val braces : 'a t -> 'a t
  val brackets : 'a t -> 'a t
  val string : string -> string t
  val integer_lit : string t
  val integer_or_float_lit : (string, float) Either.t t
  val string_lit : string t
  val char_lit : char t
  val pos : int t
  val attach_pos : 'a t -> ('a * OptRange.t) t
  val satisfy : (char -> bool) -> char t
  val count : int -> 'a t -> 'a list t
end

module type Internal = sig
  type +'a t = ('a * OptRange.t) Angstrom.t

  val is_alpha : char -> bool
  val is_digit : char -> bool
  val is_whitespace : char -> bool
  val integer_lit : string t
  val integer_or_float_lit : (string, float) Either.t t
  val string_lit : string t
  val char_lit : char t
  val identifier : string t
  val pos : 'a t -> OptRange.t Angstrom.t
  val value : 'a t -> 'a Angstrom.t
end

module AngstromStr = struct
  open Angstrom

  (* Copyright (c) 2016, Inhabited Type LLC

     All rights reserved.

     Redistribution and use in source and binary forms, with or without modification, are
     permitted provided that the following conditions are met:

     1. Redistributions of source code must retain the above copyright notice, this list
     of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright notice, this
     list of conditions and the following disclaimer in the documentation and/or other
     materials provided with the distribution.

     3. Neither the name of the author nor the names of his contributors may be used to
     endorse or promote products derived from this software without specific prior written
     permission.

     THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED
     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
     AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR
     CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
     CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
     OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
     AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
     SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. *)

  type t =
    [ `Unescaped
    | `Escaped
    | `UTF8 of char list
    | `UTF16 of int * [ `S | `U | `C of char list ]
    | `Error of string
    | `Done
    ]

  let to_string : [ `Terminate | t ] -> string = function
    | `Unescaped -> "unescaped"
    | `Escaped -> "escaped"
    | `UTF8 _ -> "utf-8 _"
    | `UTF16 _ -> "utf-16 _ _"
    | `Error e -> Printf.sprintf "error %S" e
    | `Terminate -> "terminate"
    | `Done -> "done"
  ;;

  let unescaped buf = function
    | '"' -> `Terminate
    | '\\' -> `Escaped
    | c ->
      if Char.(c <= '\031')
      then `Error (Printf.sprintf "unexpected character '%c'" c)
      else (
        Buffer.add_char buf c;
        `Unescaped)
  ;;

  let escaped buf = function
    | '\x22' ->
      Buffer.add_char buf '\x22';
      `Unescaped
    | '\x5c' ->
      Buffer.add_char buf '\x5c';
      `Unescaped
    | '\x2f' ->
      Buffer.add_char buf '\x2f';
      `Unescaped
    | '\x62' ->
      Buffer.add_char buf '\x08';
      `Unescaped
    | '\x66' ->
      Buffer.add_char buf '\x0c';
      `Unescaped
    | '\x6e' ->
      Buffer.add_char buf '\x0a';
      `Unescaped
    | '\x72' ->
      Buffer.add_char buf '\x0d';
      `Unescaped
    | '\x74' ->
      Buffer.add_char buf '\x09';
      `Unescaped
    | '\x75' -> `UTF8 []
    | _ -> `Error "invalid escape sequence"
  ;;

  let hex c =
    match c with
    | '0' .. '9' -> Caml.Char.code c - 0x30 (* '0' *)
    | 'a' .. 'f' -> Caml.Char.code c - 87
    | 'A' .. 'F' -> Caml.Char.code c - 55
    | _ -> 255
  ;;

  let utf_8 buf d = function
    | [ c; b; a ] ->
      let a = hex a
      and b = hex b
      and c = hex c
      and d = hex d in
      if a lor b lor c lor d = 255
      then `Error "invalid hex escape"
      else (
        let cp = (a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
        if cp >= 0xd800 && cp <= 0xdbff
        then `UTF16 (cp, `S)
        else (
          Buffer.add_char
            buf
            (Caml.Char.unsafe_chr (0b11100000 lor ((cp lsr 12) land 0b00001111)));
          Buffer.add_char
            buf
            (Caml.Char.unsafe_chr (0b10000000 lor ((cp lsr 6) land 0b00111111)));
          Buffer.add_char buf (Caml.Char.unsafe_chr (0b10000000 lor (cp land 0b00111111)));
          `Unescaped))
    | cs -> `UTF8 (d :: cs)
  ;;

  let utf_16 buf d x s =
    match s, d with
    | `S, '\\' -> `UTF16 (x, `U)
    | `U, 'u' -> `UTF16 (x, `C [])
    | `C [ c; b; a ], _ ->
      let a = hex a
      and b = hex b
      and c = hex c
      and d = hex d in
      if a lor b lor c lor d = 255
      then `Error "invalid hex escape"
      else (
        let y = (a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
        if y >= 0xdc00 && y <= 0xdfff
        then (
          let hi = x - 0xd800 in
          let lo = y - 0xdc00 in
          let cp = 0x10000 + ((hi lsl 10) lor lo) in
          Buffer.add_char
            buf
            (Caml.Char.unsafe_chr (0b11110000 lor ((cp lsr 18) land 0b00000111)));
          Buffer.add_char
            buf
            (Caml.Char.unsafe_chr (0b10000000 lor ((cp lsr 12) land 0b00111111)));
          Buffer.add_char
            buf
            (Caml.Char.unsafe_chr (0b10000000 lor ((cp lsr 6) land 0b00111111)));
          Buffer.add_char buf (Caml.Char.unsafe_chr (0b10000000 lor (cp land 0b00111111)));
          `Unescaped)
        else `Error "invalid escape sequence for utf-16 low surrogate")
    | `C cs, _ -> `UTF16 (x, `C (d :: cs))
    | _, _ -> `Error "invalid escape sequence for utf-16 low surrogate"
  ;;

  let str buf =
    let state : t ref = ref `Unescaped in
    skip_while (fun c ->
        match
          match !state with
          | `Unescaped -> unescaped buf c
          | `Escaped -> escaped buf c
          | `UTF8 cs -> utf_8 buf c cs
          | `UTF16 (x, cs) -> utf_16 buf c x cs
          | (`Error _ | `Done) as state -> state
        with
        | `Error _ | `Done -> false
        | `Terminate ->
          state := `Done;
          true
        | #t as state' ->
          state := state';
          true)
    >>= fun () ->
    match !state with
    | `Done ->
      let result = Buffer.contents buf in
      Buffer.clear buf;
      state := `Unescaped;
      return result
    | `Error msg ->
      Buffer.clear buf;
      state := `Unescaped;
      fail msg
    | `Unescaped | `Escaped | `UTF8 _ | `UTF16 _ ->
      Buffer.clear buf;
      state := `Unescaped;
      fail "unterminated string"
  ;;
end

module Internal : Internal = struct
  let ( ( *> )
      , ( <* )
      , ( >>= )
      , ( >>| )
      , advance
      , any_char
      , char
      , choice
      , fail
      , lift3
      , lift4
      , option
      , peek_char
      , pos
      , return
      , satisfy
      , take_while
      , take_while1 )
    =
    Angstrom.(
      ( ( *> )
      , ( <* )
      , ( >>= )
      , ( >>| )
      , advance
      , any_char
      , char
      , choice
      , fail
      , lift3
      , lift4
      , option
      , peek_char
      , pos
      , return
      , satisfy
      , take_while
      , take_while1 ))
  ;;

  type 'a t = ('a * OptRange.t) Angstrom.t

  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_whitespace = function '\x20' | '\x0a' | '\x0d' | '\x09' -> true | _ -> false
  let integer_lit_no_range = take_while1 is_digit

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
    | Some c when is_digit c -> return ""
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

  let pos p = p >>| fun (_, pos) -> pos
  let value p = p >>| fun (v, _) -> v
end

let whitespace = Angstrom.(take_while Internal.is_whitespace *> return ())
let whitespace1 = Angstrom.(take_while1 Internal.is_whitespace *> return ())

module NoComment : Comment_int = struct
  let comment = Angstrom.fail "no comment"
end

module CComment : Comment_int = struct
  open Angstrom

  let comment =
    string "//" >>= fun _ -> many (satisfy Char.(fun x -> x <> '\n')) >>| fun _ -> ()
  ;;
end

module Mk (Comment : Comment_int) : Parsers = struct
  let ( <* ) = Angstrom.( <* )

  type 'a t = ('a * OptRange.t) Angstrom.t

  let ( >>== ) a f = Angstrom.(a >>= fun (a, pos) -> f ~pos a)
  let ( >>= ) a f = a >>== fun ~pos:_ a -> f a
  let ( >>|| ) a f = Angstrom.(a >>| fun (a, pos) -> f ~pos a)
  let ( >>| ) a f = a >>|| fun ~pos a -> f a, pos
  let ( <$> ) f a = a >>| f
  let ( <*> ) f a = f >>= fun f' -> a >>| f'
  let junk = Angstrom.(many (whitespace1 <|> Comment.comment) >>| fun _ -> (), None)

  let mk_list_parser : ('a * OptRange.t) list Angstrom.t -> 'a list t =
   fun p ->
    Angstrom.lift3
      (fun start lst finish -> List.map lst ~f:fst, Some Range.{ start; finish })
      Angstrom.pos
      p
      Angstrom.pos
 ;;

  let mk_pos_parser : 'a Angstrom.t -> 'a t =
   fun p ->
    Angstrom.lift4
      (fun start result finish _junk -> result, Some Range.{ start; finish })
      Angstrom.pos
      p
      Angstrom.pos
      junk
 ;;

  let many p = mk_list_parser (Angstrom.many p)
  let many1 p = mk_list_parser (Angstrom.many1 p)
  let sep_by s p = mk_list_parser (Angstrom.sep_by s p)
  let sep_by1 s p = mk_list_parser (Angstrom.sep_by1 s p)
  let char c = mk_pos_parser (Angstrom.char c)

  let mk_bracket_parser : char -> char -> 'a t -> 'a t =
   fun open_c close_c p ->
    Angstrom.lift4
      (fun (_, rng1) (v, _) (_, rng2) _junk -> v, OptRange.union rng1 rng2)
      (char open_c)
      p
      (char close_c)
      junk
 ;;

  let identifier = Internal.identifier <* junk
  let parens p = mk_bracket_parser '(' ')' p
  let braces p = mk_bracket_parser '{' '}' p
  let brackets p = mk_bracket_parser '[' ']' p
  let string str = mk_pos_parser (Angstrom.string str)
  let integer_lit = Internal.integer_lit <* junk
  let integer_or_float_lit = Internal.integer_or_float_lit <* junk
  let string_lit = Internal.string_lit <* junk
  let char_lit = Internal.char_lit <* junk
  let pos = Angstrom.(pos >>| fun p -> p, OptRange.mk p p)
  let option a = Angstrom.option (a, None)
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

  let ( <?> ) = Angstrom.( <?> )
  let ( <|> ) = Angstrom.( <|> )
  let ( <* ) = Angstrom.( <* )
  let ( *> ) = Angstrom.( *> )
  let fail = Angstrom.fail
  let return ?(pos = None) a = Angstrom.return (a, pos)
  let fix = Angstrom.fix
  let choice = Angstrom.choice
  let attach_pos p = p >>|| fun ~pos t -> (t, pos), pos

  let satisfy f =
    Angstrom.(pos >>= fun p -> satisfy f >>| fun c -> c, OptRange.mk p (p + 1))
  ;;

  let count n p =
    Angstrom.(
      pos
      >>= fun p1 ->
      count n p
      >>= fun result ->
      pos >>= fun p2 -> return (result |> List.map ~f:fst, OptRange.mk p1 p2))
  ;;
end

let%test_module "Parsing" =
  (module struct
    let ( = ) = Caml.( = )

    module Parse = Mk (NoComment)

    let mk a b = Some (Range.mk a b)

    let%test _ = parse_string_pos Parse.string_lit {|"abc"|} = Ok ("abc", mk 0 5)
    let%test _ = parse_string_pos Parse.string_lit {|"\""|} = Ok ({|"|}, mk 0 4)
    let%test _ = parse_string_pos Parse.string_lit {|"\\"|} = Ok ({|\|}, mk 0 4)

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
  end)
;;
