open Base
open Lvca_provenance

module AngstromStr = struct
  open Angstrom

  (* Copyright (c) 2016, Inhabited Type LLC

   All rights reserved.

   Redistribution and use in source and binary forms, with or without modification, are
   permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of
   conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list
   of conditions and the following disclaimer in the documentation and/or other materials
   provided with the distribution.

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
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. *)

  type t =
    [ `Unescaped
    | `Escaped
    | `UTF8 of char list
    | `UTF16 of int * [ `S | `U | `C of char list ]
    | `Error of string
    | `Done
    ]

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
    | '0' .. '9' -> Char.to_int c - 0x30 (* '0' *)
    | 'a' .. 'f' -> Char.to_int c - 87
    | 'A' .. 'F' -> Char.to_int c - 55
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
            (Char.of_int_exn (0b11100000 lor ((cp lsr 12) land 0b00001111)));
          Buffer.add_char
            buf
            (Char.of_int_exn (0b10000000 lor ((cp lsr 6) land 0b00111111)));
          Buffer.add_char buf (Char.of_int_exn (0b10000000 lor (cp land 0b00111111)));
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
            (Char.of_int_exn (0b11110000 lor ((cp lsr 18) land 0b00000111)));
          Buffer.add_char
            buf
            (Char.of_int_exn (0b10000000 lor ((cp lsr 12) land 0b00111111)));
          Buffer.add_char
            buf
            (Char.of_int_exn (0b10000000 lor ((cp lsr 6) land 0b00111111)));
          Buffer.add_char buf (Char.of_int_exn (0b10000000 lor (cp land 0b00111111)));
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

type +'a t = (Opt_range.t * 'a) Angstrom.t

let parse_string_pos p str = Angstrom.parse_string ~consume:All p str
let parse_string p str = parse_string_pos p str |> Result.map ~f:snd
let parse_string_or_failwith p str = parse_string p str |> Result.ok_or_failwith

(** Helpers that may call pos. *)
module Basic = struct
  open Angstrom

  let go p =
    lift3 (fun start result finish -> Some Range.{ start; finish }, result) pos p pos
  ;;

  let integer_lit_no_range = take_while1 Char.is_digit
  let integer_lit = go integer_lit_no_range

  let sign =
    peek_char
    >>= function
    | Some '-' -> advance 1 >>| fun () -> "-"
    | Some '+' -> advance 1 >>| fun () -> "+"
    | Some c when Char.is_digit c -> return ""
    | _ -> fail "Sign or digit expected"
  ;;

  let integer_or_float_lit =
    let p =
      sign
      >>= fun sign ->
      integer_lit_no_range
      >>= fun whole ->
      choice
        [ (char '.'
          >>= fun _ ->
          option "" integer_lit_no_range
          >>= fun part ->
          let str = sign ^ whole ^ "." ^ part in
          match Stdlib.float_of_string_opt str with
          | Some f -> return (Either.Second f)
          | None -> fail (Printf.sprintf "Failed to convert %s to float" str))
        ; return (Either.First (sign ^ whole))
        ]
    in
    go p
  ;;

  let escape_sequence =
    (* TODO: use decimal_code, char_for_octal_code, char_for_hexadecimal_code
       from ocaml's lexer.mll? Or, share logic with AngstromStr. *)
    let cvt cs =
      let str = String.of_char_list cs in
      match str |> Stdlib.int_of_string_opt |> Option.bind ~f:Char.of_int with
      | None -> fail (Printf.sprintf "couldn't convert %S to character" str)
      | Some c -> return c
    in
    (* decimal code \ddd *)
    let decimal =
      satisfy Char.is_digit
      >>= fun d1 ->
      satisfy Char.is_digit
      >>= fun d2 -> satisfy Char.is_digit >>= fun d3 -> cvt [ d1; d2; d3 ]
    in
    (* hex code \xhh *)
    let hex =
      char 'x'
      >>= fun _ ->
      satisfy Char.is_alphanum
      >>= fun h1 -> satisfy Char.is_alphanum >>= fun h2 -> cvt [ '0'; 'x'; h1; h2 ]
    in
    (* octal code \oooo *)
    let octal =
      let ( <= ), ( >= ) = Char.(( <= ), ( >= )) in
      char 'o'
      >>= fun _ ->
      satisfy (fun c -> c >= '0' && c <= '3')
      >>= fun o1 ->
      satisfy (fun c -> c >= '0' && c <= '7')
      >>= fun o2 ->
      satisfy (fun c -> c >= '0' && c <= '7') >>= fun o3 -> cvt [ '0'; 'o'; o1; o2; o3 ]
    in
    let p =
      char '\\'
      >>= fun _ ->
      choice
        ~failure_msg:"illegal escape sequence"
        [ (* backslash, double quote, single quote, newline, tab, backspace, or carriage return *)
          satisfy (List.mem [ '\\'; '"'; '\''; 'n'; 't'; 'b'; 'r' ] ~equal:Char.( = ))
        ; decimal <?> "decimal"
        ; hex <?> "hex"
        ; octal <?> "octal"
        ]
    in
    p <?> "escape sequence"
  ;;

  let regular_char = satisfy Char.(fun c -> c <> '\\') <?> "regular char"
  let string_lit = go (char '"' *> AngstromStr.str (Buffer.create 0x100))
  let char_lit = go (char '\'' *> (regular_char <|> escape_sequence) <* char '\'')
  let char c = go (char c)
  let satisfy f = go (satisfy f)
  let string str = go (string str)
  let is_start_upper = Char.(fun c -> is_uppercase c || c = '_')
  let is_start_lower = Char.(fun c -> is_lowercase c || c = '_')
  let is_continue = Char.(fun c -> is_alpha c || is_digit c || c = '_' || c = '\'')

  let identifier' ~is_start ?(is_continue = is_continue) reserved_words =
    let p =
      go
        (lift2
           (fun c cs -> String.(of_char c ^ cs))
           (Angstrom.satisfy is_start)
           (take_while is_continue))
    in
    p
    >>= fun (range, ident) ->
    if Set.mem reserved_words ident
    then fail (Printf.sprintf "identifier: reserved word (%s)" ident)
    else return (range, ident)
  ;;

  let upper_identifier reserved_words =
    identifier' ~is_start:is_start_upper reserved_words
  ;;

  let lower_identifier reserved_words =
    identifier' ~is_start:is_start_lower reserved_words
  ;;
end

let ( ( >>|| )
    , ( >>== )
    , ( *> )
    , ( <* )
    , ( <|> )
    , ( <?> )
    , ( >>| )
    , ( >>= )
    , fail
    , fix
    , return )
  =
  Angstrom.(
    ( ( >>| )
    , ( >>= )
    , ( *> )
    , ( <* )
    , ( <|> )
    , ( <?> )
    , ( >>| )
    , ( >>= )
    , fail
    , fix
    , return ))
;;

let ( <*> ) f_p a_p =
  f_p >>== fun (range1, f) -> a_p >>|| fun a -> Opt_range.union range1 (fst a), f a
;;

let ( >>| ) p f = p >>| Lvca_util.Tuple2.map2 ~f

let ( >>= ) p f =
  p
  >>= fun (range1, value) ->
  f value >>= fun (range2, value2) -> return (Opt_range.union range1 range2, value2)
;;

let whitespace = Angstrom.take_while Char.is_whitespace >>|| fun _ -> None, ()
let whitespace1 = Angstrom.take_while1 Char.is_whitespace >>|| fun _ -> None, ()
let adapt p = Angstrom.(p >>= return)
let of_angstrom p = adapt (Basic.go p)
let ( <$> ) f p = p >>| f
let fail msg = fail msg
let make1 mk a = a >>|| fun (info, a) -> info, mk ~info a
let make0 mk a = make1 (fun ~info _ -> mk ~info) a
let lift mk a = a >>|| fun (info, a) -> info, mk (info, a)

let make2 mk a b =
  a
  >>== fun (a_range, a_val) ->
  b
  >>|| fun (b_range, b_val) ->
  let info = Opt_range.union a_range b_range in
  info, mk ~info a_val b_val
;;

let lift2 mk a b =
  a
  >>== fun a ->
  b
  >>|| fun b ->
  let info = Opt_range.union (fst a) (fst b) in
  info, mk a b
;;

let make3 mk a b c =
  a
  >>== fun (a_range, a_val) ->
  b
  >>== fun (b_range, b_val) ->
  c
  >>|| fun (c_range, c_val) ->
  let info = Opt_range.list_range [ a_range; b_range; c_range ] in
  info, mk ~info a_val b_val c_val
;;

let lift3 mk a b c =
  a
  >>== fun a ->
  b
  >>== fun b ->
  c
  >>|| fun c ->
  let info = Opt_range.list_range [ fst a; fst b; fst c ] in
  info, mk a b c
;;

let make4 mk a b c d =
  a
  >>== fun (a_range, a_val) ->
  b
  >>== fun (b_range, b_val) ->
  c
  >>== fun (c_range, c_val) ->
  d
  >>|| fun (d_range, d_val) ->
  let info = Opt_range.list_range [ a_range; b_range; c_range; d_range ] in
  info, mk ~info a_val b_val c_val d_val
;;

let lift4 mk a b c d =
  a
  >>== fun a ->
  b
  >>== fun b ->
  c
  >>== fun c ->
  d
  >>|| fun d ->
  let info = Opt_range.list_range [ fst a; fst b; fst c; fst d ] in
  info, mk a b c d
;;

let make5 mk a b c d e =
  a
  >>== fun (a_range, a_val) ->
  b
  >>== fun (b_range, b_val) ->
  c
  >>== fun (c_range, c_val) ->
  d
  >>== fun (d_range, d_val) ->
  e
  >>|| fun (e_range, e_val) ->
  let info = Opt_range.list_range [ a_range; b_range; c_range; d_range; e_range ] in
  info, mk ~info a_val b_val c_val d_val e_val
;;

let make6 mk a b c d e f =
  a
  >>== fun (a_range, a_val) ->
  b
  >>== fun (b_range, b_val) ->
  c
  >>== fun (c_range, c_val) ->
  d
  >>== fun (d_range, d_val) ->
  e
  >>== fun (e_range, e_val) ->
  f
  >>|| fun (f_range, f_val) ->
  let info =
    Opt_range.list_range [ a_range; b_range; c_range; d_range; e_range; f_range ]
  in
  info, mk ~info a_val b_val c_val d_val e_val f_val
;;

let return ?(range = None) value = return (range, value)
let option ?(range = None) value p = Angstrom.option (range, value) p
let option' p = option None (p >>| Option.return)
let choice ?failure_msg ps = Angstrom.choice ?failure_msg ps

let handle_list lst =
  match List.hd lst, List.last lst with
  | Some (head_range, _), Some (last_range, _) ->
    Opt_range.union head_range last_range, List.map lst ~f:snd
  | _, _ -> None, []
;;

let handle_list' = function
  | [] -> Lvca_util.invariant_violation [%here] "must be a non-empty list"
  | x :: xs -> x, xs
;;

let many p = Angstrom.many p >>|| handle_list
let many1 p = Angstrom.many1 p >>|| handle_list
let many1' p = many1 p >>| handle_list'
let sep_by s p = Angstrom.sep_by s p >>|| handle_list
let sep_by1 s p = Angstrom.sep_by1 s p >>|| handle_list
let sep_by1' s p = sep_by1 s p >>| handle_list'
let count n p = Angstrom.count n p >>|| handle_list
let attach_pos p = Angstrom.(p >>| fun (rng, a) -> rng, (rng, a))

let sep_end_by s p =
  fix (fun seb ->
      let seb1 =
        p >>= fun x -> choice [ lift2 (fun _ (_, xs) -> x :: xs) s seb; return [ x ] ]
      in
      choice [ seb1; return [] ])
;;

let sep_end_by1 s p =
  p
  >>= fun x ->
  choice [ (s >>= fun _ -> sep_end_by s p >>| fun xs -> x :: xs); return [ x ] ]
;;

let sep_end_by1' s p = sep_end_by1 s p >>| handle_list'

module type Junk_parser = sig
  val junk : unit t
  val junk1 : unit t
end

module type Character_parser = sig
  val junk : unit t
  val junk1 : unit t
  val char_lit : char t

  val identifier'
    :  is_start:(char -> bool)
    -> ?is_continue:(char -> bool)
    -> Lvca_util.String.Set.t
    -> string t

  val upper_identifier : Lvca_util.String.Set.t -> string t
  val lower_identifier : Lvca_util.String.Set.t -> string t
  val integer_lit : string t
  val integer_or_float_lit : (string, float) Base.Either.t t
  val string_lit : string t
  val satisfy : (char -> bool) -> char t
  val char : char -> char t
  val string : string -> string t
  val keyword : string -> string t
  val parens : 'a t -> 'a t
  val braces : 'a t -> 'a t
  val brackets : 'a t -> 'a t
end

module No_junk : Character_parser = struct
  let junk = fail "no junk"
  let junk1 = fail "no junk"
  let char_lit = adapt Basic.char_lit

  let identifier' ~is_start ?is_continue reserved_words =
    adapt (Basic.identifier' ~is_start ?is_continue reserved_words)
  ;;

  let upper_identifier reserved_words =
    adapt (Basic.upper_identifier reserved_words) <?> "upper-case identifier"
  ;;

  let lower_identifier reserved_words =
    adapt (Basic.lower_identifier reserved_words) <?> "lower-case identifier"
  ;;

  let integer_lit = adapt Basic.integer_lit <?> "integer literal"

  let integer_or_float_lit =
    adapt Basic.integer_or_float_lit <?> "integer or float literal"
  ;;

  let string_lit = adapt Basic.string_lit
  let char c = adapt (Basic.char c) <?> Fmt.str "char %C" c
  let satisfy f = adapt (Basic.satisfy f)
  let string str = adapt (Basic.string str) <?> Fmt.str "string %S" str
  let keyword str = adapt (Basic.string str) <?> Fmt.str "keyword %S" str

  let mk_bracket_parser open_c close_c p =
    lift3 (fun _ (_, v) _ -> v) (char open_c <* whitespace) p (whitespace *> char close_c)
  ;;

  let parens p = mk_bracket_parser '(' ')' p
  let braces p = mk_bracket_parser '{' '}' p
  let brackets p = mk_bracket_parser '[' ']' p
end

module Mk_character_parser (Junk : Junk_parser) : Character_parser = struct
  include Junk

  let char_lit = No_junk.char_lit <* Junk.junk

  let identifier' ~is_start ?is_continue reserved_words =
    No_junk.identifier' ~is_start ?is_continue reserved_words <* Junk.junk
  ;;

  let upper_identifier reserved_words =
    No_junk.upper_identifier reserved_words <* Junk.junk
  ;;

  let lower_identifier reserved_words =
    No_junk.lower_identifier reserved_words <* Junk.junk
  ;;

  let integer_lit = No_junk.integer_lit <* Junk.junk
  let integer_or_float_lit = No_junk.integer_or_float_lit <* Junk.junk
  let string_lit = No_junk.string_lit <* Junk.junk
  let char c = No_junk.char c <* Junk.junk
  let satisfy f = No_junk.satisfy f <* Junk.junk
  let string str = No_junk.string str <* Junk.junk
  let keyword str = No_junk.string str <* Junk.junk1
  let parens p = No_junk.parens p <* Junk.junk
  let braces p = No_junk.braces p <* Junk.junk
  let brackets p = No_junk.brackets p <* Junk.junk
end

let no_comment = fail "no comment"

let c_comment =
  let comment =
    many (No_junk.satisfy Char.(fun c -> c <> '\n')) >>| String.of_char_list
  in
  No_junk.string "//" *> comment <* whitespace
;;

module Whitespace_parser : Character_parser = Mk_character_parser (struct
  let junk = whitespace
  let junk1 = whitespace1
end)

module C_comment_parser : Character_parser = Mk_character_parser (struct
  let junk = whitespace <* option "" c_comment
  let junk1 = choice [ whitespace1 <* option "" c_comment; (c_comment >>| fun _ -> ()) ]
end)

module Let_syntax = struct
  let return x = Angstrom.return x
  let map a ~f = lift f a
  let bind a ~f = Angstrom.(a >>= f)
  let both a b = lift2 (fun (_, a) (_, b) -> a, b) a b
  let map2 a b ~f = lift2 (fun (_, a) (_, b) -> f a b) a b
  let map3 a b c ~f = lift3 (fun (_, a) (_, b) (_, c) -> f a b c) a b c
  let map4 a b c d ~f = lift4 (fun (_, a) (_, b) (_, c) (_, d) -> f a b c d) a b c d
end

let ( let+ ) a f = lift f a
let ( let* ) a f = Angstrom.(a >>= f)
let ( and+ ) = Let_syntax.both

let%test_module "Parsing" =
  (module struct
    module Ws = Whitespace_parser

    let parse_print p pp str =
      match parse_string_pos p str with
      | Error msg -> Fmt.pr "%s" msg
      | Ok (range, value) -> Fmt.pr "@[(%a, %a)@]\n" Opt_range.pp range pp value
    ;;

    let%expect_test _ =
      let pp_str ppf str = Fmt.pf ppf "%S" str in
      parse_print c_comment pp_str {|// comment|};
      [%expect {|{ value = " comment"; range = {2,10} }|}]
    ;;

    let pp_char ppf str = Fmt.pf ppf "%C" str
    let go = parse_print Ws.char_lit pp_char
    let go' = parse_print C_comment_parser.char_lit pp_char

    let%expect_test _ =
      go {|'x'|};
      go' {|'x' // comment|};
      [%expect
        {|
        { value = 'x'; range = {0,3} }
        { value = 'x'; range = {0,3} }|}]
    ;;

    let%expect_test _ =
      go {|'\a'|};
      [%expect {|escape sequence: illegal escape sequence|}]
    ;;

    let%expect_test _ =
      go {|'\123'|};
      [%expect {|{ value = '{'; range = {0,6} }|}]
    ;;

    let%expect_test _ =
      go {|'\xbb'|};
      [%expect {|{ value = '\187'; range = {0,6} }|}]
    ;;

    let%expect_test _ =
      go {|'\xb'|};
      [%expect {|escape sequence: illegal escape sequence|}]
    ;;

    let%expect_test _ =
      go {|'\xxb'|};
      [%expect {|escape sequence: illegal escape sequence|}]
    ;;

    let%expect_test _ =
      go {|'\o377'|};
      [%expect {|{ value = '\255'; range = {0,7} }|}]
    ;;

    let%expect_test _ =
      go {|'\o877'|};
      [%expect {|escape sequence: illegal escape sequence|}]
    ;;

    let%expect_test _ =
      go {|'\\'|};
      [%expect {|{ value = '\\'; range = {0,4} }|}]
    ;;

    let%expect_test _ =
      go {|'\''|};
      [%expect {|{ value = '\''; range = {0,4} }|}]
    ;;

    let pp_str ppf str = Fmt.pf ppf "%S" str
    let go = parse_print Ws.string_lit pp_str

    let%expect_test _ =
      go {|"abc"|};
      [%expect {|{ value = "abc"; range = {0,5} }|}]
    ;;

    let%expect_test _ =
      go {|"\""|};
      [%expect {|{ value = "\""; range = {0,4} }|}]
    ;;

    let%expect_test _ =
      go {|"\\"|};
      [%expect {|{ value = "\\"; range = {0,4} }|}]
    ;;

    let go = parse_print Ws.(parens string_lit) pp_str

    let%expect_test _ =
      go {|("a")|};
      (*   012345 *)
      [%expect {|{ value = "a"; range = {0,5} }|}]
    ;;

    let pp ppf = Fmt.(pf ppf "[%a]" (list ~sep:(any "; ") pp_str))
    let go = parse_print Ws.(parens (sep_by whitespace string_lit)) pp
    let go' = parse_print C_comment_parser.(parens (sep_by whitespace string_lit)) pp

    let%expect_test _ =
      go {|("a" "b")|};
      (*   0123456789 *)
      go' {|("a" "b")  // comment|};
      [%expect
        {|
        { value = ["a"; "b"]; range = {0,9} }
        { value = ["a"; "b"]; range = {0,9} }|}]
    ;;

    let pp ppf = function
      | Either.First str -> Fmt.pf ppf {|First %S|} str
      | Second fl -> Fmt.pf ppf {|Second %a|} Float.pp fl
    ;;

    let go = parse_print Ws.integer_or_float_lit pp
    let go' = parse_print C_comment_parser.integer_or_float_lit pp

    let%expect_test _ =
      go "123";
      go' "123";
      [%expect
        {|
        { value = First "123"; range = {0,3} }
        { value = First "123"; range = {0,3} }|}]
    ;;

    let%expect_test _ =
      go "-123";
      [%expect {|{ value = First "-123"; range = {0,4} }|}]
    ;;

    let%expect_test _ =
      go "+123";
      [%expect {|{ value = First "+123"; range = {0,4} }|}]
    ;;

    let%expect_test _ =
      go "1.1";
      [%expect {|{ value = Second 1.1; range = {0,3} }|}]
    ;;

    let%expect_test _ =
      go "-1.1";
      [%expect {|{ value = Second -1.1; range = {0,4} }|}]
    ;;

    let%expect_test _ =
      go "+1.1";
      [%expect {|{ value = Second 1.1; range = {0,4} }|}]
    ;;

    let%expect_test _ =
      go "1.";
      [%expect {|{ value = Second 1.; range = {0,2} }|}]
    ;;

    let pp ppf = Fmt.(pf ppf "[%a]" (list ~sep:(any "; ") pp_str))
    let go = parse_print Ws.(whitespace *> sep_end_by (char ';') string_lit) pp

    let go' =
      parse_print C_comment_parser.(whitespace *> sep_end_by (char ';') string_lit) pp
    ;;

    let%expect_test _ =
      go "";
      go {|"abc"|};
      go {|"abc"; "def"|};
      [%expect
        {|
      { value = []; range = _ }
      { value = ["abc"]; range = {0,5} }
      { value = ["abc"; "def"]; range = {0,12} }|}]
    ;;

    let%expect_test _ =
      go {|"abc"; "def";|};
      go' {|
      "abc";  // this is abc
      "def";  // this one is def
      |};
      [%expect
        {|
        { value = ["abc"; "def"]; range = {0,13} }
        { value = ["abc"; "def"]; range = {7,42} }|}]
    ;;

    let go = parse_print Ws.(whitespace *> sep_end_by1 (char ';') string_lit) pp

    let%expect_test _ =
      go {|"abc";|};
      go "";
      [%expect
        {|
        { value = ["abc"]; range = {0,6} }
        : not enough input |}]
    ;;

    let pp ppf = Fmt.(pf ppf "[%a]" (list ~sep:(any "; ") (list ~sep:(any ". ") pp_str)))

    let go =
      parse_print
        Ws.(whitespace *> sep_end_by (char ';') (sep_end_by (char '.') string_lit))
        pp
    ;;

    let go'' =
      parse_print
        C_comment_parser.(
          whitespace *> sep_end_by (char ';') (sep_end_by (char '.') string_lit))
        pp
    ;;

    let%expect_test _ =
      go {|"a". "b"; "c". "d"|};
      go'' {|
        "a".  // a
        "b";  // b
        "c". "d"|};
      [%expect
        {|
        { value = ["a". "b"; "c". "d"]; range = {0,18} }
        { value = ["a". "b"; "c". "d"]; range = {9,55} }|}]
    ;;

    let go str =
      let open C_comment_parser in
      let parse =
        parse_string_pos
          (sep_by1 (whitespace *> string "->") (whitespace *> char '*')
          <* whitespace
          <* string "foo")
      in
      match parse str with
      | Ok (range, _) -> Fmt.pr "%a\n" Opt_range.pp range
      | _ -> Fmt.pr "not okay\n"
    ;;

    let%expect_test _ =
      go "* -> *\nfoo // comment";
      (*  0123456 7890 *)
      [%expect {|{0,6}|}]
    ;;

    let%expect_test _ =
      go "* -> * foo // comment";
      (*  01234567890 *)
      [%expect {|{0,6}|}]
    ;;

    let%expect_test _ =
      go "* -> * foo";
      (*  01234567890 *)
      [%expect {|{0,6}|}]
    ;;

    let go str =
      let parse = parse_string (No_junk.string "foo" <* whitespace1) in
      match parse str with Ok _ -> Fmt.pr "okay\n" | _ -> Fmt.pr "not okay\n"
    ;;

    let%expect_test _ =
      go "foo";
      go "foo ";
      go "foo // comment";
      [%expect {|
      not okay
      okay
      not okay
      |}]
    ;;

    let go str =
      let parse = parse_string (Whitespace_parser.keyword "foo") in
      match parse str with Ok _ -> Fmt.pr "okay\n" | _ -> Fmt.pr "not okay\n"
    ;;

    let%expect_test _ =
      go "foo";
      go "foo ";
      go "foo // comment";
      [%expect {|
      not okay
      okay
      not okay
      |}]
    ;;

    let go str =
      let parse = parse_string (C_comment_parser.keyword "foo") in
      match parse str with Ok _ -> Fmt.pr "okay\n" | _ -> Fmt.pr "not okay\n"
    ;;

    let%expect_test _ =
      go "foo";
      go "foo ";
      go "foo// comment";
      go "foo // comment";
      [%expect {|
      not okay
      okay
      okay
      okay
      |}]
    ;;

    let reserved_words = Lvca_util.String.Set.of_list [ "reserved"; "Reserved" ]

    let go str =
      let parse = parse_string (Whitespace_parser.lower_identifier reserved_words) in
      match parse str with Ok _ -> Fmt.pr "okay\n" | _ -> Fmt.pr "not okay\n"
    ;;

    let%expect_test _ =
      go "foo";
      go "_foo1";
      go "Bar";
      go "reserved";
      [%expect {|
        okay
        okay
        not okay
        not okay |}]
    ;;

    let go str =
      let parse = parse_string (Whitespace_parser.upper_identifier reserved_words) in
      match parse str with Ok _ -> Fmt.pr "okay\n" | _ -> Fmt.pr "not okay\n"
    ;;

    let%expect_test _ =
      go "Foo";
      go "_foo1";
      go "bar";
      go "Reserved";
      [%expect {|
        okay
        okay
        not okay
        not okay |}]
    ;;
  end)
;;
