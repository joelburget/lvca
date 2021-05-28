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

module ParseResult = struct
  type 'a t =
    { latest_pos : int
    ; value : 'a
    ; range : OptRange.t
    }

  let equal a_eq r1 r2 =
    Base.Int.(r1.latest_pos = r2.latest_pos)
    && a_eq r1.value r2.value
    && OptRange.(r1.range = r2.range)
  ;;

  let pp pp_a ppf { value; range; latest_pos } =
    Fmt.pf
      ppf
      "{ value = %a; range = %a; latest_pos = %d }"
      pp_a
      value
      OptRange.pp
      range
      latest_pos
  ;;
end

open ParseResult

type +'a t = latest_pos:int -> 'a ParseResult.t Angstrom.t

(* TODO: change start point from 0! *)
let parse_string_pos p str = Angstrom.parse_string ~consume:All (p ~latest_pos:0) str

let parse_string p str =
  parse_string_pos p str |> Result.map ~f:(fun { value; _ } -> value)
;;

(** Parser combinators that *don't* handle trailing whitespace. *)
module Junkless = struct
  open Angstrom

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
        let str = sign ^ whole ^ "." ^ part in
        match Stdlib.float_of_string_opt str with
        | Some f -> return (Either.Second f)
        | None -> fail (Printf.sprintf "Failed to convert %s to float" str))
      ; return (Either.First (sign ^ whole))
      ]
    >>= fun result -> pos >>| fun finish -> result, Some Range.{ start; finish }
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
      (char '\'' *> (regular_char <|> escape_sequence) <* char '\'')
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
end

let whitespace = Angstrom.(take_while Char.is_whitespace *> return ())
let whitespace1 = Angstrom.(take_while1 Char.is_whitespace *> return ())

let ( <|> ), ( <?> ), ( >>| ), ( >>= ), fail, return =
  Angstrom.(( <|> ), ( <?> ), ( >>| ), ( >>= ), fail, return)
;;

let ( <* ) p1 p2 ~latest_pos =
  p1 ~latest_pos
  >>= fun result ->
  p2 ~latest_pos:result.latest_pos >>| fun { latest_pos; _ } -> { result with latest_pos }
;;

let ( *> ) p1 p2 ~latest_pos = p1 ~latest_pos >>= fun { latest_pos; _ } -> p2 ~latest_pos
let ( <|> ) p1 p2 ~latest_pos = p1 ~latest_pos <|> p2 ~latest_pos

let ( <*> ) p1 p2 ~latest_pos =
  p1 ~latest_pos
  >>= fun result1 ->
  p2 ~latest_pos:result1.latest_pos
  >>= fun result2 ->
  return
    { value = result1.value result2.value
    ; latest_pos = result2.latest_pos
    ; range = OptRange.union result1.range result2.range
    }
;;

let ( >>|| ) p f ~latest_pos = p ~latest_pos >>| f

let ( >>| ) p f ~latest_pos =
  p ~latest_pos >>| fun result -> { result with value = f result.value }
;;

let ( >>== ) p f ~latest_pos =
  p ~latest_pos >>= fun ({ latest_pos; _ } as parse_result) -> f parse_result ~latest_pos
;;

let ( >>= ) p f ~latest_pos =
  p ~latest_pos
  >>= fun { value; latest_pos; range = range1 } ->
  f value ~latest_pos
  >>= fun ({ range = range2; _ } as result) ->
  Angstrom.return { result with range = OptRange.union range1 range2 }
;;

let ( <?> ) p msg ~latest_pos = p ~latest_pos <?> msg

let whitespace ~latest_pos =
  Angstrom.(whitespace >>| fun _ -> { value = (); range = None; latest_pos })
;;

let whitespace1 ~latest_pos =
  Angstrom.(whitespace1 >>| fun _ -> { value = (); range = None; latest_pos })
;;

let adapt_junkless junkless_p =
  let p ~latest_pos =
    let f (value, range) =
      let latest_pos =
        match range with None -> latest_pos | Some Range.{ finish; _ } -> finish
      in
      return { value; range; latest_pos }
    in
    Angstrom.(junkless_p >>= f)
  in
  p <* whitespace
;;

let char_lit = adapt_junkless Junkless.char_lit
let identifier = adapt_junkless Junkless.identifier
let integer_lit = adapt_junkless Junkless.integer_lit
let integer_or_float_lit = adapt_junkless Junkless.integer_or_float_lit
let string_lit = adapt_junkless Junkless.string_lit
let char c = adapt_junkless (Junkless.char c)
let string str = adapt_junkless (Junkless.string str)
let ( <$> ) f p = p >>| f
let fail msg ~latest_pos:_ = fail msg
let lift f a = f <$> a

let lift2 f a b =
  a
  >>== fun { value = a_val; range = a_range; _ } ->
  b
  >>|| fun { value = b_val; range = b_range; latest_pos } ->
  { value = f a_val b_val; range = OptRange.union a_range b_range; latest_pos }
;;

let debug = ref false

let lift3 f a b c =
  a
  >>== fun { value = a_val; range = a_range; _ } ->
  b
  >>== fun { value = b_val; range = b_range; _ } ->
  c
  >>|| fun { value = c_val; range = c_range; latest_pos } ->
  let range = OptRange.list_range [ a_range; b_range; c_range ] in
  if !debug
  then
    Fmt.pr
      "lift3 a_range: %a; b_range: %a; c_range: %a, range: %a\n"
      OptRange.pp
      a_range
      OptRange.pp
      b_range
      OptRange.pp
      c_range
      OptRange.pp
      range;
  { value = f a_val b_val c_val; range; latest_pos }
;;

let lift4 f a b c d =
  a
  >>== fun { value = a_val; range = a_range; _ } ->
  b
  >>== fun { value = b_val; range = b_range; _ } ->
  c
  >>== fun { value = c_val; range = c_range; _ } ->
  d
  >>|| fun { value = d_val; range = d_range; latest_pos } ->
  { value = f a_val b_val c_val d_val
  ; range = OptRange.list_range [ a_range; b_range; c_range; d_range ]
  ; latest_pos
  }
;;

let count n p ~latest_pos =
  let open Angstrom in
  pos
  >>= fun p1 ->
  count n (p ~latest_pos)
  >>= fun result ->
  pos
  >>= fun p2 ->
  let value = List.map result ~f:(fun elem -> elem.value) in
  return { value; range = OptRange.mk p1 p2; latest_pos = p2 }
;;

let satisfy f ~latest_pos:_ =
  Angstrom.(
    pos
    >>= fun pos ->
    satisfy f
    >>| fun c -> { value = c; range = OptRange.mk pos (pos + 1); latest_pos = pos + 1 })
;;

let attach_pos p ~latest_pos =
  Angstrom.(
    p ~latest_pos
    >>| fun ({ value; range; _ } as parse_result) ->
    { parse_result with value = value, range })
;;

let return ?(pos = None) value ~latest_pos =
  Angstrom.return { value; range = pos; latest_pos }
;;

let option value p ~latest_pos =
  Angstrom.option { value; range = None; latest_pos } (p ~latest_pos)
;;

let pos ~latest_pos = Angstrom.return { value = latest_pos; range = None; latest_pos }

let fix f ~latest_pos =
  let pos_ref = ref latest_pos in
  let result =
    Angstrom.fix (fun angstrom_t ->
        f
          (fun ~latest_pos ->
            pos_ref := latest_pos;
            angstrom_t)
          ~latest_pos:!pos_ref)
  in
  let adjust_latest_pos { value; range; latest_pos } =
    let latest_pos =
      match range with Some { finish; _ } -> finish | None -> latest_pos
    in
    { value; range; latest_pos }
  in
  Angstrom.(result >>| adjust_latest_pos)
;;

let choice ?failure_msg ps ~latest_pos =
  Angstrom.choice ?failure_msg (List.map ps ~f:(fun p -> p ~latest_pos))
;;

let handle_list lst ~latest_pos =
  match List.hd lst, List.last lst with
  | Some head, Some last ->
    { value = List.map lst ~f:(fun elem -> elem.value)
    ; range = OptRange.union head.range last.range
    ; latest_pos = last.latest_pos
    }
  | _, _ -> { value = []; range = None; latest_pos }
;;

let many p ~latest_pos = Angstrom.(many (p ~latest_pos) >>| handle_list ~latest_pos)
let many1 p ~latest_pos = Angstrom.(many1 (p ~latest_pos) >>| handle_list ~latest_pos)

let sep_by s p ~latest_pos =
  Angstrom.(sep_by (s ~latest_pos) (p ~latest_pos) >>| handle_list ~latest_pos)
;;

let sep_by1 s p ~latest_pos =
  Angstrom.(sep_by1 (s ~latest_pos) (p ~latest_pos) >>| handle_list ~latest_pos)
;;

let sep_end_by s p =
  fix (fun seb ->
      let seb1 =
        p >>= fun x -> choice [ lift2 (fun _ xs -> x :: xs) s seb; return [ x ] ]
      in
      choice [ seb1; return [] ])
;;

let mk_bracket_parser open_c close_c p =
  lift3 (fun _ v _ -> v) (char open_c) p (char close_c)
;;

let parens p = mk_bracket_parser '(' ')' p
let braces p = mk_bracket_parser '{' '}' p
let brackets p = mk_bracket_parser '[' ']' p

let%test_module "Parsing" =
  (module struct
    let parse_print p pp str =
      match parse_string_pos p str with
      | Error msg -> Stdio.print_string msg
      | Ok value -> ParseResult.pp pp Fmt.stdout value
    ;;

    let pp_char ppf str = Fmt.pf ppf "%C" str
    let go = parse_print char_lit pp_char

    let%expect_test _ =
      go {|'x'|};
      [%expect {|{ value = 'x'; range = {0,3}; latest_pos = 3 }|}]
    ;;

    let%expect_test _ =
      go {|'\a'|};
      [%expect {|escape sequence: illegal escape sequence|}]
    ;;

    let%expect_test _ =
      go {|'\123'|};
      [%expect {|{ value = '{'; range = {0,6}; latest_pos = 6 }|}]
    ;;

    let%expect_test _ =
      go {|'\xbb'|};
      [%expect {|{ value = '\187'; range = {0,6}; latest_pos = 6 }|}]
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
      [%expect {|{ value = '\255'; range = {0,7}; latest_pos = 7 }|}]
    ;;

    let%expect_test _ =
      go {|'\o877'|};
      [%expect {|escape sequence: illegal escape sequence|}]
    ;;

    let%expect_test _ =
      go {|'\\'|};
      [%expect {|{ value = '\\'; range = {0,4}; latest_pos = 4 }|}]
    ;;

    let%expect_test _ =
      go {|'\''|};
      [%expect {|{ value = '\''; range = {0,4}; latest_pos = 4 }|}]
    ;;

    let pp_str ppf str = Fmt.pf ppf "%S" str
    let go = parse_print string_lit pp_str

    let%expect_test _ =
      go {|"abc"|};
      [%expect {|{ value = "abc"; range = {0,5}; latest_pos = 5 }|}]
    ;;

    let%expect_test _ =
      go {|"\""|};
      [%expect {|{ value = "\""; range = {0,4}; latest_pos = 4 }|}]
    ;;

    let%expect_test _ =
      go {|"\\"|};
      [%expect {|{ value = "\\"; range = {0,4}; latest_pos = 4 }|}]
    ;;

    let go = parse_print (parens string_lit) pp_str

    let%expect_test _ =
      go {|("a")|};
      (*   012345 *)
      [%expect {|{ value = "a"; range = {0,5}; latest_pos = 5 }|}]
    ;;

    let pp ppf = Fmt.(pf ppf "[%a]" (list ~sep:(any "; ") pp_str))
    let go = parse_print (parens (sep_by whitespace string_lit)) pp

    let%expect_test _ =
      go {|("a" "b")|};
      (*   0123456789 *)
      [%expect {|{ value = ["a"; "b"]; range = {0,9}; latest_pos = 9 }|}]
    ;;

    let go =
      let pp ppf = function
        | Either.First str -> Fmt.pf ppf {|First %S|} str
        | Second fl -> Fmt.pf ppf {|Second %a|} Float.pp fl
      in
      parse_print integer_or_float_lit pp
    ;;

    let%expect_test _ =
      go "123";
      [%expect {|{ value = First "123"; range = {0,3}; latest_pos = 3 }|}]
    ;;

    let%expect_test _ =
      go "-123";
      [%expect {|{ value = First "-123"; range = {0,4}; latest_pos = 4 }|}]
    ;;

    let%expect_test _ =
      go "+123";
      [%expect {|{ value = First "+123"; range = {0,4}; latest_pos = 4 }|}]
    ;;

    let%expect_test _ =
      go "1.1";
      [%expect {|{ value = Second 1.1; range = {0,3}; latest_pos = 3 }|}]
    ;;

    let%expect_test _ =
      go "-1.1";
      [%expect {|{ value = Second -1.1; range = {0,4}; latest_pos = 4 }|}]
    ;;

    let%expect_test _ =
      go "+1.1";
      [%expect {|{ value = Second 1.1; range = {0,4}; latest_pos = 4 }|}]
    ;;

    let%expect_test _ =
      go "1.";
      [%expect {|{ value = Second 1.; range = {0,2}; latest_pos = 2 }|}]
    ;;

    let go =
      let pp ppf = Fmt.(pf ppf "[%a]" (list ~sep:(any "; ") pp_str)) in
      parse_print (sep_end_by (char ';') string_lit) pp
    ;;

    let%expect_test _ =
      go {|"abc"|};
      [%expect {|{ value = ["abc"]; range = {0,5}; latest_pos = 5 }|}]
    ;;

    let%expect_test _ =
      go {|"abc"; "def"|};
      [%expect {|{ value = ["abc"; "def"]; range = {0,12}; latest_pos = 12 }|}]
    ;;

    let%expect_test _ =
      go {|"abc"; "def";|};
      [%expect {|{ value = ["abc"; "def"]; range = {0,13}; latest_pos = 13 }|}]
    ;;

    let go str =
      let parse =
        parse_string_pos
          (sep_by1 (whitespace *> string "->") (whitespace *> char '*')
          <* whitespace
          <* string "foo")
      in
      match parse str with
      | Ok { range; _ } -> Fmt.pr "%a\n" OptRange.pp range
      | _ -> Fmt.pr "not okay\n"
    ;;

    let%expect_test _ =
      go "* -> *\nfoo";
      (*  0123456 7890 *)
      [%expect {|{0,6}|}]
    ;;

    let%expect_test _ =
      go "* -> * foo";
      (*  01234567890 *)
      [%expect {|{0,6}|}]
    ;;
  end)
;;