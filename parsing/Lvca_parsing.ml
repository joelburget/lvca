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

module Parse_result = struct
  type 'a t =
    { value : 'a
    ; range : Opt_range.t
    }

  let equal a_eq r1 r2 = a_eq r1.value r2.value && Opt_range.(r1.range = r2.range)

  let pp pp_a ppf { value; range } =
    Fmt.pf ppf "{ value = %a; range = %a }" pp_a value Opt_range.pp range
  ;;
end

open Parse_result

type +'a t = 'a Parse_result.t Angstrom.t

let parse_string_pos p str = Angstrom.parse_string ~consume:All p str

let parse_string p str =
  parse_string_pos p str |> Result.map ~f:(fun { value; range = _ } -> value)
;;

(** Helpers that may call pos. *)
module Basic = struct
  open Angstrom

  let go p =
    lift3 (fun start result finish -> result, Some Range.{ start; finish }) pos p pos
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
  let initial_char_p = Char.(fun c -> is_alpha c || c = '_')
  let char_p = Char.(fun c -> is_alpha c || is_digit c || c = '_' || c = '\'')

  let identifier' ?(initial_char_p = initial_char_p) ?(char_p = char_p) () =
    go
      (lift2
         (fun c cs -> String.(of_char c ^ cs))
         (Angstrom.satisfy initial_char_p)
         (take_while char_p))
  ;;

  let identifier = identifier' ()
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
  f_p
  >>== fun { value = f; range = range1 } ->
  a_p
  >>|| fun { value = a; range = range2 } ->
  { value = f a; range = Opt_range.union range1 range2 }
;;

let ( >>| ) p f = p >>| fun { range; value } -> { range; value = f value }
let ( >>~ ) p f = p >>|| fun { range; value } -> { range; value = f range value }

let ( >>= ) p f =
  p
  >>= fun { value; range = range1 } ->
  f value
  >>= fun ({ range = range2; _ } as result) ->
  return { result with range = Opt_range.union range1 range2 }
;;

let whitespace =
  Angstrom.take_while Char.is_whitespace >>|| fun _ -> { value = (); range = None }
;;

let whitespace1 =
  Angstrom.take_while1 Char.is_whitespace >>|| fun _ -> { value = (); range = None }
;;

let adapt p =
  let f (value, range) = return { value; range } in
  Angstrom.(p >>= f)
;;

let ( <$> ) f p = p >>| f
let fail msg = fail msg
let make1 mk a = a >>~ fun info a -> mk ~info a
let make0 mk a = make1 (fun ~info _ -> mk ~info) a
let lift mk a = make1 (fun ~info:_ -> mk) a

let make2 mk a b =
  a
  >>== fun { value = a_val; range = a_range } ->
  b
  >>|| fun { value = b_val; range = b_range } ->
  let info = Opt_range.union a_range b_range in
  { value = mk ~info a_val b_val; range = info }
;;

let lift2 mk a b = make2 (fun ~info:_ -> mk) a b

let make3 mk a b c =
  a
  >>== fun { value = a_val; range = a_range } ->
  b
  >>== fun { value = b_val; range = b_range } ->
  c
  >>|| fun { value = c_val; range = c_range } ->
  let info = Opt_range.list_range [ a_range; b_range; c_range ] in
  { value = mk ~info a_val b_val c_val; range = info }
;;

let lift3 mk a b c = make3 (fun ~info:_ -> mk) a b c

let make4 mk a b c d =
  a
  >>== fun { value = a_val; range = a_range } ->
  b
  >>== fun { value = b_val; range = b_range } ->
  c
  >>== fun { value = c_val; range = c_range } ->
  d
  >>|| fun { value = d_val; range = d_range } ->
  let info = Opt_range.list_range [ a_range; b_range; c_range; d_range ] in
  { value = mk ~info a_val b_val c_val d_val; range = info }
;;

let lift4 mk a b c d = make4 (fun ~info:_ -> mk) a b c d

let make5 mk a b c d e =
  a
  >>== fun { value = a_val; range = a_range } ->
  b
  >>== fun { value = b_val; range = b_range } ->
  c
  >>== fun { value = c_val; range = c_range } ->
  d
  >>== fun { value = d_val; range = d_range } ->
  e
  >>|| fun { value = e_val; range = e_range } ->
  let info = Opt_range.list_range [ a_range; b_range; c_range; d_range; e_range ] in
  { value = mk ~info a_val b_val c_val d_val e_val; range = info }
;;

let make6 mk a b c d e f =
  a
  >>== fun { value = a_val; range = a_range } ->
  b
  >>== fun { value = b_val; range = b_range } ->
  c
  >>== fun { value = c_val; range = c_range } ->
  d
  >>== fun { value = d_val; range = d_range } ->
  e
  >>== fun { value = e_val; range = e_range } ->
  f
  >>|| fun { value = f_val; range = f_range } ->
  let info =
    Opt_range.list_range [ a_range; b_range; c_range; d_range; e_range; f_range ]
  in
  { value = mk ~info a_val b_val c_val d_val e_val f_val; range = info }
;;

let attach_pos p =
  Angstrom.(
    p
    >>| fun ({ value; range } as parse_result) ->
    { parse_result with value = value, range })
;;

let attach_pos' p =
  Angstrom.(
    p
    >>| fun ({ value; range } as parse_result) ->
    { parse_result with value = range, value })
;;

let return ?(range = None) value = return { value; range }
let option value p = Angstrom.option { value; range = None } p
let option' p = option None (p >>| Option.return)
let choice ?failure_msg ps = Angstrom.choice ?failure_msg ps

let handle_list lst =
  match List.hd lst, List.last lst with
  | Some head, Some last ->
    { value = List.map lst ~f:(fun elem -> elem.value)
    ; range = Opt_range.union head.range last.range
    }
  | _, _ -> { value = []; range = None }
;;

let many p = Angstrom.many p >>|| handle_list
let many1 p = Angstrom.many1 p >>|| handle_list
let sep_by s p = Angstrom.sep_by s p >>|| handle_list
let sep_by1 s p = Angstrom.sep_by1 s p >>|| handle_list
let count n p = Angstrom.count n p >>|| handle_list

let sep_end_by s p =
  fix (fun seb ->
      let seb1 =
        p >>= fun x -> choice [ lift2 (fun _ xs -> x :: xs) s seb; return [ x ] ]
      in
      choice [ seb1; return [] ])
;;

module No_ws = struct
  let char_lit = adapt Basic.char_lit

  let identifier' ?initial_char_p ?char_p () =
    adapt (Basic.identifier' ?initial_char_p ?char_p ())
  ;;

  let identifier = adapt Basic.identifier
  let integer_lit = adapt Basic.integer_lit
  let integer_or_float_lit = adapt Basic.integer_or_float_lit
  let string_lit = adapt Basic.string_lit
  let char c = adapt (Basic.char c)
  let satisfy f = adapt (Basic.satisfy f)
  let string str = adapt (Basic.string str)

  let mk_bracket_parser open_c close_c p =
    lift3 (fun _ v _ -> v) (char open_c <* whitespace) p (whitespace *> char close_c)
  ;;

  let parens p = mk_bracket_parser '(' ')' p
  let braces p = mk_bracket_parser '{' '}' p
  let brackets p = mk_bracket_parser '[' ']' p
end

module Ws = struct
  let char_lit = No_ws.char_lit <* whitespace

  let identifier' ?initial_char_p ?char_p () =
    No_ws.identifier' ?initial_char_p ?char_p () <* whitespace
  ;;

  let identifier = No_ws.identifier <* whitespace
  let integer_lit = No_ws.integer_lit <* whitespace
  let integer_or_float_lit = No_ws.integer_or_float_lit <* whitespace
  let string_lit = No_ws.string_lit <* whitespace
  let char c = No_ws.char c <* whitespace
  let satisfy f = No_ws.satisfy f <* whitespace
  let string str = No_ws.string str <* whitespace
  let parens p = No_ws.parens p <* whitespace
  let braces p = No_ws.braces p <* whitespace
  let brackets p = No_ws.brackets p <* whitespace
end

let no_comment = fail "no comment"

let c_comment =
  let comment = many (No_ws.satisfy Char.(fun c -> c <> '\n')) >>| String.of_char_list in
  No_ws.string "//" *> comment <* whitespace
;;

let%test_module "Parsing" =
  (module struct
    let parse_print p pp str =
      match parse_string_pos p str with
      | Error msg -> Stdio.print_string msg
      | Ok value -> Parse_result.pp pp Fmt.stdout value
    ;;

    let%expect_test _ =
      let pp_str ppf str = Fmt.pf ppf "%S" str in
      parse_print c_comment pp_str {|// comment|};
      [%expect {|{ value = " comment"; range = {2,10} }|}]
    ;;

    let pp_char ppf str = Fmt.pf ppf "%C" str
    let go = parse_print Ws.char_lit pp_char

    let%expect_test _ =
      go {|'x'|};
      [%expect {|{ value = 'x'; range = {0,3} }|}]
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

    let%expect_test _ =
      go {|("a" "b")|};
      (*   0123456789 *)
      [%expect {|{ value = ["a"; "b"]; range = {0,9} }|}]
    ;;

    let go =
      let pp ppf = function
        | Either.First str -> Fmt.pf ppf {|First %S|} str
        | Second fl -> Fmt.pf ppf {|Second %a|} Float.pp fl
      in
      parse_print Ws.integer_or_float_lit pp
    ;;

    let%expect_test _ =
      go "123";
      [%expect {|{ value = First "123"; range = {0,3} }|}]
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

    let go =
      let pp ppf = Fmt.(pf ppf "[%a]" (list ~sep:(any "; ") pp_str)) in
      parse_print Ws.(sep_end_by (char ';') string_lit) pp
    ;;

    let%expect_test _ =
      go {|"abc"|};
      [%expect {|{ value = ["abc"]; range = {0,5} }|}]
    ;;

    let%expect_test _ =
      go {|"abc"; "def"|};
      [%expect {|{ value = ["abc"; "def"]; range = {0,12} }|}]
    ;;

    let%expect_test _ =
      go {|"abc"; "def";|};
      [%expect {|{ value = ["abc"; "def"]; range = {0,13} }|}]
    ;;

    let go =
      let pp ppf =
        Fmt.(pf ppf "[%a]" (list ~sep:(any "; ") (list ~sep:(any ". ") pp_str)))
      in
      parse_print Ws.(sep_end_by (char ';') (sep_end_by (char '.') string_lit)) pp
    ;;

    let%expect_test _ =
      go {|"a". "b"; "c". "d"|};
      [%expect {|{ value = ["a". "b"; "c". "d"]; range = {0,18} }|}]
    ;;

    let go str =
      let open Ws in
      let parse =
        parse_string_pos
          (sep_by1 (whitespace *> string "->") (whitespace *> char '*')
          <* whitespace
          <* string "foo")
      in
      match parse str with
      | Ok { range; value = _ } -> Fmt.pr "%a\n" Opt_range.pp range
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
