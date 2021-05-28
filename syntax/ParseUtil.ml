open Base
open Lvca_provenance
open ParseUtil_intf
module ParseResult = ParseResult
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

let debug = ref false

let adapt_junkless junkless_p =
  let p ~latest_pos =
    let f (value, range) =
      let latest_pos =
        match range with
        | None ->
          if !debug then Fmt.pr "adapt_junkless | None -> %d\n" latest_pos;
          latest_pos
        | Some Range.{ finish; _ } ->
          if !debug then Fmt.pr "adapt_junkless | Some _ -> %d\n" finish;
          finish
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

let lift3 f a b c =
  a
  >>== fun { value = a_val; range = a_range; _ } ->
  b
  >>== fun { value = b_val; range = b_range; _ } ->
  c
  >>|| fun { value = c_val; range = c_range; latest_pos } ->
  { value = f a_val b_val c_val
  ; range = OptRange.list_range [ a_range; b_range; c_range ]
  ; latest_pos
  }
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
  (* TODO: okay to ignore latest_pos? *)
  Angstrom.fix (fun angstrom_t -> f (fun ~latest_pos:_ -> angstrom_t) ~latest_pos)
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

    let%expect_test _ =
      let parse =
        parse_string_pos
          (sep_by1 (whitespace *> string "->") (whitespace *> char '*')
          <* whitespace
          <* string "foo")
      in
      let result1 = parse "* -> *\nfoo" in
      (*                   0123456 7890 *)
      let result2 = parse "* -> * // asjdflksajfklasjfsal\nfoo" in
      (*                   0123456789012345678901234567890 1234
                                     1         2         3
       *)
      (match result1, result2 with
      | Ok { range = rng1; _ }, Ok { range = rng2; _ } ->
        Fmt.pr "%a, %a\n" OptRange.pp rng1 OptRange.pp rng2
      | _ -> Fmt.pr "not okay\n");
      [%expect {|{0,6}, {0,6}|}]
    ;;
  end)
;;
