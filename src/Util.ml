open Base

(* Used by bidirectional *)
module Map = struct
  include Base.Map

  let remove_many : ('k, 'v, 'cmp) t -> string array -> ('k, 'v, 'cmp) t =
   fun map keys -> Array.fold keys ~init:map ~f:Map.remove
  ;;

  let union_left_biased m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_k ->
      function `Both (v, _) -> Some v | `Left v | `Right v -> Some v)
  ;;

  let union_right_biased m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_k ->
      function `Both (_, v) -> Some v | `Left v | `Right v -> Some v)
  ;;
end

module String = struct
  include Base.String

  module Map = struct
    type 'a t = (string, 'a, Base.String.comparator_witness) Base.Map.t
    type key = string
    let empty = Base.Map.empty (module Base.String)
    let singleton k v = Base.Map.singleton (module Base.String) k v
    let of_alist lst = Base.Map.of_alist (module Base.String) lst
    let of_alist_exn lst = Base.Map.of_alist_exn (module Base.String) lst

    (* Used by Core.Types, Bidirectional *)
    let unions_left_biased : 'a t list -> 'a t
      = List.fold_left ~init:empty
          ~f:(Map.merge_skewed ~combine:(fun ~key:_k v1 _v2 -> v1))

    let unions_right_biased : 'a t list -> 'a t
      = List.fold_left ~init:empty
          ~f:(Map.merge_skewed ~combine:(fun ~key:_k _v1 v2 -> v2))

    exception DuplicateKey of string

    let strict_union
      : 'a t -> 'a t -> [ `Ok of 'a t | `Duplicate_key of string ]
    = fun m1 m2 -> try
        `Ok (Map.merge m1 m2 ~f:(fun ~key -> function
          | `Both _ -> raise (DuplicateKey key)
          | `Left v | `Right v -> Some v))
      with
        DuplicateKey k -> `Duplicate_key k
    ;;

    let strict_unions
      : 'a t list -> [ `Ok of 'a t | `Duplicate_key of string ]
      = fun lst -> try
          `Ok (List.fold_left lst
            ~init:empty
            ~f:(fun accum m -> match strict_union accum m with
              | `Ok m' -> m'
              | `Duplicate_key key -> raise (DuplicateKey key))
          )
        with
          DuplicateKey k -> `Duplicate_key k
  end

  module Set = struct
    type t = (string, Base.String.comparator_witness) Base.Set.t
    let empty = Base.Set.empty (module Base.String)
    let of_list = Base.Set.of_list (module Base.String)
  end

  let slice : string -> int -> int -> string
    = fun t start stop ->
      let normalize t' i = if Int.(i < 0) then i + length t' else i in
      let stop = if Int.(stop = 0) then length t else stop in
      let pos = normalize t start in
      let len = normalize t stop - pos in
      sub t ~pos ~len
end

module List = struct
  let rec unsnoc lst =
    match lst with
    | [] -> failwith "unsnoc empty list"
    | [ x ] -> [], x
    | x :: lst' ->
      let front, last = unsnoc lst' in
      x :: front, last
  ;;
end

exception InvariantViolation of string

let invariant_violation str = raise (InvariantViolation str)

module Option = struct
  let get_or_raise : 'b -> 'a option -> 'a =
   fun err -> function None -> raise err | Some a -> a
  ;;

  let get_invariant : (unit -> string) -> 'a option -> 'a =
   fun msg -> function
    | None -> raise (InvariantViolation ("invariant violation: " ^ msg ()))
    | Some a -> a
  ;;
end

module Json = struct
  type t =
    | String of string
    | Array of t array
    | Float of float

  let array : t array -> t = fun arr -> Array arr
  let string : string -> t = fun str -> String str
  let float : float -> t = fun f -> Float f

  let rec (=) : t -> t -> bool
    = fun t1 t2 -> match t1, t2 with
      | String s1, String s2 -> String.(s1 = s2)
      | Array arr1, Array arr2 -> (match Array.zip arr1 arr2 with
        | None -> false
        | Some arr -> Array.for_all arr ~f:(fun (t1, t2) -> t1 = t2))
      | Float f1, Float f2 -> Float.(f1 = f2)
      | _, _ -> false
end

module Sha256 = struct
  let hash_str : string -> Digestif.SHA256.t = Digestif.SHA256.digest_string
  let to_hex : Digestif.SHA256.t -> string = Digestif.SHA256.to_hex

  let hash : Bytes.t -> string =
   fun bytes -> bytes |> Bytes.to_string |> hash_str |> to_hex
 ;;
end

module Cbor = struct
  let rec of_json : Json.t -> CBOR.Simple.t = function
    | String str -> `Text str
    | Array arr -> `Array (arr |> Array.map ~f:of_json |> Array.to_list)
    | Float f -> `Float f
  ;;

  let rec to_json : CBOR.Simple.t -> Json.t option = function
    | `Text str -> Some (String str)
    | `Array arr ->
      arr
      |> Base.List.map ~f:to_json
      |> Base.Option.all
      |> Base.Option.map ~f:(fun lst -> Json.Array (Array.of_list lst))
    | `Float f -> Some (Float f)
    | _ -> None
  ;;

  let encode : Json.t -> Bytes.t =
   fun json -> json |> of_json |> CBOR.Simple.encode |> Bytes.of_string
 ;;

  let decode : Bytes.t -> Json.t option =
   fun buf -> buf |> Bytes.to_string |> CBOR.Simple.decode |> to_json
 ;;
end

module Angstrom = struct
  module S = struct
    open Angstrom

    (*
     Copyright (c) 2016, Inhabited Type LLC

     All rights reserved.

     Redistribution and use in source and binary forms, with or without
     modification, are permitted provided that the following conditions
     are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. Neither the name of the author nor the names of his contributors
        may be used to endorse or promote products derived from this software
        without specific prior written permission.

     THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
     OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
     DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
     ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
     OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
     STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
     ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
     POSSIBILITY OF SUCH DAMAGE.
      *)

    type t =
      [ `Unescaped
      | `Escaped
      | `UTF8  of char list
      | `UTF16 of int * [`S | `U | `C of char list]
      | `Error of string
      | `Done ]

    let to_string : [`Terminate | t] -> string = function
      | `Unescaped -> "unescaped"
      | `Escaped   -> "escaped"
      | `UTF8 _    -> "utf-8 _"
      | `UTF16 _   -> "utf-16 _ _"
      | `Error e   -> Printf.sprintf "error %S" e
      | `Terminate -> "terminate"
      | `Done      -> "done"

    let unescaped buf = function
      | '"'  -> `Terminate
      | '\\' -> `Escaped
      | c    ->
        if Char.(c <= '\031')
        then `Error (Printf.sprintf "unexpected character '%c'" c)
        else begin Buffer.add_char buf c; `Unescaped end

    let escaped buf = function
      | '\x22' -> Buffer.add_char buf '\x22'; `Unescaped
      | '\x5c' -> Buffer.add_char buf '\x5c'; `Unescaped
      | '\x2f' -> Buffer.add_char buf '\x2f'; `Unescaped
      | '\x62' -> Buffer.add_char buf '\x08'; `Unescaped
      | '\x66' -> Buffer.add_char buf '\x0c'; `Unescaped
      | '\x6e' -> Buffer.add_char buf '\x0a'; `Unescaped
      | '\x72' -> Buffer.add_char buf '\x0d'; `Unescaped
      | '\x74' -> Buffer.add_char buf '\x09'; `Unescaped
      | '\x75' -> `UTF8 []
      | _      -> `Error "invalid escape sequence"

    let hex c =
      match c with
      | '0' .. '9' -> Caml.Char.code c - 0x30 (* '0' *)
      | 'a' .. 'f' -> Caml.Char.code c - 87
      | 'A' .. 'F' -> Caml.Char.code c - 55
      | _          -> 255

    let utf_8 buf d = function
      | [c;b;a] ->
        let a = hex a and b = hex b and c = hex c and d = hex d in
        if a lor b lor c lor d = 255 then
          `Error "invalid hex escape"
        else
          let cp = (a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
          if cp >= 0xd800 && cp <= 0xdbff then
            `UTF16(cp, `S)
          else begin
            Buffer.add_char buf (Caml.Char.unsafe_chr (0b11100000 lor ((cp lsr 12) land 0b00001111)));
            Buffer.add_char buf (Caml.Char.unsafe_chr (0b10000000 lor ((cp lsr  6) land 0b00111111)));
            Buffer.add_char buf (Caml.Char.unsafe_chr (0b10000000 lor (cp          land 0b00111111)));
            `Unescaped
          end
      | cs -> `UTF8 (d::cs)

    let utf_16 buf d x s =
      match s, d with
      | `S        , '\\' -> `UTF16(x, `U)
      | `U        , 'u'  -> `UTF16(x, `C [])
      | `C [c;b;a], _    ->
        let a = hex a and b = hex b and c = hex c and d = hex d in
        if a lor b lor c lor d = 255 then
          `Error "invalid hex escape"
        else
          let y = (a  lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
          if y >= 0xdc00 && y <= 0xdfff then begin
            let hi = x - 0xd800 in
            let lo = y - 0xdc00 in
            let cp = 0x10000 + ((hi lsl 10) lor lo) in
            Buffer.add_char buf (Caml.Char.unsafe_chr (0b11110000 lor ((cp lsr 18) land 0b00000111)));
            Buffer.add_char buf (Caml.Char.unsafe_chr (0b10000000 lor ((cp lsr 12) land 0b00111111)));
            Buffer.add_char buf (Caml.Char.unsafe_chr (0b10000000 lor ((cp lsr  6) land 0b00111111)));
            Buffer.add_char buf (Caml.Char.unsafe_chr (0b10000000 lor (cp          land 0b00111111)));
            `Unescaped
          end else
            `Error "invalid escape sequence for utf-16 low surrogate"
      | `C cs,      _    -> `UTF16(x, `C (d::cs))
      | _, _             -> `Error "invalid escape sequence for utf-16 low surrogate"

    let str buf =
      let state : t ref = ref `Unescaped in
      skip_while (fun c ->
        match
          begin match !state with
          | `Unescaped    -> unescaped buf c
          | `Escaped      -> escaped   buf c
          | `UTF8 cs      -> utf_8     buf c cs
          | `UTF16(x, cs) -> utf_16    buf c x cs
          | (`Error _ | `Done) as state -> state
          end
        with
          | (`Error _) | `Done -> false
          | `Terminate         -> state := `Done; true
          | #t as state'       -> state := state'; true)
      >>= fun () ->
        match !state with
        | `Done ->
          let result = Buffer.contents buf in
          Buffer.clear buf;
          state := `Unescaped;
          return result
        | `Error msg ->
          Buffer.clear buf; state := `Unescaped; fail msg
        | `Unescaped | `Escaped | `UTF8 _ | `UTF16 _ ->
          Buffer.clear buf; state := `Unescaped; fail "unterminated string"
  end

  module Internal = struct
    open Angstrom

    let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
    let is_digit = function '0'..'9' -> true | _ -> false

    let integer_lit = take_while1 is_digit

    let is_whitespace = function
      | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
      | _ -> false

    let sign =
      peek_char
      >>= function
        | Some '-' -> advance 1 >>| fun () -> "-"
        | Some '+' -> advance 1 >>| fun () -> "+"
        | Some c  when (is_digit c) -> return ""
        | _ -> fail "Sign or digit expected"

    let integer_or_float_lit : (string, float) Either.t Angstrom.t
      = sign >>= fun sign ->
        integer_lit >>= fun whole ->
        choice
          [ begin
              char '.' >>= fun _ ->
              option "" integer_lit >>= fun part ->
              return (Either.Second (Float.of_string (sign ^ whole ^ "." ^ part)))
            end
          ; return (Either.First (sign ^ whole))
          ]

    let string_lit = char '"' *> S.str (Buffer.create 0x100)
    let char_lit = char '\'' *> any_char <* char '\''

    let identifier =
      satisfy Char.(fun c -> is_alpha c || c = '_') >>= fun c ->
      take_while Char.(fun c -> is_alpha c || is_digit c || c = '_') >>| fun cs ->
      String.(of_char c ^ cs)
  end

  let whitespace = Angstrom.(take_while Internal.is_whitespace *> return ())
  let whitespace1 = Angstrom.(take_while1 Internal.is_whitespace *> return ())

  module type Comment_int = sig
    val comment : unit Angstrom.t
  end

  module NoComment : Comment_int = struct
    let comment = Angstrom.fail "no comment"
  end

  module CComment : Comment_int = struct
    open Angstrom
    let comment =
      string "//" >>= fun _ ->
      many (satisfy Char.(fun x -> x <> '\n')) >>| fun _ ->
      ()
  end

  module Mk (Comment : Comment_int) = struct
    let many, (>>=), (<|>), (<*), ( *> ), fail, return =
      Angstrom.(many, (>>=), (<|>), (<*), ( *> ), fail, return)

    let junk = many (whitespace1 <|> Comment.comment)
    let identifier = Internal.identifier <* junk
    let char c = Angstrom.char c <* junk
    let parens p = char '(' *> p <* Angstrom.char ')' <* junk
    let braces p = char '{' *> p <* Angstrom.char '}' <* junk
    let brackets p = char '[' *> p <* Angstrom.char ']' <* junk
    let string str = Angstrom.string str <* junk
    let integer_or_float_lit = Internal.integer_or_float_lit <* junk
    let string_lit = Internal.string_lit <* junk
    let char_lit = Internal.char_lit <* junk
  end

  (* Parse one or more occurences of e, separated by op. Returns a value obtained by a
   * left-associative application of all functions returned by op to the values returned
   * by p. *)
  let chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
    = fun e op ->
    let open Angstrom in
    let rec go acc =
      (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
    e >>= go

  let%test_module "Parsing" = (module struct
    let (=) = Caml.(=)
    let parse' parser = Angstrom.parse_string ~consume:All parser
    module Parse = Mk(NoComment)

    let%test _ = parse' Parse.string_lit {|"abc"|} = Ok "abc"
    let%test _ = parse' Parse.string_lit {|"\""|} = Ok {|"|}
    let%test _ = parse' Parse.string_lit {|"\\"|} = Ok {|\|}
    let%test _ = parse' Parse.integer_or_float_lit "123" = Ok (First "123")
    let%test _ = parse' Parse.integer_or_float_lit "-123" = Ok (First "-123")
    let%test _ = parse' Parse.integer_or_float_lit "+123" = Ok (First "+123")
    let%test _ = parse' Parse.integer_or_float_lit "1.1" = Ok (Second 1.1)
    let%test _ = parse' Parse.integer_or_float_lit "-1.1" = Ok (Second (-1.1))
    let%test _ = parse' Parse.integer_or_float_lit "+1.1" = Ok (Second 1.1)
    let%test _ = parse' Parse.integer_or_float_lit "1." = Ok (Second 1.)
  end);;
end
