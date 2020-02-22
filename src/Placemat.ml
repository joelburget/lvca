open Core_kernel

module Json = struct
  type t =
    | String of string
    | Array of t array

  let array : t array -> t
    = fun arr -> Array arr

  let string : string -> t
    = fun str -> String str
end

module Sha256 = struct
  let hash_str : string -> Digestif.SHA256.t
    = Digestif.SHA256.digest_string

  let to_hex : Digestif.SHA256.t -> string
    = Digestif.SHA256.to_hex

  let hash : Bytes.t -> string
    = fun bytes -> bytes
      |> Bytes.to_string
      |> hash_str
      |> to_hex
end

module Cbor = struct
  let rec of_json : Json.t -> CBOR.Simple.t
    = function
      | String str -> `Text str
      | Array arr -> `Array (arr
        |> Array.map ~f:of_json
        |> Array.to_list
      )

  let rec to_json : CBOR.Simple.t -> Json.t option
    = function
      | `Text str -> Some (String str)
      | `Array arr -> arr
        |> List.map ~f:to_json
        |> Option.all
        |> Option.map ~f:(fun lst -> Json.Array (Array.of_list lst))
      | _ -> None

  let encode : Json.t -> Bytes.t
    = fun json -> json
      |> of_json
      |> CBOR.Simple.encode
      |> Bytes.of_string

  let decode : Bytes.t -> Json.t option
    = fun buf -> buf
      |> Bytes.to_string
      |> CBOR.Simple.decode
      |> to_json
end

(*
module String = struct
  let concat_array : ?sep:string -> string array -> string
    = String.concat_array

  (** raises [Invalid_argument] *)
  let get : string -> int -> char
    = String.get
end

module Re = struct
  type t = Re.re

  let replace : re:t -> replacement:string -> string -> string
    = fun ~re ~replacement str -> Re.replace re ~f:(fun _ -> replacement) str
end
*)

module Lex : sig
  type token =
    { name : string
    ; start : int (* inclusive *)
    ; finish : int (* exclusive *)
    }

  type token_name = string
  type lexer = (token_name * Regex.t) list

  type position = int

  type lex_error =
    { start_pos : position
    ; end_pos : position
    ; message : string
    }

  type lexbuf =
    { buf : string
    ; mutable pos : position
    }

  exception LexError of lex_error

  val string_of_tokens : token array -> string
  val lex : lexer -> string -> (token array, lex_error) Result.t
end = struct
  type token =
    { name : string
    ; start : int (* inclusive *)
    ; finish : int (* exclusive *)
    }

  type token_name = string
  type lexer = (token_name * Regex.t) list

  type position = int

  type lex_error =
    { start_pos : position
    ; end_pos : position
    ; message : string
    }

  type lexbuf =
    { buf : string
    ; mutable pos : position
    }

  exception LexError of lex_error

  let string_of_lexer = fun token_row -> token_row
    |> List.map ~f:(fun (name, re) ->
       Printf.sprintf "%s := /%s/" name (Regex.to_string re))
    |> String.concat ~sep:"\n"

  let string_of_tokens : token array -> string
    = fun toks -> toks
                  |> Array.map ~f:(fun { name; _ } -> name)
                  |> String.concat_array ~sep:" "

  (** raises: [LexError] *)
  let get_next_tok_exn : lexer -> string Int.Map.t -> Re.re -> lexbuf -> token
    = fun lexer tok_names re { buf; pos } ->

      match Re.exec_opt ~pos re buf with
        | None -> raise
          (LexError
             { start_pos = pos
             ; end_pos = pos
             ; message = Printf.sprintf
               "No match found.\n\nLexbuf:\n%s\n\nLexer:\n%s"
               buf
               (string_of_lexer lexer)
             })
        | Some groups ->
          let group_nums = Map.keys tok_names in
          let match_num, match_end = List.fold_until group_nums
            ~init:()
            ~f:(fun () group_num ->
              if Re.Group.test groups group_num
              then
                Stop (group_num, Re.Group.stop groups group_num)
              else
                Continue ())
            ~finish:(fun () -> raise (LexError
              { start_pos = pos
              ; end_pos = pos
              ; message = Printf.sprintf
                "Invariant violation: no match found.\n\nLexbuf:\n%s\n\nLexer:\n%s"
                buf
                (string_of_lexer lexer)
              }))
          in

          let name = Map.find_exn tok_names match_num in
          { name; start = pos; finish = match_end }
  ;;

  (** raises: [LexError] *)
  let tokenize : lexer -> string -> token array
    = fun lexer input ->

      let result = Queue.create () in
      let lexbuf = { buf = input; pos = 0 } in

      let re : Re.re = lexer
        |> List.map ~f:(fun (_, re) -> Regex.to_re re)
        |> Re.alt
        |> Re.compile
      in

      let tok_names = lexer
        |> List.mapi ~f:(fun i (name, _re) -> i + 1, name)
        |> Int.Map.of_alist_exn
      in

      while lexbuf.pos < String.length lexbuf.buf do
        let tok = get_next_tok_exn lexer tok_names re lexbuf in
        let { start; finish; _ } = tok in
        assert (start = lexbuf.pos);
        lexbuf.pos <- finish;
        ignore (Queue.enqueue result tok : unit)
      done;

      Queue.to_array result

  let lex
    : lexer -> string -> (token array, lex_error) Result.t
    = fun lexer input ->
      try
        Ok (tokenize lexer input)
      with
        LexError err -> Error err

  let test_print_result = function
    | Ok toks -> Array.iter toks ~f:(fun { name; start; finish }
    -> printf "%s %n %n\n" name start finish)
    | Error { message; start_pos; end_pos; _ } -> printf
      "Lexing error at characters %n - %n: %s\n"
      start_pos end_pos message

  let%expect_test "lex 1" =
    let lexer1 = Regex.(
      [ "IF", ReString "if";
        "THEN", ReString "then";
        "ELSE", ReString "else";
        "OP", ReChoice [
          ReString "<";
          ReString ">";
          ReString "<=";
          ReString ">=";
          ReString "==";
          ReString "!=";
        ];
        "ID", ReConcat [
          ReClass (PosClass Word);
          ReSet "a-zA-Z0-9_";
        ];
        "NUM", ReClass (PosClass Digit);
        "WHITE", ReClass (PosClass Whitespace);
      ])
    in

    let result = lex lexer1 "if a > b then 90 else 91" in
                          (* 012345678901234567890123 *)

    test_print_result result;

    [%expect{|
      IF 0 2
      WHITE 2 3
      ID 3 4
      WHITE 4 5
      OP 5 6
      WHITE 6 7
      ID 7 8
      WHITE 8 9
      THEN 9 13
      WHITE 13 14
      NUM 14 16
      WHITE 16 17
      ELSE 17 21
      WHITE 21 22
      NUM 22 24 |}]

   let%expect_test "lex 2" =
    let lexer2 = Regex.(
      [ "+", ReString "+";
        "*", ReString "*";
        "(", ReString "(";
        ")", ReString ")";
        "id", ReClass (PosClass Word);
        "WHITE", ReClass (PosClass Whitespace);
      ])
    in

    let result = lex lexer2 "foo + bar" in
                          (* 012345678 *)

    test_print_result result;
    [%expect{|
      id 0 3
      WHITE 3 4
      + 4 5
      WHITE 5 6
      id 6 9 |}]

  let%expect_test "lex 3" =
    let lexer3 = Regex.(
      [ "COLON", ReString ":";
        "IF", ReString "if";
        "THEN", ReString "then";
        "ELSE", ReString "else";
        "FUN", ReString "fun";
        "ARROW", ReString "->";
        "TRUE", ReString "true";
        "FALSE", ReString "false";
        "BOOL", ReString "bool";
        "ID",
        ReConcat [
          ReClass (PosClass Word);
          ReSet "a-zA-Z0-9_";
        ];
        "SPACE", ReClass (PosClass Whitespace);
      ])
    in

    let result = lex lexer3 "if false then false else true" in
                          (* 01234567890123456789012345678 *)
    test_print_result result;

    [%expect{|
      IF 0 2
      SPACE 2 3
      FALSE 3 8
      SPACE 8 9
      THEN 9 13
      SPACE 13 14
      FALSE 14 19
      SPACE 19 20
      ELSE 20 24
      SPACE 24 25
      TRUE 25 29 |}]

end
