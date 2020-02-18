(** Sits on top of the tablecloth, but still below everything else *)

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
  let hash_str : string -> Sha256.t
    = Sha256.string

  let to_hex : Sha256.t -> string
    = Sha256.to_hex

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
*)

module Re = struct
  type t = Str.regexp

  let of_string : string -> t
    = Str.regexp

  (* XXX error behavior *)
  let replace : re:t -> replacement:string -> string -> string
    = fun ~re ~replacement str -> Str.global_replace re replacement str
end

module Lex : sig
  type token =
    { name : string
    ; start : int (* inclusive *)
    ; finish : int (* exclusive *)
    }

  type token_name = string
  type regex = string
  type lexer = (regex * token_name) list

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
  type regex = string
  type lexer = (regex * token_name) list

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

  let string_of_tokens : token array -> string
    = fun toks -> toks
                  |> Array.map ~f:(fun { name; _ } -> name)
                  |> String.concat_array ~sep:" "

  (** raises: [LexError] *)
  let get_next_tok_exn : string Int.Map.t -> Str.regexp -> lexbuf -> token
    = fun tok_names re { buf; pos } ->

      let input = String.slice buf
        pos
        (String.length buf)
      in
      let matches = Str.string_match re input pos in

      if matches
      then
        let match_end = Str.match_end () in

        let possible_match = tok_names
          |> Int.Map.to_sequence
          |> Sequence.find
            ~f:(fun (i, _re) ->
              try
                let _ : string = Str.matched_group i input in true
              with
                Caml.Not_found -> false
            )
        in

        match possible_match with
          | Some (_match_num, name)
          -> { name; start = pos; finish = match_end }
          | None -> raise (LexError
            { start_pos = pos
            ; end_pos = match_end
            ; message =
              "Failed to find matching group even though a match was reported"
            })
      else
        raise
          (LexError
             { start_pos = pos
             ; end_pos = pos
             ; message = "Failed lex, \nlexbuf: " ^ buf
             })
  ;;

  (** raises: [LexError] *)
  let tokenize : lexer -> string -> token array
    = fun lexer input ->

      let result = Queue.create () in
      let lexbuf = { buf = input; pos = 0 } in
      let mut_tok_names = Int.Table.create () in

      let re_str = lexer
        |> List.mapi ~f:(fun i (re_str, tok_name) ->
          Int.Table.set mut_tok_names ~key:i ~data:tok_name;
          "(" ^ re_str ^ ")")
        |> String.concat ~sep:"|"
      in

      let tok_names = failwith "TODO"
      (*
      mut_tok_names
        |> Int.Table.to_array
        |> Int.Map.from_array
        *)
      in
      let re = Str.regexp re_str in

      while lexbuf.pos < String.length lexbuf.buf do
        let tok = get_next_tok_exn tok_names re lexbuf in
        let { start; finish; _ } = tok in
        assert (start = lexbuf.pos);
        lexbuf.pos <- finish;
        ignore (Queue.enqueue result tok : unit)
      done;

      Queue.to_array result

  let lex : lexer -> string -> (token array, lex_error) Result.t
    = fun lexer input ->
      try
        Ok (tokenize lexer input)
      with
        LexError err -> Error err

end
