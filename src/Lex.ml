module IntDict = Tablecloth.IntDict

type token_name = string
type regex = string
type lexer = (regex * token_name) list

(* TODO: a menhir token is not expected to carry location information *)
type token =
  { name : token_name
  ; start : int (* inclusive *)
  ; finish : int (* exclusive *)
  }

let string_of_tokens : token array -> string
  = fun toks -> toks
                |. Tablecloth.Array.map ~f:(fun { name } -> name)
                |. Js.Array2.joinWith " "

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
exception FoundFirstCapture of int

let find_first_capture : string IntDict.t -> 'a Js.nullable array -> string option =
  fun tok_names captures ->
  try
    for i = 1 to Js.Array2.length captures - 1 do
      if Js.Nullable.isNullable captures.(i)
      then () (* Captures offset by one due to entire match appearing first *)
      else raise (FoundFirstCapture (i - 1))
    done;
    None
  with
    FoundFirstCapture i -> Some (tok_names
    |> IntDict.get ~key:i
    |> Util.get_option' (fun () -> "unable to find token " ^ string_of_int i)
  )
;;

(** raises: [LexError] *)
let get_next_tok_exn : string IntDict.t -> Js.Re.t -> lexbuf -> token =
  fun tok_names re { buf; pos } -> re
  |. Js.Re.exec_ (buf |. Js.String2.sliceToEnd ~from:pos)
  |. function
    | Some result ->
      let captures = Js.Re.captures result in
      (match Js.Nullable.toOption captures.(0), find_first_capture tok_names captures with
       | Some token_contents, Some name ->
         { name; start = pos; finish = pos + Js.String2.length token_contents }
       | _, _ ->
         raise (LexError { start_pos = pos; end_pos = pos (* TODO *); message = "TODO 1" }))
    | None ->
      raise
        (LexError
           { start_pos = pos
           ; end_pos = pos (* TODO *)
           ; message = "Failed lex, re: " ^ Js.Re.source re ^ "\nlexbuf: " ^ buf
           })
;;

(** raises: [LexError] *)
let lex_exn : lexer -> string -> token array =
  fun lexer input ->
  let result = [||] in
  let lexbuf = { buf = input; pos = 0 } in
  let mut_tok_names = Placemat.MutableMap.Int.make () in
  let re_str = lexer
    |> Placemat.List.map_with_index ~f:(fun i (re, tok_name) ->
      Placemat.MutableMap.Int.set mut_tok_names i tok_name;
      "(" ^ re ^ ")")
    |> Util.stringify_list Util.id "|"
  in
  let tok_names = mut_tok_names
    |> Placemat.MutableMap.Int.to_list
    |> IntDict.from_list
  in
  let re = Js.Re.fromString re_str in
  while lexbuf.pos < Js.String2.length lexbuf.buf do
    let tok = get_next_tok_exn tok_names re lexbuf in
    let { start; finish } = tok in
    assert (start = lexbuf.pos);
    lexbuf.pos <- finish;
    ignore (Js.Array2.push result tok)
  done;
  result
;;

let lex : lexer -> string -> (lex_error, token array) Tablecloth.Result.t =
  fun lexer input ->
  try
    Ok (lex_exn lexer input)
  with
    LexError err -> Error err
;;
