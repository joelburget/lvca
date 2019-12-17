module A = Belt.Array
module L = Belt.List
module M = Belt.Map.Int
module MM = Belt.MutableMap.Int
module SI = Belt.Set.Int
module SS = Belt.Set.String
module MSI = Belt.MutableSet.Int
module Result = Belt.Result

type token_name = string
type regex = string
type lexer = (regex * token_name) list

(* TODO: a menhir token is not expected to carry location information *)
type token =
  { name : token_name
  ; start : int
  ; (* inclusive *)
    finish : int (* exclusive *)
  }

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

let find_first_capture : string M.t -> 'a Js.nullable array -> string option =
 fun tok_names captures ->
  try
    for i = 1 to Js.Array2.length captures - 1 do
      if Js.Nullable.isNullable captures.(i)
      then () (* Captures offset by one due to entire match appearing first *)
      else raise (FoundFirstCapture (i - 1))
    done;
    None
  with
  | FoundFirstCapture i -> Some (tok_names |. M.getExn i)
;;

(* TODO: no exn *)

let get_next_tok : string M.t -> Js.Re.t -> lexbuf -> token =
 fun tok_names re { buf; pos } ->
  re
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

let lex' : lexer -> string -> token array =
 fun lexer input ->
  let result = [||] in
  let lexbuf = { buf = input; pos = 0 } in
  let mut_tok_names = MM.make () in
  let re_str =
    lexer
    |. L.toArray
    |. A.mapWithIndex (fun i (re, tok_name) ->
           MM.set mut_tok_names i tok_name;
           "(" ^ re ^ ")")
    |. Js.Array2.joinWith "|"
  in
  let tok_names = mut_tok_names |. MM.toArray |> M.fromArray in
  let re = Js.Re.fromString @@ re_str in
  while lexbuf.pos < Js.String2.length lexbuf.buf do
    let tok = get_next_tok tok_names re lexbuf in
    let { start; finish } = tok in
    assert (start = lexbuf.pos);
    lexbuf.pos <- finish;
    ignore (Js.Array2.push result tok)
  done;
  result
;;

let lex : lexer -> string -> (token array, lex_error) Result.t =
 fun lexer input ->
  try Result.Ok (lex' lexer input) with
  | LexError err -> Result.Error err
;;
