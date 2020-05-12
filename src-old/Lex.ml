open Core_kernel

type token =
  { name : string
  ; start : int (* inclusive *)
  ; finish : int (* exclusive *)
  }

type token_name = string
type lexer = (token_name * Regex.t) list

type lexbuf =
  { buf : string
  ; mutable lnum : int
  (** The current line number (= number of newlines we've crossed) *)
  ; mutable bol_pos : int
  (** The character number of the beginning of the current line *)
  ; mutable abs_pos : int
  (** The absolute position in the buffer of the current position *)
  }

  (*
let string_of_lexer token_row =
  token_row
  |> List.map ~f:(fun (name, re) ->
         Printf.sprintf "%s := /%s/" name (Regex.to_string re))
  |> String.concat ~sep:"\n"
;;
*)

let string_of_tokens : token array -> string =
 fun toks ->
  toks |> Array.map ~f:(fun { name; _ } -> name) |> String.concat_array ~sep:" "
;;

(** @raise [LexicalError] *)
let get_next_tok_exn : (int * string) list -> Re.re -> lexbuf -> token =
  fun tok_names re { buf; lnum; bol_pos; abs_pos } ->
  let position = Position.
    { pos_lnum = lnum
    ; pos_bol = bol_pos
    ; pos_cnum = abs_pos
    }
  in
  match Re.exec_opt ~pos:abs_pos re buf with
  | None ->
    raise (LexerUtil.LexicalError { position ; message = "No match found" })
  | Some groups ->
    let name, match_end =
      List.fold_until
        tok_names
        ~init:()
        ~f:(fun () (group_num, tok_name) ->
          if Re.Group.test groups group_num
          then Stop (tok_name, Re.Group.stop groups group_num)
          else Continue ())
        ~finish:(fun () ->
          raise
            (LexerUtil.LexicalError
               { position
               ; message = "Invariant violation: no match found."
               }))
    in
    { name; start = abs_pos; finish = match_end }
;;

(** @raise [LexicalError] *)
let tokenize_exn : lexer -> string -> token array =
 fun lexer input ->
  (* Printf.printf "tokenize \"%s\"\n" input; *)
  let result = Queue.create () in
  let lexbuf = { buf = input; lnum = 0; bol_pos = 0; abs_pos = 0 } in
  let re : Re.re =
    lexer
    |> List.map ~f:(fun (_, re) -> Re.group (Regex.to_re re))
    |> Re.alt
    |> Re.compile
  in
  let tok_names = List.mapi lexer ~f:(fun i (name, _re) -> i + 1, name) in
  while lexbuf.abs_pos < String.length lexbuf.buf do
    let tok = get_next_tok_exn tok_names re lexbuf in
    let { start; finish; _ } = tok in
    (* Printf.printf "name: %s, start: %n, finish: %n\n" name start finish; *)
    assert (start = lexbuf.abs_pos);
    let substr = String.slice input start finish in
    let newline_count = String.count substr ~f:(fun c -> Char.(c = '\n')) in
    if newline_count > 0 then (
      lexbuf.lnum <- lexbuf.lnum + newline_count;
      lexbuf.bol_pos <-
        String.rfindi ~pos:(finish - 1) input ~f:(fun _i c -> Char.(c = '\n'))
        |> Util.get_option' (fun () -> "There must be a newline in this string");
    );
    lexbuf.abs_pos <- finish;
    ignore (Queue.enqueue result tok : unit)
  done;
  Queue.to_array result
;;

let lex : lexer -> string -> (token array, LexerUtil.lex_error) Result.t =
 fun lexer input ->
   try
     Ok (tokenize_exn lexer input)
   with
     LexerUtil.LexicalError err -> Error err
;;

let test_print_result = function
  | Ok toks ->
    Array.iter toks ~f:(fun { name; start; finish } ->
        printf "%s %n %n\n" name start finish)
  | Error (LexerUtil.{ message; position }) ->
    printf "Lexing error at %s: %s\n" (Position.to_string position) message
;;

let%expect_test "lex 1" =
  let lexer1 =
    Regex.
      [ "IF", re_str "if"
      ; "THEN", re_str "then"
      ; "ELSE", re_str "else"
      ; ( "OP"
        , ReChoice
            [ ReChar '<'
            ; ReChar '>'
            ; re_str "<="
            ; re_str ">="
            ; re_str "=="
            ; re_str "!="
            ] )
      ; ( "ID"
        , ReConcat
            [ (* ReClass (PosClass Word); *)
              Classes.alpha
            ; ReStar Classes.underscore_words
            ] )
      ; "NUM", (* RePlus (ReSet [Range ('0', '9')]); *)
               RePlus (ReClass (PosClass Digit))
      ; "WHITE", (* RePlus (ReChar " "); *)
                 RePlus (ReClass (PosClass Whitespace))
      ]
  in
  let result = lex lexer1 "if a > b then 90 else 91" in
  (* 012345678901234567890123 *)
  test_print_result result;
  [%expect
    {|
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
;;

let%expect_test "lex 2" =
  let lexer2 =
    Regex.
      [ "+", ReChar '+'
      ; "*", ReChar '*'
      ; "(", ReChar '('
      ; ")", ReChar ')'
      ; "id", ReConcat [ Classes.alpha; ReStar Classes.underscore_words ]
      ; (* ReClass (PosClass Word); *)
        "WHITE", RePlus (ReChar ' ')
        (* ReClass (PosClass Whitespace); *)
      ]
  in
  let result = lex lexer2 "foo + bar" in
  (* 012345678 *)
  test_print_result result;
  [%expect {|
    id 0 3
    WHITE 3 4
    + 4 5
    WHITE 5 6
    id 6 9 |}]
;;

let%expect_test "lex 3" =
  let lexer3 =
    Regex.
      [ "COLON", ReChar ':'
      ; "IF", re_str "if"
      ; "THEN", re_str "then"
      ; "ELSE", re_str "else"
      ; "FUN", re_str "fun"
      ; "ARROW", re_str "->"
      ; "TRUE", re_str "true"
      ; "FALSE", re_str "false"
      ; "BOOL", re_str "bool"
      ; ( "ID"
        , ReConcat
            [ Classes.alpha
            ; Classes.underscore_words
              (* ReSet "a-zA-Z"; *)
              (* ReSet "a-zA-Z0-9_"; *)
            ] )
      ; "SPACE", RePlus (ReChar ' ')
        (* ReClass (PosClass Whitespace); *)
      ]
  in
  let result = lex lexer3 "if false then false else true" in
  (* 01234567890123456789012345678 *)
  test_print_result result;
  [%expect
    {|
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
;;
