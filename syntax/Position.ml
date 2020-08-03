(** A position in a buffer / source file. Includes line number and column number.

 This representation borrows heavily from Ocaml's built in module Lexing.

 - [pos_lnum] is the line number.
 - [pos_bol] is the offset of the beginning of the line (number of characters between the
   beginning of the lexbuf and the beginning of the line).
 - [pos_cnum] is the offset of the position (number of characters between the beginning of
   the lexbuf and the position).

 The difference between [pos_cnum] and [pos_bol] is the character offset within the line (i.e. the column number, assuming each character is one column wide).

 Note that this doesn't include [pos_fname] from Lexing.
 *)

type t =
  { pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }

let zero_pos : t
  = { pos_lnum = 0
    ; pos_bol = 0
    ; pos_cnum = 0
    }

let of_lexing_position : Lexing.position -> t
  = fun { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } ->
    { pos_lnum; pos_bol; pos_cnum }

let of_lexbuf : Lexing.lexbuf -> t
  = fun lexbuf -> of_lexing_position lexbuf.lex_curr_p

let to_string : t -> string
  = fun { pos_lnum; pos_bol; pos_cnum }
  -> Printf.sprintf "%n:(%n)%n" pos_lnum pos_bol pos_cnum
