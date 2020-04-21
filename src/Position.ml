open Sexplib.Std

type t =
  { pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  } [@@deriving sexp]

let zero_pos : t
  = { pos_lnum = 0
    ; pos_bol = 0
    ; pos_cnum = 0
    }

let of_lexing_position : Lexing.position -> t
  = fun { pos_lnum; pos_bol; pos_cnum; _ } -> { pos_lnum; pos_bol; pos_cnum }

let of_lexbuf : Lexing.lexbuf -> t
  = fun lexbuf -> of_lexing_position lexbuf.lex_curr_p

let to_string : t -> string
  = fun { pos_lnum; pos_bol; pos_cnum }
  -> Printf.sprintf "%n:(%n)%n" pos_lnum pos_bol pos_cnum
