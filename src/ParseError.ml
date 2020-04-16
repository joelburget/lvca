type t =
  { start_pos : Position.t
  ; end_pos : Position.t
  ; message : string
  }

let to_string : t -> string
  = fun { start_pos; end_pos; message }
  -> Printf.sprintf "position %s-%s: %s"
    (Position.to_string start_pos) (Position.to_string end_pos) message

let length : t -> int
  = fun { start_pos; end_pos; _ } -> end_pos.pos_cnum - start_pos.pos_cnum

exception SyntaxError of t
