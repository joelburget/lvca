type token = char * int
type token_tag = char
type token_set = Char_class.t

type stream =
  { str : string
  ; pos : int ref
  ; len : int
  }

let make_stream str = { str; pos = ref 0; len = String.length str }

module Token = struct
  type t = token
  type tag = token_tag
  type set = token_set

  let tag (c, _) = c
  let pp ppf (c, _) = Base.Char.pp ppf c

  module Tag = Base.Char

  module Set = struct
    include Char_class
    include Char_class.Char
  end
end

module Stream = struct
  type element = token
  type t = stream

  let peek { str; pos; len } =
    if !pos >= len
    then None
    else (
      let pos' = !pos in
      Base.Int.incr pos;
      Some (Base.String.unsafe_get str pos', pos'))
  ;;

  let junk { pos; _ } = Base.Int.incr pos
end
