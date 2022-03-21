open Base

module Literal = struct
  module M = struct
    type t =
      | Double_quoted
      | Single_quoted
      | Integer
      | Floating
      | Regex
    [@@deriving compare, sexp]
  end

  include M
  include Comparable.Make (M)

  let pp ppf = function
    | Double_quoted -> Fmt.pf ppf "Double_quoted"
    | Single_quoted -> Fmt.pf ppf "Single_quoted"
    | Integer -> Fmt.pf ppf "Integer"
    | Floating -> Fmt.pf ppf "Floating"
    | Regex -> Fmt.pf ppf "Regex"
  ;;
end

module Token_tag = struct
  module M = struct
    type t =
      | Symbol of string
      | Comment
      | Lower_ident
      | Upper_ident
      | Literal of Literal.t
      | Keyword of string
      | Whitespace
    [@@deriving compare, sexp]
  end

  include M
  include Comparable.Make (M)

  let pp ppf = function
    | Symbol str -> Fmt.pf ppf "Symbol %S" str
    | Lower_ident -> Fmt.pf ppf "Lower_ident"
    | Upper_ident -> Fmt.pf ppf "Upper_ident"
    | Keyword str -> Fmt.pf ppf "Keyword %S" str
    | Whitespace -> Fmt.pf ppf "Whitespace"
    | Comment -> Fmt.pf ppf "Comment"
    | Literal l -> Fmt.pf ppf "Literal %a" Literal.pp l
  ;;
end

module type Token_tag = sig
  include Taparse.Signatures.Token_tag
  include Base.Comparator.S with type t := t
end

module Token_set (Tag : Token_tag) :
  Taparse.Signatures.Token_set
    with type t = (Tag.t, Tag.comparator_witness) Set.t
     and type tag = Tag.t = struct
  type t = (Tag.t, Tag.comparator_witness) Set.t
  type tag = Tag.t

  let pp ppf set = Fmt.(braces (list Tag.pp ~sep:comma)) ppf (Set.to_list set)
  let empty = Set.empty (module Tag)
  let is_empty = Set.is_empty
  let ( = ) = Set.equal
  let inter = Set.inter
  let union = Set.union
  let mem = Set.mem
  let is_subset a ~of_ = Set.is_subset a ~of_
  let of_list = Set.of_list (module Tag)

  module Infix = struct
    let ( - ) = Set.diff
  end
end

module Token = struct
  module T = struct
    (* TODO: remove nesting? *)
    module T' = struct
      type t = Token_tag.t * string * Lvca_provenance.Opt_range.t
      [@@deriving compare, sexp]
    end

    include T'
    include Comparable.Make (T')

    let pp ppf (tag, str, _) = Fmt.pf ppf "(%a, %S, _)" Token_tag.pp tag str
  end

  include T

  let range (_, _, x) = x

  type tag = Token_tag.t

  let tag (tag, _, _) = tag

  module Tag = Token_tag
  module Set = Token_set (Tag)

  type set = Set.t
end

module Stream = struct
  open Stdlib

  type t = Token.t Stream.t
  type element = Token.t

  let peek, junk = Stream.(peek, junk)
end

type token = Token.t
type token_tag = Token_tag.t
type token_set = Token.set
type stream = Stream.t
