open Base

module Literal : sig
  type t =
    | Double_quoted
    | Single_quoted
    | Integer
    | Floating
    | Regex

  val pp : t Fmt.t

  include Sexpable.S with type t := t
  include Comparable.S with type t := t
end

module Token_tag : sig
  type t =
    | Symbol of string
    | Comment
    | Lower_ident
    | Upper_ident
    | Literal of Literal.t
    | Keyword of string
    | Whitespace

  val pp : t Fmt.t

  include Sexpable.S with type t := t
  include Comparable.S with type t := t
end

type token_set = (Token_tag.t, Token_tag.comparator_witness) Set.t

module Token : sig
  include
    Taparse.Signatures.Token
      with type t = Token_tag.t * string * Lvca_provenance.Opt_range.t
       and type tag = Token_tag.t
       and type set = token_set

  val pp : t Fmt.t
  val range : t -> Lvca_provenance.Opt_range.t
end

module Stream :
  Taparse.Signatures.Stream
    with type t = Token.t Stdlib.Stream.t
     and type element = Token.t

type token = Token.t
type token_tag = Token_tag.t
type stream = Stream.t
