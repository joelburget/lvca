open Base
open Lvca_provenance

module type ParseResult_s = sig
  type 'a t =
    { latest_pos : int
    ; value : 'a
    ; range : OptRange.t
    }

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module ParseResult : ParseResult_s = struct
  type 'a t =
    { latest_pos : int
    ; value : 'a
    ; range : OptRange.t
    }

  let equal a_eq r1 r2 =
    Int.(r1.latest_pos = r2.latest_pos)
    && a_eq r1.value r2.value
    && OptRange.(r1.range = r2.range)
  ;;
end

(** Interface for parsers that *don't* handle trailing whitespace. *)
module type Junkless_s = sig
  type +'a t = ('a * OptRange.t) Angstrom.t

  val ( >>== ) : 'a t -> (pos:OptRange.t -> 'a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>|| ) : 'a t -> (pos:OptRange.t -> 'a -> 'b * OptRange.t) -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val char_lit : char t
  val identifier : string t
  val integer_lit : string t
  val integer_or_float_lit : (string, float) Either.t t
  val string_lit : string t
  val pos : int t
  val option : 'a -> 'a t -> 'a t
  val return : ?pos:OptRange.t -> 'a -> 'a t
  val attach_pos : 'a t -> ('a * OptRange.t) t
  val satisfy : (char -> bool) -> char t
  val count : int -> 'a t -> 'a list t
  val lift : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val char : char -> char t
  val string : string -> string t
  val many : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t
  val sep_by : _ t -> 'a t -> 'a list t
  val sep_by1 : _ t -> 'a t -> 'a list t
  val sep_end_by : _ t -> 'a t -> 'a list t
  val parens : 'a t -> 'a t
  val braces : 'a t -> 'a t
  val brackets : 'a t -> 'a t
  val fail : string -> 'a t
  val ( *> ) : 'a Angstrom.t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b Angstrom.t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val choice : ?failure_msg:string -> 'a t list -> 'a t
  val ( <?> ) : 'a t -> string -> 'a t
  val fix : ('a t -> 'a t) -> 'a t
  val parse_string_pos : 'a t -> string -> ('a * OptRange.t, string) Base.Result.t
  val parse_string : 'a t -> string -> ('a, string) Base.Result.t
end

module type Parsers_s = sig
  module ParseResult = ParseResult

  type +'a t = latest_pos:int -> 'a ParseResult.t Angstrom.t

  val ( >>== ) : 'a t -> ('a ParseResult.t -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>|| ) : 'a t -> ('a ParseResult.t -> 'b ParseResult.t) -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <?> ) : 'a t -> string -> 'a t
  val choice : ?failure_msg:string -> 'a t list -> 'a t
  val fix : ('a t -> 'a t) -> 'a t
  val char_lit : char t
  val identifier : string t
  val integer_lit : string t
  val integer_or_float_lit : (string, float) Either.t t
  val string_lit : string t
  val pos : int t (* TODO: remove? *)

  val option : 'a -> 'a t -> 'a t
  val return : ?pos:OptRange.t -> 'a -> 'a t (* TODO: rename to range? *)

  val attach_pos : 'a t -> ('a * OptRange.t) t (* TODO: remove? *)

  val satisfy : (char -> bool) -> char t
  val count : int -> 'a t -> 'a list t
  val lift : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val char : char -> char t
  val string : string -> string t
  val many : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t
  val sep_by : _ t -> 'a t -> 'a list t
  val sep_by1 : _ t -> 'a t -> 'a list t
  val sep_end_by : _ t -> 'a t -> 'a list t
  val parens : 'a t -> 'a t
  val braces : 'a t -> 'a t
  val brackets : 'a t -> 'a t
  val fail : string -> 'a t
  val whitespace : unit t
  val whitespace1 : unit t
  val parse_string_pos : 'a t -> string -> ('a ParseResult.t, string) Base.Result.t
  val parse_string : 'a t -> string -> ('a, string) Base.Result.t
end
