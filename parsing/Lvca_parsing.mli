open Lvca_provenance

module ParseResult : sig
  type 'a t =
    { latest_pos : int
          (** The first position *after* the last non-whitespace character read. This may
              be used as the endpoint (non-inclusive) of the range for enclosing parsers *)
    ; value : 'a
    ; range : Lvca_provenance.OptRange.t
    }

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val pp : 'a Fmt.t -> 'a t Fmt.t
end

type +'a t = latest_pos:int -> 'a ParseResult.t Angstrom.t

val debug : bool ref
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>== ) : 'a t -> ('a ParseResult.t -> 'b t) -> 'b t
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
val integer_or_float_lit : (string, float) Base.Either.t t
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

(** [sep_end_by sep p]: Parse zero or more occurences of [p], separated and optionally
    ended by [sep]. *)
val sep_end_by : _ t -> 'a t -> 'a list t

val parens : 'a t -> 'a t
val braces : 'a t -> 'a t
val brackets : 'a t -> 'a t
val fail : string -> 'a t
val whitespace : unit t
val whitespace1 : unit t
val parse_string_pos : 'a t -> string -> ('a ParseResult.t, string) Base.Result.t
val parse_string : 'a t -> string -> ('a, string) Base.Result.t
