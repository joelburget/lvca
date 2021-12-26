open Lvca_provenance

module Parse_result : sig
  type 'a t =
    { value : 'a
    ; range : Opt_range.t
    }

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val pp : 'a Fmt.t -> 'a t Fmt.t
end

type +'a t = 'a Parse_result.t Angstrom.t

val of_angstrom : 'a Angstrom.t -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
val ( *> ) : 'a t -> 'b t -> 'b t
val ( <* ) : 'a t -> 'b t -> 'a t
val ( <|> ) : 'a t -> 'a t -> 'a t
val ( <?> ) : 'a t -> string -> 'a t
val choice : ?failure_msg:string -> 'a t list -> 'a t
val fix : ('a t -> 'a t) -> 'a t
val option : 'a -> 'a t -> 'a t
val option' : 'a t -> 'a option t
val return : ?range:Opt_range.t -> 'a -> 'a t
val attach_pos' : 'a t -> (Opt_range.t * 'a) t (* TODO: remove? *)

val count : int -> 'a t -> 'a list t
val lift : ('a -> 'b) -> 'a t -> 'b t
val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
val make0 : (info:Opt_range.t -> 'b) -> _ t -> 'b t
val make1 : (info:Opt_range.t -> 'a -> 'b) -> 'a t -> 'b t
val make2 : (info:Opt_range.t -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val make3 : (info:Opt_range.t -> 'a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

val make4
  :  (info:Opt_range.t -> 'a -> 'b -> 'c -> 'd -> 'e)
  -> 'a t
  -> 'b t
  -> 'c t
  -> 'd t
  -> 'e t

val make5
  :  (info:Opt_range.t -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
  -> 'a t
  -> 'b t
  -> 'c t
  -> 'd t
  -> 'e t
  -> 'f t

val make6
  :  (info:Opt_range.t -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
  -> 'a t
  -> 'b t
  -> 'c t
  -> 'd t
  -> 'e t
  -> 'f t
  -> 'g t

val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t
val sep_by : _ t -> 'a t -> 'a list t
val sep_by1 : _ t -> 'a t -> 'a list t

(** [sep_end_by sep p]: Parse zero or more occurences of [p], separated and optionally
    ended by [sep]. *)
val sep_end_by : _ t -> 'a t -> 'a list t

(** [sep_end_by sep p]: Parse one or more occurences of [p], separated and optionally
    ended by [sep]. *)
val sep_end_by1 : _ t -> 'a t -> 'a list t

val fail : string -> 'a t
val whitespace : unit t
val whitespace1 : unit t
val no_comment : string t
val c_comment : string t

module type Junk_parser = sig
  val junk : unit t
  val junk1 : unit t
end

module type Character_parser = sig
  val junk : unit t
  val junk1 : unit t
  val char_lit : char t

  val identifier'
    :  is_start:(char -> bool)
    -> ?is_continue:(char -> bool)
    -> Lvca_util.String.Set.t
    -> string t

  val upper_identifier : Lvca_util.String.Set.t -> string t
  val lower_identifier : Lvca_util.String.Set.t -> string t
  val integer_lit : string t
  val integer_or_float_lit : (string, float) Base.Either.t t
  val string_lit : string t
  val satisfy : (char -> bool) -> char t
  val char : char -> char t
  val string : string -> string t

  (** The same as a string parser, but requires trailing whitespace or a comment. *)
  val keyword : string -> string t

  val parens : 'a t -> 'a t
  val braces : 'a t -> 'a t
  val brackets : 'a t -> 'a t
end

module Mk_character_parser (Junk : Junk_parser) : Character_parser
module No_junk : Character_parser
module C_comment_parser : Character_parser
module Whitespace_parser : Character_parser

val parse_string_pos : 'a t -> string -> ('a Parse_result.t, string) Base.Result.t
val parse_string : 'a t -> string -> ('a, string) Base.Result.t
val parse_string_or_failwith : 'a t -> string -> 'a

module Let_syntax : sig
  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
  val map4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t
end

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
