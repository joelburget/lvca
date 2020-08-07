type 'a located = 'a * Range.t option
type 'a t = 'a located Angstrom.t

val char : char -> char t
(** [char c] accepts [c] and returns it. *)

val string : string -> string t
(** [string s] accepts [s] exactly and returns it. *)

val take_while : (char -> bool) -> string t
(** [take_while f] accepts input as long as [f] returns [true] and returns the
    accepted characters as a string.
    This parser does not fail. If [f] returns [false] on the first character,
    it will return the empty string. *)

val take_while1 : (char -> bool) -> string t
(** [take_while1 f] accepts input as long as [f] returns [true] and returns the
    accepted characters as a string.
    This parser requires that [f] return [true] for at least one character of
    input, and will fail otherwise. *)

val satisfy : (char -> bool) -> char t
(** [satisfy f] accepts any character for which [f] returns [true] and
    returns the accepted character. In the case that none of the parser
    succeeds, then the parser will fail indicating the offending
    character. *)

val option : 'a t -> 'a option t
(** [option p] runs [p], returning [Some v] (where [v] is the result of [p]) if it
    succeeds, or [None] if it fails *)

val count : int -> 'a t -> 'a list t
(** [count n p] runs [p] [n] times, returning a list of the results. *)

val many : 'a t -> 'a list t
(** [many p] runs [p] {i zero} or more times and returns a list of results from
    the runs of [p]. *)

val many1 : 'a t -> 'a list t
(** [many1 p] runs [p] {i one} or more times and returns a list of results from
    the runs of [p]. *)

val fix : ('a t -> 'a t) -> 'a t
(** [fix f] computes the fixpoint of [f] and runs the resultant parser. The
    argument that [f] receives is the result of [fix f], which [f] must use,
    paradoxically, to define [fix f].
    [fix] is useful when constructing parsers for inductively-defined types
    such as sequences, trees, etc. *)

val (<|>) : 'a t -> 'a t -> 'a t
(** [p <|> q] runs [p] and returns the result if succeeds. If [p] fails, then
    the input will be reset and [q] will run instead. *)

val (<?>) : 'a t -> string -> 'a t
(** [p <?> name] associates [name] with the parser [p], which will be reported
    in the case of failure. *)

val return : 'a located -> 'a t
(** [return v] creates a parser that will always succeed and return [v] *)

val fail : string -> _ t
(** [fail msg] creates a parser that will always fail with the message [msg] *)

val (>>=) : 'a t -> ('a located -> 'b t) -> 'b t
(** [p >>= f] creates a parser that will run [p], pass its result to [f], run
    the parser that [f] produces, and return its result. *)

val lift : ('a located -> 'b located) -> 'a t -> 'b t
val lift2 : ('a located -> 'b located -> 'c located) -> 'a t -> 'b t -> 'c t
val lift3 : ('a located -> 'b located -> 'c located -> 'd located) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 : ('a located -> 'b located -> 'c located -> 'd located -> 'e located) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

val var : string t -> Range.t Binding.Nominal.term t
val primitive : Primitive.t t -> Range.t Binding.Nominal.term t
val operator
  : string located
  -> Range.t Binding.Nominal.scope list
  -> Range.t Binding.Nominal.term t
