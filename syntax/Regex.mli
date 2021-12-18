module Class_base : sig
  type t =
    | Word (** \w / \W *)
    | Whitespace (** \s / \S *)
    | Digit (** \d / \D *)
    | Boundary (** \b / \B *)

  (* val to_re : t -> Re.t *)
  val ( = ) : t -> t -> bool
  val to_predicate : t -> char -> bool
end

module Class : sig
  (** Accept the positive or negative version of a class *)
  type t =
    | Pos of Class_base.t
    | Neg of Class_base.t

  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
  val parse : t Angstrom.t
  val to_predicate : t -> char -> bool
end

module Set_member : sig
  type t =
    | Single_char of char
    | Range of char * char

  val ( = ) : t -> t -> bool
  val debug_pp : t Fmt.t
  val pp : t Fmt.t
  val parse : t Angstrom.t
  val to_angstrom : t -> char Angstrom.t
end

module Set : sig
  type t = Set_member.t list

  val debug_pp : t Fmt.t
  val pp : t Fmt.t
  val parse : t Angstrom.t
  val to_angstrom : t -> char Angstrom.t
end

(** A regular expression used for lexical analysis. *)
type t =
  | Char of char (** Just a character, eg 'a' *)
  | Class of Class.t
      (** A character class, eg [\w] or [\d]. Syntactically, these are all started by a
          backslash. We just use javascript character classes. *)
  | Set of Set.t (** A character set, eg [\[a-z\]] or [\[^abc\]] *)
  | Star of t (** Zero-or-more repetition, eg [(ab)*] *)
  | Plus of t (** One-or-more repetition, eg [(ab)+] *)
  | Count of t * int
      (** A specific number of repetitions, eg [(ab){5}]. Must be greater than 0. *)
  | Option of t (** Option, eg [(ab)?] *)
  | Choice of t list (** Choice, eg [a|b] *)
  | Any (** Any character *)
  | Concat of t list

val to_re : t -> Re.t
val to_angstrom : t -> string Angstrom.t
val accepts_empty : t -> bool
val is_literal : t -> string option
val to_nonbinding : t -> Nonbinding.t
val debug_pp : ?need_parens:bool -> t Fmt.t
val pp : t Fmt.t
val parse : t Angstrom.t

(** Common classes *)
module Classes : sig
  (** [a-z] *)
  val lower_alpha : t

  (** [a-zA-Z] *)
  val alpha : t

  (** [a-zA-Z0-9] *)
  val words : t

  (** [a-zA-Z0-9_] *)
  val underscore_words : t
end
