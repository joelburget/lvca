open Base
open Lvca_util

(** Operators have left, right, or no fixity. *)
module Fixity : sig
  type t =
    | Left
    | None
    | Right

  val pp : t Fmt.t
end

(** An operator has a name and a fixity. *)
module Operator_fixity : sig
  type t = Provenance.t * Fixity.t * string

  val info : t -> Provenance.t
  val parse : t Lvca_parsing.t
  val pp : t Fmt.t
end

(** Ranking of operator precedence. We rank by level, with all operators within a level
    having equal precedence, and earlier levels with higher precedence than later levels. *)
module Operator_ranking : sig
  type t = Provenance.t * Operator_fixity.t list list

  val info : t -> Provenance.t
  val parse : t Lvca_parsing.t
  val pp : t Fmt.t
  val check : t -> string option
end

(** A concrete syntax sequence item is either a variable, literal, or space. *)
module Sequence_item : sig
  type t =
    | Var of Provenance.t * string
    | Literal of Provenance.t * string
    | Space of Provenance.t

  val info : t -> Provenance.t
  val vars : t -> string list
  val parse : t Lvca_parsing.t
  val pp : t Fmt.t
end

(** A sequence of items corresponding to one way to parse / print an operator. *)
module Operator_concrete_syntax_row : sig
  type t = Provenance.t * Sequence_item.t list

  val info : t -> Provenance.t
  val vars : t -> string list
  val parse : t Lvca_parsing.t
  val pp : t Fmt.t

  val is_binary_operator
    :  operator_names:Lvca_util.String.Set.t
    -> t
    -> (string * string * string) option
end

(** The regex for parsing a variable name. *)
module Variable_syntax_row : sig
  type t =
    { info : Provenance.t
    ; var_name : string
    ; re : Regex.t
    }

  val info : t -> Provenance.t
  val parse : t Lvca_parsing.t
  val pp : t Fmt.t
end

(** Part of a pattern representing a subterm with possible binders (what appears between
    semicolons in a term). *)
module Operator_pattern_slot : sig
  type t =
    { info : Provenance.t
    ; variable_names : string list
    ; body_name : string
    }

  val vars : t -> string list
  val pp : t Fmt.t
  val parse : t Lvca_parsing.t
end

(** A pattern for an operator with subterms and binders. *)
module Operator_pattern : sig
  type t =
    { info : Provenance.t
    ; name : string
    ; slots : Operator_pattern_slot.t list
    }

  val vars : t -> string list
  val pp : t Fmt.t
  val parse : t Lvca_parsing.t
end

(** One way to parse, print, and check an operator. *)
module Operator_syntax_row : sig
  type t =
    { info : Provenance.t
    ; pattern : Operator_pattern.t
    ; concrete_syntax : Operator_concrete_syntax_row.t
    }

  val info : t -> Provenance.t
  val parse : t Lvca_parsing.t
  val pp : t Fmt.t

  val check
    :  sort_name:string
    -> sort_def:Sort_def.t
    -> operator_names:String.Set.t
    -> t
    -> string option
end

(** How to parse, print, and check a sort. *)
module Sort_syntax : sig
  type t =
    { info : Provenance.t
    ; name : Provenance.t * string
    ; operators : Operator_syntax_row.t list
    ; variables : Variable_syntax_row.t option
    ; operator_ranking : Operator_ranking.t option
    }

  type operator_infos = (int * Fixity.t) String.Map.t

  val parse : t Lvca_parsing.t
  val pp : t Fmt.t
  val check : sort_defs:Sort_def.t String.Map.t -> t -> string option
  val operator_infos : t -> operator_infos
  val operator_names : operator_infos -> String.Set.t
end

(** How to parse, print, and check a language. This is a list so we can print back a
    language in the same order it was parsed. See [Unordered.t] for another useful form. *)
type t = Sort_syntax.t list

val parse : t Lvca_parsing.t
val pp : t Fmt.t
val check : Sort_def.t String.Map.t -> t -> string option
val keywords : t -> String.Set.t

(** Find the sort of a variable in a pattern. *)
val find_var_sort
  :  Operator_pattern_slot.t list
  -> Valence.t list
  -> string
  -> Sort_slot.t

(** How to parse / print a language. Also see the ordered form [t]. *)
module Unordered : sig
  type t = Sort_syntax.t String.Map.t

  val build : Sort_syntax.t list -> [ `Duplicate_key of string | `Ok of t ]
  val keywords : t -> String.Set.t

  (** Pretty-print a term using this concrete syntax. *)
  val pp_term : t -> string -> Nominal.Term.t Fmt.t
end
