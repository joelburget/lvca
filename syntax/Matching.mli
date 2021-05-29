(* TODO: add heuristics *)
(** Pattern matching. See Maranget's "Compiling pattern matching to good decision trees". *)
open Lvca_util

(** The cases in a pattern match (matching one term). *)
type ('info, 'rhs) cases = ('info Pattern.t * 'rhs) list

(** The terms bound by a pattern match. *)
type 'info env = 'info NonBinding.term String.Map.t

(** An entry in a pattern matching matrix (matching multiple terms simultaneously). *)
type 'info matrix_entry =
  { term_no : int
  ; path : Path.t
  ; pattern : 'info Pattern.t
  }

(** A row in a pattern matching matrix (matching multiple terms simultaneously). *)
type ('info, 'rhs) matrix_row = 'info matrix_entry list * 'rhs

(** A pattern matching matrix (matching multiple terms simultaneously). *)
type ('info, 'rhs) matrix = ('info, 'rhs) matrix_row list

type binding_instruction =
  { term_no : int
  ; name : string
  ; path : Path.t
  }

val pp_instruction : binding_instruction Fmt.t

type ('info, 'rhs) decision_tree =
  | OperatorCases of
      ('info, 'rhs) decision_tree String.Map.t * ('info, 'rhs) decision_tree option
  | PrimCases of ('info Primitive.t option * ('info, 'rhs) decision_tree) list
  | Matched of binding_instruction list * 'rhs
  | Swap of int * ('info, 'rhs) decision_tree

val pp_tree : _ decision_tree Fmt.t

type 'info match_compilation_error =
  | BadSort of 'info Pattern.t * 'info Sort.t * 'info Sort.t
  | RedundantPattern of 'info Pattern.t
  | NonExhaustive of unit Pattern.t list
  | DuplicateName of 'info Pattern.t * string

(** Match a term against a pattern, extracting bindings *)
val match_pattern : 'info NonBinding.term -> 'info Pattern.t -> 'info env option

(** Match a term against an ordered set of patterns, producing a branch and bindings if
    there is a match *)
val simple_find_match
  :  'info NonBinding.term
  -> ('info, 'rhs) cases
  -> ('rhs * 'info env) option

(** Compile an ordered set of patterns to a [decision_tree] that can be evaluated
    efficiently. This has the side-effect of checking the cases for well-formedness,
    including coverage and redundancy. *)

val compile_cases
  :  'info AbstractSyntax.Unordered.t
  -> 'info Sort.t
  -> ('info, 'rhs) cases
  -> (('info, 'rhs) decision_tree, 'info match_compilation_error) Result.t

(** Compile an ordered matrix of patterns to a [decision_tree] that can be evaluated
    efficiently. This has the side-effect of checking the cases for well-formedness,
    including coverage and redundancy. *)
val compile_matrix
  :  'info AbstractSyntax.Unordered.t
  -> 'info Sort.t list
  -> ('info, 'rhs) matrix
  -> (('info, 'rhs) decision_tree, 'info match_compilation_error) Result.t

(** Match a term against a [decision_tree] (a compiled list of cases), resulting in a
    branch and an environment if there is a match. *)
val run_match
  :  'info NonBinding.term
  -> ('info, 'rhs) decision_tree
  -> ('rhs * 'info env) option

val run_matches
  :  'info NonBinding.term list
  -> ('info, 'rhs) decision_tree
  -> ('rhs * 'info env) option

(** Check a matrix for coverage, possibly returning an example pattern that's not matched. *)
val check_matrix
  :  'info AbstractSyntax.Unordered.t
  -> 'info Sort.t list
  -> ('info, 'rhs) matrix
  -> unit Pattern.t list option

(** {1 Parsing} *)
module Parse : sig
  type 'info matrix_row = 'info matrix_entry list * 'info NonBinding.term

  val branch
    : (Lvca_provenance.OptRange.t Pattern.t * Lvca_provenance.OptRange.t NonBinding.term)
      Lvca_parsing.t

  val branches
    : (Lvca_provenance.OptRange.t Pattern.t * Lvca_provenance.OptRange.t NonBinding.term)
      list
      Lvca_parsing.t

  val matrix_row : Lvca_provenance.OptRange.t matrix_row Lvca_parsing.t
  val matrix_rows : Lvca_provenance.OptRange.t matrix_row list Lvca_parsing.t
end

module Properties : sig
  type term = unit NonBinding.term

  (** Check that [run_match . compile_cases] gives the same result as [simple_find_match] *)
  val match_equivalent : term -> (unit, term) cases -> PropertyResult.t

  (* TODO: could also check that check_matrix finds an example if compile_matrix does. *)
end
