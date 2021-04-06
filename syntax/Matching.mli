(** Pattern matching. See Maranget's "Compiling pattern matching to good decision trees". *)

(* TODO: add heuristics *)

(** The cases in a pattern match (matching one term). *)
type ('info, 'prim, 'rhs) cases = (('info, 'prim) Pattern.t * 'rhs) list

(** The terms bound by a pattern match. *)
type ('info, 'prim) env = ('info, 'prim) NonBinding.term Lvca_util.String.Map.t

(** An entry in a pattern matching matrix (matching multiple terms simultaneously). *)
type ('info, 'prim) matrix_entry =
  { term_no : int
  ; path : Path.t
  ; pattern : ('info, 'prim) Pattern.t
  }

(** A row in a pattern matching matrix (matching multiple terms simultaneously). *)
type ('info, 'prim, 'rhs) matrix_row = ('info, 'prim) matrix_entry list * 'rhs

(** A pattern matching matrix (matching multiple terms simultaneously). *)
type ('info, 'prim, 'rhs) matrix = ('info, 'prim, 'rhs) matrix_row list

type binding_instruction =
  { term_no : int
  ; name : string
  ; path : Path.t
  }

val pp_instruction : binding_instruction Fmt.t

type ('info, 'prim, 'rhs) decision_tree =
  | OperatorCases of
      ('info, 'prim, 'rhs) decision_tree Lvca_util.String.Map.t
      * ('info, 'prim, 'rhs) decision_tree option
  | PrimCases of ('prim option * ('info, 'prim, 'rhs) decision_tree) list
  | Matched of binding_instruction list * 'rhs
  | Swap of int * ('info, 'prim, 'rhs) decision_tree

val pp_tree : _ decision_tree Fmt.t

type ('info, 'prim) match_compilation_error =
  | BadSort of ('info, 'prim) Pattern.t * 'info Sort.t * 'info Sort.t
  | RedundantPattern of ('info, 'prim) Pattern.t
  | NonExhaustive of (unit, 'prim) Pattern.t list
  | DuplicateName of ('info, 'prim) Pattern.t * string

(** Match a term against a pattern, extracting bindings *)
val match_pattern
  :  prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) NonBinding.term
  -> ('info, 'prim) Pattern.t
  -> ('info, 'prim) env option

(** Match a term against an ordered set of patterns, producing a branch and bindings if
    there is a match *)
val simple_find_match
  :  prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) NonBinding.term
  -> ('info, 'prim, 'rhs) cases
  -> ('rhs * ('info, 'prim) env) option

(** Compile an ordered set of patterns to a [decision_tree] that can be evaluated
    efficiently. This has the side-effect of checking the cases for well-formedness,
    including coverage and redundancy. *)

val compile_cases
  :  'info AbstractSyntax.Unordered.t
  -> 'info Sort.t
  -> ('info, 'prim, 'rhs) cases
  -> (('info, 'prim, 'rhs) decision_tree, ('info, 'prim) match_compilation_error) Result.t

(** Compile an ordered matrix of patterns to a [decision_tree] that can be evaluated
    efficiently. This has the side-effect of checking the cases for well-formedness,
    including coverage and redundancy. *)
val compile_matrix
  :  'info AbstractSyntax.Unordered.t
  -> 'info Sort.t list
  -> ('info, 'prim, 'rhs) matrix
  -> (('info, 'prim, 'rhs) decision_tree, ('info, 'prim) match_compilation_error) Result.t

(** Match a term against a [decision_tree] (a compiled list of cases), resulting in a
    branch and an environment if there is a match. *)
val run_match
  :  prim_pp:'prim Fmt.t
  -> prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) NonBinding.term
  -> ('info, 'prim, 'rhs) decision_tree
  -> ('rhs * ('info, 'prim) env) option

(** Check a matrix for coverage, possibly returning an example pattern that's not matched. *)
val check_matrix
  :  'info AbstractSyntax.Unordered.t
  -> 'info Sort.t list
  -> ('info, 'prim, 'rhs) matrix
  -> (unit, 'prim) Pattern.t list option

module Properties : sig
  type term = (unit, unit Primitive.t) NonBinding.term

  (** Check that [run_match . compile_cases] gives the same result as [simple_find_match] *)
  val match_equivalent : term -> (unit, unit Primitive.t, term) cases -> PropertyResult.t

  (* TODO: could also check that check_matrix finds an example if compile_matrix does. *)
end
