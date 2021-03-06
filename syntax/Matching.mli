(** Pattern matching. See Maranget's "Compiling pattern matching to good decision trees". *)

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

type ('info, 'prim, 'rhs) decision_tree =
  | OperatorCases of
      ('info, 'prim, 'rhs) decision_tree Lvca_util.String.Map.t
      * ('info, 'prim, 'rhs) decision_tree option
  | PrimCases of ('prim option * ('info, 'prim, 'rhs) decision_tree) list
  | Matched of binding_instruction list * 'rhs
  | Swap of int * ('info, 'prim, 'rhs) decision_tree

type ('info, 'prim) match_compilation_error =
  | BadSort of ('info, 'prim) Pattern.t * 'info Sort.t * 'info Sort.t
  | RedundantPattern of ('info, 'prim) Pattern.t
  | NonExhaustive of ('info, 'prim) Pattern.t
  | DuplicateName of ('info, 'prim) Pattern.t * string

(** Match a term against a pattern, extracting bindings *)
val match_pattern
  :  prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) NonBinding.term
  -> ('info, 'prim) Pattern.t
  -> ('info, 'prim) env option

(** Match a term against an ordered set of patterns, producing a branch and bindings if
    there is a match *)
val simple_match
  :  prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) NonBinding.term
  -> ('info, 'prim, 'rhs) cases
  -> ('rhs * ('info, 'prim) env) option

(** Compile an ordered set of patterns to a [decision_tree] that can be evaluated
    efficiently. This has the side-effect of checking the cases for well-formedness,
    including coverage and redundancy. *)

val compile_cases
  :  'info AbstractSyntax.unordered
  -> 'info Sort.t
  -> ('info, 'prim, 'rhs) cases
  -> (('info, 'prim, 'rhs) decision_tree, ('info, 'prim) match_compilation_error) Result.t

(** Compile an ordered matrix of patterns to a [decision_tree] that can be evaluated
    efficiently. This has the side-effect of checking the cases for well-formedness,
    including coverage and redundancy. *)
val compile_matrix
  :  'info AbstractSyntax.unordered
  -> 'info Sort.t list
  -> ('info, 'prim, 'rhs) matrix
  -> ('info, 'prim, 'rhs) decision_tree

(** Match a term against a [decision_tree] (a compiled list of cases), resulting in a
    branch and an environment if there is a match. *)
val run_match
  :  prim_pp:'prim Fmt.t
  -> prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) NonBinding.term
  -> ('info, 'prim, 'rhs) decision_tree
  -> ('rhs * ('info, 'prim) env) option

(* val check_coverage *)

module Properties : sig
  type term = (unit, Primitive.t) NonBinding.term

  val match_equivalent : term -> (unit, Primitive.t, term) cases -> PropertyResult.t
end
