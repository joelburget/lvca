(** Types for representing languages *)

type sort_name = string

(** Sorts divide ASTs into syntactic categories. *)
type sort =
  (** A higher-kinded sort can be applied *)
  | SortAp of sort_name * sort array

(** A valence represents the sort of an argument (to an operator), as well as
 * the number and sorts of the variables bound within it *)
type valence =
  | FixedValence    of sort list * sort
  (** A fixed valence is known a priori *)
  | VariableValence of string * sort
  (** A variable valence binds a number of variables not known a priori. All
      must be of the same sort. *)

(** An arity specifies the arguments to an operator *)
type arity =
  | Arity of string list * valence list
  (** An arity is defined its arity indices and valences. Arity indices are
      variables bound in an arity rule specifying the length of variable-length
      slots. *)

type operatorDef =
  | OperatorDef of string * arity
  (** An operator is defined by its tag and arity *)

type sortDef =
  | SortDef of string list * operatorDef list
  (** A sort is defined by a set of variables and a set of operators *)

type language =
  | Language of sortDef Belt.Map.String.t
  (** A language is defined by its sorts *)

type primitive =
  | PrimInteger of Bigint.t
  | PrimString  of string

let string_of_primitive = function
  | PrimInteger i  -> Bigint.to_string i
  | PrimString str -> "\"" ^ String.escaped str ^ "\""

let prim_eq p1 p2 = match (p1, p2) with
  | PrimInteger i1, PrimInteger i2 -> Bigint.(i1 = i2) [@warning "-44"]
  | PrimString  s1, PrimString  s2 -> s1 = s2
  | _                              -> false

type import =
  { imported_symbols: (string * string) list;
    location: string;
  }

type abstract_syntax =
  { imports: import list;
    language: language;
  }

let sort_names : abstract_syntax -> Belt.Set.String.t
  = fun { language = Language sorts } -> sorts
  |. Belt.Map.String.keysToArray
  |. Belt.Set.String.fromArray
