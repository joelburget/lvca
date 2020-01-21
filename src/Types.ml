(** Types for representing languages *)

type sort_name = string

(** Sorts divide ASTs into syntactic categories.
 *
 * Notes about our representation:
 * - Concrete sorts are always represented by a [SortAp], even if not applied
 *   to anything. For example, [integer] is represented as [SortAp ("integer",
 *   [||])].
 * - We don't allow higher-order sorts. In other words, no functions at the
 *   sort level. In other words, the head of an application is always concrete.
 *)
type sort =
  (** A higher-kinded sort can be applied *)
  | SortAp of sort_name * sort array
  | SortVar of string

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

type import =
  { imported_symbols: (string * string) list;
    location: string;
  }

type abstract_syntax =
  { imports: import list;
    language: language;
  }

let string_of_primitive = function
  | PrimInteger i  -> Bigint.to_string i
  | PrimString str -> "\"" ^ String.escaped str ^ "\""

let prim_eq p1 p2 = match (p1, p2) with
  | PrimInteger i1, PrimInteger i2 -> Bigint.(i1 = i2) [@warning "-44"]
  | PrimString  s1, PrimString  s2 -> s1 = s2
  | _                              -> false

let sort_names : abstract_syntax -> Belt.Set.String.t
  = fun { language = Language sorts } -> sorts
  |. Belt.Map.String.keysToArray
  |. Belt.Set.String.fromArray

let string_of_sort : sort -> string
  = let rec go = fun needs_parens -> function
      | SortAp (name, args) ->
        let args' = Belt.Array.map args (go true) in
        (match args' with
          | [||] -> name
          | _ ->
            let pre_result = name ^ " " ^ Js.Array2.joinWith args' " " in
            if needs_parens then "(" ^ pre_result ^ ")" else pre_result)
      | SortVar name -> name
    in go false
;;

let rec instantiate_sort : sort Belt.Map.String.t -> sort -> sort
  = fun arg_mapping -> function
    | SortVar name -> (match Belt.Map.String.get arg_mapping name with
      | None -> failwith "TODO: error"
      | Some sort' -> sort')
    | SortAp (name, args) ->
      SortAp (name, Belt.Array.map args (instantiate_sort arg_mapping))
