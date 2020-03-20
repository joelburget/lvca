open Core_kernel

(** Types for representing languages *)

type sort_name = string

(** Sorts divide ASTs into syntactic categories. * * Notes about our representation: * -
    Concrete sorts are always represented by a [SortAp], even if not applied * to
    anything. For example, [integer] is represented as [SortAp ("integer", * \[||\])]. *
    \- We don't allow higher-order sorts. In other words, no functions at the * sort
    level. In other words, the head of an application is always concrete. *)
type sort =
  | SortAp of sort_name * sort array (** A higher-kinded sort can be applied *)
  | SortVar of string

(** A valence represents the sort of an argument (to an operator), as well as * the number
    and sorts of the variables bound within it *)
type valence =
  | FixedValence of sort list * sort (** A fixed valence is known a priori *)
  | VariableValence of sort * sort
      (** A variable valence binds a number of variables not known a priori. All must be
          of the same sort. *)

(** An arity specifies the arguments to an operator *)
type arity = Arity of string list * valence list
  (** An arity is defined its arity indices and valences. Arity indices are variables
      bound in an arity rule specifying the length of variable-length slots. *)

type operator_def = OperatorDef of string * arity
  (** An operator is defined by its tag and arity *)

type sort_def = SortDef of string list * operator_def list
  (** A sort is defined by a set of variables and a set of operators *)

type sort_defs = SortDefs of sort_def String.Map.t
  (** A language is defined by its sorts *)

type primitive =
  | PrimInteger of Bigint.t
  | PrimString of string

type import =
  { imported_symbols : (string * string) list
  ; location : string
  }

type abstract_syntax =
  { imports : import list
  ; sort_defs : sort_defs
  }

let sort_defs_eq (SortDefs x) (SortDefs y) = String.Map.equal Caml.(=) x y

let eq : abstract_syntax -> abstract_syntax -> bool
  = fun x y -> Caml.(x.imports = y.imports) &&
    sort_defs_eq x.sort_defs y.sort_defs

let string_of_primitive = function
  | PrimInteger i -> Bigint.to_string i
  | PrimString str -> "\"" ^ Caml.String.escaped str ^ "\""
;;

let prim_eq p1 p2 =
  match p1, p2 with
  | PrimInteger i1, PrimInteger i2 -> Bigint.(i1 = i2) [@warning "-44"]
  | PrimString s1, PrimString s2 -> String.(s1 = s2)
  | _ -> false
;;

let sort_names : abstract_syntax -> String.Set.t =
 fun { sort_defs = SortDefs sorts; _ } -> sorts |> String.Map.keys |> String.Set.of_list
;;

let rec string_of_sort : sort -> string
  = function
    | SortAp (name, args) -> Printf.sprintf "%s(%s)"
      name
      (args |> Array.map ~f:string_of_sort |> String.concat_array)
    | SortVar name -> name
;;

let rec instantiate_sort : sort String.Map.t -> sort -> sort =
 fun arg_mapping -> function
  | SortVar name ->
    (match String.Map.find arg_mapping name with
    | None -> failwith "TODO: error"
    | Some sort' -> sort')
  | SortAp (name, args) -> SortAp (name, Array.map args ~f:(instantiate_sort arg_mapping))
;;
