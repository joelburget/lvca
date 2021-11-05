(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [scope], [case_scope], (and
      [Binding_aware_pattern.t.t]).
    - [eval] is used to evaluate a core term *)

open Lvca_syntax
open Lvca_util

(** {1 Types} *)

module List_model : sig
  include [%lvca.abstract_syntax_module_sig "list a := Nil() | Cons(a; list a)"]

  val of_list : 'a list -> 'a List.t
  val to_list : 'a List.t -> 'a list
  val map : f:('a -> 'b) -> 'a List.t -> 'b List.t
end

module Option_model : sig
  include [%lvca.abstract_syntax_module_sig "option a := None() | Some(a)"]

  val of_option : 'a option -> 'a Option.t
  val to_option : 'a Option.t -> 'a option
  val map : f:('a -> 'b) -> 'a Option.t -> 'b Option.t
end

module Empty : sig
  include [%lvca.abstract_syntax_module_sig "empty :="]

  type t = Empty.t

  val pp : t Fmt.t
  val parse : t Lvca_parsing.t
end

module Binding_aware_pattern_model : [%lvca.abstract_syntax_module_sig
{|
string : *
primitive : *
list : * -> *

pattern :=
  | Operator(string; list scope)
  | Primitive(primitive)
  | Var(string)

scope := Scope(list string; pattern)
|}
, { string = "Primitive.String"; primitive = "Primitive.All"; list = "List_model.List" }]

module Sort_model : sig
  include
    [%lvca.abstract_syntax_module_sig
  {|
string : *

sort :=
  | Ap(string; ap_list)
  | Name(string)

ap_list :=
  | Nil()
  | Cons(sort; ap_list)
|}
  , { string = "Primitive.String" }]
end

module Sort : sig
  (* include module type of Sort_model.Sort *)

  val into : Lvca_syntax.Sort.t -> Sort_model.Sort.t
  val out : Sort_model.Sort.t -> Lvca_syntax.Sort.t
end

module Type : sig
  include
    [%lvca.abstract_syntax_module_sig
  {|
sort : *

ty :=
  | Sort(sort)
  | Arrow(ty; ty)
  | Forall((list empty)[ty]. ty)
  |}
  , { sort = "Sort_model.Sort"; empty = "Empty" }]

  type t = Ty.t

  val pp : t Fmt.t
  val parse : t Lvca_parsing.t
end

module Lang : [%lvca.abstract_syntax_module_sig
{|
nominal : *
list : * -> *
option : * -> *
binding_aware_pattern : * -> *
string : *

letrec_row := Letrec_row(ty; term)

term :=
  | Quoted(nominal)
  | Primitive(primitive)
  | Constructor(string; list term)
  | Ap(term; list term)
  | Case(term; list case_scope)
  | Lambda(ty; term. term)
  | Let(term; option ty; term. term)
  | Let_rec(list letrec_row; (list empty)[term]. term)
  | Subst(term. term; term)

case_scope := Case_scope(binding_aware_pattern; term)
|}
, { ty = "Type"
  ; nominal = "Nominal.Term"
  ; list = "List_model.List"
  ; option = "Option_model.Option"
  ; binding_aware_pattern = "Binding_aware_pattern_model.Pattern"
  ; primitive = "Primitive.All"
  ; string = "Primitive.String"
  ; empty = "Empty"
  }]

module Term : sig
  include Nominal.Convertible.Extended_s with type t = Lang.Term.t

  val parse_concrete : t Lvca_parsing.t
  val pp_concrete : t Fmt.t
end

module Parse : sig
  val term : Lang.Term.t Lvca_parsing.t
end

(** {1 Core type} *)
module Module : sig
  type t =
    { externals : (string * Type.t) list
    ; defs : (string * Lang.Term.t) list
    }

  val pp : t Fmt.t
  val parse : t Lvca_parsing.t
end

(** {1 Checking} *)

type type_env = Type.t String.Map.t

type check_env =
  { type_env : type_env
  ; syntax : Abstract_syntax.t
  }

module Check_error' : sig
  type t =
    | Cant_infer_case
    | Cant_infer_lambda
    | Var_not_found
    | Operator_not_found
    | Mismatch of Type.t * Type.t
    | Binding_pattern_check of string
    | Overapplication
end

module Check_error : sig
  type t =
    { env : check_env
    ; tm : Lang.Term.t
    ; ty : Type.t
    ; error : Check_error'.t
    }
end

module Infer_error : sig
  type t =
    { env : check_env
    ; tm : Lang.Term.t
    ; error : Check_error'.t
    }
end

(** Typecheck a term in an environment. *)
val infer : check_env -> Lang.Term.t -> (Type.t, Infer_error.t) Result.t

val check : check_env -> Lang.Term.t -> Type.t -> Check_error.t option

(** {1 Evaluation} *)

type eval_env = Lang.Term.t String.Map.t
type eval_error = string * Lang.Term.t

val eval_in_ctx : eval_env -> Lang.Term.t -> (Nominal.Term.t, eval_error) Base.Result.t
val eval : Lang.Term.t -> (Nominal.Term.t, eval_error) Base.Result.t

(** {1 Patterns} *)
val match_pattern
  :  Nominal.Term.t
  -> Binding_aware_pattern.t
  -> Nominal.Term.t String.Map.t option

val find_match
  :  Nominal.Term.t
  -> Lang.Case_scope.t list
  -> (Lang.Term.t * eval_env) option

(* val coverage_check :  Cases.t -> *)
val preimage : Lang.Case_scope.t list -> Binding_aware_pattern.t list
val reverse : Nominal.Term.t -> Lang.Case_scope.t list -> eval_env option
