(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [scope], [case_scope], (and
      [Binding_aware_pattern.t.t]).
    - [eval] is used to evaluate a core term *)

open Lvca_syntax
open Lvca_util

(** {1 Types} *)

module Type : sig
  (* TODO(24): remove *)
  module Kernel : sig
    val language : Abstract_syntax.t
  end

  (* TODO(24): generate *)
  type t =
    | Sort of Provenance.t * Sort_model.t
    | Arrow of Provenance.t * t * t

  include Nominal.Convertible.Extended_s with type t := t

  val mk_Sort : info:Lvca_syntax.Provenance.t -> Sort_model.t -> t
  val mk_Arrow : info:Lvca_syntax.Provenance.t -> t -> t -> t
end

module Term_syntax : [%lvca.abstract_syntax_module_sig
{|
ty : *
list : * -> *
option : * -> *
binding_aware_pattern : * -> *
pattern : *
primitive : *
string : *
empty : *

term :=
  | Primitive(primitive)
  | Operator(string; list operator_scope)
  | Ap(term; term)
  | Case(term; list case_scope)
  | Lambda(ty; term. term)
  | Let(term; option ty; term. term)
  // let rec defines a group (represented as a list but unordered) of definitions at once
  | Let_rec(list letrec_row; (list empty)[term]. term)
  | Subst(term. term; term)
  | Quote(term)
  | Unquote(term)

letrec_row := Letrec_row(ty; term)
operator_scope := Operator_scope(list pattern; term)
case_scope := Case_scope(binding_aware_pattern; term)
|}
, { ty = "Type"
  ; list = "List_model"
  ; option = "Option_model.Option"
  ; binding_aware_pattern = "Binding_aware_pattern"
  ; empty = "Empty"
  ; pattern = "Pattern_model.Pattern"
  ; primitive = "Primitive.All"
  ; string = "Primitive.String"
  }]

module Value_syntax : [%lvca.abstract_syntax_module_sig
{|
ty : *
list : * -> *
pattern : *
primitive : *
string : *

value :=
  | Primitive(primitive)
  | Operator(string; list operator_scope)
  | Lambda(ty; neutral. value)
  | Neutral(neutral)

neutral := Ap(neutral; value)
operator_scope := Operator_scope(list pattern; value)
|}
, { ty = "Type"
  ; list = "List_model"
  ; pattern = "Pattern_model.Pattern"
  ; primitive = "Primitive.All"
  ; string = "Primitive.String"
  }]

module Value : sig
  include Nominal.Convertible.Extended_s with type t = Value_syntax.Value.t

  val of_nominal' : Nominal.Term.t -> t
end

module Term : sig
  include Nominal.Convertible.Extended_s with type t = Term_syntax.Term.t

  val parse_concrete : t Lvca_parsing.t
  val pp_concrete : t Fmt.t
  val of_nominal' : Nominal.Term.t -> t
  val of_value : Value.t -> t
end

module Properties : sig
  val quote_unquote : Value.t -> Property_result.t
end

module Parse : sig
  val term : Term_syntax.Term.t Lvca_parsing.t
end

(** {1 Core type} *)
module Module : sig
  type t =
    { externals : (string * Type.t) list
    ; defs : (string * Term_syntax.Term.t) list
    }

  val pp : t Fmt.t
  val parse : t Lvca_parsing.t
end

(** {1 Evaluation} *)

type eval_env = Value.t String.Map.t
type eval_error = string * Term_syntax.Term.t

val eval_in_ctx : eval_env -> Term_syntax.Term.t -> (Value.t, eval_error) Base.Result.t
val eval : Term_syntax.Term.t -> (Value.t, eval_error) Base.Result.t

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
    | Message of string
    | Pattern_check_failure of Pattern.t Check_failure.t
    | Binding_pattern_check_failure of Binding_aware_pattern.t Check_failure.t
    | Eval_error of eval_error

  val pp : Term.t -> t Fmt.t
end

module Check_error : sig
  type t =
    { env : check_env
    ; tm : Term_syntax.Term.t
    ; ty : Type.t
    ; error : Check_error'.t
    }

  val pp : t Fmt.t
end

module Infer_error : sig
  type t =
    { env : check_env
    ; tm : Term_syntax.Term.t
    ; error : Check_error'.t
    }

  val pp : t Fmt.t
end

(** Typecheck a term in an environment. *)
val infer : check_env -> Term_syntax.Term.t -> (Type.t, Infer_error.t) Result.t

val check : check_env -> Term_syntax.Term.t -> Type.t -> Check_error.t option

(** {1 Patterns} *)
val match_pattern : Binding_aware_pattern.t -> Value.t -> Value.t String.Map.t option

val find_match
  :  Value.t
  -> Term_syntax.Case_scope.t list
  -> (Term_syntax.Term.t * eval_env) option

(* val coverage_check :  Cases.t -> *)
val preimage : Term_syntax.Case_scope.t list -> Binding_aware_pattern.t list
val reverse : Nominal.Term.t -> Term_syntax.Case_scope.t list -> eval_env option
