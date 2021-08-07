(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [scope], [case_scope], (and
      [Binding_aware_pattern.t.t]).
    - [eval] is used to evaluate a core term *)

open Lvca_provenance
open Lvca_syntax
open Lvca_util

(** {1 Types} *)

module List_model : sig
  include [%lvca.abstract_syntax_module_sig "list a := Nil() | Cons(a; list a)"]

  val of_list : empty_info:'info -> 'a list -> ('info, 'a) List.t
  val to_list : (_, 'a) List.t -> 'a list
  val map : f:('a -> 'b) -> ('info, 'a) List.t -> ('info, 'b) List.t
end

module Option_model : sig
  include [%lvca.abstract_syntax_module_sig "option a := None() | Some(a)"]

  val of_option : empty_info:'info -> 'a option -> ('info, 'a) Option.t
  val to_option : (_, 'a) Option.t -> 'a option
  val map : f:('a -> 'b) -> ('info, 'a) Option.t -> ('info, 'b) Option.t
end

module Binding_aware_pattern_model : [%lvca.abstract_syntax_module_sig
{|
string : *  // module Primitive.String
primitive : *  // module Primitive.All
list : * -> *  // module List_model.List

pattern :=
  | Operator(string; list scope)
  | Primitive(primitive)
  | Var(string)

scope := Scope(list string; pattern)
|}]

module Sort_model : sig
  include
    [%lvca.abstract_syntax_module_sig
  {|
string : *  // module Primitive.String

sort :=
  | Ap(string; ap_list)
  | Name(string)

ap_list :=
  | Nil()
  | Cons(sort; ap_list)
|}]
end

module Sort : sig
  (* include module type of Sort_model.Sort *)

  val into : 'info Lvca_syntax.Sort.t -> 'info Sort_model.Sort.t
  val out : 'info Sort_model.Sort.t -> 'info Lvca_syntax.Sort.t
end

module Lang : [%lvca.abstract_syntax_module_sig
{|
sort : *  // module Sort_model.Sort
nominal : *  // module Nominal.Term
list : * -> *  // module List_model.List
option : * -> *  // module Option_model.Option
binding_aware_pattern : * -> *  // module Binding_aware_pattern_model.Pattern

is_rec := Rec() | No_rec()

ty := Sort(sort) | Arrow(ty; ty)

term :=
  | Term(nominal)
  | Ap(term; list term)
  | Case(term; list case_scope)
  | Lambda(ty; term. term)
  | Let(is_rec; term; option ty; term. term)

case_scope := Case_scope(binding_aware_pattern; term)
|}]

module Term : sig
  include Nominal.Convertible.Extended_s with type 'info t = 'info Lang.Term.t
  (* and module Plain = Lang.Term.Plain *)

  val parse_concrete : comment:'a Lvca_parsing.t -> 'a Commented.t t Lvca_parsing.t
  val pp_generic_concrete : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
  val pp_concrete : _ t Fmt.t
end

module Type : sig
  type 'info t = 'info Lang.Ty.t

  val parse : comment:'a Lvca_parsing.t -> 'a Commented.t t Lvca_parsing.t
end

module Parse : sig
  val term : comment:'a Lvca_parsing.t -> 'a Commented.t Lang.Term.t Lvca_parsing.t
end

(** {1 Core type} *)
module Module : sig
  type 'info t =
    { externals : (string * 'info Type.t) list
    ; defs : (string * 'info Lang.Term.t) list
    }

  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
  val pp : _ t Fmt.t
  val parse : comment:'a Lvca_parsing.t -> 'a Commented.t t Lvca_parsing.t
end

(** {1 Checking} *)

type 'info check_env =
  { type_env : 'info Type.t String.Map.t
  ; syntax : 'info Abstract_syntax.t
  }

module Check_error' : sig
  type term_failure_inference_reason =
    | Term_var_not_found of string
    | Operator_not_supported

  type 'info t =
    | Cant_infer_case
    | Cant_infer_lambda
    | Var_not_found
    | Operator_not_found
    | Mismatch of 'info Type.t
    | Term_isnt_arrow
    | Failed_term_inference of term_failure_inference_reason
    | Failed_check_term of
        ('info, ('info Pattern.t, 'info Nominal.Term.t) Base.Either.t) Check_failure.t
end

module Check_error : sig
  type 'info t =
    { env : 'info check_env
    ; tm : 'info Lang.Term.t
    ; ty : 'info Type.t
    ; error : 'info Check_error'.t
    }
end

module Infer_error : sig
  type 'info t =
    { env : 'info check_env
    ; tm : 'info Lang.Term.t
    ; error : 'info Check_error'.t
    }
end

(** Typecheck a term in an environment. *)
val infer
  :  'info option check_env
  -> 'info option Lang.Term.t
  -> ('info option Type.t, 'info option Infer_error.t) Result.t

val check
  :  'info option check_env
  -> 'info option Lang.Term.t
  -> 'info option Type.t
  -> 'info option Check_error.t option

(** {1 Evaluation} *)

type 'info env = 'info Nominal.Term.t String.Map.t
type 'info eval_error = string * 'info Lang.Term.t

val eval_in_ctx
  :  no_info:'info
  -> 'info env
  -> 'info Lang.Term.t
  -> ('info Nominal.Term.t, 'info eval_error) Base.Result.t

val eval
  :  no_info:'info
  -> 'info Lang.Term.t
  -> ('info Nominal.Term.t, 'info eval_error) Base.Result.t

(** {1 Patterns} *)
val match_pattern
  :  'info Nominal.Term.t
  -> 'info Binding_aware_pattern.t
  -> 'info env option

val find_match
  :  'info Nominal.Term.t
  -> 'b Lang.Case_scope.t list
  -> ('b Lang.Term.t * 'info env) option

(* val coverage_check : 'info Cases.t -> *)
val preimage : 'info Lang.Case_scope.t list -> 'info Binding_aware_pattern.t list
val reverse : 'info Nominal.Term.t -> 'info Lang.Case_scope.t list -> 'info env option
