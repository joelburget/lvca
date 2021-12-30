module Rule : sig
  type t =
    | Str_lit of string
    | Re_lit of Regex.t
    | Symbol of string
    | Seq of t list
    | Choice of t list
    | Repeat of t
    | Repeat1 of t
    | Optional of t
    | Prec of int * t
    | Left of int option * t
    | Right of int option * t
    | Dynamic of int * t
    | Token of t
    | Immediate of t
    | Alias of string * t
    | Field of string * t

  val pp : t Fmt.t
  val of_sort_syntax : sort_def:Sort_def.t -> Concrete.Sort_syntax.t -> string * t
end

module Grammar : sig
  type t =
    { name : string
    ; rules : (string * Rule.t) list
    ; extras : string list option
    ; inline : string list option
    ; conflicts : string list list option
    ; externals : string list option
    ; word : string option
    ; supertypes : string list option
    }

  val mk
    :  ?extras:string list
    -> ?inline:string list
    -> ?conflicts:string list list
    -> ?externals:string list
    -> ?word:string
    -> ?supertypes:string list
    -> string
    -> (string * Rule.t) list
    -> t

  val of_concrete
    :  abstract:Sort_def.t Lvca_util.String.Map.t
    -> name:string
    -> Concrete.Sort_syntax.t list
    -> t
end
