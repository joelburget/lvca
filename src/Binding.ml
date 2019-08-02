open Types
module Result = Belt.Result
module Option = Belt.Option
module L = Belt.List
module M = Belt.Map.String

let (sequence_list_option, sequence_list_result, union) =
  Util.(sequence_list_option, sequence_list_result, union)

module rec DeBruijn : sig
  type scope = Scope of string list * term

  and term =
    | Operator  of string * scope list
    | Var       of int
    | Sequence  of term list
    | Primitive of primitive

  val to_nominal : term -> Nominal.term option

  val from_nominal
    : language
    -> string
    -> Nominal.term
    -> (term, string) Result.t

  val from_nominal_with_bindings
    :  language
    -> string
    -> int Belt.Map.String.t
    -> Nominal.term
    -> (term, string) Result.t

end = struct

  type scope =
    | Scope of string list * term

  and term =
    | Operator  of string * scope list
    | Var       of int
    | Sequence  of term list
    | Primitive of primitive

  let rec find_operator (operators : operatorDef list) (tag : string)
    : operatorDef option
    = match operators with
      | [] -> None
      | (OperatorDef (hd, _) as od) :: tl ->
        if hd = tag
        then Some od
        else find_operator tl tag

  module L = Belt.List

  let rec to_nominal' ctx = function
    | Var ix
    -> Option.map (L.get ctx ix) (fun name -> Nominal.Var name)
    | Operator (tag, subtms)
    -> Option.map
      (sequence_list_option (L.map subtms (scope_to_nominal ctx)))
      (fun subtms' -> Nominal.Operator (tag, subtms'))
    | Sequence tms
    -> Option.map
      (sequence_list_option (L.map tms (to_nominal' ctx)))
      (fun tms' -> Nominal.Sequence tms')
    | Primitive prim
    -> Some (Nominal.Primitive prim)

  and scope_to_nominal ctx (Scope (binders, body)) = Option.map
    (to_nominal' (L.concat binders ctx) body)
    (fun body' -> Nominal.Scope (binders, body'))

  let to_nominal = to_nominal' []

  let rec from_nominal_with_bindings (Language sorts as lang) current_sort env
    = function
      | Nominal.Operator(tag, subtms) -> (match M.get sorts current_sort with
        | None -> Result.Error
          ("from_nominal_with_bindings: couldn't find sort " ^ current_sort)
        | Some (SortDef (_vars, operators)) -> (match find_operator operators tag with
          | None -> Error
            ("from_nominal_with_bindings: couldn't find operator " ^ tag ^
            " (in sort " ^ current_sort ^ ")")
          | Some (OperatorDef (_tag, Arity (_binds, valences))) ->
            if L.(length valences != length subtms)
            then Error (
              "Unexpected number of subterms (does not match the valence of " ^
              tag ^ ")"
            )
            else Result.map
              (sequence_list_result
                (L.zipBy valences subtms
                (fun valence subtm -> match valence with
                  | FixedValence (_binds, SortAp (result_sort, _))
                    -> scope_from_nominal lang result_sort env subtm
                  | _ -> Result.Error "TODO 2")))
              (fun subtms' -> Operator (tag, subtms'))))
      | Var name -> (match M.get env name with
        | None    -> Error ("couldn't find variable " ^ name)
        | Some ix -> Ok (Var ix))
      | Sequence tms -> Result.map
        (sequence_list_result
          (L.map tms (from_nominal_with_bindings lang current_sort env)))
        (fun x' -> Sequence x')
      | Primitive prim -> Ok (DeBruijn.Primitive prim)

  and scope_from_nominal
    lang
    (current_sort : string)
    env
    (Nominal.Scope (names, body))
    =
      let n = L.length names in
      let argNums = L.(zip names (makeBy n (fun i -> i))) in
      let env' = union
            (M.map env (fun i -> i + n))
            (M.fromArray (L.toArray argNums))
      in Result.map
           (from_nominal_with_bindings lang current_sort env' body)
           (fun body' -> (Scope (names, body')))

  let from_nominal lang current_sort =
    from_nominal_with_bindings lang current_sort M.empty

end

and Nominal : sig
  type scope =
    | Scope of string list * term

  and term =
    | Operator  of string * scope list
    | Var       of string
    | Sequence  of term list
    | Primitive of primitive

  val pp_term  : Format.formatter -> Nominal.term -> unit
  val pp_term' : Nominal.term -> string

  val jsonify   : Nominal.term -> Js.Json.t
  val serialize : Nominal.term -> Uint8Array.t
  val hash      : Nominal.term -> string

end = struct
  type scope =
    | Scope of string list * term

  and term =
    | Operator  of string * scope list
    | Var       of string
    | Sequence  of term list
    | Primitive of primitive

  open Format

  let rec pp_term ppf = function
    | Operator (tag, subtms) -> fprintf ppf "@[%s(%a)@]"
      tag
      pp_scope_list subtms
    | Var v        -> fprintf ppf "%s" v
    | Sequence tms -> fprintf ppf "@[[%a]@]" pp_inner_list tms
    | Primitive p  -> pp_prim ppf p

  and pp_inner_list ppf = function
    | []      -> ()
    | [x]     -> fprintf ppf "%a"     pp_term x
    | x :: xs -> fprintf ppf "%a, %a" pp_term x pp_inner_list xs

  and pp_scope_list ppf = function
    | []      -> ()
    | [x]     -> fprintf ppf "%a"     pp_scope x
    | x :: xs -> fprintf ppf "%a, %a" pp_scope x pp_scope_list xs

  and pp_scope ppf (Scope (bindings, body)) = match bindings with
    | [] -> pp_term ppf body
    | _  -> fprintf ppf "%a %a" pp_bindings bindings pp_term body

  and pp_bindings ppf = function
    | []      -> ()
    | [x]     -> fprintf ppf "%s." x
    | x :: xs -> fprintf ppf "%s. %a" x pp_bindings xs

  and pp_prim ppf = function
    | PrimInteger i -> fprintf ppf "%s" (Bigint.to_string i)
    | PrimString  s -> fprintf ppf "\"%s\"" s

  let pp_term' = asprintf "%a" pp_term

  let array_map f args = Js.Json.array (Belt.List.(toArray (map args f)))

  let jsonify_prim = Js.Json.(function
    | PrimInteger i -> array [| string "i"; string (Bigint.to_string i) |]
    | PrimString  s -> array [| string "s"; string s                    |]
  )

  let rec jsonify (tm : term) : Js.Json.t = Js.Json.(match tm with
    | Operator (tag, tms)
    -> array [|
      string "t";
      string tag;
      array_map jsonify_scope tms
    |]
    | Var name     -> array [| string "v"; string name           |]
    | Sequence tms -> array [| string "s"; array_map jsonify tms |]
    | Primitive p  -> array [| string "p"; jsonify_prim p        |]
  )

  and jsonify_scope (Scope (args, body)) : Js.Json.t
    = Js.Json.(array [| array_map string args; jsonify body |])

  (* serialize by converting to JSON then cboring *)
  let serialize (tm : term) : Uint8Array.t
    = Uint8Array.from_array_buffer (Cbor.encode_ab (jsonify tm))

    (*
  let deserialize (buf : Uint8Array.t) : term option
    = dejsonify (Cbor.decode_ab (Uint8Array.to_array_buffer buf))
    *)

  let hash tm = Sha256.hash_ba (BitArray.from_u8_array (serialize tm))
end
