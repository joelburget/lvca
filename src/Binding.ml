open Types
module Result = Belt.Result
module Option = Belt.Option
module L = Belt.List
module M = Belt.Map.String

module rec DeBruijn : sig
  type scope = Scope of Pattern.t list * term

  and term =
    | Operator of string * scope list
    | Var of int * int
    | Sequence of term list
    | Primitive of primitive

  val to_nominal : term -> Nominal.term option
  val from_nominal : Nominal.term -> (term, string) Result.t

  val from_nominal_with_bindings
    :  (int * int) Belt.Map.String.t
    -> Nominal.term
    -> (term, string) Result.t
end = struct
  type scope = Scope of Pattern.t list * term

  and term =
    | Operator of string * scope list
    | Var of int * int
    | Sequence of term list
    | Primitive of primitive

  module L = Belt.List

  let rec to_nominal' ctx = function
    | Var (ix1, ix2) ->
      ctx
      |. L.get ix1
      |. Option.flatMap (fun x -> x |. L.get ix2)
      |. Option.map (fun name -> Nominal.Var name)
    | Operator (tag, subtms) ->
      Option.map
        (Util.sequence_list_option (L.map subtms (scope_to_nominal ctx)))
        (fun subtms' -> Nominal.Operator (tag, subtms'))
    | Sequence tms ->
      Option.map
        (Util.sequence_list_option (L.map tms (to_nominal' ctx)))
        (fun tms' -> Nominal.Sequence tms')
    | Primitive prim -> Some (Nominal.Primitive prim)

  and scope_to_nominal ctx (Scope (binders, body)) =
    Option.map
      (to_nominal' (L.concat (L.map binders Pattern.list_vars_of_pattern) ctx) body)
      (fun body' -> Nominal.Scope (binders, body'))
  ;;

  let to_nominal = to_nominal' []

  exception FailedFromNominal of string

  let rec from_nominal_with_bindings' env = function
    | Nominal.Operator (tag, subtms) ->
      Operator (tag, subtms |. L.map (scope_from_nominal' env))
    | Var name ->
      (match M.get env name with
       | None -> raise (FailedFromNominal ("couldn't find variable " ^ name))
       | Some (i, j) -> Var (i, j))
    | Sequence tms -> Sequence (tms |. L.map (from_nominal_with_bindings' env))
    | Primitive prim -> Primitive prim

  and scope_from_nominal' env (Nominal.Scope (pats, body)) =
    let n = L.length pats in
    let varNums : (string * (int * int)) list =
      L.(
        zip pats (makeBy n (fun i -> i))
        |. map (fun (pat, i) ->
          let vars = Pattern.list_vars_of_pattern pat in
          zip vars (makeBy (length vars) (fun j -> i, j)))
        |. flatten)
    in
    let env' : (int * int) M.t =
      Util.map_union (M.map env (fun (i, j) -> i + n, j)) (M.fromArray (L.toArray varNums))
    in
    Scope (pats, from_nominal_with_bindings' env' body)
  ;;

  let from_nominal_with_bindings bindings tm =
    try Result.Ok (from_nominal_with_bindings' bindings tm) with
    | FailedFromNominal msg -> Error msg
  ;;

  let from_nominal = from_nominal_with_bindings M.empty
end

and Nominal : sig
  type scope = Scope of Pattern.t list * term

  and term =
    | Operator of string * scope list
    | Var of string
    | Sequence of term list
    | Primitive of primitive

  val pp_term : Format.formatter -> Nominal.term -> unit
  val pp_term' : Nominal.term -> string
  val jsonify : Nominal.term -> Js.Json.t
  val serialize : Nominal.term -> Uint8Array.t
  val hash : Nominal.term -> string

  exception ToPatternScopeEncountered
  val to_pattern_exn : Nominal.term -> Pattern.t
end = struct
  type scope = Scope of Pattern.t list * term

  and term =
    | Operator of string * scope list
    | Var of string
    | Sequence of term list
    | Primitive of primitive

  open Format

  let rec pp_term ppf = function
    | Operator (tag, subtms) -> fprintf ppf "@[%s(%a)@]" tag pp_scope_list subtms
    | Var v -> fprintf ppf "%s" v
    | Sequence tms -> fprintf ppf "@[[%a]@]" pp_inner_list tms
    | Primitive p -> pp_prim ppf p

  and pp_inner_list ppf = function
    | [] -> ()
    | [ x ] -> fprintf ppf "%a" pp_term x
    | x :: xs -> fprintf ppf "%a, %a" pp_term x pp_inner_list xs

  and pp_scope_list ppf = function
    | [] -> ()
    | [ x ] -> fprintf ppf "%a" pp_scope x
    | x :: xs -> fprintf ppf "%a, %a" pp_scope x pp_scope_list xs

  and pp_scope ppf (Scope (bindings, body)) =
    match bindings with
    | [] -> pp_term ppf body
    | _ -> fprintf ppf "%a %a" pp_bindings bindings pp_term body

  and pp_pattern_list ppf = function
    | [] -> ()
    | [ pat ] -> pp_pattern ppf pat
    | pat :: pats -> fprintf ppf "%a, %a" pp_pattern pat pp_pattern_list pats

  and pp_pattern ppf (pat : Pattern.t) =
    match pat with
    | Var name -> fprintf ppf "%s" name
    | Operator (name, pats) -> fprintf ppf "%s(%a)" name pp_pattern_list pats
    | Sequence pats -> fprintf ppf "[%a]" pp_pattern_list pats
    | Primitive prim -> pp_prim ppf prim

  and pp_bindings ppf = function
    | [] -> ()
    | [ x ] -> fprintf ppf "%a." pp_pattern x
    | x :: xs -> fprintf ppf "%a. %a" pp_pattern x pp_bindings xs

  and pp_prim ppf = function
    | PrimInteger i -> fprintf ppf "%s" (Bigint.to_string i)
    | PrimString s -> fprintf ppf "\"%s\"" s
  ;;

  let pp_term' = asprintf "%a" pp_term
  let pp_scope' = asprintf "%a" pp_scope
  let array_map f args = Js.Json.array Belt.List.(toArray (map args f))

  let jsonify_prim =
    Js.Json.(
      function
      | PrimInteger i -> array [| string "i"; string (Bigint.to_string i) |]
      | PrimString s -> array [| string "s"; string s |])
  ;;

  let rec jsonify (tm : term) : Js.Json.t =
    Js.Json.(
      match tm with
      | Operator (tag, tms) ->
        array [| string "o"; string tag; array_map jsonify_scope tms |]
      | Var name -> array [| string "v"; string name |]
      | Sequence tms -> array [| string "s"; array_map jsonify tms |]
      | Primitive p -> array [| string "p"; jsonify_prim p |])

  and jsonify_pat (pat : Pattern.t) : Js.Json.t =
    Js.Json.(
      match pat with
      | Operator (tag, tms) ->
        array [| string "o"; string tag; array_map jsonify_pat tms |]
      | Var name -> array [| string "v"; string name |]
      | Sequence tms -> array [| string "s"; array_map jsonify_pat tms |]
      | Primitive p -> array [| string "p"; jsonify_prim p |])

  and jsonify_scope (Scope (pats, body)) : Js.Json.t =
    Js.Json.(array [| array_map jsonify_pat pats; jsonify body |])
  ;;

  (* serialize by converting to JSON then cboring *)
  let serialize (tm : term) : Uint8Array.t =
    Uint8Array.from_array_buffer (Cbor.encode_ab (jsonify tm))
  ;;

  (*
     let deserialize (buf : Uint8Array.t) : term option
     = dejsonify (Cbor.decode_ab (Uint8Array.to_array_buffer buf))
  *)

  let hash tm = Sha256.hash_ba (BitArray.from_u8_array (serialize tm))

  exception ToPatternScopeEncountered

  (* raises ToPatternScopeEncountered *)
  let rec to_pattern_exn : term -> Pattern.t = function
    | Var name -> Var name
    | Operator (name, tms) -> Operator (name, tms |. Belt.List.map scope_to_pattern_exn)
    | Sequence tms -> Sequence (Belt.List.map tms to_pattern_exn)
    | Primitive prim -> Primitive prim

  (* raises ToPatternScopeEncountered *)
  and scope_to_pattern_exn : scope -> Pattern.t = function
    | Scope ([], tm) -> to_pattern_exn tm
    | scope -> failwith ("Parse error: invalid pattern: " ^ pp_scope' scope)
  ;;
end
