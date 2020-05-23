open Base
module Format = Caml.Format
module Printf = Caml.Printf
module Cbor = Util.Cbor
module Json = Util.Json
module String = Util.String

module rec DeBruijn : sig
  type scope = Scope of Pattern.t list * term

  and term =
    | Operator of string * scope list
    | Var of int * int
    | Primitive of Primitive.t

  val to_nominal : term -> Nominal.term option
  val from_nominal : Nominal.term -> (term, string) Result.t

  val from_nominal_with_bindings
    :  (int * int) String.Map.t
    -> Nominal.term
    -> (term, string) Result.t

  val alpha_equivalent : term -> term -> bool
end = struct
  type scope = Scope of Pattern.t list * term

  and term =
    | Operator of string * scope list
    | Var of int * int
    | Primitive of Primitive.t

  let rec to_nominal' ctx = function
    | Var (ix1, ix2) ->
      List.nth ctx ix1
      |> Option.bind ~f:(Fn.flip List.nth ix2)
      |> Option.map ~f:(fun name -> Nominal.Var name)
    | Operator (tag, subtms) ->
      subtms
      |> List.map ~f:(scope_to_nominal ctx)
      |> Option.all
      |> Option.map ~f:(fun subtms' -> Nominal.Operator (tag, subtms'))
    | Primitive prim -> Some (Nominal.Primitive prim)

  and scope_to_nominal ctx (Scope (binders, body)) =
    binders
    |> List.map ~f:Pattern.list_vars_of_pattern
    |> List.append ctx
    |> fun ctx' ->
    to_nominal' ctx' body |> Option.map ~f:(fun body' -> Nominal.Scope (binders, body'))
  ;;

  let to_nominal = to_nominal' []

  exception FailedFromNominal of string

  let rec from_nominal_with_bindings' env = function
    | Nominal.Operator (tag, subtms) ->
      Operator (tag, List.map subtms ~f:(scope_from_nominal' env))
    | Var name ->
      (match Map.find env name with
      | None -> raise (FailedFromNominal ("couldn't find variable " ^ name))
      | Some (i, j) -> Var (i, j))
    | Primitive prim -> Primitive prim

  and scope_from_nominal' env (Nominal.Scope (pats, body)) =
    let n = List.length pats in
    let var_nums : (string * (int * int)) list =
      pats
      |> List.mapi ~f:(fun i pat -> pat
        |> Pattern.list_vars_of_pattern
        |> List.mapi ~f:(fun j var -> var, (i, j)))
      |> List.join
    in
    match String.Map.of_alist var_nums with
    | `Ok var_map ->
      let env' : (int * int) String.Map.t = env
        |> Map.map ~f:(fun (i, j) -> i + n, j)
        |> Util.Map.union_right_biased var_map
      in
      Scope (pats, from_nominal_with_bindings' env' body)
    | `Duplicate_key _key -> failwith "TODO: raise error"
  ;;

  let from_nominal_with_bindings bindings tm =
    try Ok (from_nominal_with_bindings' bindings tm) with
    | FailedFromNominal msg -> Error msg
  ;;

  let from_nominal = from_nominal_with_bindings String.Map.empty

  let rec alpha_equivalent = fun t1 t2 ->
    match t1, t2 with
      | Operator (h1, subtms1), Operator (h2, subtms2)
      -> String.(h1 = h2) && (match List.zip subtms1 subtms2 with
        | Ok zipped -> List.for_all zipped ~f:(fun (Scope (_, body1), Scope (_, body2)) ->
            alpha_equivalent body1 body2)
        | Unequal_lengths -> false
      )
      | Var (i1, j1), Var (i2, j2)
      -> i1 = i2 && j1 = j2
      | Primitive p1, Primitive p2
      -> Primitive.(p1 = p2)
      | _, _
      -> false
end

and Nominal : sig
  type scope = Scope of Pattern.t list * term

  and term =
    | Operator of string * scope list
    | Var of string
    | Primitive of Primitive.t

  val pp_term : Format.formatter -> Nominal.term -> unit
  val pp_term_str : Nominal.term -> string

  val pp_scope : Format.formatter -> Nominal.scope -> unit
  val pp_scope_str : Nominal.scope -> string

  val serialize : Nominal.term -> Bytes.t
  val deserialize : Bytes.t -> term option

  val jsonify : Nominal.term -> Json.t
  val unjsonify : Json.t -> Nominal.term option

  val hash : Nominal.term -> string

  exception ToPatternScopeEncountered of scope

  val to_pattern_exn : Nominal.term -> Pattern.t
  val to_pattern : Nominal.term -> (Pattern.t, scope) Result.t
  val pattern_to_term : Pattern.t -> Nominal.term
end = struct
  type scope = Scope of Pattern.t list * term

  and term =
    | Operator of string * scope list
    | Var of string
    | Primitive of Primitive.t

  let any, list, str, string, semi, pf = Fmt.(any, list, str, string, semi, pf)

  let rec pp_term ppf = function
    | Operator (tag, subtms)
    -> pf ppf "@[%s(%a)@]"
      tag
      (list ~sep:semi pp_scope) subtms
    | Var v
    -> string ppf v
    | Primitive p
    -> Primitive.pp ppf p

  and pp_scope ppf (Scope (bindings, body)) = match bindings with
    | [] -> pp_term ppf body
    | _ -> pf ppf "%a.@ %a" (* pp_bindings bindings pp_term body *)
      (list ~sep:(any ".@ ") Pattern.pp) bindings
      pp_term body
  ;;

  let pp_term_str = str "%a" pp_term
  let pp_scope_str = str "%a" pp_scope
  let array_map f args = args |> List.map ~f |> Array.of_list |> Json.array

  let rec jsonify (tm : term) : Json.t =
    Json.(
      match tm with
      | Operator (tag, tms) ->
        array [| string "o"; string tag; array_map jsonify_scope tms |]
      | Var name -> array [| string "v"; string name |]
      | Primitive p -> array [| string "p"; Primitive.jsonify p |])

  and jsonify_scope (Scope (pats, body)) : Json.t =
    Json.(array [| array_map Pattern.jsonify pats; jsonify body |])
  ;;

  let rec unjsonify =
    let open Option.Let_syntax in
    Json.(function
    | Array [| String "o"; String tag; Array scopes |]
    -> let%map scopes' = scopes
         |> Array.to_list
         |> List.map ~f:unjsonify_scope
         |> Option.all
      in
      Operator (tag, scopes')
    | Array [| String "v"; String name |]
    -> Some (Var name)
    | Array [| String "p"; prim |]
    -> let%map prim' = Primitive.unjsonify prim in
       Primitive prim'
    | _ -> None)

  and unjsonify_scope = Json.(function
    | Array [||]
    -> None
    | Array arr
    -> let open Option.Let_syntax in
       let binders, body = arr |> Array.to_list |> Util.List.unsnoc in
       let%bind binders' = binders
         |> List.map ~f:Pattern.unjsonify
         |> Option.all
       in
       let%bind body' = unjsonify body in
       Some (Scope (binders', body'))
    | _ -> None
  )
  ;;

  let serialize = fun tm -> tm |> jsonify |> Cbor.encode

  let deserialize = fun buf -> buf |> Cbor.decode |> Option.bind ~f:unjsonify

  let hash = fun tm -> tm |> serialize |> Util.Sha256.hash

  exception ToPatternScopeEncountered of scope

  (** @raise ToPatternScopeEncountered *)
  let rec to_pattern_exn : term -> Pattern.t = function
    | Var name -> if String.is_substring_at name ~pos:0 ~substring:"_"
      then Ignored (String.slice name 1 0)
      else Var name
    | Operator (name, tms) -> Operator (name, List.map tms ~f:scope_to_pattern_exn)
    | Primitive prim -> Primitive prim

  (** @raise ToPatternScopeEncountered *)
  and scope_to_pattern_exn : scope -> Pattern.t = function
    | Scope ([], tm) -> to_pattern_exn tm
    | scope -> raise (ToPatternScopeEncountered scope)
  ;;

  let to_pattern : term -> (Pattern.t, scope) Result.t
    = fun tm ->
      try
        Ok (to_pattern_exn tm)
      with
        ToPatternScopeEncountered scope -> Error scope

  let rec pattern_to_term : Pattern.t -> Nominal.term = function
    | Operator (name, pats) ->
      Operator (name, List.map pats ~f:(fun pat -> Scope ([], pattern_to_term pat)))
    | Primitive prim -> Primitive prim
    | Var name -> Var name
    | Ignored name -> Var ("_" ^ name)
  ;;
end

let%test_module "Nominal" =
  (module struct
    open Nominal

    let serialize tm = Nominal.serialize tm

    let print_serialize tm =
      let bytes = serialize tm in
      bytes
      |> Bytes.to_array
      |> Array.iter ~f:(fun char -> Printf.printf "%02x" (Char.to_int char))
    ;;

    let print_hash tm = Printf.printf "%s" (hash tm)
    let ( = ) = Caml.( = )
    let tm = Var "x"

    let%test "" = jsonify tm = Json.(Array [| String "v"; String "x" |])

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 8261766178 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| bbc37ed1e26f8481398dc797bd0b3ec2e3ae954e1c3f69b461a487d8703ec3d6 |}]
    ;;

    let tm = Operator ("Z", [])

    let%test "" =
      jsonify tm = Json.(Array [| String "o"; String "Z"; Array [||] |])
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 83616f615a80 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| 2380ed848a0c5ce3d0ad7420e841578e4068f394b37b9b11bd3c34cea391436c |}]
    ;;

    let tm = Operator ("S", [ Scope ([ Var "x" ], Var "x") ])

    let%test "" =
      jsonify tm
      = Json.(
          Array
            [| String "o"
             ; String "S"
             ; Array
                 [| Array
                      [| Array [| Array [| String "v"; String "x" |] |]
                       ; Array [| String "v"; String "x" |]
                      |]
                 |]
            |])
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 83616f615381828182617661788261766178 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| 391e4a6e3dc6964d60c642c52416d18b102dca357a3e4953834dfefc0e02dfbc |}]
    ;;

    let tm = Primitive (PrimInteger (Bigint.of_string "12345"))

    let%test "" =
      jsonify tm
      = Json.(Array [| String "p"; Array [| String "i"; String "12345" |] |])
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 826170826169653132333435 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5 |}]
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 826170826169653132333435 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5 |}]
    ;;

    let%test _ = to_pattern_exn (Var "abc") = Var "abc"
    let%test _ = to_pattern_exn (Var "_abc") = Ignored "abc"
    let%test _ = to_pattern_exn (Var "_") = Ignored ""
    let%test _ = pattern_to_term (Ignored "abc") = Var "_abc"
    let%test _ = pattern_to_term (Ignored "") = Var "_"
  end)
;;
