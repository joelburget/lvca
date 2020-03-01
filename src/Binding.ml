open Core_kernel
open Types

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
    :  (int * int) String.Map.t
    -> Nominal.term
    -> (term, string) Result.t
end = struct
  type scope = Scope of Pattern.t list * term

  and term =
    | Operator of string * scope list
    | Var of int * int
    | Sequence of term list
    | Primitive of primitive

  let rec to_nominal' ctx = function
    | Var (ix1, ix2) -> List.nth ctx ix1
      |> Option.bind ~f:(Fn.flip List.nth ix2)
      |> Option.map ~f:(fun name -> Nominal.Var name)
    | Operator (tag, subtms) -> subtms
      |> List.map ~f:(scope_to_nominal ctx)
      |> Option.all
      |> Option.map ~f:(fun subtms' -> Nominal.Operator (tag, subtms'))
    | Sequence tms -> tms
      |> List.map ~f:(to_nominal' ctx)
      |> Option.all
      |> Option.map ~f:(fun tms' -> Nominal.Sequence tms')
    | Primitive prim -> Some (Nominal.Primitive prim)

  and scope_to_nominal ctx (Scope (binders, body)) = binders
    |> List.map ~f:Pattern.list_vars_of_pattern
    |> List.append ctx
    |> fun ctx' -> to_nominal' ctx' body
    |> Option.map ~f:(fun body' -> Nominal.Scope (binders, body'))
  ;;

  let to_nominal = to_nominal' []

  exception FailedFromNominal of string

  let rec from_nominal_with_bindings' env = function
    | Nominal.Operator (tag, subtms) ->
      Operator (tag, List.map subtms ~f:(scope_from_nominal' env))
    | Var name ->
      (match String.Map.find env name with
       | None -> raise (FailedFromNominal ("couldn't find variable " ^ name))
       | Some (i, j) -> Var (i, j))
    | Sequence tms -> Sequence (List.map tms ~f:(from_nominal_with_bindings' env))
    | Primitive prim -> Primitive prim

  and scope_from_nominal' env (Nominal.Scope (pats, body)) =
    let n = List.length pats in
    let var_nums : (string * (int * int)) list = pats
      |> List.mapi ~f:(fun i pat ->
          let vars = Pattern.list_vars_of_pattern pat in
          let twod_indexes = List.init (List.length vars) ~f:(fun j -> i, j) in
          List.zip_exn vars twod_indexes
         )
      |> List.join
    in
    match String.Map.of_alist var_nums with
      | `Ok var_map ->
        let env' : (int * int) String.Map.t = Util.map_union
          (String.Map.map env ~f:(fun (i, j) -> i + n, j))
          var_map
        in
        Scope (pats, from_nominal_with_bindings' env' body)
      | `Duplicate_key _key -> failwith "TODO: raise error"
  ;;

  let from_nominal_with_bindings bindings tm =
    try
      Ok (from_nominal_with_bindings' bindings tm)
    with
      FailedFromNominal msg -> Error msg
  ;;

  let from_nominal = from_nominal_with_bindings String.Map.empty
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
  val jsonify : Nominal.term -> Placemat.Json.t
  val serialize : Nominal.term -> Bytes.t
  val hash : Nominal.term -> string

  exception ToPatternScopeEncountered
  val to_pattern_exn : Nominal.term -> Pattern.t
  val pattern_to_term : Pattern.t -> Nominal.term
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
    | x :: xs -> fprintf ppf "%a; %a" pp_scope x pp_scope_list xs

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
  let array_map f args = args
    |> List.map ~f:f
    |> Array.of_list
    |> Placemat.Json.array

  let jsonify_prim =
    Placemat.Json.(
      function
      | PrimInteger i -> array [| string "i"; string (Bigint.to_string i) |]
      | PrimString s -> array [| string "s"; string s |])
  ;;

  let rec jsonify (tm : term) : Placemat.Json.t =
    Placemat.Json.(
      match tm with
      | Operator (tag, tms) ->
        array [| string "o"; string tag; array_map jsonify_scope tms |]
      | Var name -> array [| string "v"; string name |]
      | Sequence tms -> array [| string "s"; array_map jsonify tms |]
      | Primitive p -> array [| string "p"; jsonify_prim p |])

  and jsonify_pat (pat : Pattern.t) : Placemat.Json.t =
    Placemat.Json.(
      match pat with
      | Operator (tag, tms) ->
        array [| string "o"; string tag; array_map jsonify_pat tms |]
      | Var name -> array [| string "v"; string name |]
      | Sequence tms -> array [| string "s"; array_map jsonify_pat tms |]
      | Primitive p -> array [| string "p"; jsonify_prim p |])

  and jsonify_scope (Scope (pats, body)) : Placemat.Json.t =
    Placemat.Json.(array [| array_map jsonify_pat pats; jsonify body |])
  ;;

  (* serialize by converting to JSON then cboring *)
  let serialize : term -> Bytes.t
    = fun tm -> tm
      |> jsonify
      |> Placemat.Cbor.encode
  ;;

  (*
     let deserialize : Bytes.t -> term option
       = fun buf -> buf
         |> Placemat.Cbor.decode
         |> dejsonify
  *)

  let hash tm = Placemat.Sha256.hash (serialize tm)

  exception ToPatternScopeEncountered

  (* raises ToPatternScopeEncountered *)
  let rec to_pattern_exn : term -> Pattern.t = function
    | Var name -> Var name
    | Operator (name, tms) -> Operator (name, List.map tms ~f:scope_to_pattern_exn)
    | Sequence tms -> Sequence (List.map tms ~f:to_pattern_exn)
    | Primitive prim -> Primitive prim

  (* raises ToPatternScopeEncountered *)
  and scope_to_pattern_exn : scope -> Pattern.t = function
    | Scope ([], tm) -> to_pattern_exn tm
    | scope -> failwith ("Parse error: invalid pattern: " ^ pp_scope' scope)
  ;;

  let rec pattern_to_term : Pattern.t -> Nominal.term
    = function
    | Var name -> Var name
    | Operator (name, pats) -> Operator
      (name, List.map pats ~f:(fun pat -> Scope ([], pattern_to_term pat)))
    | Sequence pats -> Sequence (List.map pats ~f:pattern_to_term)
    | Primitive prim -> Primitive prim
end

let%test_module "Nominal" = (module struct
  open Nominal

  let serialize tm =  Nominal.serialize tm
  let print_serialize tm =
    let bytes = serialize tm in
    bytes
      |> Bytes.to_array
      |> Array.iter ~f:(fun char -> printf "%02x" (int_of_char char))
  let print_hash tm = printf "%s" (hash tm)

  let (=) = Caml.(=)

  let tm = Var "x"
  let%test "" = jsonify tm = Placemat.Json.(Array [| String "v"; String "x" |])
  let%expect_test _ = print_serialize tm;
    [%expect{| 8261766178 |}]
  let%expect_test _ = print_hash tm;
    [%expect{| bbc37ed1e26f8481398dc797bd0b3ec2e3ae954e1c3f69b461a487d8703ec3d6 |}]

  let tm = Operator ("Z", [])
  let%test "" = jsonify tm =
    Placemat.Json.(Array [| String "o"; String "Z"; Array [||] |])
  let%expect_test _ = print_serialize tm;
    [%expect{| 83616f615a80 |}]
  let%expect_test _ = print_hash tm;
    [%expect{| 2380ed848a0c5ce3d0ad7420e841578e4068f394b37b9b11bd3c34cea391436c |}]

  let tm = Operator ("S", [Scope ([Var "x"], Var "x")])
  let%test "" = jsonify tm = Placemat.Json.(Array
    [| String "o";
       String "S";
       Array [|
         Array [|
           Array [|
             Array [| String "v"; String "x" |]
           |];
           Array [| String "v"; String "x" |]
         |]
       |]
    |])
  let%expect_test _ = print_serialize tm;
    [%expect{| 83616f615381828182617661788261766178 |}]
  let%expect_test _ = print_hash tm;
    [%expect{| 391e4a6e3dc6964d60c642c52416d18b102dca357a3e4953834dfefc0e02dfbc |}]

  let tm = Primitive (PrimInteger (Bigint.of_string "12345"))
  let%test "" = jsonify tm = Placemat.Json.(Array
    [| String "p";
       Array [| String "i"; String "12345" |]
    |])
  let%expect_test _ = print_serialize tm;
    [%expect {| 826170826169653132333435 |}]
  let%expect_test _ = print_hash tm;
    [%expect{| e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5 |}]

  let tm = Sequence []
  let%test "" = jsonify tm = Placemat.Json.(Array [| String "s"; Array [||] |])
  let%expect_test _ = print_serialize tm;
    [%expect{| 82617380 |}]
  let%expect_test _ = print_hash tm;
    [%expect{| 8afbfb879b5a95214c4c483c401313235040663bbdc08220992a5841801a421e |}]

  let tm = Sequence [Var "x"]
  let%test "" = jsonify tm = Placemat.Json.(Array
    [| String "s";
       Array [| Array [| String "v"; String "x" |] |]
    |])
  let%expect_test _ = print_serialize tm;
    [%expect{| 826173818261766178 |}]
  let%expect_test _ = print_hash tm;
    [%expect{| 28b6e8f2124dd5931d69e1a5350f5c44ebdec7e0f6be9f98d2c717fcf09fa3d8 |}]
end)
