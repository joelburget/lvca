open Base
open Lvca_provenance

module type AllTermS = LanguageObject_intf.S

module type NonBindingTermS = sig
  include AllTermS

  val of_nonbinding : 'info NonBinding.term -> ('info t, 'info NonBinding.term) Result.t
  val to_nonbinding : 'info t -> 'info NonBinding.term
end

module type BindingTermS = sig
  include AllTermS

  val to_nominal : 'info t -> 'info Nominal.Term.t
  val of_nominal : 'info Nominal.Term.t -> ('info t, 'info Nominal.Term.t) Result.t
end

module type ExtendedTermS = sig
  type 'a t

  val erase : _ t -> unit t
  val pp : _ t Fmt.t
  val to_string : _ t -> string

  val select_path
    :  path:int list
    -> 'info t
    -> ('info t, (string, 'info Nominal.Term.t) Either.t) Result.t

  val jsonify : _ t Lvca_util.Json.serializer
  val unjsonify : unit t Lvca_util.Json.deserializer
  val serialize : _ t -> Bytes.t
  val deserialize : Bytes.t -> unit t option
  val hash : _ t -> string

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val whitespace_t : OptRange.t t ParseUtil.t
  end
end

module Mk (Object : BindingTermS) : ExtendedTermS with type 'info t = 'info Object.t =
struct
  type 'info t = 'info Object.t

  let erase tm = Object.map_info ~f:(fun _ -> ()) tm

  let pp ppf tm =
    Object.pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf tm
  ;;

  let to_string tm = Fmt.to_to_string pp tm

  let select_path ~path tm =
    match tm |> Object.to_nominal |> Nominal.Term.select_path ~path with
    | Ok tm ->
      (match Object.of_nominal tm with
      | Ok tm -> Ok tm
      | Error tm -> Error (Either.Second tm))
    | Error msg -> Error (Either.First msg)
  ;;

  let jsonify tm = tm |> Object.to_nominal |> Nominal.Term.jsonify

  let unjsonify json =
    let open Option.Let_syntax in
    let%bind nom = json |> Nominal.Term.unjsonify in
    match Object.of_nominal nom with Ok tm -> Some tm | Error _ -> None
  ;;

  let serialize tm = tm |> jsonify |> Lvca_util.Cbor.encode
  let deserialize buf = buf |> Lvca_util.Cbor.decode |> Option.bind ~f:unjsonify
  let hash tm = tm |> serialize |> Lvca_util.Sha256.hash

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)
    module Parse = Object.Parse (Comment)

    let whitespace_t = Parsers.(junk *> Parse.t)
  end
end

module type Properties = Properties_intf.S

module CheckProperties (Object : BindingTermS) :
  Properties with type 'info t = 'info Object.t = struct
  module Parse = Object.Parse (ParseUtil.NoComment)
  open PropertyResult
  module Object' = Object
  module Object = Mk (Object)

  type 'info t = 'info Object.t

  let pp = Object.pp
  let to_string = Fmt.to_to_string Object.pp
  let parse = ParseUtil.parse_string Parse.t

  let json_round_trip1 t =
    match t |> Object.jsonify |> Object.unjsonify with
    | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
    | Some t' ->
      PropertyResult.check
        (Object'.equal ~info_eq:Unit.( = ) t t')
        (Fmt.str "%a <> %a" pp t' pp t)
  ;;

  let json_round_trip2 json =
    match json |> Object.unjsonify with
    | None -> Uninteresting
    | Some t ->
      PropertyResult.check
        Lvca_util.Json.(Object.jsonify t = json)
        "jsonify t <> json (TODO: print)"
  ;;

  let string_round_trip1 t =
    match t |> to_string |> parse with
    | Ok t' ->
      let t'' = Object.erase t' in
      PropertyResult.check
        Object'.(equal ~info_eq:Unit.( = ) t'' t)
        (Fmt.str "%a <> %a" pp t'' pp t)
    | Error msg -> Failed (Fmt.str {|parse_string "%a": %s|} pp t msg)
  ;;

  let string_round_trip2 str =
    match parse str with
    | Error _ -> Uninteresting
    | Ok t ->
      let str' = t |> Object.erase |> to_string in
      if String.(str' = str)
      then Ok
      else (
        match parse str with
        | Error msg -> Failed msg
        | Ok t' ->
          let str'' = t' |> Object.erase |> to_string in
          PropertyResult.check String.(str'' = str') (Fmt.str {|"%s" <> "%s"|} str'' str'))
  ;;
end
