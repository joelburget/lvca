open Base

module type AllTermS = sig
  type 'info t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val info : 'info t -> 'info
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
end

module type NonBindingTermS = sig
  include AllTermS

  val to_nonbinding : 'info t -> ('info, Lvca_util.Void.t) NonBinding.term

  val of_nonbinding
    :  ('info, 'prim) NonBinding.term
    -> ('info t, ('info, 'prim) NonBinding.term) Result.t
end

module type BindingTermS = sig
  include AllTermS

  val to_nominal : 'info t -> ('info, Lvca_util.Void.t) Nominal.term

  val of_nominal
    :  ('info, 'prim) Nominal.term
    -> ('info t, ('info, 'prim) Nominal.term) Result.t
end

module type ExtendedTermS = sig
  type 'a t

  val erase : _ t -> unit t
  val pp : _ t Fmt.t
  val to_string : _ t -> string

  val select_path
    :  path:int list
    -> 'info t
    -> ('info t, (string, ('info, Lvca_util.Void.t) Nominal.term) Either.t) Result.t

  val jsonify : _ t Lvca_util.Json.serializer
  val unjsonify : unit t Lvca_util.Json.deserializer
  val serialize : _ t -> Bytes.t
  val deserialize : Bytes.t -> unit t option
  val hash : _ t -> string
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
    match tm |> Object.to_nominal |> Nominal.select_path ~path with
    | Ok tm ->
      (match Object.of_nominal tm with
      | Ok tm -> Ok tm
      | Error tm -> Error (Either.Second tm))
    | Error msg -> Error (Either.First msg)
  ;;

  let jsonify tm = tm |> Object.to_nominal |> Nominal.jsonify Lvca_util.Void.absurd

  let unjsonify json =
    let open Option.Let_syntax in
    let%bind nom = json |> Nominal.unjsonify (Fn.const None) in
    match Object.of_nominal nom with Ok tm -> Some tm | Error _ -> None
  ;;

  let serialize tm = tm |> jsonify |> Lvca_util.Cbor.encode
  let deserialize buf = buf |> Lvca_util.Cbor.decode |> Option.bind ~f:unjsonify
  let hash tm = tm |> serialize |> Lvca_util.Sha256.hash
end

module type Properties = sig
  type 'info t

  val json_round_trip1 : unit t -> PropertyResult.t
  val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
  val string_round_trip1 : unit t -> PropertyResult.t
  val string_round_trip2 : string -> PropertyResult.t
end

module CheckProperties (Object : BindingTermS) :
  Properties with type 'info t = 'info Object.t = struct
  open PropertyResult
  module Object = Mk (Object)

  type 'info t = 'info Object.t

  let pp = Object.pp

  let json_round_trip1 t =
    match t |> Object.jsonify |> Object.unjsonify with
    | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
    | Some t' -> PropertyResult.check Caml.(t = t') (Fmt.str "%a <> %a" pp t' pp t)
  ;;

  let json_round_trip2 json =
    match json |> Object.unjsonify with
    | None -> Uninteresting
    | Some t ->
      PropertyResult.check
        Lvca_util.Json.(Object.jsonify t = json)
        "jsonify t <> json (TODO: print)"
  ;;

  let string_round_trip1 _ = failwith "TODO: Add when we add parsing to LanguageObject."
  let string_round_trip2 _ = failwith "TODO: Add when we add parsing to LanguageObject."
end
