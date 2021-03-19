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
  val select_path : path:int list -> 'info t -> ('info t, string) Result.t
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
  let select_path = failwith "TODO"
  let jsonify _ = failwith "TODO"

  (* let jsonify tm = tm |> Object.to_term |> Term.jsonify *)
  let unjsonify _ = failwith "TODO"
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
