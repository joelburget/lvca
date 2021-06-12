open Base
open Lvca_util

module Extend (Object : Language_object_intf.S) :
  Language_object_intf.Extended_s
    with type 'info t = 'info Object.t
     and module Plain = Object.Plain = struct
  include Object

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
    let%bind nom = Nominal.Term.unjsonify json in
    match Object.of_nominal nom with Ok tm -> Some tm | Error _ -> None
  ;;

  let serialize tm = tm |> jsonify |> Cbor.encode
  let deserialize buf = buf |> Cbor.decode |> Option.bind ~f:unjsonify
  let hash tm = tm |> serialize |> Sha256.hash

  module Parse = struct
    include Parse

    let whitespace_t = Lvca_parsing.(whitespace *> t)
  end
end

module Check_parse_pretty (Object : Language_object_intf.S) :
  Properties_intf.Parse_pretty_s with type 'info t = 'info Object.t = struct
  open Property_result
  module Object = Extend (Object)
  open Object

  type 'info t = 'info Object.t

  let to_string = Fmt.to_to_string Object.pp
  let parse = Lvca_parsing.parse_string Parse.t

  let string_round_trip1 t =
    match t |> to_string |> parse with
    | Ok t' ->
      let t'' = Object.erase t' in
      Property_result.check
        Object.(equal ~info_eq:Unit.( = ) t'' t)
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
          Property_result.check
            String.(str'' = str')
            (Fmt.str {|"%s" <> "%s"|} str'' str'))
  ;;
end

module Check_json (Object : Language_object_intf.S) :
  Properties_intf.Json_s with type 'info t = 'info Object.t = struct
  open Property_result
  module Object = Extend (Object)
  open Object

  type 'info t = 'info Object.t

  let json_round_trip1 t =
    match t |> Object.jsonify |> Object.unjsonify with
    | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
    | Some t' ->
      Property_result.check
        (Object.equal ~info_eq:Unit.( = ) t t')
        (Fmt.str "%a <> %a" pp t' pp t)
  ;;

  let json_round_trip2 json =
    match json |> Object.unjsonify with
    | None -> Uninteresting
    | Some t ->
      Property_result.check
        Json.(Object.jsonify t = json)
        "jsonify t <> json (TODO: print)"
  ;;
end

module Check_properties (Object : Language_object_intf.S) :
  Properties_intf.S with type 'info t = 'info Object.t = struct
  include Check_parse_pretty (Object)
  include Check_json (Object)
end
