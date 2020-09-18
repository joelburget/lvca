open Lvca_syntax
open Core_kernel

(* The range vars are allowed to span. This is a list of de bruijn indices and
 * the number of vars bound at each index. *)
type var_range = int list

(*
let de_bruijn_gen : var_range -> (unit, Primitive.t) DeBruijn.term Crowbar.gen
  =
  (* let open Crowbar in *)
  Crowbar.fix (fun de_bruijn_gen ->
    let open DeBruijn in
    let open Primitive in
    let options =
      [ Crowbar.(map [int] (fun i -> Primitive ((), PrimInteger (Bigint.of_int i))))
      ; Crowbar.(map [bytes] (fun str -> Primitive ((), PrimString str)))
      (* ; Crowbar.(map [list de_bruijn_gen] (fun tms -> Sequence tms)) *)
      (* TODO: use range *)
      (* ; Crowbar.(map [pair *)
      ]
    in
    let options' = match var_range with
      | [] -> options
      | _ ->
        let var_gen = Crowbar.(dynamic_bind (range (List.length var_range))
          (fun var_ix -> map [range (List.nth_exn var_range var_ix)]
            (fun var_num -> Var (var_ix, var_num))))
        in var_gen :: options
    in
    Crowbar.choose options')

module ParseNominal = Nominal.Parse (ParseUtil.NoComment)
module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)

let term_str_conf tm_str =
   let parser = ParseNominal.whitespace_t ParsePrimitive.t in
   match ParseUtil.parse_string parser tm_str with
     | Error _ -> Crowbar.bad_test ()
     | Ok tm ->
         let tm_str' = Nominal.pp_term_str Primitive.pp tm in
         Crowbar.check_eq tm_str tm_str'
         *)

let () =
  (* let _term_body = In_channel.read_all "term.lvca" in *)
  (* Crowbar.(add_test ~name:"Nominal round trip" [ bytes ] term_str_conf); *)
  Crowbar.(add_test ~name:"Primitive string_round_trip1" [ bytes ]
    (fun str -> str |> Primitive.Properties.string_round_trip2 |> check));
  ()
;;
