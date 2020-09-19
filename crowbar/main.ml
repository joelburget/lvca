open Base
open Lvca_syntax

(* json *)

let rec json_gen =
  lazy Crowbar.(Lvca_util.(choose
    [ const (Json.array [||])
    ; map [float] Json.float
    ; map [bytes] Json.string
    ; map [list (unlazy json_gen)] (fun ts -> ts |> Array.of_list |> Json.array)
    ]))
let lazy json_gen = json_gen

(* primitive *)

let prim_gen : Primitive.t Crowbar.gen
  = let open Crowbar in
    let open Primitive in
  let options =
    [ map [int] (fun i -> PrimInteger (Bigint.of_int i))
    (* TODO: string and float parsing has a couple issues *)
    (* ; map [bytes] (fun str -> PrimString str) *)
    (* ; map [float] (fun f -> PrimFloat f) *)
    ; map [char] (fun c -> PrimChar c)
    ]
  in
  choose options

(* de bruijn *)

  (*
let rec de_bruijn_gen = fun binding_var_count -> lazy Crowbar.(DeBruijn.(
  let subtm_scope = map [force (de_bruijn_scope_gen binding_var_count)]
    (fun scope -> Either.First scope)
  in
  let subtm_tms = map [list (force (de_bruijn_gen binding_var_count))]
    (fun tms -> Either.Second tms)
  in
  let subtms = list (choose [subtm_scope; subtm_tms]) in
  let no_bound_var_options =
    [ map [prim_gen] (fun p -> Primitive ((), p))
    ; map [bytes; subtms] (fun name scopes -> Operator ((), name, scopes))
    ; map [bytes] (fun name -> FreeVar ((), name))
    ]
  in
  let options = match binding_var_count with
    | 0 -> no_bound_var_options
    | _ ->
      let var_gen = map [range binding_var_count]
        (fun var_ix -> BoundVar ((), var_ix))
      in
      var_gen :: no_bound_var_options
  in
  choose options))

and de_bruijn_scope_gen = fun binding_var_count -> lazy Crowbar.(
  map [bytes; list (force (de_bruijn_gen (binding_var_count + 1)))]
    (fun name tms -> DeBruijn.Scope ((), name, tms))
)

let lazy de_bruijn_gen = de_bruijn_gen 0
*)

(* pattern *)

let pattern_gen = Crowbar.(Pattern.(fix (fun pattern_gen ->
  choose
    [ map [prim_gen] (fun p -> Primitive ((), p))
    ; map [bytes; list (list pattern_gen)]
      (fun name subpats -> Operator ((), name, subpats))
    ; map [bytes] (fun name -> Var ((), name))
    ; map [bytes] (fun name -> Ignored ((), name))
    ])))

(* nominal *)

let rec nominal_gen = lazy Crowbar.(Nominal.(
  let subtms = list (force nominal_scope_gen) in
  choose
    [ map [prim_gen] (fun p -> Primitive ((), p))
    ; map [bytes; subtms] (fun name scopes -> Operator ((), name, scopes))
    ; map [bytes] (fun name -> Var ((), name))
    ]))

and nominal_scope_gen = lazy Crowbar.(
    map [list pattern_gen; list (force nominal_gen)]
      (fun pats tms -> Nominal.Scope (pats, tms))
)

let lazy nominal_gen = nominal_gen

(* testing *)

let add_test ~name ~gen ~f = Crowbar.add_test ~name [ gen ]
  (fun a -> match f a with
    | PropertyResult.Ok -> ()
    | Failed msg -> Crowbar.fail msg
    | Uninteresting -> Crowbar.bad_test ())
;;

let () =

  (* 0 *)
  add_test
    ~name:"Primitive json_round_trip1"
    ~gen:prim_gen
    ~f:Primitive.Properties.json_round_trip1;

  (* 1 *)
  add_test
    ~name:"Primitive json_round_trip2"
    ~gen:json_gen
    ~f:Primitive.Properties.json_round_trip2;

  (* 2 *)
  add_test
    ~name:"Primitive string_round_trip1"
    ~gen:prim_gen
    ~f:Primitive.Properties.string_round_trip1;

  (* 3 *)
  add_test
    ~name:"Primitive string_round_trip2"
    ~gen:Crowbar.bytes
    ~f:Primitive.Properties.string_round_trip2;

  (* 4 *)
  (* TODO: failing *)
  add_test
    ~name:"Nominal json_round_trip1"
    ~gen:nominal_gen
    ~f:Nominal.Properties.json_round_trip1;

  (* 5 *)
  add_test
    ~name:"Nominal json_round_trip2"
    ~gen:json_gen
    ~f:Nominal.Properties.json_round_trip2;

  (* 6 *)
  (* TODO: failing *)
  add_test
    ~name:"Nominal string_round_trip1"
    ~gen:nominal_gen
    ~f:Nominal.Properties.string_round_trip1;

  (* 7 *)
  add_test
    ~name:"Nominal string_round_trip2"
    ~gen:Crowbar.bytes
    ~f:Nominal.Properties.string_round_trip2;

  (* 8 *)
  add_test
    ~name:"Pattern json_round_trip1"
    ~gen:pattern_gen
    ~f:Pattern.Properties.json_round_trip1;

  (* 9 *)
  add_test
    ~name:"Pattern json_round_trip2"
    ~gen:json_gen
    ~f:Pattern.Properties.json_round_trip2;

  (* TODO: failing *)
  (* 10 *)
  add_test
    ~name:"Pattern string_round_trip1"
    ~gen:pattern_gen
    ~f:Pattern.Properties.string_round_trip1;

  (* 11 *)
  add_test
    ~name:"Pattern string_round_trip2"
    ~gen:Crowbar.bytes
    ~f:Pattern.Properties.string_round_trip2;

  ()
;;
