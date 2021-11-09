open Base
open Lvca_syntax

let ident_char_gen =
  Crowbar.(
    choose
      [ map [ range ~min:65 26 ] Char.of_int_exn (* A - Z *)
      ; map [ range ~min:97 26 ] Char.of_int_exn (* a - z *)
      ])
;;

let char_gen = Crowbar.(map [ range ~min:32 94 ] Char.of_int_exn)
let ident_gen = Crowbar.(map [ list1 ident_char_gen ] String.of_char_list)
let str_gen = Crowbar.(map [ list char_gen ] String.of_char_list)
let nonempty_str_gen = Crowbar.(map [ list1 char_gen ] String.of_char_list)

(* json *)

let rec json_gen =
  lazy
    Crowbar.(
      Lvca_util.(
        choose
          [ const (Json.array [||])
          ; map [ float ] Json.float
          ; map [ str_gen ] Json.string
          ; map [ list (unlazy json_gen) ] (fun ts -> ts |> Array.of_list |> Json.array)
          ]))
;;

let (lazy json_gen) = json_gen

(* primitive *)

let prov = Provenance.of_here [%here]

let prim_gen : Primitive.All.t Crowbar.gen =
  let open Crowbar in
  let options =
    [ map [ int ] (fun i -> prov, Primitive_impl.All_plain.Integer (Z.of_int i))
      (* TODO: string and float parsing has a couple issues *)
    ; map [ str_gen ] (fun str -> prov, Primitive_impl.All_plain.String str)
      (* ; map [float] (fun f -> PrimFloat f) *)
    ; map [ char ] (fun c -> prov, Primitive_impl.All_plain.Char c)
    ]
  in
  choose options
;;

(* de bruijn *)

(*
let rec de_bruijn_gen = fun binding_var_count -> lazy Crowbar.(DeBruijn.(
  let subtm_scope = map [unlazy (de_bruijn_scope_gen binding_var_count)]
    (fun scope -> Either.First scope)
  in
  let subtm_tms = map [list (unlazy (de_bruijn_gen binding_var_count))]
    (fun tms -> Either.Second tms)
  in
  let subtms = list (choose [subtm_scope; subtm_tms]) in
  let no_bound_var_options =
    [ map [prim_gen] (fun p -> Primitive (prov, p))
    ; map [str_gen; subtms] (fun name scopes -> Operator (prov, name, scopes))
    ; map [str_gen] (fun name -> FreeVar (prov, name))
    ]
  in
  let options = match binding_var_count with
    | 0 -> no_bound_var_options
    | _ ->
      let var_gen = map [range binding_var_count]
        (fun var_ix -> BoundVar (prov, var_ix))
      in
      var_gen :: no_bound_var_options
  in
  choose options))

and de_bruijn_scope_gen = fun binding_var_count -> lazy Crowbar.(
  map [str_gen; list (unlazy (de_bruijn_gen (binding_var_count + 1)))]
    (fun name tms -> DeBruijn.Scope (prov, name, tms))
)

let lazy de_bruijn_gen = de_bruijn_gen 0 *)

(* pattern *)

let pattern_gen =
  Crowbar.(
    Pattern.(
      fix (fun pattern_gen ->
          choose
            [ map [ prim_gen ] (fun p -> Primitive p)
            ; map [ nonempty_str_gen; list pattern_gen ] (fun name subpats ->
                  Operator (prov, name, subpats))
            ; map [ nonempty_str_gen ] (fun name -> Var (prov, name))
            ])))
;;

(* nominal *)

let rec nominal_gen =
  lazy
    (let choose, list, map, unlazy = Crowbar.(choose, list, map, unlazy) in
     choose
       Nominal.Term.
         [ map [ prim_gen ] (fun p -> Primitive p)
         ; map
             [ nonempty_str_gen; list (unlazy nominal_scope_gen) ]
             (fun name scopes -> Operator (prov, name, scopes))
         ; map [ nonempty_str_gen ] (fun name -> Var (prov, name))
         ])

and nominal_scope_gen =
  lazy
    Crowbar.(
      map [ list pattern_gen; unlazy nominal_gen ] (fun pats tm ->
          Nominal.Scope.Scope (pats, tm)))
;;

let (lazy nominal_gen) = nominal_gen

(*
let term_str_conf tm_str =
  let parser = ParseNominal.whitespace_t ParsePrimitive.t in
  match ParseUtil.parse_string parser tm_str with
  | Error _ -> Crowbar.bad_test ()
  | Ok tm ->
    let tm_str' = Nominal.pp_term_str tm in
    Crowbar.check_eq tm_str tm_str'
;;
*)

(* Limited core generator: only generates nominal terms. *)
let core_gen =
  Crowbar.map [ nominal_gen ] (fun tm -> Lvca_del.Core.Lang.Term.Nominal (prov, tm))
;;

(* Limitations of parser generator:
   - Uses core_gen, so inherits its limitations
   - Doesn't generate well-typed or well-bound parsers
 *)
let parser_gen =
  let mk_var name = Single_var.{ name; info = prov } in
  Crowbar.(
    Lvca_languages.Parser.Lang.Term.(
      Lvca_languages.Parser.Sequence.(
        fix (fun parser_gen ->
            let binder_gen =
              choose
                [ map [ str_gen; parser_gen ] (fun name p -> Some name, p)
                ; map [ parser_gen ] (fun p -> None, p)
                ]
            in
            let rec mk_sequence binders core =
              match binders with
              | [] -> Empty_sequence (prov, core)
              | (Some name, p) :: binders ->
                Binding (prov, p, (mk_var name, mk_sequence binders core))
              | (None, p) :: binders -> Non_binding (prov, p, mk_sequence binders core)
            in
            let sequence_gen = map [ list binder_gen; core_gen ] mk_sequence in
            choose
              [ const (Any_char prov)
              ; map [ char ] (fun c -> Char (prov, (prov, c)))
              ; map [ str_gen ] (fun s -> String (prov, (prov, s)))
              ; map [ ident_gen; core_gen ] (fun ident tm ->
                    Satisfy (prov, (prov, ident), tm))
              ; map [ core_gen ] (mk_Fail ~info:prov)
              ; map [ ident_gen; parser_gen; parser_gen ] (fun ident p1 p2 ->
                    mk_Let ~info:prov p1 (mk_var ident, p2))
              ; map [ parser_gen ] (mk_Option ~info:prov)
              ; map [ parser_gen; core_gen ] (mk_Count ~info:prov)
              ; map [ parser_gen ] (mk_Many ~info:prov)
              ; map [ parser_gen ] (mk_Many1 ~info:prov)
              ; map [ ident_gen; parser_gen ] (fun ident p ->
                    Fix (prov, (mk_var ident, p)))
              ; map [ parser_gen; parser_gen ] (fun p1 p2 ->
                    Choice (prov, Lvca_del.List_model.of_list [ p1; p2 ]))
              ; map [ sequence_gen ] (mk_Sequence ~info:prov)
              ; map [ ident_gen ] (mk_Term_var ~info:prov)
              ]))))
;;

let add_test ~name ~gen ~f =
  Crowbar.add_test ~name [ gen ] (fun a ->
      match f a with
      | Lvca_util.Property_result.Ok -> ()
      | Failed msg -> Crowbar.fail msg
      | Uninteresting -> Crowbar.bad_test ())
;;

let () =
  (*
  (* Primitive: *)
  (* 0 *)
  add_test
    ~name:"Primitive json_round_trip1"
    ~gen:prim_gen
    ~f:Primitive.All.Properties.json_round_trip1;
  (* 1 *)
  add_test
    ~name:"Primitive json_round_trip2"
    ~gen:json_gen
    ~f:Primitive.All.Properties.json_round_trip2;
  (* 2 *)
  add_test
    ~name:"Primitive string_round_trip1"
    ~gen:prim_gen
    ~f:Primitive.All.Properties.string_round_trip1;
  (* 3 *)
  add_test
    ~name:"Primitive string_round_trip2"
    ~gen:nonempty_str_gen
    ~f:Primitive.All.Properties.string_round_trip2;
  (* (* 4 *) (* TODO: failing *) add_test ~name:"Nominal json_round_trip1"
     ~gen:nominal_gen ~f:Nominal.Properties.json_round_trip1; *)

  (* Nominal: *)
  (* 5 *)
  add_test
    ~name:"Nominal json_round_trip2"
    ~gen:json_gen
    ~f:Nominal.Properties.json_round_trip2;
  (* (* 6 *) (* TODO: failing *) add_test ~name:"Nominal string_round_trip1"
     ~gen:nominal_gen ~f:Nominal.Properties.string_round_trip1; *)

  (* 7 *)
  add_test
    ~name:"Nominal string_round_trip2"
    ~gen:nonempty_str_gen
    ~f:Nominal.Properties.string_round_trip2;

  (* Pattern: *)
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
    ~gen:nonempty_str_gen
    ~f:Pattern.Properties.string_round_trip2;

  (* Parser: *)
  (* 12 *)
  *)
  add_test
    ~name:"Parser string_round_trip1"
    ~gen:parser_gen
    ~f:Lvca_languages.Parser.Properties.string_round_trip1;
  (* 13 *)
  add_test
    ~name:"Parser string_round_trip2"
    ~gen:nonempty_str_gen
    ~f:Lvca_languages.Parser.Properties.string_round_trip2;
  ()
;;
