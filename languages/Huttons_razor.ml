open Base
open Lvca_syntax
open Lvca_provenance
open Stdio

module Description = struct
  let abstract_syntax =
    [%lvca.abstract_syntax
      {|
     expr :=
     | Lit(integer)
     | Add(expr; expr)
     ;

     type := Int();  |}]
  ;;

  let concrete_syntax =
    [%lvca.concrete_syntax
      {|
    expr:
      | Lit(i)    ~ i
      | Add(x; y) ~ x _ "+" _ y
      | i         ~ /[a-z][a-zA-Z0-1_]/
      \ ()"+"

    type:
      | Int() ~ "int"
      ;
    |}]
  ;;

  let statics =
    {|
  ----------------------
  ctx >> Lit(_) => Int()

  -------------------------
  ctx >> Add(_; _) => Int()
    |}
  ;;

  let dynamics_str =
    {|
    let rec dynamics = \(expr : expr()) -> match expr with {
      | Add(a; b) ->
        let a' = dynamics a in
        let b' = dynamics b in
        {Add(a'; b')}
      | Lit(i) -> i
    } in dynamics
    |}
  ;;

  (*
  let dynamics =
    Lvca_parsing.(parse_string (whitespace *> Del.Core.Term.parse)) dynamics_str
    |> Result.ok_or_failwith
  ;;
  *)
end

(* Write by hand first, later assert the generated parser is equivalent *)
module Parse = struct
  open Lvca_parsing
  open C_comment_parser

  let lit : Nonbinding.t Lvca_parsing.t =
    let%map range, str = integer_lit in
    let range = Provenance.of_range range in
    Nonbinding.Operator (range, "Lit", [ Primitive (range, Integer (Z.of_string str)) ])
  ;;

  let t : Nonbinding.t Lvca_parsing.t =
    fix (fun t ->
        let atom = lit <|> parens t in
        let plus = char '+' in
        let f (rng1, l) (rng2, r) =
          let rng = Opt_range.union rng1 rng2 in
          let info = Provenance.of_range rng in
          rng, Nonbinding.Operator (info, "Add", [ l; r ])
        in
        let%bind init = atom in
        let%map _, list = many (plus *> attach_pos atom) in
        list |> List.fold ~init ~f |> snd)
  ;;
end

let pp =
  (* re prec: ambient precedence level: either 0 or 1. A (+) in an ambient precedence of 1
     needs parens. *)
  let open Stdlib.Format in
  let with_stag ppf stag f =
    pp_open_stag ppf stag;
    f ();
    pp_close_stag ppf ()
  in
  let rec pp' prec ppf tm =
    match tm with
    | Nonbinding.Operator (_, "Add", [ a; b ]) ->
      with_stag
        ppf
        (String_tag (Nonbinding.hash tm))
        (fun () ->
          if prec > 0
          then Fmt.pf ppf "(%a + %a)" (pp' 0) a (pp' 1) b
          else Fmt.pf ppf "%a + %a" (pp' 0) a (pp' 1) b)
    | Operator (_, "Lit", [ Primitive (_, Integer i) ]) ->
      with_stag ppf (String_tag (Nonbinding.hash tm)) (fun () -> Z.pp_print ppf i)
    | tm -> Fmt.failwith "Invalid Hutton's Razor term %a" Nonbinding.pp tm
  in
  pp' 0
;;

let rec eval_tm : Nonbinding.t -> (Z.t, string) Result.t = function
  | Operator (_, "Add", [ a; b ]) ->
    (match eval_tm a, eval_tm b with
    | Ok a', Ok b' -> Ok Z.(a' + b')
    | Error msg, _ | _, Error msg -> Error msg)
  | Operator (_, "Lit", [ Primitive (_, Integer i) ]) -> Ok i
  | tm -> Error (Fmt.str "found un-evaluable term: %a" Nonbinding.pp tm)
;;

let eval_str : string -> (Z.t, string) Result.t =
 fun str ->
  match Lvca_parsing.(parse_string (whitespace *> Parse.t) str) with
  | Error str -> Error str
  | Ok tm -> eval_tm tm
;;

(* let eval_2 : string -> (Z.t, string) Result.t = let module Parse =
   AngstromParse(Lvca_parsing.NoComment) in fun str -> match Lvca_parsing.parse_string
   Parse.whitespace_t str with | Error str -> Error str | Ok tm -> begin match Core.(eval
   (CoreApp (Description.dynamics, Term (Nonbinding.to_nominal tm)))) with | Error (msg,
   tm) -> Error (msg ^ ": " ^ Core.to_string tm) | Ok (Primitive (_, Integer i)) -> Ok i
   | _ -> Error "unexpected non-integer result" end *)

let ident_stag_funs =
  Stdlib.Format.
    { mark_open_stag =
        (function
        | Range.Stag rng -> Fmt.str "<%a>" Range.pp rng
        | String_tag str -> Fmt.str "<%s>" (String.subo str ~len:6)
        | _ -> "")
    ; mark_close_stag =
        (function
        | Range.Stag rng -> Fmt.str "</%a>" Range.pp rng
        | String_tag str -> Fmt.str "</%s>" (String.subo str ~len:6)
        | _ -> "")
    ; print_open_stag = (fun _ -> ())
    ; print_close_stag = (fun _ -> ())
    }
;;

let%test_module "Hutton's Razor" =
  (module struct
    module Concrete = Concrete.Make (struct
      let abstract =
        match Abstract_syntax.mk_unordered Description.abstract_syntax with
        | `Ok unordered -> unordered.sort_defs
        | `Duplicate_key _ -> failwith "duplicate key"
      ;;

      let concrete =
        match Concrete.Unordered.build Description.concrete_syntax with
        | `Ok unordered -> unordered
        | `Duplicate_key _ -> failwith "duplicate key"
      ;;

      let start_sort = "expr"
    end)

    let parse str = Lvca_parsing.(parse_string (whitespace *> Parse.t) str)

    let parse2 str =
      Lvca_parsing.(
        parse_string
          (whitespace
          *> Concrete.parse_term (Provenance.Parse_input.Buffer_name "test input"))
          str)
    ;;

    let margin = Stdlib.Format.(pp_get_margin std_formatter ())

    let () =
      let open Stdlib.Format in
      pp_set_margin std_formatter 200;
      set_formatter_stag_functions ident_stag_funs;
      set_tags true;
      set_mark_tags true
    ;;

    let print_representations str =
      match parse str, parse2 str with
      | Error str, _ | _, Error str -> print_string str
      | Ok tm1, Ok tm2 ->
        Fmt.pr "%a\n" Nonbinding.pp tm1;
        Fmt.pr "%a\n" pp tm1;
        Fmt.pr "%a\n" Nominal.Term.pp tm2
    ;;

    let%expect_test _ =
      print_representations "1";
      [%expect {|
      Lit(1)
      <c91b95>1</c91b95>
      Lit(1)
    |}]
    ;;

    let%expect_test _ =
      print_representations "1 + 2";
      (*012345*)
      [%expect
        {|
      Add(Lit(1); Lit(2))
      <a809f3><c91b95>1</c91b95> + <356aaf>2</356aaf></a809f3>
      Add(Lit(1); Lit(2))
    |}]
    ;;

    let%expect_test _ =
      print_representations "1 + 2 + 3";
      (*0123456789*)
      [%expect
        {|
      Add(Add(Lit(1); Lit(2)); Lit(3))
      <409d43><a809f3><c91b95>1</c91b95> + <356aaf>2</356aaf></a809f3> + <756fbf>3</756fbf></409d43>
      Add(Add(Lit(1); Lit(2)); Lit(3))
    |}]
    ;;

    let%expect_test _ =
      print_representations "1 + (2 + 3)";
      (*012345678901*)
      [%expect
        {|
      Add(Lit(1); Add(Lit(2); Lit(3)))
      <9060da><c91b95>1</c91b95> + <170de4>(<356aaf>2</356aaf> + <756fbf>3</756fbf>)</170de4></9060da>
      Add(Lit(1); Add(Lit(2); Lit(3)))
    |}]
    ;;

    let print_eval : string -> unit =
     fun str ->
      let msg = match eval_str str with Error msg -> msg | Ok i -> Z.to_string i in
      print_string msg
   ;;

    let%expect_test _ =
      print_eval "1";
      [%expect {| 1 |}]
    ;;

    let%expect_test _ =
      print_eval "1 + 2";
      [%expect {| 3 |}]
    ;;

    let%expect_test _ =
      print_eval "1 + 2 + 3";
      [%expect {| 6 |}]
    ;;

    let%expect_test _ =
      print_eval "1 + (2 + 3)";
      [%expect {| 6 |}]
    ;;

    let () = Stdlib.Format.(pp_set_margin std_formatter margin)
  end)
;;
