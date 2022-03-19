open Base
open Lvca_provenance

module Parse_input = struct
  type t =
    | Input_unknown
    | Buffer_name of string
    | String of string

  let pp ppf = function
    | Input_unknown -> Fmt.pf ppf "Input_unknown"
    | Buffer_name name -> Fmt.pf ppf "Buffer_name %S" name
    | String name -> Fmt.pf ppf "String %S" name
  ;;

  let ( = ) a b =
    match a, b with
    | Input_unknown, Input_unknown -> true
    | Buffer_name s1, Buffer_name s2 | String s1, String s2 -> String.(s1 = s2)
    | _ -> false
  ;;
end

module Parse_located = struct
  type t =
    { input : Parse_input.t
    ; range : Opt_range.t
    }

  let pp ppf { input; range } =
    Fmt.pf ppf "{ input = %a; range = %a }" Parse_input.pp input Opt_range.pp range
  ;;

  let ( = ) a b = Parse_input.(a.input = b.input) && Opt_range.(a.range = b.range)
end

module Located = struct
  type t =
    | Source_located of Source_code_position.t
    | Parse_located of Parse_located.t

  let ( = ) a b =
    match a, b with
    | Source_located a, Source_located b -> Source_code_position.(a = b)
    | Parse_located a, Parse_located b -> Parse_located.(a = b)
    | _, _ -> false
  ;;

  let pp ppf = function
    | Source_located { pos_fname; pos_lnum; pos_cnum; _ } ->
      Fmt.pf ppf "%s:%d:%d" pos_fname pos_lnum pos_cnum
    | Parse_located opt_range -> Parse_located.pp ppf opt_range
  ;;
end

(* A term is either written directly or computed from others *)
type t =
  | Located of Located.t
  | Calculated of Located.t * t list
  | Indexed of int

type Stdlib.Format.stag += Stag of t

let unsafe_parse_located = function
  | Located (Parse_located located) -> located
  | _ -> failwith "Expected Located (Parse_located _)"
;;

let unsafe_range t =
  let Parse_located.{ range; _ } = unsafe_parse_located t in
  range
;;

let open_stag ppf rng = Stdlib.Format.pp_open_stag ppf (Stag rng)
let close_stag ppf _ = Stdlib.Format.pp_close_stag ppf ()
let fmt_stag prov = Lvca_util.Format.fmt_stag (Stag prov)
let calculated_here here provs = Calculated (Located.Source_located here, provs)
let of_here here = Located (Located.Source_located here)

let of_range ?(input = Parse_input.Input_unknown) range =
  Located (Located.Parse_located { input; range })
;;

let rec ( = ) a b =
  match a, b with
  | Located a, Located b -> Located.(a = b)
  | Calculated (x, xs), Calculated (y, ys) -> Located.(x = y) && List.equal ( = ) xs ys
  | Indexed a, Indexed b -> Int.(a = b)
  | _, _ -> false
;;

let pp ppf = function
  | Located located -> Located.pp ppf located
  | Calculated (located, _) -> Located.pp ppf located
  | Indexed a -> Fmt.pf ppf "%i" a
;;

let stag_functions =
  Stdlib.Format.
    { mark_open_stag = (function Stag t -> Fmt.str "<%a>" pp t | _ -> "")
    ; mark_close_stag = (function Stag t -> Fmt.str "</%a>" pp t | _ -> "")
    ; print_open_stag = (fun _ -> ())
    ; print_close_stag = (fun _ -> ())
    }
;;

module Test_setup = struct
  open Stdlib.Format

  type t = formatter_stag_functions

  let setup () =
    let stag_fns = get_formatter_stag_functions () in
    set_formatter_stag_functions stag_functions;
    set_tags true;
    set_mark_tags true;
    stag_fns
  ;;

  let teardown stag_fns =
    set_formatter_stag_functions stag_fns;
    set_tags false;
    set_mark_tags false
  ;;
end

let%test_module _ =
  (module struct
    let x = Test_setup.setup ()

    let%expect_test _ =
      Stdlib.Format.open_stag (Stag (Indexed 0));
      Fmt.pr "here@.";
      Stdlib.Format.close_stag ();
      [%expect {| <0>here</0> |}]
    ;;

    let () = Test_setup.teardown x
  end)
;;
