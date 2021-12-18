open Base
open Lvca_util
module ISet = Lvca_util.Int.Set

type t =
  | Ap of Provenance.t * string * t list (** A higher-kinded sort can be applied *)
  | Name of Provenance.t * string

let mk_Ap ?(provenance = Provenance.of_here [%here]) name args =
  Ap (provenance, name, args)
;;

let mk_Name ?(provenance = Provenance.of_here [%here]) name = Name (provenance, name)

let rec equivalent ?(info_eq = fun _ _ -> true) s1 s2 =
  match s1, s2 with
  | Ap (i1, name1, ts1), Ap (i2, name2, ts2) ->
    info_eq i1 i2 && String.(name1 = name2) && List.equal (equivalent ~info_eq) ts1 ts2
  | Name (i1, name1), Name (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )
let info = function Ap (i, _, _) | Name (i, _) -> i

let pp ppf sort =
  let open Fmt in
  let rec pp need_parens ppf sort =
    let info = info sort in
    Provenance.open_stag ppf info;
    (match sort with
    | Ap (_, name, args) ->
      let list = list (pp true) ~sep:sp in
      if need_parens
      then pf ppf "@[(%s %a)@]" name list args
      else pf ppf "@[%s %a@]" name list args
    | Name (_, name) -> string ppf name);
    Provenance.close_stag ppf info
  in
  pp false ppf sort
;;

let rec instantiate arg_mapping = function
  | Name (info, name) ->
    (match Map.find arg_mapping name with
    | None -> Name (info, name)
    | Some sort' -> sort')
  | Ap (info, name, ts) -> Ap (info, name, List.map ~f:(instantiate arg_mapping) ts)
;;

let update_env env name n =
  Map.update env name ~f:(function None -> ISet.singleton n | Some set -> Set.add set n)
;;

let rec kind_check env sort =
  match sort with
  | Name (_, name) -> update_env env name 0
  | Ap (_, name, args) ->
    let env = List.fold args ~init:env ~f:kind_check in
    update_env env name (List.length args)
;;

(** Split a sort into a name and its arguments. *)
let split = function Name (_, name) -> name, [] | Ap (_, name, ts) -> name, ts

let name = split >> fst

let parse reserved_word =
  let open Lvca_parsing in
  let open C_comment_parser in
  fix (fun sort ->
      let atomic_sort =
        choice
          ~failure_msg:"looking for parens or an identifier"
          [ parens sort
          ; (lower_identifier reserved_word
            >>~ fun loc value -> Name (Provenance.of_range loc, value))
          ]
      in
      many1 atomic_sort
      >>== fun Parse_result.{ value = atoms; range } ->
      match atoms with
      (* A single ap is just parenthesized. An ap applied to things is a problem. *)
      | [ (Ap _ as atom) ] -> return ~range atom
      | Ap _ :: _ ->
        fail
          "Higher-order sorts are not allowed. The head of a sort application must be \
           concrete"
      | [ (Name _ as value) ] -> return ~range value
      | Name (info, name) :: args -> return ~range (Ap (info, name, args))
      | [] -> assert false)
;;

let%test_module "Sort_Parser" =
  (module struct
    let parse_with parser str =
      match Lvca_parsing.parse_string parser str with
      | Ok value -> value
      | Error msg -> failwith msg
    ;;

    let a = mk_Name "a"
    let abc = mk_Ap "a" [ mk_Name "b"; mk_Name "c" ]
    let abcd = mk_Ap "a" [ mk_Ap "b" [ mk_Name "c" ]; mk_Name "d" ]
    let ( = ) = equivalent ~info_eq:(fun _ _ -> true)
    let parse = parse String.Set.empty

    let%test_unit _ = assert (parse_with parse "a" = a)
    let%test_unit _ = assert (parse_with parse "(a)" = a)
    let%test_unit _ = assert (parse_with parse "a b c" = abc)
    let%test_unit _ = assert (parse_with parse "(a b c)" = abc)
    let%test_unit _ = assert (parse_with parse "a (b c) d" = abcd)

    let%expect_test _ =
      Fmt.pr "%a" pp a;
      [%expect {| a |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp abc;
      [%expect {| a b c |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp abcd;
      [%expect {| a (b c) d |}]
    ;;
  end)
;;
