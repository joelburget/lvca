open Base
module ISet = Lvca_util.Int.Set

type 'info t =
  | Ap of 'info * string * 'info t list
  | Name of 'info * string

module Plain = struct
  type t =
    | Ap of string * t list
    | Name of string
end

let rec of_plain plain =
  match plain with
  | Plain.Ap (name, ts) -> Ap (plain, name, List.map ts ~f:of_plain)
  | Plain.Name name -> Name (plain, name)
;;

let rec to_plain = function
  | Ap (_, name, ts) -> Plain.Ap (name, List.map ts ~f:to_plain)
  | Name (_, name) -> Plain.Name name
;;

let rec equal info_eq s1 s2 =
  match s1, s2 with
  | Ap (i1, name1, s1), Ap (i2, name2, s2) ->
    info_eq i1 i2 && String.(name1 = name2) && List.equal (equal info_eq) s1 s2
  | Name (i1, name1), Name (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false
;;

let info = function Ap (i, _, _) | Name (i, _) -> i

let pp ppf sort =
  let rec pp need_parens ppf = function
    | Ap (_, name, args) ->
      if need_parens
      then Fmt.pf ppf "@[(%s %a)@]" name Fmt.(list (pp true) ~sep:sp) args
      else Fmt.pf ppf "@[%s %a@]" name Fmt.(list (pp true) ~sep:sp) args
    | Name (_, name) -> Fmt.string ppf name
  in
  pp false ppf sort
;;

let pp_generic ~open_loc ~close_loc ppf sort =
  let info = info sort in
  open_loc ppf info;
  pp ppf sort;
  close_loc ppf info
;;

let rec instantiate arg_mapping = function
  | Name (info, name) ->
    (match Map.find arg_mapping name with
    | None -> Name (info, name)
    | Some sort' -> sort')
  | Ap (info, name, args) -> Ap (info, name, List.map args ~f:(instantiate arg_mapping))
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

(*
let rec of_term tm =
  match tm with
  | Nominal.Var (info, name) -> Ok (Name (info, name))
  | Operator (info, name, args) ->
    args
    |> List.map ~f:(function Scope ([], arg) -> of_term arg | _ -> Error tm)
    |> Result.all
    |> Result.map ~f:(fun args' -> Ap (info, name, args'))
  | _ -> Error tm
;;
*)

let rec map_info ~f = function
  | Name (info, name) -> Name (f info, name)
  | Ap (info, name, args) -> Ap (f info, name, List.map args ~f:(map_info ~f))
;;

let erase_info sort = map_info ~f:(fun _ -> ()) sort

(** Split a sort into a name and its arguments. *)
let split = function Name (_, name) -> name, [] | Ap (_, name, args) -> name, args

module Parse = struct
  open Lvca_parsing

  let t =
    fix (fun sort ->
        let atomic_sort =
          choice
            [ parens sort
            ; (identifier
              >>|| fun ParseResult.{ value; range } ->
              { value = Name (range, value); range })
            ]
        in
        many1 atomic_sort
        >>== fun ParseResult.{ value = atoms; range; _ } ->
        match atoms with
        (* A single ap is just parenthesized. An ap applied to things is a problem. *)
        | [ (Ap _ as atom) ] -> return ~range atom
        | Ap _ :: _ ->
          fail
            "Higher-order sorts are not allowed. The head of a sort application must be \
             concrete"
        | [ (Name _ as value) ] -> return ~range value
        | Name (_, name) :: args -> return ~range (Ap (range, name, args))
        | [] -> assert false)
  ;;
end

let%test_module "Sort_Parser" =
  (module struct
    let parse_with parser str =
      match Lvca_parsing.parse_string parser str with
      | Ok value -> value
      | Error msg -> failwith msg
    ;;

    let a = Name ((), "a")
    let abc = Ap ((), "a", [ Name ((), "b"); Name ((), "c") ])
    let abcd = Ap ((), "a", [ Ap ((), "b", [ Name ((), "c") ]); Name ((), "d") ])
    let ( = ) = equal Unit.( = )

    let%test_unit _ = assert (parse_with Parse.t "a" |> erase_info = a)
    let%test_unit _ = assert (parse_with Parse.t "(a)" |> erase_info = a)
    let%test_unit _ = assert (parse_with Parse.t "a b c" |> erase_info = abc)
    let%test_unit _ = assert (parse_with Parse.t "(a b c)" |> erase_info = abc)
    let%test_unit _ = assert (parse_with Parse.t "a (b c) d" |> erase_info = abcd)

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
