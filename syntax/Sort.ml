open Base
open Lvca_util
module ISet = Lvca_util.Int.Set

type 'info t =
  | Ap of 'info * string * 'info ap_list (** A higher-kinded sort can be applied *)
  | Name of 'info * string

and 'info ap_list =
  | Nil of 'info
  | Cons of 'info * 'info t * 'info ap_list

module Plain = struct
  type t =
    | Ap of string * t list
    | Name of string
end

let rec equal ~info_eq s1 s2 =
  match s1, s2 with
  | Ap (i1, name1, ts1), Ap (i2, name2, ts2) ->
    info_eq i1 i2 && String.(name1 = name2) && equal_list ~info_eq ts1 ts2
  | Name (i1, name1), Name (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false

and equal_list ~info_eq l1 l2 =
  match l1, l2 with
  | Nil i1, Nil i2 -> info_eq i1 i2
  | Cons (i1, x, xs), Cons (i2, y, ys) ->
    info_eq i1 i2 && equal ~info_eq x y && equal_list ~info_eq xs ys
  | _, _ -> false
;;

let info = function Ap (i, _, _) | Name (i, _) -> i

let rec map_info ~f = function
  | Name (info, name) -> Name (f info, name)
  | Ap (info, name, ts) -> Ap (f info, name, list_map_info ~f ts)

and list_map_info ~f = function
  | Nil i -> Nil (f i)
  | Cons (i, x, xs) -> Cons (f i, map_info ~f x, list_map_info ~f xs)
;;

let erase_info sort = map_info ~f:(fun _ -> ()) sort

let rec of_plain = function
  | Plain.Ap (name, ts) -> Ap ((), name, list_of_plain ts)
  | Name name -> Name ((), name)

and list_of_plain = function
  | [] -> Nil ()
  | x :: xs -> Cons ((), of_plain x, list_of_plain xs)
;;

let rec to_plain = function
  | Ap (_, name, ts) -> Plain.Ap (name, list_to_plain ts)
  | Name (_, name) -> Name name

and list_to_plain = function
  | Nil _ -> []
  | Cons (_, x, xs) -> to_plain x :: list_to_plain xs
;;

module Ap_list = struct
  let rec to_list = function Nil _ -> [] | Cons (_, x, xs) -> x :: to_list xs

  let rec of_list ~default_info = function
    | [] -> Nil default_info
    | x :: xs -> Cons (default_info, x, of_list ~default_info xs)
  ;;

  let rec map ~f = function Nil i -> Nil i | Cons (i, x, xs) -> Cons (i, f x, map ~f xs)
end

let pp ppf sort =
  let rec pp need_parens ppf = function
    | Ap (_, name, args) ->
      let args = Ap_list.to_list args in
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
  | Ap (info, name, ts) -> Ap (info, name, Ap_list.map ~f:(instantiate arg_mapping) ts)
;;

let update_env env name n =
  Map.update env name ~f:(function None -> ISet.singleton n | Some set -> Set.add set n)
;;

let rec kind_check env sort =
  match sort with
  | Name (_, name) -> update_env env name 0
  | Ap (_, name, args) ->
    let args = Ap_list.to_list args in
    let env = List.fold args ~init:env ~f:kind_check in
    update_env env name (List.length args)
;;

(** Split a sort into a name and its arguments. *)
let split = function
  | Name (_, name) -> name, []
  | Ap (_, name, ts) -> name, Ap_list.to_list ts
;;

let parse ~comment =
  let open Lvca_parsing in
  fix (fun sort ->
      let atomic_sort =
        choice
          ~failure_msg:"looking for parens or an identifier"
          [ Ws.parens sort
          ; (Ws.identifier
            >>== fun Parse_result.{ value; range } ->
            option' comment
            >>|| fun { value = comment; _ } ->
            { value = Name (Lvca_provenance.Commented.{ range; comment }, value); range }
            )
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
      | Name (info, name) :: args ->
        return ~range (Ap (info, name, Ap_list.of_list ~default_info:info args))
      | [] -> assert false)
;;

let%test_module "Sort_Parser" =
  (module struct
    let parse_with parser str =
      match Lvca_parsing.parse_string parser str with
      | Ok value -> value
      | Error msg -> failwith msg
    ;;

    let a = Name ((), "a")

    let abc =
      Ap ((), "a", Ap_list.of_list ~default_info:() [ Name ((), "b"); Name ((), "c") ])
    ;;

    let abcd =
      Ap
        ( ()
        , "a"
        , Ap_list.of_list
            ~default_info:()
            [ Ap ((), "b", Ap_list.of_list ~default_info:() [ Name ((), "c") ])
            ; Name ((), "d")
            ] )
    ;;

    let ( = ) = equal ~info_eq:Unit.( = )
    let parse_no_comment = parse ~comment:Lvca_parsing.no_comment

    let%test_unit _ = assert (parse_with parse_no_comment "a" |> erase_info = a)
    let%test_unit _ = assert (parse_with parse_no_comment "(a)" |> erase_info = a)
    let%test_unit _ = assert (parse_with parse_no_comment "a b c" |> erase_info = abc)
    let%test_unit _ = assert (parse_with parse_no_comment "(a b c)" |> erase_info = abc)

    let%test_unit _ =
      assert (parse_with parse_no_comment "a (b c) d" |> erase_info = abcd)
    ;;

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
