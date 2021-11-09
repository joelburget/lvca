open Base
open Lvca_util
module ISet = Lvca_util.Int.Set

type t =
  | Ap of Provenance.t * string * ap_list (** A higher-kinded sort can be applied *)
  | Name of Provenance.t * string

and ap_list =
  | Nil of Provenance.t
  | Cons of Provenance.t * t * ap_list

let mk_Ap ?(provenance = Provenance.of_here [%here]) name args =
  Ap (provenance, name, args)
;;

let mk_Name ?(provenance = Provenance.of_here [%here]) name = Name (provenance, name)
let mk_Nil ?(provenance = Provenance.of_here [%here]) () = Nil provenance
let mk_Cons ?(provenance = Provenance.of_here [%here]) x xs = Cons (provenance, x, xs)

let rec equivalent ?(info_eq = fun _ _ -> true) s1 s2 =
  match s1, s2 with
  | Ap (i1, name1, ts1), Ap (i2, name2, ts2) ->
    info_eq i1 i2 && String.(name1 = name2) && equal_list ~info_eq ts1 ts2
  | Name (i1, name1), Name (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false

and equal_list ~info_eq l1 l2 =
  match l1, l2 with
  | Nil i1, Nil i2 -> info_eq i1 i2
  | Cons (i1, x, xs), Cons (i2, y, ys) ->
    info_eq i1 i2 && equivalent ~info_eq x y && equal_list ~info_eq xs ys
  | _, _ -> false
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )
let info = function Ap (i, _, _) | Name (i, _) -> i

module Ap_list = struct
  let rec to_list = function Nil _ -> [] | Cons (_, x, xs) -> x :: to_list xs

  let rec of_list = function
    | [] -> Nil (Provenance.of_here [%here])
    | x :: xs -> Cons (Provenance.of_here [%here], x, of_list xs)
  ;;

  let rec map ~f = function Nil i -> Nil i | Cons (i, x, xs) -> Cons (i, f x, map ~f xs)
  let info = function Nil info | Cons (info, _, _) -> info
end

let pp' ppf sort =
  let rec pp' need_parens ppf = function
    | Ap (_, name, args) ->
      let args = Ap_list.to_list args in
      if need_parens
      then Fmt.pf ppf "@[(%s %a)@]" name Fmt.(list (pp' true) ~sep:sp) args
      else Fmt.pf ppf "@[%s %a@]" name Fmt.(list (pp' true) ~sep:sp) args
    | Name (_, name) -> Fmt.string ppf name
  in
  pp' false ppf sort
;;

let pp ppf sort =
  let info = info sort in
  Provenance.open_stag ppf info;
  pp' ppf sort;
  Provenance.close_stag ppf info
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
      | Name (info, name) :: args -> return ~range (Ap (info, name, Ap_list.of_list args))
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
    let abc = mk_Ap "a" (Ap_list.of_list [ mk_Name "b"; mk_Name "c" ])

    let abcd =
      mk_Ap
        "a"
        (Ap_list.of_list [ mk_Ap "b" (Ap_list.of_list [ mk_Name "c" ]); mk_Name "d" ])
    ;;

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
