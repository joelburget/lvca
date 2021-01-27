open Base

type 'info t =
  | Ap of 'info * string * 'info t list
  | Name of 'info * string

let rec equal info_eq s1 s2 =
  match s1, s2 with
  | Ap (i1, name1, s1), Ap (i2, name2, s2) ->
    info_eq i1 i2 && String.(name1 = name2) && List.equal (equal info_eq) s1 s2
  | Name (i1, name1), Name (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false
;;

let rec pp : Stdlib.Format.formatter -> _ t -> unit =
 fun ppf -> function
  | Ap (_, name, args) -> Fmt.pf ppf "%s @[%a@]" name pp_args args
  | Name (_, name) -> Fmt.pf ppf "%s" name

and pp_args ppf = function
  | [] -> ()
  | [ x ] -> Fmt.pf ppf "%a" pp x
  | x :: xs -> Fmt.pf ppf "%a %a" pp x pp_args xs
;;

let rec to_string : _ t -> string = function
  | Ap (_, name, args) ->
    Printf.sprintf "%s(%s)" name (args |> List.map ~f:to_string |> String.concat)
  | Name (_, name) -> name
;;

let rec instantiate arg_mapping = function
  | Name (info, name) ->
    (match Map.find arg_mapping name with
    | None -> Name (info, name)
    | Some sort' -> sort')
  | Ap (info, name, args) -> Ap (info, name, List.map args ~f:(instantiate arg_mapping))
;;

let rec of_term tm =
  match tm with
  | Nominal.Var (info, name) -> Ok (Name (info, name))
  | Operator (info, name, args) ->
    args
    |> List.map ~f:(function Scope ([], [ arg ]) -> of_term arg | _ -> Error tm)
    |> Result.all
    |> Result.map ~f:(fun args' -> Ap (info, name, args'))
  | _ -> Error tm
;;

let rec map_info ~f = function
  | Name (info, name) -> Name (f info, name)
  | Ap (info, name, args) -> Ap (f info, name, List.map args ~f:(map_info ~f))
;;

let erase_info sort = map_info ~f:(fun _ -> ()) sort

module Parse (Comment : ParseUtil.Comment_int) = struct
  type 'info sort = 'info t

  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  let t : OptRange.t sort Parsers.t =
    fix (fun sort ->
        let atomic_sort =
          choice
            [ parens sort; (identifier >>|| fun ~pos ident -> Name (pos, ident), pos) ]
        in
        many1 atomic_sort
        >>== fun ~pos atoms ->
        match atoms with
        | Ap _ :: _ ->
          fail
            "Higher-order sorts are not allowed. The head of a sort application must be \
             concrete"
        | [ Name (pos, name) ] -> return ~pos (Name (pos, name))
        | Name (_, name) :: args -> return ~pos (Ap (pos, name, args))
        | [] -> assert false)
  ;;
end

let%test_module "Sort_Parser" =
  (module struct
    module Parse = Parse (ParseUtil.NoComment)

    let parse_with : 'a ParseUtil.t -> string -> 'a =
     fun p str ->
      match Angstrom.parse_string ~consume:All p str with
      | Ok (t, _pos) -> t
      | Error msg -> failwith msg
   ;;

    let a = Name ((), "a")
    let abc = Ap ((), "a", [ Name ((), "b"); Name ((), "c") ])
    let abcd = Ap ((), "a", [ Ap ((), "b", [ Name ((), "c") ]); Name ((), "d") ])
    let ( = ) = equal Unit.( = )

    let%test_unit _ = assert (parse_with Parse.t "a" |> erase_info = a)
    let%test_unit _ = assert (parse_with Parse.t "a b c" |> erase_info = abc)
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
      [%expect {| a b c d |}]
    ;;
  end)
;;
