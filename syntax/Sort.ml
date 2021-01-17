open Base

type t =
  | Ap of string * t list
  | Name of string

let ( = ) = Caml.( = )

let rec pp : Stdlib.Format.formatter -> t -> unit =
 fun ppf -> function
  | Ap (name, args) -> Fmt.pf ppf "%s(@[%a@])" name pp_args args
  | Name name -> Fmt.pf ppf "%s" name

and pp_args ppf = function
  | [] -> ()
  | [ x ] -> Fmt.pf ppf "%a" pp x
  | x :: xs -> Fmt.pf ppf "%a; %a" pp x pp_args xs
;;

let rec to_string : t -> string = function
  | Ap (name, args) ->
    Printf.sprintf "%s(%s)" name (args |> List.map ~f:to_string |> String.concat)
  | Name name -> name
;;

let rec instantiate arg_mapping = function
  | Name name ->
    (match Map.find arg_mapping name with None -> Name name | Some sort' -> sort')
  | Ap (name, args) -> Ap (name, List.map args ~f:(instantiate arg_mapping))
;;

let rec of_term tm =
  match tm with
  | Nominal.Var (_, name) -> Ok (Name name)
  | Operator (_, name, args) ->
    args
    |> List.map ~f:(function Scope ([], [ arg ]) -> of_term arg | _ -> Error tm)
    |> Result.all
    |> Result.map ~f:(fun args' -> Ap (name, args'))
  | _ -> Error tm
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  type sort = t

  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  let semi = char ';'

  let t : sort Parsers.t =
    fix (fun sort ->
        identifier
        >>== fun ~pos ident ->
        choice
          [ (parens (sep_by semi sort) >>| fun sorts -> Ap (ident, sorts))
          ; return ~pos (Name ident)
          ])
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

    let tm = Ap ("tm", [])

    let%test_unit _ = assert (Caml.(parse_with Parse.t "tm()" = tm))
  end)
;;
