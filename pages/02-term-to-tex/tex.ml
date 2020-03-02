open Core_kernel
open Lvca
open Result.Let_syntax

let abstract_syntax =
  {|
letter :=
  | alpha
  | beta
  | gamma
  | delta
  | epsilon

delimiter :=
  | lparen
  | rparen
  | lbrack
  | rbrack
  | lbrace
  | rbrace
  | langle
  | rangle

atom :=
  | letter(letter)
  | delimiter(delimiter)
|}
;;

type letter =
  | Alpha
  | Beta
  | Gamma
  | Delta
  | Epsilon

(* TODO *)

let letter_to_string = function
  | Alpha -> {|\Alpha|}
  | Beta -> {|\Beta|}
  | Gamma -> {|\Gamma|}
  | Delta -> {|\Delta|}
  | Epsilon -> {|\Epsilon|}
;;

let letter_of_term : NonBinding.term -> (letter, string) Result.t = failwith "TODO"

type delimiter =
  | Lparen
  | Rparen
  | Lbrack
  | Rbrack
  | Lbrace
  | Rbrace
  | Langle
  | Rangle

let delimiter_to_string = function
  | Lparen -> {|\lparen|}
  | Rparen -> {|\rparen|}
  | Lbrack -> {|\lbrack|}
  | Rbrack -> {|\rbrack|}
  | Lbrace -> {|\lbrace|}
  | Rbrace -> {|\rbrace|}
  | Langle -> {|\langle|}
  | Rangle -> {|\rangle|}
;;

type atom =
  | Letter of letter
  | Delimiter of delimiter

let atom_to_string = function
  | Letter letter -> letter_to_string letter
  | Delimiter delimiter -> delimiter_to_string delimiter
;;

let atom_of_term = function
  | NonBinding.Operator ("letter", [ tm ]) ->
    let%map letter = letter_of_term tm in
    Letter letter
  | _ -> Error "Tex.atom_of_term: expected a sequence of atoms"
;;

type t = atom list

let to_string = String.concat ~sep:" "

let of_term : NonBinding.term -> (t, string) Result.t = function
  | Sequence atoms -> atoms |> List.map ~f:atom_of_term |> Result.all
  | _ -> Error "Tex.of_term: expected a sequence of atoms"
;;
