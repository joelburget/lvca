type t =
  | PrimInteger of Bigint.t
  | PrimString of string

let to_string = function
  | PrimInteger i -> Bigint.to_string i
  | PrimString str -> "\"" ^ Caml.String.escaped str ^ "\""
;;

let (=) p1 p2 =
  match p1, p2 with
  | PrimInteger i1, PrimInteger i2 -> Bigint.(i1 = i2) [@warning "-44"]
  | PrimString s1, PrimString s2 -> s1 = s2
  | _ -> false
;;

let pp : Format.formatter -> t -> unit
  = fun ppf -> function
  | PrimInteger i -> Format.fprintf ppf "%s" (Bigint.to_string i)
  | PrimString s -> Format.fprintf ppf "\"%s\"" s
;;
