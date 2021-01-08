open Base
open Brr
open Brr_note

let main ?d ?at = El.v ?d ?at (Jstr.v "main")

let class' str = At.class' (Jstr.v str)

let classes str = str
  |> String.split ~on:' '
  |> List.map ~f:(fun cls -> At.class' (Jstr.v cls))

let inputmode str = At.v (Jstr.v "inputmode") (Jstr.v str)

let txt str = El.txt (Jstr.v str)

module Navigator = struct
  include Brr.Navigator
  let user_agent = Jv.Jstr.get (Navigator.to_jv G.navigator) "userAgent"
  let platform = Jv.Jstr.get (Navigator.to_jv G.navigator) "platform"
end

module Window = struct
  include Brr.Window

  let get_selection w = Jv.call w "getSelection" [||]
end

module Selection : sig
  type t
  val to_jstr : t -> Jstr.t
end = struct
  type t = Jv.t
  let to_jstr s = Jv.to_jstr (Jv.call s "toString" [||])
end

let selection_start = El.Prop.int (Jstr.v "selectionStart")
let selection_end = El.Prop.int (Jstr.v "selectionEnd")

let mk_reactive cons ?d ?at s =
  let result = cons ?d ?at [] in
  let () = Elr.def_children result s in
  result

let mk_reactive' cons ?d ?at s =
  mk_reactive cons ?d ?at (s |> Note.S.map (fun elem -> [elem]))
