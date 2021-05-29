open Base
open Brr
open Prelude

type platform =
  | Mac
  | IOs
  | Windows
  | Android
  | Linux
  | Unknown

let get_platform () =
  let user_agent = Jstr.to_string Navigator.user_agent in
  let platform = Jstr.to_string Navigator.platform in
  List.find_map_exn
    String.
      [ is_substring platform ~substring:"Mac" (* Macintosh|MacIntel|MacPPC|Mac68K *), Mac
      ; is_substring platform ~substring:"iPhone", IOs
      ; is_substring platform ~substring:"iPad", IOs
      ; is_substring platform ~substring:"iPod", IOs
      ; is_substring platform ~substring:"Win" (* Win32|Win64|Windows|WinCE *), Windows
      ; is_substring user_agent ~substring:"Android", Android
      ; is_substring platform ~substring:"Linux", Linux
      ; true, Unknown
      ]
    ~f:(fun (matches, result) -> if matches then Some result else None)
;;

let is_enter key_evt = String.(key_evt |> Ev.Keyboard.key |> Jstr.to_string = "Enter")

(** Is this an enter keypress plus meta, shift, or ctrl, depending on the platform *)
let is_special_enter key_evt =
  let platform = get_platform () in
  let is_meta = Ev.Keyboard.meta_key key_evt in
  let is_shift = Ev.Keyboard.shift_key key_evt in
  let is_ctrl = Ev.Keyboard.ctrl_key key_evt in
  let is_platform_special =
    match platform with
    | Mac -> is_meta || is_shift
    | Windows -> is_ctrl || is_shift
    | Linux -> is_ctrl || is_shift
    | Unknown -> is_ctrl || is_shift || is_meta
    | IOs | Android -> false
  in
  is_enter key_evt && is_platform_special
;;

let platform_special_combo () : El.t list option =
  let txt str = El.txt (Jstr.v str) in
  let kbd str = El.kbd [ txt str ] in
  let template special =
    Some
      [ kbd special; txt "-"; kbd "Enter"; txt " or "; kbd "Shift"; txt "-"; kbd "Enter" ]
  in
  match get_platform () with
  | IOs | Android -> None
  | Mac -> template "⌘"
  | Windows -> template "⊞ Win"
  | Linux | Unknown -> template "Ctrl"
;;
