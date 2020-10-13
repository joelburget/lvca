open Base
open Js_of_ocaml

type platform =
  | Mac
  | IOs
  | Windows
  | Android
  | Linux
  | Unknown

let get_platform () =
  let user_agent = Dom_html.window##.navigator##.userAgent |> Js.to_string in
  let platform = Dom_html.window##.navigator##.platform |> Js.to_string in

  let mac_re = Regexp.regexp "Macintosh|MacIntel|MacPPC|Mac68K" in
  let ios_re = Regexp.regexp "iPhone|iPad|iPod" in
  let windows_re = Regexp.regexp "Win32|Win64|Windows|WinCE" in
  let android_re = Regexp.regexp "Android" in
  let linux_re = Regexp.regexp "Linux" in

  let matches_re re str = match Regexp.string_match re str 0 with
    | Some _ -> true
    | None -> false
  in

  List.find_map_exn
    [ matches_re mac_re platform, Mac
    ; matches_re ios_re platform, IOs
    ; matches_re windows_re platform, Windows
    ; matches_re android_re user_agent, Android
    ; matches_re linux_re platform, Linux
    ; true, Unknown
    ]
    ~f:(fun (matches, result) -> if matches then Some result else None)

let is_enter key_evt =
  String.equal
    "Enter"
    (key_evt##.code |> Js.Optdef.to_option |> Option.value_exn |> Js.to_string)
;;

let is_meta key_evt = Js.to_bool key_evt##.metaKey
let is_shift key_evt = Js.to_bool key_evt##.shiftKey
let is_ctrl key_evt = Js.to_bool key_evt##.ctrlKey

(** Is this an enter keypress plus meta, shift, or ctrl, depending on the platform *)
let is_special_enter key_evt =
  let platform = get_platform () in
  let is_meta = is_meta key_evt in
  let is_shift = is_shift key_evt in
  let is_ctrl = is_ctrl key_evt in

  let is_platform_special = match platform with
    | Mac -> is_meta || is_shift
    | Windows -> is_ctrl || is_shift
    | Linux -> is_ctrl || is_shift
    | Unknown -> is_ctrl || is_shift || is_meta
    | IOs | Android -> false
  in

  is_enter key_evt && is_platform_special
;;

let platform_special_combo ()
  : [< Html_types.span_content_fun ] Js_of_ocaml_tyxml.Tyxml_js.To_dom.elt list option =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  match get_platform () with
  | IOs | Android -> None
  | Mac -> Some [%html{|<kbd>⌘</kbd>-<kbd>Enter</kbd> or <kbd>Shift</kbd>-<kbd>Enter</kbd>|}]
  | Windows -> Some [%html{|<kbd>⊞ Win</kbd>-<kbd>Enter</kbd> or <kbd>Shift</kbd>-<kbd>Enter</kbd>|}]
  | Linux | Unknown -> Some [%html{|<kbd>Ctrl</kbd>-<kbd>Enter</kbd> or <kbd>Shift</kbd>-<kbd>Enter</kbd>|}]
