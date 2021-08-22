open Base
open Lvca_syntax
open Lvca_util
open Lvca_provenance

module Content_store = struct
  (** Mapping from hash to content *)
  type t = (string, string Commented.t Nominal.Term.t) Hashtbl.t

  let singleton : t = Hashtbl.create (module String)
  let find = Hashtbl.find singleton
end

module Tag_store = struct
  (** Mapping from tag to hash *)
  type t = (string, string) Hashtbl.t

  let singleton : t = Hashtbl.create (module String)
  let find = Hashtbl.find singleton
end

let add_document ~slug ~blob =
  let doc =
    blob
    |> Lvca_languages.Document.parse
    |> Lvca_languages.Document.Lang.Doc.to_nominal
    |> Nominal.Term.map_info ~f:(fun _ -> Commented.none)
  in
  let hash = Nominal.Term.hash doc in
  Hashtbl.set Content_store.singleton ~key:hash ~data:doc;
  Hashtbl.set Tag_store.singleton ~key:slug ~data:hash
;;

let () =
  add_document
    ~slug:"make-code-review-easier"
    ~blob:[%blob "md/make-code-review-easier.md"];
  add_document ~slug:"finding-terms" ~blob:[%blob "md/finding-terms.md"]
;;

let find tag =
  let open Option.Let_syntax in
  let%bind addr = Hashtbl.find Tag_store.singleton tag in
  Hashtbl.find Content_store.singleton addr
;;
