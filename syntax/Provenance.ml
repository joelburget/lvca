open Base
open Lvca_provenance

(*
type implementation =
  { filename : string
  ; range : Range.t
  }
   *)

(* A source location is either in the implementation of LVCA itself or a term *)
module Source_location = struct
  type t = [ `Implementation of Source_code_position.t ]

  (* | Term of term *)
  let ( = ) (`Implementation a) (`Implementation b) = Source_code_position.(a = b)
end

type located = { at : Source_location.t }

(* A term is either computed from others or written directly *)
type t =
  [ `Empty
  | `Source_located of located
  | `Parse_located of Opt_range.t
  ]
(*
  | Computed of
      { from : term list
      ; at : source_location
      }
                  *)

let of_here pos = `Source_located { at = `Implementation pos }

let ( = ) a b =
  match a, b with
  | `Source_located a, `Source_located b -> Source_location.(a.at = b.at)
  | `Parse_located a, `Parse_located b -> Opt_range.(a = b)
  | `Empty, `Empty -> true
  | _, _ -> false
;;
