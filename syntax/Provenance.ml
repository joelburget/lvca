open Base

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
  | `Located of located
  | `Todo_commented of string Lvca_provenance.Commented.t
  | `Todo_opt_range of Lvca_provenance.Opt_range.t
  ]
(*
  | Computed of
      { from : term list
      ; at : source_location
      }
                  *)

let of_here pos = `Located { at = `Implementation pos }

let ( = ) a b =
  match a, b with
  | `Located a, `Located b -> Source_location.(a.at = b.at)
  | `Empty, `Empty -> true
  | `Todo_commented a, `Todo_commented b ->
    Lvca_provenance.Commented.equal String.( = ) a b
  | `Todo_opt_range a, `Todo_opt_range b -> Lvca_provenance.Opt_range.(a = b)
  | _, _ -> false
;;
