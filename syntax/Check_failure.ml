open Base
module Format = Stdlib.Format

module Frame = struct
  type ('info, 'term) t =
    { term : 'term
    ; sort : 'info Sort.t
    }

  let pp term_pp ppf { term; sort } =
    Fmt.pf ppf "- @[%a,@ sort: %a@]" term_pp term Sort.pp sort
  ;;
end

type ('info, 'term) t =
  { message : string
  ; stack : ('info, 'term) Frame.t list
  }

let err message = { message; stack = [] }

let map_frame_terms ~f { message; stack } =
  let stack =
    stack |> List.map ~f:(fun { term; sort } -> Frame.{ term = f term; sort })
  in
  { message; stack }
;;

let pp term_pp ppf { message; stack } =
  Fmt.string ppf message;
  if List.length stack > 0
  then (
    Format.pp_force_newline ppf ();
    Fmt.pf ppf "stack:";
    List.iter stack ~f:(fun frame ->
        Format.pp_force_newline ppf ();
        Frame.pp term_pp ppf frame))
;;
