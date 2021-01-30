open Base

type ('info, 'term) frame =
  { term : 'term
  ; sort : 'info Sort.t
  }

type ('info, 'term) t =
  { message : string
  ; stack : ('info, 'term) frame list
  }

let err message = { message; stack = [] }

let map_frame_terms ~f { message; stack } =
  let stack = stack |> List.map ~f:(fun { term; sort } -> { term = f term; sort }) in
  { message; stack }
;;

let pp frame_pp ppf { message; stack } =
  Fmt.string ppf message;
  if List.length stack > 0
  then (
    Stdlib.Format.pp_force_newline ppf ();
    Fmt.pf ppf "stack:";
    List.iter stack ~f:(fun frame ->
        Stdlib.Format.pp_force_newline ppf ();
        frame_pp ppf frame))
;;

(*
      (fun { term; sort } ->
        match term with
        | First pat ->
          Fmt.pf ppf "- @[pattern: %a,@ sort: %a@]" (Pattern.pp prim_pp) pat Sort.pp sort
        | Second tm ->
          Fmt.pf ppf "- @[term: %a,@ sort: %a@]" (Nominal.pp_term prim_pp) tm Sort.pp sort))
  *)
