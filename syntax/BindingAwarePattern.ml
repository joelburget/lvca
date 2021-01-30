open Base
module Util = Lvca_util
module String = Util.String
module Format = Stdlib.Format

type ('info, 'prim) t =
  | Operator of 'info * string * ('info, 'prim) scope list
  | Primitive of 'info * 'prim
  | Var of 'info * string
  | Ignored of 'info * string

and ('info, 'prim) scope = Scope of ('info * string) list * ('info, 'prim) t

let rec equal info_eq prim_eq t1 t2 =
  match t1, t2 with
  | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
    info_eq i1 i2
    && String.(name1 = name2)
    && List.equal (scope_eq info_eq prim_eq) scopes1 scopes2
  | Primitive (i1, p1), Primitive (i2, p2) -> info_eq i1 i2 && prim_eq p1 p2
  | Var (i1, name1), Var (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | Ignored (i1, name1), Ignored (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false

and scope_eq info_eq prim_eq (Scope (names1, t1)) (Scope (names2, t2)) =
  List.equal (Lvca_util.Tuple2.equal info_eq String.( = )) names1 names2
  && equal info_eq prim_eq t1 t2
;;

let rec vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats |> List.map ~f:vars_of_scope |> Set.union_list (module String)
  | Primitive _ -> String.Set.empty
  | Var (_, name) -> String.Set.of_list [ name ]
  | Ignored _ -> String.Set.empty

and vars_of_scope (Scope (vars, t)) =
  let vars = vars |> List.map ~f:snd in
  Set.union (String.Set.of_list vars) (vars_of_pattern t)
;;

let rec list_vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats
    (* simpler, morally equivalent: List.concat_map ~f:(List.concat_map
       ~f:list_vars_of_scope) *)
    |> List.map ~f:list_vars_of_scope
    |> List.concat_no_order
  | Primitive _ -> []
  | Var (loc, name) -> [ loc, name ]
  | Ignored _ -> []

and list_vars_of_scope (Scope (names, pat)) =
  let names' = List.map names ~f:snd in
  let pat_vars =
    pat
    |> list_vars_of_pattern
    |> List.filter ~f:(fun (_i, name) -> not (List.mem names' name ~equal:String.( = )))
  in
  List.unordered_append names pat_vars
;;

let rec map_info ~f = function
  | Operator (info, tag, subpats) ->
    Operator (f info, tag, subpats |> List.map ~f:(scope_map_info ~f))
  | Primitive (info, prim) -> Primitive (f info, prim)
  | Var (info, name) -> Var (f info, name)
  | Ignored (info, name) -> Ignored (f info, name)

and scope_map_info ~f (Scope (names, t)) =
  Scope (List.map names ~f:(fun (i, name) -> f i, name), map_info ~f t)
;;

let erase pat = map_info ~f:(fun _ -> ()) pat

let info = function
  | Operator (i, _, _) | Primitive (i, _) | Var (i, _) | Ignored (i, _) -> i
;;

let any, list, string, semi, pf = Fmt.(any, list, string, semi, pf)

let rec pp_generic ~open_loc ~close_loc ~pp_prim ppf tm =
  open_loc ppf (info tm);
  (match tm with
  | Operator (_, tag, subtms) ->
    pf
      ppf
      "@[<hv>%s(%a)@]"
      tag
      (list ~sep:semi (pp_scope_generic ~open_loc ~close_loc ~pp_prim))
      subtms
  | Var (_, v) -> pf ppf "%a" string v
  | Ignored (_, v) -> pf ppf "_%a" string v
  | Primitive (_, p) -> pf ppf "%a" pp_prim p);
  close_loc ppf (info tm)

and pp_scope_generic ~open_loc ~close_loc ~pp_prim ppf (Scope (bindings, body)) =
  let pp_body = pp_generic ~open_loc ~close_loc ~pp_prim in
  let pp_binding ppf (info, name) =
    open_loc ppf info;
    string ppf name;
    close_loc ppf info
  in
  match bindings with
  | [] -> pp_body ppf body
  | _ -> pf ppf "%a.@ %a" (list ~sep:(any ".@ ") pp_binding) bindings pp_body body
;;

let pp pp_prim ppf tm =
  pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ~pp_prim ppf tm
;;

let pp_scope pp_prim ppf tm =
  pp_scope_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ~pp_prim ppf tm
;;

let pp_range pp_prim ppf tm =
  pp_generic ~open_loc:OptRange.open_stag ~close_loc:OptRange.close_stag ~pp_prim ppf tm
;;

let pp_scope_range pp_prim ppf tm =
  pp_scope_generic
    ~open_loc:OptRange.open_stag
    ~close_loc:OptRange.close_stag
    ~pp_prim
    ppf
    tm
;;

let pp_ranges pp_prim ppf tm =
  pp_generic
    ~open_loc:(fun ppf info -> Stdlib.Format.pp_open_stag ppf (SourceRanges.Stag info))
    ~close_loc:(fun ppf _loc -> Stdlib.Format.pp_close_stag ppf ())
    ~pp_prim
    ppf
    tm
;;

let pp_scope_ranges pp_prim ppf tm =
  pp_scope_generic
    ~open_loc:(fun ppf info -> Stdlib.Format.pp_open_stag ppf (SourceRanges.Stag info))
    ~close_loc:(fun ppf _loc -> Stdlib.Format.pp_close_stag ppf ())
    ~pp_prim
    ppf
    tm
;;

let rec select_path ~path pat =
  match path with
  | [] -> Ok pat
  | i :: path ->
    (match pat with
    | Var _ | Ignored _ | Primitive _ -> Error "TODO: message"
    | Operator (_, _, scopes) ->
      (match List.nth scopes i with
      | None -> Error "TODO: message"
      | Some (Scope (_vars, pat)) -> select_path ~path pat))
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  type ('info, 'prim) pattern = ('info, 'prim) t

  module Parsers = ParseUtil.Mk (Comment)
  module Primitive = Primitive.Parse (Comment)

  let to_var = function
    | Var (i, name) -> Ok (i, name)
    | Ignored (i, name) -> Ok (i, "_" ^ name)
    | _ -> Error "not a var"
  ;;

  type 'prim tm_or_sep =
    | Tm of (OptRange.t, 'prim) pattern
    | Sep of char

  let t : 'prim ParseUtil.t -> (OptRange.t, 'prim) pattern ParseUtil.t =
   fun parse_prim ->
    let open Parsers in
    fix (fun pat ->
        let t_or_sep : 'prim tm_or_sep ParseUtil.t =
          choice
            [ (fun c -> Sep c) <$> choice [ char '.'; char ';' ]
            ; (fun tm -> Tm tm) <$> pat
            ]
        in
        (* (b11. ... b1n. t11, ... t1n; b21. ... b2n. t21, ... t2n) *)
        let accumulate
            :  OptRange.t -> string -> 'prim tm_or_sep list
            -> (OptRange.t, 'prim) pattern ParseUtil.t
          =
         fun range tag tokens ->
          (* vars encountered between '.'s, before hitting ',' / ';' *)
          let binding_queue : (OptRange.t, 'prim) pattern Queue.t = Queue.create () in
          (* scopes encountered *)
          let scope_queue : (OptRange.t, 'prim) scope Queue.t = Queue.create () in
          let rec go = function
            | [] -> return ~pos:range (Operator (range, tag, Queue.to_list scope_queue))
            | Tm tm :: Sep '.' :: rest ->
              Queue.enqueue binding_queue tm;
              go rest
            | Tm tm :: Sep ';' :: rest (* Note: allow trailing ';' *)
            | Tm tm :: ([] as rest) ->
              (match
                 binding_queue |> Queue.to_list |> List.map ~f:to_var |> Result.all
               with
              | Error _ ->
                fail "Unexpectedly found a variable binding in pattern position"
              | Ok binders ->
                Queue.clear binding_queue;
                Queue.enqueue scope_queue (Scope (binders, tm));
                go rest)
            | _ -> fail "Malformed pattern"
          in
          go tokens
        in
        pos
        >>= fun p1 ->
        choice
          [ (parse_prim
            >>|| fun ~pos prim -> Primitive (OptRange.extend_to pos p1, prim), pos)
          ; (identifier
            >>= fun ident ->
            choice
              [ (parens (many t_or_sep)
                >>= fun tokens ->
                pos >>= fun p2 -> accumulate (OptRange.mk p1 p2) ident tokens)
              ; (pos >>| fun p2 -> Var (OptRange.mk p1 p2, ident))
              ])
          ])
    <?> "binding-aware pattern"
 ;;

  let whitespace_t parse_prim = Parsers.(junk *> t parse_prim)
end

let%test_module "Parsing" =
  (module struct
    module Parser = Parse (ParseUtil.NoComment)
    module PrimParser = Primitive.Parse (ParseUtil.NoComment)

    let () =
      Format.set_formatter_stag_functions Range.stag_functions;
      Format.set_tags true;
      Format.set_mark_tags true
    ;;

    let print_parse tm =
      match ParseUtil.parse_string (Parser.t PrimParser.t) tm with
      | Ok pat -> Fmt.pr "%a\n%a" (pp Primitive.pp) pat (pp_range Primitive.pp) pat
      | Error msg -> Fmt.pr "failed: %s\n" msg
    ;;

    let%expect_test _ =
      print_parse {|"str"|};
      [%expect {|
      "str"
      <0-5>"str"</0-5>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a()|};
      [%expect {|
      a()
      <0-3>a()</0-3>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b)|};
      [%expect {|
      a(b)
      <0-4>a(<2-3>b</2-3>)</0-4>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c)|};
      (*0123456*)
      [%expect {|
      a(b; c)
      <0-6>a(<2-3>b</2-3>; <4-5>c</4-5>)</0-6>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c;d;e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b; c; d; e)
      <0-11>a(<2-3>b</2-3>; <4-5>c</4-5>; <6-7>d</6-7>; <8-9>e</8-9>)</0-11>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b.c.d;e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b. c. d; e)
      <0-11>a(<2-3>b</2-3>. <4-5>c</4-5>. <6-7>d</6-7>; <8-9>e</8-9>)</0-11>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;;c)|};
      [%expect {| failed: : end_of_input |}]
    ;;
  end)
;;
