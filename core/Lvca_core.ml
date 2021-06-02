(** A small "core" language. *)

open Base
open Lvca_provenance
open Lvca_syntax
module Format = Stdlib.Format
module Util = Lvca_util
module SMap = Util.String.Map

module Is_rec = struct
  type t =
    | Rec
    | No_rec

  let ( = ) x y = match x, y with Rec, Rec | No_rec, No_rec -> true | _, _ -> false
end

module Type = struct
  type 'info t =
    | Arrow of 'info t list (* TODO: or should we handle just a pair? *)
    | Sort of 'info Sort.t

  let rec map_info ~f = function
    | Arrow ts -> Arrow (List.map ~f:(map_info ~f) ts)
    | Sort sort -> Sort (Sort.map_info ~f sort)
  ;;

  let rec equal ~info_eq x y =
    match x, y with
    | Arrow xs, Arrow ys -> List.equal (equal ~info_eq) xs ys
    | Sort x, Sort y -> Sort.equal info_eq x y
    | _, _ -> false
  ;;

  let rec pp_generic ~open_loc ~close_loc ppf = function
    | Arrow ts -> Fmt.(list ~sep:(any " -> ") (pp_generic ~open_loc ~close_loc)) ppf ts
    | Sort s -> Sort.pp_generic ~open_loc ~close_loc ppf s
  ;;

  module Parse = struct
    open Lvca_parsing

    let t =
      fix (fun t ->
          let atom = parens t <|> (Sort.Parse.t >>| fun sort -> Sort sort) in
          sep_by1 (string "->") atom >>| function [ t ] -> t | ts -> Arrow ts)
      <?> "core type"
    ;;
  end
end

module Types = struct
  type 'info term =
    | Term of 'info Nominal.Term.t
    | Core_app of 'info * 'info term * 'info term list
    | Case of 'info * 'info term * 'info case_scope list (** Cases match patterns *)
    | Lambda of 'info * 'info Sort.t * 'info scope
        (** Lambdas bind variables. Patterns not allowed. *)
    | Let of 'info let_ (** Lets bind variables. Patterns not allowed. *)
    | Var of 'info * string

  and 'info let_ =
    { info : 'info
    ; is_rec : Is_rec.t
    ; tm : 'info term
    ; ty : 'info Type.t option
    ; scope : 'info scope
    }

  and 'info scope = Scope of string * 'info term

  and 'info case_scope = Case_scope of 'info Binding_aware_pattern.t * 'info term
end

type 'info t =
  { externals : (string * 'info Type.t) list
  ; defs : (string * 'info Types.term) list
  }

module Equal = struct
  let rec term ~info_eq x y =
    match x, y with
    | Types.Term x, Types.Term y -> Nominal.Term.equal ~info_eq x y
    | Core_app (x1, x2, x3), Core_app (y1, y2, y3) ->
      info_eq x1 y1 && term ~info_eq x2 y2 && List.equal (term ~info_eq) x3 y3
    | Case (x1, x2, x3), Case (y1, y2, y3) ->
      info_eq x1 y1 && term ~info_eq x2 y2 && List.equal (case_scope ~info_eq) x3 y3
    | Lambda (x1, x2, x3), Lambda (y1, y2, y3) ->
      info_eq x1 y1 && Sort.equal info_eq x2 y2 && scope ~info_eq x3 y3
    | Let x, Let y -> let_ ~info_eq x y
    | Var (x1, x2), Var (y1, y2) -> info_eq x1 y1 && String.(x2 = y2)
    | _, _ -> false

  and case_scope ~info_eq (Case_scope (x1, x2)) (Case_scope (y1, y2)) =
    Binding_aware_pattern.equal ~info_eq x1 y1 && term ~info_eq x2 y2

  and scope ~info_eq (Scope (x1, x2)) (Scope (y1, y2)) =
    String.(x1 = y1) && term ~info_eq x2 y2

  and let_ ~info_eq x y =
    info_eq x.info y.info
    && Is_rec.(x.is_rec = y.is_rec)
    && term ~info_eq x.tm y.tm
    && Option.equal (Type.equal ~info_eq) x.ty y.ty
    && scope ~info_eq x.scope y.scope
  ;;
end

module Info = struct
  let term = function
    | Types.Term tm -> Nominal.Term.info tm
    | Core_app (info, _, _)
    | Case (info, _, _)
    | Lambda (info, _, _)
    | Let { info; _ }
    | Var (info, _) ->
      info
  ;;

  let let_ Types.{ info; _ } = info
end

module Map_info = struct
  let rec term ~f = function
    | Types.Term tm -> Types.Term (Nominal.Term.map_info ~f tm)
    | Core_app (info, t1, args) ->
      Core_app (f info, term ~f t1, List.map ~f:(term ~f) args)
    | Case (info, tm, cases) ->
      Case (f info, term ~f tm, List.map cases ~f:(case_scope ~f))
    | Lambda (info, sort, scope') ->
      Lambda (f info, Sort.map_info ~f sort, scope ~f scope')
    | Let x -> Let (let_ ~f x)
    | Var (info, name) -> Var (f info, name)

  and let_ ~f { info; is_rec; tm; ty; scope = scope' } =
    { info = f info
    ; is_rec
    ; tm = term ~f tm
    ; ty = Option.map ~f:(Type.map_info ~f) ty
    ; scope = scope ~f scope'
    }

  and scope ~f (Scope (name, tm)) = Scope (name, term ~f tm)

  and case_scope ~f (Case_scope (pat, tm)) =
    Case_scope (Binding_aware_pattern.map_info ~f pat, term ~f tm)
  ;;
end

module Pp_generic = struct
  let braces, list, any, pf, sp = Fmt.(braces, list, any, pf, sp)

  (* TODO: add parse <-> pretty tests *)

  let rec term : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info Types.term Fmt.t =
   fun ~open_loc ~close_loc ppf tm ->
    let pp = term ~open_loc ~close_loc in
    open_loc ppf (Info.term tm);
    (match tm with
    | Var (_, v) -> Fmt.string ppf v
    | Term tm -> braces (Nominal.Term.pp_generic ~open_loc ~close_loc) ppf tm
    | Lambda (_, sort, Scope (name, body)) ->
      pf ppf "\\(%s : %a) ->@ %a" name (Sort.pp_generic ~open_loc ~close_loc) sort pp body
    (* TODO: parens if necessary *)
    | Core_app (_, f, args) -> pf ppf "@[<h>%a@ @[<hov>%a@]@]" pp f (list ~sep:sp pp) args
    | Case (_, arg, cases') ->
      pf
        ppf
        "@[<hv>match %a with {%t%a@ }@]"
        pp
        arg
        (* Before `|`, emit a single space if on the same line, or two when broken *)
        (Format.pp_print_custom_break ~fits:("", 1, "") ~breaks:("", 2, "| "))
        (cases ~open_loc ~close_loc)
        cases'
    | Let x -> let_ ~open_loc ~close_loc ppf x);
    close_loc ppf (Info.term tm)

  and let_
      ~open_loc
      ~close_loc
      ppf
      { info = _; is_rec; tm; ty; scope = Scope (name, body) }
    =
    let pp_ty ppf = function
      | Some ty -> pf ppf ": %a" (Type.pp_generic ~open_loc ~close_loc) ty
      | None -> ()
    in
    pf
      ppf
      "@[let %s%s%a =@ %a in@ @[%a@]@]"
      (match is_rec with Rec -> "rec " | No_rec -> "")
      name
      pp_ty
      ty
      (term ~open_loc ~close_loc)
      tm
      (term ~open_loc ~close_loc)
      body

  and cases ~open_loc ~close_loc =
    list ~sep:(any "@;<1 2>| ") (case_scope ~open_loc ~close_loc)

  and case_scope ~open_loc ~close_loc ppf (Case_scope (pat, body)) =
    pf
      ppf
      "@[%a@ -> %a@]"
      (Binding_aware_pattern.pp_generic ~open_loc ~close_loc)
      pat
      (term ~open_loc ~close_loc)
      body
  ;;
end

module Parse = struct
  open Lvca_parsing

  let reserved = Util.String.Set.of_list [ "let"; "rec"; "in"; "match"; "with" ]

  let identifier =
    identifier
    >>= fun ident ->
    if Set.mem reserved ident
    then fail (Printf.sprintf "identifier: reserved word (%s)" ident)
    else return ident
  ;;

  let make_apps : 'a Types.term list -> 'a Types.term = function
    | [] -> Util.invariant_violation ~here:[%here] "must be a nonempty list"
    | [ x ] -> x
    | f :: args as xs ->
      let pos = xs |> List.map ~f:Info.term |> Opt_range.list_range in
      Core_app (pos, f, args)
  ;;

  let term =
    fix (fun term ->
        let atomic_term =
          choice
            [ parens term
            ; (identifier
              >>|| fun { value; range } -> { value = Types.Var (range, value); range })
            ; braces Nominal.Term.Parse.t >>| (fun tm -> Types.Term tm) <?> "quoted term"
            ]
        in
        let pattern = Binding_aware_pattern.Parse.t <?> "pattern" in
        let case_line =
          lift3 (fun pat _ tm -> Types.Case_scope (pat, tm)) pattern (string "->") term
          <?> "case line"
        in
        choice
          [ lift4
              (fun (_, lam_loc) ((name, sort), parens_loc) _ body ->
                let info = Opt_range.union lam_loc parens_loc in
                Types.Lambda (info, sort, Scope (name, body)))
              (attach_pos (char '\\'))
              (attach_pos
                 (parens
                    (lift3
                       (fun ident _ sort -> ident, sort)
                       identifier
                       (char ':')
                       Sort.Parse.t)))
              (string "->")
              term
            <?> "lambda"
          ; lift4
              (fun (_let, let_pos) is_rec name ty _eq tm _in (body, body_pos) ->
                let info = Opt_range.union let_pos body_pos in
                Types.Let { info; is_rec; ty; tm; scope = Scope (name, body) })
              (attach_pos (string "let"))
              Is_rec.(option No_rec (Fn.const Rec <$> string "rec"))
              identifier
              (option None (char ':' *> Type.Parse.t >>| fun tm -> Some tm))
            <*> string "="
            <*> term
            <*> string "in"
            <*> attach_pos term
            <?> "let"
          ; lift4
              (fun (_match, match_pos) tm _with (lines, lines_pos) ->
                let pos = Opt_range.union match_pos lines_pos in
                Types.Case (pos, tm, lines))
              (attach_pos (string "match"))
              term
              (string "with")
              (attach_pos (braces (option '|' (char '|') *> sep_by (char '|') case_line)))
            <?> "match"
          ; many1 atomic_term >>| make_apps <?> "application"
          ])
    <?> "core term"
  ;;

  let external_decl =
    lift3 (fun ident _ ty -> ident, ty) identifier (string ":") Type.Parse.t
  ;;

  let def = lift3 (fun ident _ tm -> ident, tm) identifier (string ":=") term

  let t =
    lift2 (fun externals defs -> { externals; defs }) (many external_decl) (many1 def)
  ;;
end

module Term = struct
  module Kernel = struct
    type 'info t = 'info Types.term =
      | Term of 'info Nominal.Term.t
      | Core_app of 'info * 'info t * 'info t list
      | Case of 'info * 'info t * 'info Types.case_scope list
      | Lambda of 'info * 'info Sort.t * 'info Types.scope
      | Let of 'info Types.let_
      | Var of 'info * string

    let equal = Equal.term
    let map_info = Map_info.term
    let pp_generic = Pp_generic.term
    let info = Info.term
  end

  (* include Language_object.Extend (Kernel) *)
  include Kernel

  let erase = map_info ~f:(fun _ -> ())
  let pp ppf tm = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf tm

  module Parse = struct
    let t = Parse.term
  end
end

let pp_generic ~open_loc ~close_loc ppf { externals; defs } =
  let open Fmt in
  let pp_externals =
    list (pair ~sep:(any " : ") string (Type.pp_generic ~open_loc ~close_loc))
  in
  let pp_def ppf (name, sort_def) =
    pf ppf "%s := %a" name (Term.pp_generic ~open_loc ~close_loc) sort_def
  in
  pf ppf "%a@,%a" pp_externals externals (list pp_def) defs
;;

let pp ppf tm = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf tm

module Let = struct
  type 'info t = 'info Types.let_ =
    { info : 'info
    ; is_rec : Is_rec.t
    ; tm : 'info Types.term
    ; ty : 'info Type.t option
    ; scope : 'info Types.scope
    }

  let equal = Equal.let_
  let map_info = Map_info.let_
  let pp_generic = Pp_generic.let_
  let info = Info.let_
end

module Scope = struct
  type 'info t = 'info Types.scope = Scope of string * 'info Types.term

  let equal = Equal.scope
  let map_info = Map_info.scope
  (* let pp_generic = Pp_generic.scope *)
end

module Case_scope = struct
  type 'info t = 'info Types.case_scope =
    | Case_scope of 'info Binding_aware_pattern.t * 'info Types.term

  let equal = Equal.case_scope
  let map_info = Map_info.case_scope
  let pp_generic = Pp_generic.case_scope
end

type 'info env = 'info Nominal.Term.t SMap.t

let preimage _ = failwith "TODO"
let reverse _tm _cases = failwith "TODO"

type 'info check_env = 'info Lvca_syntax.Sort.t SMap.t
type 'info check_error

let check _env tm =
  match tm with
  | Term.Term _ -> failwith "TODO"
  | Core_app _ -> failwith "TODO"
  | Case _ -> failwith "TODO"
  | Lambda _ -> failwith "TODO"
  | Let _ -> failwith "TODO"
  | Var _ -> failwith "TODO"
;;

let merge_results
    : 'a Nominal.Term.t SMap.t option list -> 'a Nominal.Term.t SMap.t option
  =
 fun results ->
  if List.for_all results ~f:Option.is_some
  then
    Some
      (results
      |> List.map
           ~f:
             (Util.Option.get_invariant ~here:[%here] (fun () ->
                  "we just checked all is_some"))
      |> SMap.strict_unions
      |> function
      | `Duplicate_key k ->
        Util.invariant_violation
          ~here:[%here]
          (Printf.sprintf "multiple variables with the same name (%s) in one pattern" k)
      | `Ok m -> m)
  else None
;;

let rec match_pattern v pat =
  match v, pat with
  | Nominal.Term.Operator (_, tag1, vals), Binding_aware_pattern.Operator (_, tag2, pats)
    ->
    if String.(tag1 = tag2)
    then (
      match List.map2 pats vals ~f:match_pattern_scope with
      | Ok results -> merge_results results
      | Unequal_lengths -> None)
    else None
  | Primitive l1, Primitive l2 ->
    let l1 = Primitive.erase l1 in
    let l2 = Primitive.erase l2 in
    if Primitive.(equal ~info_eq:Unit.( = ) l1 l2) then Some SMap.empty else None
  | _, Ignored _ -> Some SMap.empty
  | tm, Var (_, v) -> Some (SMap.of_alist_exn [ v, tm ])
  | _ -> None

and match_pattern_scope
    (Binding_aware_pattern.Scope (_XXXnames, pat))
    (Nominal.Scope.Scope (_XXXpats, body))
  =
  match_pattern body pat
;;

let find_match v branches =
  branches
  |> List.find_map ~f:(fun (Types.Case_scope (pat, rhs)) ->
         match match_pattern v pat with
         | None -> None
         | Some bindings -> Some (rhs, bindings))
;;

type 'a eval_error = string * 'a Term.t

let true_tm info = Nominal.Term.Operator (info, "true", [])
let false_tm info = Nominal.Term.Operator (info, "false", [])

let eval_char_bool_fn eval_ctx' name f ctx tm c =
  let open Result.Let_syntax in
  let%bind c_result = eval_ctx' ctx c in
  match c_result with
  | Nominal.Term.Primitive (info, Char c') ->
    Ok (if f c' then true_tm info else false_tm info)
  | _ -> Error (Printf.sprintf "Invalid argument to %s" name, tm)
;;

type 'info primitive_eval =
  ('info env -> 'info Term.t -> ('info Nominal.Term.t, 'info eval_error) Result.t)
  -> ('info env
      -> 'info Nominal.Term.t
      -> ('info Nominal.Term.t, 'info eval_error) Result.t)
  -> 'info env
  -> 'info Term.t
  -> string
  -> 'info Term.t list
  -> ('info Nominal.Term.t, 'info eval_error) Result.t

let rec eval_ctx
    :  'info primitive_eval -> 'info Nominal.Term.t SMap.t -> 'info Term.t
    -> ('info Nominal.Term.t, 'info eval_error) Result.t
  =
 fun eval_primitive ctx tm ->
  let open Result.Let_syntax in
  let eval_ctx = eval_ctx eval_primitive in
  match tm with
  | Var (_, v) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, tm))
  | Case (_, tm, branches) ->
    let%bind tm_val = eval_ctx ctx tm in
    (match find_match tm_val branches with
    | None -> Error ("no match found in case", Term tm_val)
    | Some (branch, bindings) ->
      eval_ctx (Util.Map.union_right_biased ctx bindings) branch)
  | Core_app (_, Lambda (_, _ty, Scope (name, body)), [ arg ]) ->
    let%bind arg_val = eval_ctx ctx arg in
    eval_ctx (Map.set ctx ~key:name ~data:arg_val) body
  | Core_app (_, Var (_, name), args) ->
    if Map.mem ctx name
    then failwith "TODO"
    else eval_primitive eval_ctx eval_ctx' ctx tm name args
  | Term tm -> Ok (Nominal.Term.subst_all ctx tm)
  | Let { tm; scope = Scope (name, body); _ } ->
    let%bind tm_val = eval_ctx ctx tm in
    eval_ctx (Map.set ctx ~key:name ~data:tm_val) body
  | _ -> Error ("Found a term we can't evaluate", tm)

and eval_ctx'
    :  'a Nominal.Term.t SMap.t -> 'a Nominal.Term.t
    -> ('a Nominal.Term.t, 'a eval_error) Result.t
  =
 fun ctx tm ->
  match tm with
  | Var (_, v) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, Term tm))
  | _ -> Ok tm
;;

let eval : 'a primitive_eval -> 'a Term.t -> ('a Nominal.Term.t, 'a eval_error) Result.t =
 fun eval_primitive core -> eval_ctx eval_primitive SMap.empty core
;;

let eval_primitive eval_ctx eval_ctx' ctx tm name args =
  let open Result.Let_syntax in
  let open Nominal.Term in
  let open Types in
  match name, args with
  | "add", [ Term a; Term b ] ->
    let%bind a_result = eval_ctx' ctx a in
    let%bind b_result = eval_ctx' ctx b in
    (match a_result, b_result with
    | Primitive (info, Integer a'), Primitive (_binfo, Integer b') ->
      (* XXX can't reuse info *)
      Ok (Nominal.Term.Primitive (info, Integer Z.(a' + b')))
    | _ -> Error ("Invalid arguments to add", tm))
  | "sub", [ Term a; Term b ] ->
    let%bind a_result = eval_ctx' ctx a in
    let%bind b_result = eval_ctx' ctx b in
    (match a_result, b_result with
    | Primitive (info, Integer a'), Primitive (_binfo, Integer b') ->
      Ok (Nominal.Term.Primitive (info, Integer Z.(a' - b')))
    | _ -> Error ("Invalid arguments to sub", tm))
  | "string_of_chars", [ char_list ] ->
    let%bind char_list = eval_ctx ctx char_list in
    (match char_list with
    | Operator (info, "list", chars) ->
      chars
      |> List.map ~f:(function
             | Nominal.Scope.Scope ([], Nominal.Term.Primitive (_, Char c)) -> Ok c
             | tm -> Error (Fmt.str "string_of_chars `list(%a)`" Nominal.Scope.pp tm))
      |> Result.all
      |> Result.map ~f:(fun cs ->
             Nominal.Term.Primitive (info, String (Base.String.of_char_list cs)))
      |> Result.map_error ~f:(fun msg -> msg, tm)
    | _ -> Error ("expected a list of characters", tm))
  | "var", [ str_tm ] ->
    let%bind str = eval_ctx ctx str_tm in
    (match str with
    | Primitive (info, String name) -> Ok (Nominal.Term.Var (info, name))
    | _ -> Error ("expected a string", tm))
  | "is_digit", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_digit" Char.is_digit ctx tm c
  | "is_lowercase", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_lowercase" Char.is_lowercase ctx tm c
  | "is_uppercase", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_uppercase" Char.is_uppercase ctx tm c
  | "is_alpha", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_alpha" Char.is_alpha ctx tm c
  | "is_alphanum", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_alphanum" Char.is_alphanum ctx tm c
  | "is_whitespace", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_whitespace" Char.is_whitespace ctx tm c
  | _ ->
    failwith (Printf.sprintf "Unknown function (%s), or wrong number of arguments" name)
;;

let%test_module "Parsing" =
  (module struct
    let parse str =
      Lvca_parsing.parse_string Parse.term str |> Result.ok_or_failwith |> Term.erase
    ;;

    let ( = ) = Term.equal ~info_eq:Unit.( = )
    let one = Nominal.Term.Primitive ((), Integer (Z.of_int 1))
    let var name = Term.Var ((), name)
    let ignored name = Binding_aware_pattern.Ignored ((), name)
    let operator tag children = Nominal.Term.Operator ((), tag, children)
    let app f a = Term.Core_app ((), f, a)

    let%test _ = parse "{1}" = Term one
    let%test _ = parse "{true()}" = Term (operator "true" [])
    let%test _ = parse "not x" = app (var "not") [ var "x" ]

    let%test _ =
      parse "let str = string_of_chars chars in {var(str)}"
      = Let
          { info = ()
          ; is_rec = No_rec
          ; ty = None
          ; tm = app (var "string_of_chars") [ var "chars" ]
          ; scope =
              Scope
                ( "str"
                , Term (operator "var" Nominal.[ Scope.Scope ([], Term.Var ((), "str")) ])
                )
          }
    ;;

    let%test _ =
      parse {|\(x : bool) -> x|}
      = Lambda ((), Sort.Name ((), "bool"), Scope ("x", var "x"))
    ;;

    let%test _ =
      parse {|match x with { _ -> {1} }|}
      = Case ((), var "x", [ Case_scope (ignored "", Term one) ])
    ;;

    let%test _ = parse {|match empty with { }|} = Case ((), var "empty", [])

    let%test _ =
      parse {|match x with { | _ -> {1} }|}
      = Case ((), var "x", [ Case_scope (ignored "", Term one) ])
    ;;

    let%test _ =
      parse {|match x with { true() -> {false()} | false() -> {true()} }|}
      = Case
          ( ()
          , var "x"
          , [ Case_scope (Operator ((), "true", []), Term (operator "false" []))
            ; Case_scope (Operator ((), "false", []), Term (operator "true" []))
            ] )
    ;;

    let%test _ =
      parse "let x = {true()} in not x"
      = Let
          { info = ()
          ; is_rec = No_rec
          ; ty = None
          ; tm = Term (operator "true" [])
          ; scope = Scope ("x", Core_app ((), var "not", [ var "x" ]))
          }
    ;;
  end)
;;

let%test_module "Core parsing" =
  (module struct
    let scope : unit Nominal.Term.t -> unit Nominal.Scope.t = fun body -> Scope ([], body)

    let dynamics_str =
      {|\(tm : ty) -> match tm with {
    | true() -> {true()}
    | false() -> {false()}
    | ite(t1; t2; t3) -> match meaning t1 with {
      | true()  -> meaning t2
      | false() -> meaning t3
    }
    | ap(f; arg) -> (meaning f) (meaning arg)
    | fun(scope) -> {lambda(list(); scope)}
  }
  |}
    ;;

    let p_var name = Binding_aware_pattern.Var ((), name)
    let c_var name = Term.Var ((), name)

    let p_operator tag children =
      Binding_aware_pattern.Operator
        ( ()
        , tag
        , children |> List.map ~f:(fun pat -> Binding_aware_pattern.Scope ([], pat)) )
    ;;

    let t_operator tag children = Nominal.Term.Operator ((), tag, children)
    let meaning x = Term.Core_app ((), c_var "meaning", [ x ])
    let ty = Sort.Name ((), "ty")

    let dynamics =
      Term.Lambda
        ( ()
        , ty
        , Scope
            ( "tm"
            , Case
                ( ()
                , c_var "tm"
                , [ Case_scope (p_operator "true" [], Term (t_operator "true" []))
                  ; Case_scope (p_operator "false" [], Term (t_operator "false" []))
                  ; Case_scope
                      ( p_operator "ite" [ p_var "t1"; p_var "t2"; p_var "t3" ]
                      , Case
                          ( ()
                          , meaning (c_var "t1")
                          , [ Case_scope (p_operator "true" [], meaning (c_var "t2"))
                            ; Case_scope (p_operator "false" [], meaning (c_var "t3"))
                            ] ) )
                  ; Case_scope
                      ( p_operator "ap" [ p_var "f"; p_var "arg" ]
                      , Core_app ((), meaning @@ c_var "f", [ meaning @@ c_var "arg" ]) )
                  ; Case_scope
                      ( p_operator "fun" [ p_var "scope" ]
                      , Term
                          (t_operator
                             "lambda"
                             [ scope @@ t_operator "list" []; scope @@ Var ((), "scope") ])
                      )
                  ] ) ) )
    ;;

    let%test "dynamics as expected" =
      let parse_term str =
        Lvca_parsing.parse_string Parse.term str |> Base.Result.ok_or_failwith
      in
      let ( = ) = Term.equal ~info_eq:Unit.( = ) in
      parse_term dynamics_str |> Term.erase = dynamics
    ;;
  end)
;;

let%test_module "Core eval" =
  (module struct
    let eval_str str =
      let parse_term str =
        Lvca_parsing.parse_string Parse.term str |> Base.Result.ok_or_failwith
      in
      let core = parse_term str in
      let result =
        match eval eval_primitive core with
        | Error (msg, tm) -> Fmt.str "%s: %a" msg Term.pp tm
        | Ok result -> Fmt.to_to_string Nominal.Term.pp result
      in
      Stdio.print_string result
    ;;

    let%expect_test _ =
      eval_str "{1}";
      [%expect {| 1 |}]
    ;;

    let%expect_test _ =
      eval_str "{foo(1)}";
      [%expect {| foo(1) |}]
    ;;

    let%expect_test _ =
      eval_str "{true()}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "{false()}";
      [%expect {| false() |}]
    ;;

    let%expect_test _ =
      eval_str
        {|match {true()} with {
          | true() -> {false()}
          | false() -> {true()}
        }
      |};
      [%expect {| false() |}]
    ;;

    let%expect_test _ =
      eval_str {|(\(x: bool) -> x) {true()}|};
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "add {1} {2}";
      [%expect {| 3 |}]
    ;;

    let%expect_test _ =
      eval_str "sub {1} {2}";
      [%expect {| -1 |}]
    ;;

    let%expect_test _ =
      eval_str "is_digit {'9'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_digit {'a'}";
      [%expect {| false() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alpha {'a'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alpha {'A'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alpha {'9'}";
      [%expect {| false() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alphanum {'9'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alphanum {'Z'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alphanum {'_'}";
      [%expect {| false() |}]
    ;;

    (* let%expect_test _ = eval_str "sub 1 2"; [%expect{| -1 |}] *)
  end)
;;

let%test_module "Core pretty" =
  (module struct
    let pretty width str =
      let str =
        match Lvca_parsing.parse_string Lvca_parsing.(whitespace *> Parse.term) str with
        | Error err -> err
        | Ok core ->
          let fmt = Format.str_formatter in
          Format.pp_set_geometry fmt ~max_indent:width ~margin:(width + 1);
          Term.pp fmt core;
          Format.flush_str_formatter ()
      in
      Stdio.print_string str
    ;;

    let%expect_test _ =
      pretty 22 "match {true()} with { true() -> {false()} | false() -> {true()} }";
      [%expect
        {|
        match {true()} with {
          | true()
            -> {false()}
          | false()
            -> {true()}
        } |}]
    ;;

    let%expect_test _ =
      pretty 23 "match {true()} with { true() -> {false()} | false() -> {true()} }";
      [%expect
        {|
        match {true()} with {
          | true() -> {false()}
          | false() -> {true()}
        } |}]
    ;;

    let%expect_test _ =
      pretty 25 "match x with { _ -> {1} }";
      [%expect {| match x with { _ -> {1} } |}]
    ;;

    let%expect_test _ =
      pretty 24 "match x with { _ -> {1} }";
      [%expect {|
        match x with {
          | _ -> {1}
        } |}]
    ;;

    let%expect_test _ =
      pretty 20 "foo a b c d e f g h i j k l";
      [%expect {|
        foo a b c d e f g h
            i j k l |}]
    ;;

    let%expect_test _ =
      pretty 20 "f a b c d e f g h i j k l";
      [%expect {|
        f a b c d e f g h i
          j k l |}]
    ;;

    let%expect_test _ =
      pretty 20 "let x = {true()} in not x";
      [%expect {|
        let x = {true()} in
        not x |}]
    ;;

    let%expect_test _ =
      pretty 20 "let x: bool = {true()} in not x";
      [%expect {|
        let x: bool =
        {true()} in not x |}]
    ;;
  end)
;;

let%test_module "Core eval in dynamics" =
  (module struct
    let eval_in dynamics_str str =
      let parse_term str =
        Lvca_parsing.parse_string Parse.term str |> Base.Result.ok_or_failwith
      in
      let defn = parse_term dynamics_str in
      let core = parse_term str in
      match eval eval_primitive (Core_app (None, defn, [ core ])) with
      | Error (msg, tm) -> Fmt.str "%s: %a" msg Term.pp tm
      | Ok result -> Fmt.to_to_string Nominal.Term.pp result
    ;;

    let dynamics_str =
      {|\(tm : ty) -> match tm with {
  | true() -> {true()}
  | false() -> {false()}
  | ite(t1; t2; t3) -> match meaning t1 with {
    | true()  -> meaning t2
    | false() -> meaning t3
  }
  | ap(f; arg) -> (meaning f) (meaning arg)
  | fun(scope) -> {lambda(list(); scope)}
}
      |}
    ;;

    let%expect_test _ =
      Stdio.print_string @@ eval_in dynamics_str "{true()}";
      [%expect {| true() |}]
    ;;

    let id_dynamics = {|\(tm : ty) -> tm|}

    let%expect_test _ =
      Stdio.print_string @@ eval_in id_dynamics "{true()}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      Stdio.print_string @@ eval_in id_dynamics "{lambda(tm. tm; list(ty()))}";
      [%expect {| lambda(tm. tm; list(ty())) |}]
    ;;
  end)
;;
