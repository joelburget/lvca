(** A small "core" language. *)

open Base
open Lvca_provenance
open Lvca_syntax
open Result.Let_syntax
module Format = Stdlib.Format
module SMap = Lvca_util.String.Map

module Is_rec = struct
  type t =
    | Rec
    | No_rec

  let ( = ) x y = match x, y with Rec, Rec | No_rec, No_rec -> true | _, _ -> false
end

(* list : * -> * *)
module Type_model =
[%lvca.abstract_syntax_module
{|
sort : *

list a :=
  | Nil()
  | Cons(a; list a)

type :=
  | Arrow(list type)
  | Sort(sort)
|}]

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

  let pp_generic ~open_loc ~close_loc ppf =
    let rec go need_parens ppf = function
      | Arrow ts ->
        let fmt = Fmt.(list ~sep:(any " -> ") (go true)) in
        let fmt = if need_parens then Fmt.parens fmt else fmt in
        fmt ppf ts
      | Sort s -> Sort.pp_generic ~open_loc ~close_loc ppf s
    in
    go false ppf
  ;;

  let pp = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ())

  module Parse = struct
    open Lvca_parsing

    let rec go tys =
      match Lvca_util.List.unsnoc tys with pre, Arrow tys' -> pre @ go tys' | _ -> tys
    ;;

    let normalize ty = match ty with Sort _ -> ty | Arrow tys -> Arrow (go tys)

    let t =
      let t =
        fix (fun t ->
            let atom = parens t <|> (Sort.Parse.t >>| fun sort -> Sort sort) in
            sep_by1 (string "->") atom >>| function [ t ] -> t | ts -> Arrow ts)
      in
      t >>| normalize <?> "core type"
    ;;

    let%test_module "normalize" =
      (module struct
        let ( = ) = equal ~info_eq:Unit.( = )
        let s = Sort.Name ((), "s")

        let%test _ = normalize (Sort s) = Sort s
        let%test _ = normalize (Arrow [ Sort s; Sort s ]) = Arrow [ Sort s; Sort s ]

        let%test _ =
          normalize (Arrow [ Arrow [ Sort s; Sort s ]; Sort s ])
          = Arrow [ Arrow [ Sort s; Sort s ]; Sort s ]
        ;;

        let%test _ =
          normalize (Arrow [ Sort s; Arrow [ Sort s; Sort s ] ])
          = Arrow [ Sort s; Sort s; Sort s ]
        ;;

        let%test _ =
          normalize (Arrow [ Sort s; Arrow [ Sort s; Arrow [ Sort s; Sort s ] ] ])
          = Arrow [ Sort s; Sort s; Sort s; Sort s ]
        ;;

        let%test _ =
          normalize (Arrow [ Arrow [ Sort s; Sort s ]; Arrow [ Sort s; Sort s ] ])
          = Arrow [ Arrow [ Sort s; Sort s ]; Sort s; Sort s ]
        ;;
      end)
    ;;
  end
end

module Model =
[%lvca.abstract_syntax_module
{|
nominal_term : *
type : *
string : *
binding_aware_pattern : *
is_rec : *
list : * -> *
option : * -> *

term :=
  | Term(nominal_term)
  | Core_app(term; list term)
  | Case(term; list case_scope)
  | Lambda(type; scope)
  | Let(is_rec; term; option type; scope)

scope := Scope(string; term)

case_scope := Case_scope(binding_aware_pattern; term)
|}]

module Types = struct
  type 'info term =
    | Term of 'info Nominal.Term.t
    | Core_app of 'info * 'info term * 'info term list
    | Case of 'info * 'info term * 'info case_scope list
    | Lambda of 'info * 'info Type.t * 'info scope
    | Let of 'info let_
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

module Equal = struct
  let rec term ~info_eq x y =
    match x, y with
    | Types.Term x, Types.Term y -> Nominal.Term.equal ~info_eq x y
    | Core_app (x1, x2, x3), Core_app (y1, y2, y3) ->
      info_eq x1 y1 && term ~info_eq x2 y2 && List.equal (term ~info_eq) x3 y3
    | Case (x1, x2, x3), Case (y1, y2, y3) ->
      info_eq x1 y1 && term ~info_eq x2 y2 && List.equal (case_scope ~info_eq) x3 y3
    | Lambda (x1, x2, x3), Lambda (y1, y2, y3) ->
      info_eq x1 y1 && Type.equal ~info_eq x2 y2 && scope ~info_eq x3 y3
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
    | Lambda (info, ty, scope') -> Lambda (f info, Type.map_info ~f ty, scope ~f scope')
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
    | Lambda (_, ty, Scope (name, body)) ->
      pf ppf "\\(%s : %a) ->@ %a" name (Type.pp_generic ~open_loc ~close_loc) ty pp body
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

  let reserved = Lvca_util.String.Set.of_list [ "let"; "rec"; "in"; "match"; "with" ]

  let identifier =
    identifier
    >>= fun ident ->
    if Set.mem reserved ident
    then fail (Printf.sprintf "identifier: reserved word (%s)" ident)
    else return ident
  ;;

  let make_apps : 'a Types.term list -> 'a Types.term = function
    | [] -> Lvca_util.invariant_violation ~here:[%here] "must be a nonempty list"
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
              (fun (_, lam_loc) ((name, ty), parens_loc) _ body ->
                let info = Opt_range.union lam_loc parens_loc in
                Types.Lambda (info, ty, Scope (name, body)))
              (attach_pos (char '\\'))
              (attach_pos
                 (parens
                    (lift3
                       (fun ident _ ty -> ident, ty)
                       identifier
                       (char ':')
                       Type.Parse.t)))
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
end

module Term = struct
  module Kernel = struct
    type 'info t = 'info Types.term =
      | Term of 'info Nominal.Term.t
      | Core_app of 'info * 'info t * 'info t list
      | Case of 'info * 'info t * 'info Types.case_scope list
      | Lambda of 'info * 'info Type.t * 'info Types.scope
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

module Module = struct
  type 'info t =
    { externals : (string * 'info Type.t) list
    ; defs : (string * 'info Types.term) list
    }

  let pp_generic ~open_loc ~close_loc ppf { externals; defs } =
    let open Fmt in
    let pp_externals ppf = function
      | [] -> ()
      | lst ->
        Fmt.pf
          ppf
          "%a;"
          (list
             ~sep:semi
             (pair ~sep:(any " : ") string (Type.pp_generic ~open_loc ~close_loc)))
          lst
    in
    let pp_def ppf (name, sort_def) =
      pf ppf "%s := %a" name (Term.pp_generic ~open_loc ~close_loc) sort_def
    in
    pf ppf "%a@,%a" pp_externals externals (list pp_def) defs
  ;;

  let pp ppf tm = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf tm

  module Parse = struct
    open Lvca_parsing

    let external_decl =
      lift4
        (fun ident _ ty _ -> ident, ty)
        identifier
        (string ":")
        Type.Parse.t
        (string ";")
    ;;

    let def = lift3 (fun ident _ tm -> ident, tm) identifier (string ":=") Term.Parse.t

    let t =
      lift2 (fun externals defs -> { externals; defs }) (many external_decl) (many1 def)
    ;;
  end
end

type 'info env = 'info Nominal.Term.t SMap.t

let preimage _ = failwith "TODO"
let reverse _tm _cases = failwith "TODO"

type 'info check_env =
  { type_env : 'info Type.t SMap.t
  ; syntax : 'info Abstract_syntax.t
  }

module Check_error' = struct
  type term_failure_inference_reason =
    | Term_var_not_found of string
    | Operator_not_supported

  type 'info t =
    | Cant_infer_case
    | Cant_infer_lambda
    | Var_not_found
    | Operator_not_found
    | Mismatch of 'info Type.t
    | Term_isnt_arrow
    | Failed_term_inference of term_failure_inference_reason
    | Failed_check_term of
        ('info, ('info Pattern.t, 'info Nominal.Term.t) Base.Either.t) Check_failure.t

  let pp ppf tm ty_opt = function
    | Cant_infer_case -> Fmt.pf ppf "can't infer cases"
    | Cant_infer_lambda -> Fmt.pf ppf "can't infer lambdas"
    | Var_not_found ->
      (match tm with
      | Term.Term (Nominal.Term.Var (_, name)) -> Fmt.pf ppf "variable %s not found" name
      | _ -> Lvca_util.invariant_violation ~here:[%here] "expected Var")
    | Operator_not_found -> Fmt.pf ppf "operator not found"
    | Mismatch ty ->
      (match ty_opt with
      | None -> Lvca_util.invariant_violation ~here:[%here] "expected Some ty'"
      | Some ty' -> Fmt.pf ppf "%a != %a" Type.pp ty Type.pp ty')
    | Term_isnt_arrow -> Fmt.pf ppf "terms can't have arrow types"
    | Failed_term_inference reason ->
      let explanation =
        match reason with
        | Term_var_not_found name -> Fmt.str "variable %s not found" name
        | Operator_not_supported -> "inference not supported for operators"
      in
      Fmt.pf ppf "failed to infer type of term %a @[(%s)@]" Term.pp tm explanation
    | Failed_check_term err ->
      let pp ppf pat_or_tm =
        match pat_or_tm with
        | Either.First pat -> Fmt.pf ppf "term: %a" Pattern.pp pat
        | Second tm -> Fmt.pf ppf "term: %a" Nominal.Term.pp tm
      in
      Fmt.pf ppf "failed to check term: %a" (Check_failure.pp pp) err
  ;;
end

module Check_error = struct
  type 'info t =
    { env : 'info check_env
    ; tm : 'info Term.t
    ; ty : 'info Type.t
    ; error : 'info Check_error'.t
    }

  let pp ppf { env = _; tm; ty; error } = Check_error'.pp ppf tm (Some ty) error
end

module Infer_error = struct
  type 'info t =
    { env : 'info check_env
    ; tm : 'info Term.t
    ; error : 'info Check_error'.t
    }

  let pp ppf { env = _; tm; error } = Check_error'.pp ppf tm None error
end

let infer_term type_env tm =
  match tm with
  | Nominal.Term.Primitive (_, prim) ->
    let name =
      match prim with
      | Integer _ -> "integer"
      | Int32 _ -> "int32"
      | String _ -> "string"
      | Float _ -> "float"
      | Char _ -> "char"
    in
    Ok (Type.Sort (Sort.Name (None, name)))
  | Var (_, v) ->
    (match Map.find type_env v with
    | Some ty -> Ok ty
    | None -> Error (Check_error'.Term_var_not_found v))
  | Operator _ -> Error Check_error'.Operator_not_supported
;;

let rec check ({ type_env; syntax } as env) tm ty =
  let check_ty ty' =
    if Type.equal ~info_eq:(fun _ _ -> true) ty ty'
    then None
    else Some Check_error.{ env; tm; ty; error = Mismatch ty' }
  in
  match tm with
  | Term.Term tm' ->
    (match ty with
    | Type.Arrow _ -> Some Check_error.{ env; tm; ty; error = Term_isnt_arrow }
    | Type.Sort sort ->
      (match Nominal.Term.check syntax sort tm' with
      | None -> None
      | Some err -> Some Check_error.{ env; tm; ty; error = Failed_check_term err }))
  | Core_app _ -> failwith "TODO"
  | Case (_, tm, branches) ->
    (match infer env tm with
    | Error Infer_error.{ env; tm; error } -> Some { env; tm; ty; error }
    | Ok _tm_ty ->
      (* TODO *)
      branches |> List.find_map ~f:(fun (Case_scope (_pat, rhs)) -> check env rhs ty))
  | Lambda (_, ty', Scope (name, body)) ->
    let type_env = Map.set type_env ~key:name ~data:ty' in
    check { type_env; syntax } body ty
  | Let { is_rec = _; tm; ty = ty'; scope = Scope (name, body); _ } ->
    let inferred_tm_ty = match ty' with None -> infer env tm | Some tm_ty -> Ok tm_ty in
    (match inferred_tm_ty with
    | Ok tm_ty ->
      let type_env = Map.set type_env ~key:name ~data:tm_ty in
      check { env with type_env } body ty
    | Error Infer_error.{ env; tm; error } -> Some Check_error.{ env; tm; ty; error })
  | Var _ ->
    (match infer env tm with
    | Error { env; tm; error } -> Some { env; tm; ty; error }
    | Ok ty' -> check_ty ty')

and infer ({ type_env; syntax = _ } as env) tm =
  match tm with
  | Term.Term tm' ->
    (match infer_term type_env tm' with
    | Error reason -> Error { env; tm; error = Failed_term_inference reason }
    | Ok ty -> Ok ty)
  | Core_app (_, f, args) ->
    let _arg_tys = args |> List.map ~f:(infer env) |> Result.all in
    (* XXX add arg tys to env *)
    infer env f
  | Case _ -> Error { env; tm; error = Cant_infer_case }
  | Lambda _ -> Error { env; tm; error = Cant_infer_lambda }
  | Let { is_rec = _; tm; ty; scope = Scope (name, body); _ } ->
    let%bind tm_ty = match ty with None -> infer env tm | Some ty -> Ok ty in
    let type_env = Map.set type_env ~key:name ~data:tm_ty in
    infer { env with type_env } body
  | Var (_, name) ->
    (match Map.find type_env name with
    | None -> Error { env; tm; error = Var_not_found }
    | Some ty' -> Ok ty')
;;

let merge_pattern_context
    : 'a Nominal.Term.t SMap.t option list -> 'a Nominal.Term.t SMap.t option
  =
 fun ctxs ->
  if List.for_all ctxs ~f:Option.is_some
  then
    Some
      (ctxs
      |> List.map
           ~f:
             (Lvca_util.Option.get_invariant ~here:[%here] (fun () ->
                  "we just checked all is_some"))
      |> SMap.strict_unions
      |> function
      | `Duplicate_key k ->
        Lvca_util.invariant_violation
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
      | Ok results -> merge_pattern_context results
      | Unequal_lengths -> None)
    else None
  | Primitive l1, Primitive l2 ->
    let l1 = Primitive.All.erase l1 in
    let l2 = Primitive.All.erase l2 in
    if Primitive.All.(equal ~info_eq:Unit.( = ) l1 l2) then Some SMap.empty else None
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

let eval_char_bool_fn eval_nominal_in_ctx name f ctx tm c =
  let%bind c = eval_nominal_in_ctx ctx c in
  match c with
  | Nominal.Term.Primitive (info, Char c) ->
    Ok (if f c then true_tm info else false_tm info)
  | _ -> Error (Printf.sprintf "Invalid argument to %s" name, tm)
;;

let rec eval_in_ctx ctx tm =
  match tm with
  | Term.Var (_, v) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, tm))
  | Case (_, tm, branches) ->
    let%bind tm_val = eval_in_ctx ctx tm in
    (match find_match tm_val branches with
    | None -> Error ("no match found in case", Term tm_val)
    | Some (branch, bindings) ->
      eval_in_ctx (Lvca_util.Map.union_right_biased ctx bindings) branch)
  | Core_app (_, Lambda (_, _ty, Scope (name, body)), [ arg ]) ->
    let%bind arg_val = eval_in_ctx ctx arg in
    eval_in_ctx (Map.set ctx ~key:name ~data:arg_val) body
  | Core_app (_, Var (_, name), args) ->
    if Map.mem ctx name
    then failwith "TODO"
    else eval_primitive eval_in_ctx eval_nominal_in_ctx ctx tm name args
  | Term tm -> Ok (Nominal.Term.subst_all ctx tm)
  | Let { tm; scope = Scope (name, body); _ } ->
    let%bind tm_val = eval_in_ctx ctx tm in
    eval_in_ctx (Map.set ctx ~key:name ~data:tm_val) body
  | _ -> Error ("Found a term we can't evaluate", tm)

and eval_nominal_in_ctx ctx tm =
  match tm with
  | Nominal.Term.Var (_, v) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, Term.Term tm))
  | _ -> Ok tm

and eval_primitive eval_in_ctx eval_nominal_in_ctx ctx tm name args =
  let open Nominal.Term in
  let%bind args = args |> List.map ~f:(eval_in_ctx ctx) |> Result.all in
  match name, args with
  | "add", [ a; b ] ->
    let%bind a_result = eval_nominal_in_ctx ctx a in
    let%bind b_result = eval_nominal_in_ctx ctx b in
    (match a_result, b_result with
    | Primitive (info, Integer a'), Primitive (_binfo, Integer b') ->
      (* XXX can't reuse info *)
      Ok (Nominal.Term.Primitive (info, Integer Z.(a' + b')))
    | _ -> Error ("Invalid arguments to add", tm))
  | "sub", [ a; b ] ->
    let%bind a_result = eval_nominal_in_ctx ctx a in
    let%bind b_result = eval_nominal_in_ctx ctx b in
    (match a_result, b_result with
    | Primitive (info, Integer a'), Primitive (_binfo, Integer b') ->
      Ok (Nominal.Term.Primitive (info, Integer Z.(a' - b')))
    | _ -> Error ("Invalid arguments to sub", tm))
  | "string_of_chars", [ char_list ] ->
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
    (match str_tm with
    | Primitive (info, String name) -> Ok (Nominal.Term.Var (info, name))
    | _ -> Error ("expected a string", tm))
  | "is_digit", [ c ] ->
    eval_char_bool_fn eval_nominal_in_ctx "is_digit" Char.is_digit ctx tm c
  | "is_lowercase", [ c ] ->
    eval_char_bool_fn eval_nominal_in_ctx "is_lowercase" Char.is_lowercase ctx tm c
  | "is_uppercase", [ c ] ->
    eval_char_bool_fn eval_nominal_in_ctx "is_uppercase" Char.is_uppercase ctx tm c
  | "is_alpha", [ c ] ->
    eval_char_bool_fn eval_nominal_in_ctx "is_alpha" Char.is_alpha ctx tm c
  | "is_alphanum", [ c ] ->
    eval_char_bool_fn eval_nominal_in_ctx "is_alphanum" Char.is_alphanum ctx tm c
  | "is_whitespace", [ c ] ->
    eval_char_bool_fn eval_nominal_in_ctx "is_whitespace" Char.is_whitespace ctx tm c
  | _ ->
    failwith
      (Fmt.str
         "Unknown function (%s), or wrong number of arguments (got [%a])"
         name
         Fmt.(list Nominal.Term.pp)
         args)
;;

let eval core = eval_in_ctx SMap.empty core

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
      = Lambda ((), Type.Sort (Name ((), "bool")), Scope ("x", var "x"))
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
    let ty = Type.Sort (Name ((), "ty"))

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
        match eval core with
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

    let%expect_test _ =
      eval_str "sub {1} {2}";
      [%expect {| -1 |}]
    ;;
  end)
;;

let%test_module "Core pretty" =
  (module struct
    let mk_test parser pp ?(width = 80) str =
      let str =
        match Lvca_parsing.parse_string Lvca_parsing.(whitespace *> parser) str with
        | Error err -> err
        | Ok core ->
          let fmt = Format.str_formatter in
          Format.pp_set_geometry fmt ~max_indent:width ~margin:(width + 1);
          pp fmt core;
          Format.flush_str_formatter ()
      in
      Stdio.print_string str
    ;;

    let term = mk_test Term.Parse.t Term.pp
    let ty = mk_test Type.Parse.t Type.pp
    let module' = mk_test Module.Parse.t Module.pp

    let%expect_test _ =
      term ~width:22 "match {true()} with { true() -> {false()} | false() -> {true()} }";
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
      term ~width:23 "match {true()} with { true() -> {false()} | false() -> {true()} }";
      [%expect
        {|
        match {true()} with {
          | true() -> {false()}
          | false() -> {true()}
        } |}]
    ;;

    let%expect_test _ =
      term ~width:25 "match x with { _ -> {1} }";
      [%expect {| match x with { _ -> {1} } |}]
    ;;

    let%expect_test _ =
      term ~width:24 "match x with { _ -> {1} }";
      [%expect {|
        match x with {
          | _ -> {1}
        } |}]
    ;;

    let%expect_test _ =
      term ~width:20 "foo a b c d e f g h i j k l";
      [%expect {|
        foo a b c d e f g h
            i j k l |}]
    ;;

    let%expect_test _ =
      term ~width:20 "f a b c d e f g h i j k l";
      [%expect {|
        f a b c d e f g h i
          j k l |}]
    ;;

    let%expect_test _ =
      term ~width:20 "let x = {true()} in not x";
      [%expect {|
        let x = {true()} in
        not x |}]
    ;;

    let%expect_test _ =
      term ~width:20 "let x: bool = {true()} in not x";
      [%expect {|
        let x: bool =
        {true()} in not x |}]
    ;;

    let%expect_test _ =
      ty "string";
      [%expect "string"]
    ;;

    let%expect_test _ =
      ty "int -> bool";
      [%expect "int -> bool"]
    ;;

    let%expect_test _ =
      ty "(int -> bool) -> string";
      [%expect "(int -> bool) -> string"]
    ;;

    let%expect_test _ =
      ty "int -> (bool -> string)";
      [%expect "int -> bool -> string"]
    ;;

    let%expect_test _ =
      ty "(int -> float) -> (bool -> string)";
      [%expect "(int -> float) -> bool -> string"]
    ;;

    let%expect_test _ =
      module' {|go : string; x := {"foo"}|};
      [%expect {|
        go : string;
        x := {"foo"} |}]
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
      match eval (Core_app (None, defn, [ core ])) with
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

let%test_module "Evaluation / inference" =
  (module struct
    let parse_term str =
      Lvca_parsing.parse_string Parse.term str |> Base.Result.ok_or_failwith
    ;;

    let parse_type str =
      Lvca_parsing.parse_string Type.Parse.t str |> Base.Result.ok_or_failwith
    ;;

    open Abstract_syntax

    let externals = [ "option", Kind.Kind (None, 2) ]

    let sort_defs =
      [ ( "bool"
        , Sort_def.Sort_def
            ( []
            , [ Operator_def.Operator_def ("true", [])
              ; Operator_def.Operator_def ("false", [])
              ] ) )
      ; ( "list"
        , let a = Sort.Name (None, "a") in
          Sort_def.Sort_def
            ( [ "a", Some (Kind.Kind (None, 1)) ]
            , [ Operator_def.Operator_def ("nil", [])
              ; Operator_def.Operator_def
                  ( "cons"
                  , [ Valence.Valence ([], a)
                    ; Valence.Valence ([], Sort.Ap (None, "list", [ a ]))
                    ] )
              ] ) )
      ]
    ;;

    let syntax = Abstract_syntax.{ externals; sort_defs }

    let check ?(type_env = SMap.empty) ty_str tm_str =
      let tm = parse_term tm_str in
      let ty = parse_type ty_str in
      let ctx = { type_env; syntax } in
      match check ctx tm ty with
      | Some err -> Check_error.pp Fmt.stdout err
      | None -> Fmt.pr "checked"
    ;;

    let infer ?(type_env = SMap.empty) tm_str =
      let tm = parse_term tm_str in
      let ctx = { type_env; syntax } in
      match infer ctx tm with
      | Error err -> Infer_error.pp Fmt.stdout err
      | Ok ty -> Fmt.pr "inferred: %a" Type.pp ty
    ;;

    let%expect_test _ =
      check "bool" "{true()}";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check "integer" "{2}";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check "bool" "{2}";
      [%expect
        {|
        failed to check term: Unexpected sort (bool) for a primitive (2)
        stack:
        - term: 2, sort: bool |}]
    ;;

    let%expect_test _ =
      infer "{2}";
      [%expect {| inferred: integer |}]
    ;;

    let%expect_test _ =
      infer "{true()}";
      [%expect
        {| failed to infer type of term {true()} (inference not supported for operators) |}]
    ;;

    let%expect_test _ =
      infer "{nil()}";
      [%expect
        {| failed to infer type of term {nil()} (inference not supported for operators) |}]
    ;;

    let%expect_test _ =
      check "a -> a" "{nil()}";
      [%expect {| terms can't have arrow types |}]
    ;;

    let%expect_test _ =
      check "list integer" "{cons(1; nil())}";
      [%expect {| checked |}]
    ;;

    let type_env = SMap.of_alist_exn [ "x", Type.Sort (Sort.Name (None, "a")) ]

    let%expect_test _ =
      infer ~type_env "x";
      [%expect {| inferred: a |}]
    ;;

    let%expect_test _ =
      infer ~type_env "{x}";
      [%expect {| inferred: a |}]
    ;;
  end)
;;
