(** A small "core" language. *)

open Base
open Lvca_provenance
open Lvca_syntax
open Result.Let_syntax
module Format = Stdlib.Format
module SMap = Lvca_util.String.Map

let ( >> ) = Lvca_util.( >> )

module List_model = struct
  include [%lvca.abstract_syntax_module "list a := Nil() | Cons(a; list a)"]

  let rec of_list ~empty_info = function
    | [] -> List.Nil empty_info
    | x :: xs -> Cons (empty_info, x, of_list ~empty_info xs)
  ;;

  let rec to_list = function List.Nil _ -> [] | Cons (_, x, xs) -> x :: to_list xs

  let rec map ~f = function
    | List.Nil i -> List.Nil i
    | Cons (i, x, xs) -> Cons (i, f x, map ~f xs)
  ;;
end

module Option_model = struct
  include [%lvca.abstract_syntax_module "option a := None() | Some(a)"]

  let of_option ~empty_info = function
    | None -> Option.None empty_info
    | Some a -> Some (empty_info, a)
  ;;

  let to_option = function Option.None _ -> None | Some (_, a) -> Some a
  let map ~f = function Option.None i -> Option.None i | Some (i, a) -> Some (i, f a)
end

module Binding_aware_pattern_model = struct
  include
    [%lvca.abstract_syntax_module
    {|
string : *  // module Primitive.String
primitive : *  // module Primitive.All
list : * -> *  // module List_model.List

pattern :=
  | Operator(string; list scope)
  | Primitive(primitive)
  | Var(string)

scope := Scope(list string; pattern)
|}]

  let rec into ~empty_info = function
    | Binding_aware_pattern.Operator (info, str, scopes) ->
      let scopes =
        scopes |> List.map ~f:(scope ~empty_info) |> List_model.of_list ~empty_info
      in
      Pattern.Operator (info, (empty_info, str), scopes)
    | Primitive (info, prim) -> Primitive (info, (empty_info, prim))
    | Var (info, str) -> Var (info, (empty_info, str))

  and scope ~empty_info (Binding_aware_pattern.Scope (names, pat)) =
    let names = names |> List_model.of_list ~empty_info in
    Scope.Scope (empty_info, names, into ~empty_info pat)
  ;;

  let rec out = function
    | Pattern.Operator (info, (_, str), scopes) ->
      let scopes = scopes |> List_model.to_list |> List.map ~f:scope in
      Binding_aware_pattern.Operator (info, str, scopes)
    | Primitive (info, (_, prim)) -> Primitive (info, prim)
    | Var (info, (_, str)) -> Var (info, str)

  and scope (Scope.Scope (_, names, pat)) =
    Binding_aware_pattern.Scope (List_model.to_list names, out pat)
  ;;
end

module Sort_model =
[%lvca.abstract_syntax_module
{|
string : *  // module Primitive.String

sort :=
  | Ap(string; ap_list)
  | Name(string)

ap_list :=
  | Nil()
  | Cons(sort; ap_list)
|}]

module Sort = struct
  include Nominal.Convertible.Extend (Sort_model.Sort)

  let rec into = function
    | Lvca_syntax.Sort.Ap (info, name, ap_list) ->
      Sort_model.Sort.Ap (info, (info, name), into_ap_list ap_list)
    | Name (info, name) -> Name (info, (info, name))

  and into_ap_list = function
    | Lvca_syntax.Sort.Nil info -> Sort_model.Ap_list.Nil info
    | Cons (info, sort, ap_list) -> Cons (info, into sort, into_ap_list ap_list)
  ;;

  let rec out = function
    | Sort_model.Sort.Ap (info, (_, name), ap_list) ->
      Lvca_syntax.Sort.Ap (info, name, out_ap_list ap_list)
    | Name (info, (_, name)) -> Name (info, name)

  and out_ap_list = function
    | Sort_model.Ap_list.Nil info -> Lvca_syntax.Sort.Nil info
    | Cons (info, sort, ap_list) -> Cons (info, out sort, out_ap_list ap_list)
  ;;
end

module Lang = struct
  include
    [%lvca.abstract_syntax_module
    {|
sort : *  // module Sort_model.Sort
nominal : *  // module Nominal.Term
list : * -> *  // module List_model.List
option : * -> *  // module Option_model.Option
binding_aware_pattern : * -> *  // module Binding_aware_pattern_model.Pattern

is_rec := Rec() | No_rec()

ty := Sort(sort) | Arrow(ty; ty)

term :=
  | Term(nominal)
  | Ap(term; list term)
  | Case(term; list case_scope)
  | Lambda(ty; term. term)
  | Let(is_rec; term; option ty; term. term)

case_scope := Case_scope(binding_aware_pattern; term)
|}]

  module Is_rec = struct
    include Nominal.Convertible.Extend (Is_rec)
    include Is_rec
  end

  module Ty = struct
    include Nominal.Convertible.Extend (Ty)
    include Ty
  end

  module Term = struct
    include Nominal.Convertible.Extend (Term)
    include Term
  end

  module Case_scope = struct
    include Nominal.Convertible.Extend (Case_scope)
    include Case_scope
  end
end

module Type = struct
  include Lang.Ty

  let pp_generic ~open_loc ~close_loc ppf =
    let rec go need_parens ppf = function
      | Arrow (_, t1, t2) ->
        Fmt.pf
          ppf
          (if need_parens then "@[<hv>(%a -> %a)@]" else "%a -> %a")
          (go true)
          t1
          (go false)
          t2
      | Sort (_, s) -> Lvca_syntax.Sort.pp_generic ~open_loc ~close_loc ppf (Sort.out s)
    in
    go false ppf
  ;;

  let pp ty = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ty

  module Parse = struct
    open Lvca_parsing

    let none = Commented.none
    let arrow s1 s2 = Arrow (none, s1, s2)

    let rec of_list = function
      | [] ->
        Lvca_util.invariant_violation
          ~here:[%here]
          "of_list must be called with a non-empty list"
      | [ sort ] -> sort
      | sort :: sorts -> arrow sort (of_list sorts)
    ;;

    let t ~comment =
      fix (fun t ->
          let atom =
            Ws.parens t
            <|> (Lvca_syntax.Sort.parse ~comment
                >>| fun sort ->
                let sort = Sort.into sort in
                Sort (Sort.info sort, sort))
          in
          sep_by1 (Ws.string "->") atom >>| of_list)
      <?> "core type"
    ;;

    let%test_module "parsing" =
      (module struct
        let parse = parse_string (t ~comment:c_comment) >> Result.ok_or_failwith >> erase
        let ( = ) = equal ~info_eq:Unit.( = )
        let bool = Sort_model.Sort.Name ((), ((), "bool"))
        let list_bool = Sort_model.Sort.Ap ((), ((), "list"), Cons ((), bool, Nil ()))

        let%test _ = parse "bool" = Sort ((), bool)
        let%test _ = parse "list bool" = Sort ((), list_bool)
      end)
    ;;

    let%test_module "of_list" =
      (module struct
        let ( = ) = equal ~info_eq:(Commented.equal String.( = ))
        let s = Sort_model.Sort.Name (none, (none, "s"))
        let sort s = Sort (none, s)

        let%test _ = of_list [ sort s ] = sort s
        let%test _ = of_list [ sort s; sort s ] = arrow (sort s) (sort s)

        let%test _ =
          of_list [ sort s; sort s; sort s ] = arrow (sort s) (arrow (sort s) (sort s))
        ;;

        let%test _ =
          of_list [ sort s; sort s; sort s; sort s ]
          = arrow (sort s) (arrow (sort s) (arrow (sort s) (sort s)))
        ;;

        let%test _ =
          of_list [ of_list [ sort s; sort s ]; sort s ]
          = arrow (arrow (sort s) (sort s)) (sort s)
        ;;

        let%test _ =
          of_list [ sort s; of_list [ sort s; sort s ] ]
          = arrow (sort s) (arrow (sort s) (sort s))
        ;;

        let%test _ =
          of_list [ arrow (sort s) (sort s); arrow (sort s) (sort s) ]
          = arrow (arrow (sort s) (sort s)) (arrow (sort s) (sort s))
        ;;
      end)
    ;;
  end

  let parse ~comment = Parse.t ~comment
end

module Pp_generic = struct
  let braces, list, any, pf, sp = Fmt.(braces, list, any, pf, sp)

  (* TODO: add parse <-> pretty tests *)

  let rec term : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info Lang.Term.t Fmt.t =
   fun ~open_loc ~close_loc ppf tm ->
    let pp = term ~open_loc ~close_loc in
    open_loc ppf (Lang.Term.info tm);
    (match tm with
    | Lang.Term.Term_var (_, v) -> Fmt.string ppf v
    | Term (_, tm) -> braces (Nominal.Term.pp_generic ~open_loc ~close_loc) ppf tm
    | Lambda (_, ty, (Single_var.{ name; info = _ }, body)) ->
      pf
        ppf
        "@[<hv>\\@[<hv>(%s : %a)@] ->@ %a@]"
        name
        (Type.pp_generic ~open_loc ~close_loc)
        ty
        pp
        body
    (* TODO: parens if necessary *)
    | Ap (_, f, args) ->
      pf ppf "@[<h>%a@ @[<hov>%a@]@]" pp f (list ~sep:sp pp) (List_model.to_list args)
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
    | Let (_, is_rec, tm, ty, (Single_var.{ name; info = _ }, body)) ->
      let_ ~open_loc ~close_loc ppf is_rec tm ty name body);
    close_loc ppf (Lang.Term.info tm)

  and let_ ~open_loc ~close_loc ppf is_rec tm ty name body =
    let pp_ty ppf = function
      | Option_model.Option.Some (_, ty) ->
        pf ppf ": %a" (Type.pp_generic ~open_loc ~close_loc) ty
      | None _ -> ()
    in
    pf
      ppf
      "@[let %s%s%a =@ %a in@ @[%a@]@]"
      (match is_rec with Rec _ -> "rec " | No_rec _ -> "")
      name
      pp_ty
      ty
      (term ~open_loc ~close_loc)
      tm
      (term ~open_loc ~close_loc)
      body

  and cases ~open_loc ~close_loc ppf x =
    list
      ~sep:(any "@;<1 2>| ")
      (case_scope ~open_loc ~close_loc)
      ppf
      (List_model.to_list x)

  and case_scope ~open_loc ~close_loc ppf (Lang.Case_scope.Case_scope (info, pat, body)) =
    open_loc ppf info;
    pf
      ppf
      "@[%a@ -> %a@]"
      (Binding_aware_pattern.pp_generic ~open_loc ~close_loc)
      (Binding_aware_pattern_model.out pat)
      (term ~open_loc ~close_loc)
      body;
    close_loc ppf info
  ;;
end

let pp_term ppf tm =
  Pp_generic.term ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf tm
;;

module Parse = struct
  open Lvca_parsing
  open Lang

  let reserved = Lvca_util.String.Set.of_list [ "let"; "rec"; "in"; "match"; "with" ]

  let identifier =
    Ws.identifier
    >>= fun ident ->
    if Set.mem reserved ident
    then fail (Printf.sprintf "identifier: reserved word (%s)" ident)
    else return ident
  ;;

  let make_apps : 'a Commented.t Term.t list -> 'a Commented.t Term.t = function
    | [] -> Lvca_util.invariant_violation ~here:[%here] "must be a nonempty list"
    | [ x ] -> x
    | f :: args as xs ->
      let pos =
        xs |> List.map ~f:(Term.info >> Commented.get_range) |> Opt_range.list_range
      in
      Ap
        ( Commented.{ range = pos; comment = None }
        , f
        , List_model.of_list ~empty_info:Commented.none args )
  ;;

  let term ~comment =
    fix (fun term ->
        let parse_prim =
          choice
            [ (Primitive_impl.All.parse ~comment
              >>| fun prim -> Nominal.Term.Primitive prim)
            ; Ws.braces term >>| Term.to_nominal
            ]
        in
        let atomic_term =
          choice
            ~failure_msg:
              "looking for a parenthesized term, identifier, or expression in braces"
            [ Ws.parens term
            ; (identifier
              >>== fun { value; range } ->
              option' comment
              >>|| fun { value = comment; _ } ->
              { value = Term.Term_var (Commented.{ range; comment }, value); range })
            ; Ws.braces (Nominal.Term.parse ~parse_prim ~comment)
              >>|| (fun { value = tm; range } ->
                     (* TODO: add comments *)
                     { value = Term.Term (Commented.of_opt_range range, tm); range })
              <?> "quoted term"
            ]
        in
        let pattern =
          Binding_aware_pattern.parse ~comment
          >>| Binding_aware_pattern_model.into ~empty_info:Commented.none
          <?> "pattern"
        in
        let case_line =
          lift3
            (fun pat _ tm -> Case_scope.Case_scope (Commented.none, pat, tm))
            pattern
            (Ws.string "->")
            term
          <?> "case line"
        in
        choice
          ~failure_msg:"looking for a lambda, let, match, or application"
          [ lift4
              (fun (_, lam_loc) ((name, ty), parens_loc) _ body ->
                let info =
                  Commented.{ range = Opt_range.union lam_loc parens_loc; comment = None }
                in
                Term.Lambda (info, ty, (Single_var.{ info = Commented.none; name }, body)))
              (attach_pos (Ws.char '\\'))
              (attach_pos
                 (Ws.parens
                    (lift3
                       (fun ident _ ty -> ident, ty)
                       identifier
                       (Ws.char ':')
                       (Type.parse ~comment))))
              (Ws.string "->")
              term
            <?> "lambda"
          ; lift4
              (fun (_let, let_pos) is_rec name ty _eq tm _in (body, body_pos) ->
                let info =
                  Commented.{ range = Opt_range.union let_pos body_pos; comment = None }
                in
                Term.Let
                  ( info
                  , is_rec
                  , tm
                  , ty
                  , (Single_var.{ name; info = Commented.none }, body) ))
              (attach_pos (Ws.string "let"))
              Lang.Is_rec.(
                option
                  (No_rec Commented.none)
                  (Ws.string "rec"
                  >>|| fun { range; _ } ->
                  { range; value = Rec Commented.{ range; comment = None } }))
              identifier
              (option
                 (Option_model.Option.None Commented.none)
                 (Ws.char ':' *> Type.parse ~comment
                 >>| fun tm -> Option_model.Option.Some (Commented.none, tm)))
            <*> Ws.string "="
            <*> term
            <*> Ws.string "in"
            <*> attach_pos term
            <?> "let"
          ; lift4
              (fun (_match, match_pos) tm _with (lines, lines_pos) ->
                let pos = Opt_range.union match_pos lines_pos in
                Term.Case (Commented.{ range = pos; comment = None }, tm, lines))
              (attach_pos (Ws.string "match"))
              term
              (Ws.string "with")
              (attach_pos
                 (Ws.braces (option '|' (Ws.char '|') *> sep_by (Ws.char '|') case_line)
                 >>| List_model.of_list ~empty_info:Commented.none))
            <?> "match"
          ; many1 atomic_term >>| make_apps <?> "application"
          ])
    <?> "core term"
  ;;
end

module Term = struct
  include Nominal.Convertible.Extend (Lang.Term)

  let parse_concrete = Parse.term
  let pp_generic_concrete = Pp_generic.term

  let pp_concrete ppf t =
    pp_generic_concrete ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t
  ;;
end

module Module = struct
  type 'info t =
    { externals : (string * 'info Type.t) list
    ; defs : (string * 'info Lang.Term.t) list
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
      pf ppf "%s := %a" name (Pp_generic.term ~open_loc ~close_loc) sort_def
    in
    pf ppf "%a@,%a" pp_externals externals (list pp_def) defs
  ;;

  let pp ppf tm = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf tm

  let parse ~comment =
    let open Lvca_parsing in
    let external_decl =
      lift4
        (fun ident _ ty _ -> ident, ty)
        Ws.identifier
        (Ws.string ":")
        (Type.parse ~comment)
        (Ws.string ";")
    in
    let def =
      lift3
        (fun ident _ tm -> ident, tm)
        Ws.identifier
        (Ws.string ":=")
        (Parse.term ~comment)
    in
    lift2 (fun externals defs -> { externals; defs }) (many external_decl) (many1 def)
  ;;
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
        ('info, ('info Pattern.t, 'info Nominal.Term.t) Either.t) Check_failure.t

  let pp ppf tm (ty_opt : _ option Type.t option) = function
    | Cant_infer_case -> Fmt.pf ppf "can't infer cases"
    | Cant_infer_lambda -> Fmt.pf ppf "can't infer lambdas"
    | Var_not_found ->
      (match tm with
      | Lang.Term.Term (_, Nominal.Term.Var (_, name)) ->
        Fmt.pf ppf "variable %s not found" name
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
      Fmt.pf ppf "failed to infer type of term %a @[(%s)@]" pp_term tm explanation
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
    ; tm : 'info Lang.Term.t
    ; ty : 'info Type.t
    ; error : 'info Check_error'.t
    }

  let pp ppf { env = _; tm; ty; error } = Check_error'.pp ppf tm (Some ty) error
end

module Infer_error = struct
  type 'info t =
    { env : 'info check_env
    ; tm : 'info Lang.Term.t
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
    Ok (Lang.Ty.Sort (None, Sort_model.Sort.Name (None, (None, name))))
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
  | Lang.Term.Term (_, tm') ->
    (match ty with
    | Lang.Ty.Arrow _ -> Some Check_error.{ env; tm; ty; error = Term_isnt_arrow }
    | Sort (_, sort) ->
      Nominal.Term.check syntax (Sort.out sort) tm'
      |> Option.map ~f:(fun err ->
             Check_error.{ env; tm; ty; error = Failed_check_term err }))
  | Ap _ -> failwith "TODO"
  | Case (_, tm, branches) ->
    (match infer env tm with
    | Error Infer_error.{ env; tm; error } -> Some { env; tm; ty; error }
    | Ok _tm_ty ->
      (* TODO *)
      branches
      |> List_model.to_list
      |> List.find_map ~f:(fun (Lang.Case_scope.Case_scope (_, _pat, rhs)) ->
             check env rhs ty))
  | Lambda (_, ty', (Single_var.{ name; info = _ }, body)) ->
    let type_env = Map.set type_env ~key:name ~data:ty' in
    check { type_env; syntax } body ty
  | Let (_, _, tm, ty', (Single_var.{ name; info = _ }, body)) ->
    let inferred_tm_ty =
      match ty' with None _ -> infer env tm | Some (_, tm_ty) -> Ok tm_ty
    in
    (match inferred_tm_ty with
    | Ok tm_ty ->
      let type_env = Map.set type_env ~key:name ~data:tm_ty in
      check { env with type_env } body ty
    | Error Infer_error.{ env; tm; error } -> Some Check_error.{ env; tm; ty; error })
  | Term_var _ ->
    (match infer env tm with
    | Error { env; tm; error } -> Some { env; tm; ty; error }
    | Ok ty' -> check_ty ty')

and infer ({ type_env; syntax = _ } as env) tm =
  match tm with
  | Lang.Term.Term (_, tm') ->
    (match infer_term type_env tm' with
    | Error reason -> Error { env; tm; error = Failed_term_inference reason }
    | Ok ty -> Ok ty)
  | Ap (_, f, args) ->
    let _arg_tys = args |> List_model.to_list |> List.map ~f:(infer env) |> Result.all in
    (* XXX add arg tys to env *)
    infer env f
  | Case _ -> Error { env; tm; error = Cant_infer_case }
  | Lambda _ -> Error { env; tm; error = Cant_infer_lambda }
  | Let (_, _, tm, ty, (Single_var.{ name; info = _ }, body)) ->
    let%bind tm_ty = match ty with None _ -> infer env tm | Some (_, ty) -> Ok ty in
    let type_env = Map.set type_env ~key:name ~data:tm_ty in
    infer { env with type_env } body
  | Term_var (_, name) ->
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
  |> List.find_map ~f:(fun (Lang.Case_scope.Case_scope (_, pat, rhs)) ->
         let pat = Binding_aware_pattern_model.out pat in
         match match_pattern v pat with
         | None -> None
         | Some bindings -> Some (rhs, bindings))
;;

type 'a eval_error = string * 'a Lang.Term.t

let true_tm info = Nominal.Term.Operator (info, "true", [])
let false_tm info = Nominal.Term.Operator (info, "false", [])

let eval_char_bool_fn ~no_info eval_nominal_in_ctx name f ctx tm c =
  let%bind c = eval_nominal_in_ctx ~no_info ctx c in
  match c with
  | Nominal.Term.Primitive (info, Char c) ->
    Ok (if f c then true_tm info else false_tm info)
  | _ -> Error (Printf.sprintf "Invalid argument to %s" name, tm)
;;

let rec eval_in_ctx ~no_info ctx tm =
  let go = eval_in_ctx ~no_info in
  match tm with
  | Lang.Term.Term_var (_, v) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, tm))
  | Case (_, tm, branches) ->
    let%bind tm_val = go ctx tm in
    (match find_match tm_val (List_model.to_list branches) with
    | None -> Error ("no match found in case", Term (no_info, tm_val))
    | Some (branch, bindings) -> go (Lvca_util.Map.union_right_biased ctx bindings) branch)
  | Ap (_, Lambda (_, _ty, (Single_var.{ name; info = _ }, body)), Cons (_, arg, Nil _))
    ->
    let%bind arg_val = go ctx arg in
    go (Map.set ctx ~key:name ~data:arg_val) body
  | Ap (_, Term_var (_, name), args) ->
    if Map.mem ctx name
    then failwith "TODO"
    else eval_primitive ~no_info go eval_nominal_in_ctx ctx tm name args
  | Term (_, tm) -> Ok (Nominal.Term.subst_all ctx tm)
  | Let (_, _, tm, _, (Single_var.{ name; info = _ }, body)) ->
    let%bind tm_val = go ctx tm in
    go (Map.set ctx ~key:name ~data:tm_val) body
  | _ -> Error ("Found a term we can't evaluate", tm)

and eval_nominal_in_ctx ~no_info ctx tm =
  match tm with
  | Nominal.Term.Var (_, v) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, Lang.Term.Term (no_info, tm)))
  | _ -> Ok tm

and eval_primitive ~no_info eval_in_ctx eval_nominal_in_ctx ctx tm name args =
  let open Nominal.Term in
  let%bind args =
    args |> List_model.to_list |> List.map ~f:(eval_in_ctx ctx) |> Result.all
  in
  match name, args with
  | "add", [ a; b ] ->
    let%bind a_result = eval_nominal_in_ctx ~no_info ctx a in
    let%bind b_result = eval_nominal_in_ctx ~no_info ctx b in
    (match a_result, b_result with
    | Primitive (info, Integer a'), Primitive (_binfo, Integer b') ->
      (* XXX can't reuse info *)
      Ok (Nominal.Term.Primitive (info, Integer Z.(a' + b')))
    | _ -> Error ("Invalid arguments to add", tm))
  | "sub", [ a; b ] ->
    let%bind a_result = eval_nominal_in_ctx ~no_info ctx a in
    let%bind b_result = eval_nominal_in_ctx ~no_info ctx b in
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
             Nominal.Term.Primitive (info, String (String.of_char_list cs)))
      |> Result.map_error ~f:(fun msg -> msg, tm)
    | _ -> Error ("expected a list of characters", tm))
  | "var", [ str_tm ] ->
    (match str_tm with
    | Primitive (info, String name) -> Ok (Nominal.Term.Var (info, name))
    | _ -> Error ("expected a string", tm))
  | "is_digit", [ c ] ->
    eval_char_bool_fn ~no_info eval_nominal_in_ctx "is_digit" Char.is_digit ctx tm c
  | "is_lowercase", [ c ] ->
    eval_char_bool_fn
      ~no_info
      eval_nominal_in_ctx
      "is_lowercase"
      Char.is_lowercase
      ctx
      tm
      c
  | "is_uppercase", [ c ] ->
    eval_char_bool_fn
      ~no_info
      eval_nominal_in_ctx
      "is_uppercase"
      Char.is_uppercase
      ctx
      tm
      c
  | "is_alpha", [ c ] ->
    eval_char_bool_fn ~no_info eval_nominal_in_ctx "is_alpha" Char.is_alpha ctx tm c
  | "is_alphanum", [ c ] ->
    eval_char_bool_fn ~no_info eval_nominal_in_ctx "is_alphanum" Char.is_alphanum ctx tm c
  | "is_whitespace", [ c ] ->
    eval_char_bool_fn
      ~no_info
      eval_nominal_in_ctx
      "is_whitespace"
      Char.is_whitespace
      ctx
      tm
      c
  | _ ->
    failwith
      (Fmt.str
         "Unknown function (%s), or wrong number of arguments (got [%a])"
         name
         Fmt.(list Nominal.Term.pp)
         args)
;;

let eval ~no_info core = eval_in_ctx ~no_info SMap.empty core

let parse_exn =
  Lvca_parsing.(parse_string (Parse.term ~comment:c_comment)) >> Result.ok_or_failwith
;;

let none = Option_model.Option.None ()
let list xs = List_model.of_list ~empty_info:() xs
let pat_var name = Binding_aware_pattern_model.Pattern.Var ((), ((), name))
let pat_var' name = Binding_aware_pattern_model.Scope.Scope ((), list [], pat_var name)
let ignored = pat_var "_"

let bpat_operator tag children =
  Binding_aware_pattern_model.Pattern.Operator ((), ((), tag), list children)
;;

let operator tag children = Nominal.Term.Operator ((), tag, children)
let ap f a = Lang.Term.Ap ((), f, list a)
let var name = Lang.Term.Term_var ((), name)
let single_var name = Single_var.{ name; info = () }
let case tm case_scopes = Lang.Term.Case ((), tm, list case_scopes)
let case_scope bpat tm = Lang.Case_scope.Case_scope ((), bpat, tm)
let term tm = Lang.Term.Term ((), tm)

let%test_module "Parsing" =
  (module struct
    module Term = Lang.Term
    module Case_scope = Lang.Case_scope

    let parse = parse_exn >> Term.erase

    let ( = ) x y =
      Nominal.Term.equal ~info_eq:Unit.( = ) (Term.to_nominal x) (Term.to_nominal y)
    ;;

    let one = Nominal.Term.Primitive ((), Integer (Z.of_int 1))

    let%test _ = parse "{1}" = Term.Term ((), one)
    let%test _ = parse "{true()}" = Term ((), operator "true" [])
    let%test _ = parse "not x" = ap (var "not") [ var "x" ]

    let%test _ =
      parse "let str = string_of_chars chars in {var(str)}"
      = Let
          ( ()
          , No_rec ()
          , ap (var "string_of_chars") [ var "chars" ]
          , none
          , ( single_var "str"
            , Term ((), operator "var" Nominal.[ Scope.Scope ([], Term.Var ((), "str")) ])
            ) )
    ;;

    let%test _ =
      parse {|\(x : bool) -> x|}
      = Lambda ((), Lang.Ty.Sort ((), Name ((), ((), "bool"))), (single_var "x", var "x"))
    ;;

    let%test _ =
      parse {|match x with { _ -> {1} }|}
      = case (var "x") [ case_scope ignored (Term ((), one)) ]
    ;;

    let%test _ = parse {|match empty with { }|} = Case ((), var "empty", Nil ())

    let%test _ =
      parse {|match x with { | _ -> {1} }|}
      = case (var "x") [ case_scope ignored (Term ((), one)) ]
    ;;

    let%test _ =
      parse {|match x with { true() -> {false()} | false() -> {true()} }|}
      = case
          (var "x")
          [ case_scope (bpat_operator "true" []) (Term ((), operator "false" []))
          ; case_scope (bpat_operator "false" []) (Term ((), operator "true" []))
          ]
    ;;

    let%test _ =
      parse "let x = {true()} in not x"
      = Let
          ( ()
          , No_rec ()
          , Term ((), operator "true" [])
          , none
          , (single_var "x", ap (var "not") [ var "x" ]) )
    ;;

    let%test _ =
      let (_ : _ Term.t) = parse {|fail {"some reason for failing"}|} in
      true
    ;;

    let%test _ =
      let (_ : _ Term.t) = parse {|match c with { 'c' -> {true()} | _ -> {false()} }|} in
      true
    ;;

    let%test _ =
      let (_ : _ Term.t) = parse "{some({let x = {1} in {some({x})}})}" in
      true
    ;;
  end)
;;

let%test_module "Core parsing" =
  (module struct
    module Term = Lang.Term

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

    let c_var name = Term.Term_var ((), name)
    let t_operator tag children = Nominal.Term.Operator ((), tag, children)
    let meaning x = ap (c_var "meaning") [ x ]
    let ty = Lang.Ty.Sort ((), Name ((), ((), "ty")))

    let dynamics =
      Term.Lambda
        ( ()
        , ty
        , ( single_var "tm"
          , Case
              ( ()
              , c_var "tm"
              , list
                  [ case_scope (bpat_operator "true" []) (term (t_operator "true" []))
                  ; case_scope (bpat_operator "false" []) (term (t_operator "false" []))
                  ; case_scope
                      (bpat_operator
                         "ite"
                         [ pat_var' "t1"; pat_var' "t2"; pat_var' "t3" ])
                      (case
                         (meaning (c_var "t1"))
                         [ case_scope (bpat_operator "true" []) (meaning (c_var "t2"))
                         ; case_scope (bpat_operator "false" []) (meaning (c_var "t3"))
                         ])
                  ; case_scope
                      (bpat_operator "ap" [ pat_var' "f"; pat_var' "arg" ])
                      (ap (meaning @@ c_var "f") [ meaning (c_var "arg") ])
                  ; case_scope
                      (bpat_operator "fun" [ pat_var' "scope" ])
                      (term
                         (t_operator
                            "lambda"
                            [ scope @@ t_operator "list" []; scope @@ Var ((), "scope") ]))
                  ] ) ) )
    ;;

    let%test "dynamics as expected" =
      let ( = ) = Lang.Term.equal ~info_eq:Unit.( = ) in
      parse_exn dynamics_str |> Lang.Term.erase = dynamics
    ;;
  end)
;;

let%test_module "Core eval" =
  (module struct
    let eval_str str =
      let result =
        match str |> parse_exn |> eval ~no_info:Commented.none with
        | Error (msg, tm) -> Fmt.str "%s: %a" msg Lang.Term.pp tm
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

    let comment = Lvca_parsing.no_comment
    let term = mk_test (Parse.term ~comment) pp_term
    let ty = mk_test (Type.parse ~comment) Type.pp
    let module' = mk_test (Module.parse ~comment) Module.pp

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
    module Term = Lang.Term

    let eval_in dynamics_str str =
      let defn = dynamics_str |> parse_exn |> Term.erase in
      let core = str |> parse_exn |> Term.erase in
      match eval ~no_info:() (ap defn [ core ]) with
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
    open Abstract_syntax
    module Term = Lang.Term

    let parse_type str =
      Lvca_parsing.parse_string (Type.parse ~comment:(Lvca_parsing.fail "no comment")) str
      |> Result.ok_or_failwith
      |> Type.map_info ~f:(Fn.const None)
    ;;

    let externals = [ "option", Kind.Kind (None, 2) ]

    let sort_defs =
      [ ( "bool"
        , Sort_def.Sort_def
            ( []
            , [ Operator_def.Operator_def (None, "true", Arity (None, []))
              ; Operator_def.Operator_def (None, "false", Arity (None, []))
              ] ) )
      ; ( "list"
        , let a = Lvca_syntax.Sort.Name (None, "a") in
          Sort_def.Sort_def
            ( [ "a", Some (Kind.Kind (None, 1)) ]
            , [ Operator_def.Operator_def (None, "nil", Arity (None, []))
              ; Operator_def.Operator_def
                  ( None
                  , "cons"
                  , Arity
                      ( None
                      , [ Valence.Valence ([], a)
                        ; Valence.Valence
                            ( []
                            , Lvca_syntax.Sort.Ap
                                ( None
                                , "list"
                                , Lvca_syntax.Sort.Ap_list.of_list
                                    ~default_info:None
                                    [ a ] ) )
                        ] ) )
              ] ) )
      ]
    ;;

    let syntax = Abstract_syntax.{ externals; sort_defs }

    let check ?(type_env = SMap.empty) ty_str tm_str =
      let tm = tm_str |> parse_exn |> Term.map_info ~f:(Fn.const None) in
      let ty = ty_str |> parse_type |> Type.map_info ~f:(Fn.const None) in
      let ctx = { type_env; syntax } in
      match check ctx tm ty with
      | Some err -> Check_error.pp Fmt.stdout err
      | None -> Fmt.pr "checked"
    ;;

    let infer ?(type_env = SMap.empty) tm_str =
      let tm = tm_str |> parse_exn |> Term.map_info ~f:(Fn.const None) in
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

    let type_env =
      SMap.of_alist_exn
        [ "x", Lang.Ty.Sort (None, Sort_model.Sort.Name (None, (None, "a"))) ]
    ;;

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
