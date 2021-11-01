(** A small "core" language. *)

open Base
open Lvca_provenance
open Lvca_syntax
open Lvca_util
open Result.Let_syntax
module Format = Stdlib.Format

module List_model = struct
  include [%lvca.abstract_syntax_module "list a := Nil() | Cons(a; list a)"]

  let rec of_list xs =
    let info = Provenance.of_here [%here] in
    match xs with [] -> List.mk_Nil ~info | x :: xs -> Cons (info, x, of_list xs)
  ;;

  let rec to_list xs =
    match xs with List.Nil _ -> [] | Cons (_, x, xs) -> x :: to_list xs
  ;;

  let rec map ~f = function
    | List.Nil i -> List.Nil i
    | Cons (i, x, xs) -> Cons (i, f x, map ~f xs)
  ;;
end

module Option_model = struct
  include [%lvca.abstract_syntax_module "option a := None() | Some(a)"]

  let of_option x =
    let info = Provenance.of_here [%here] in
    match x with None -> Option.None info | Some a -> Some (info, a)
  ;;

  let to_option = function Option.None _ -> None | Some (_, a) -> Some a
  let map ~f = function Option.None i -> Option.None i | Some (i, a) -> Some (i, f a)
end

module Empty = struct
  include [%lvca.abstract_syntax_module "empty :="]

  type t = Empty.t

  let pp _ppf = function (_ : t) -> .
  let parse = Lvca_parsing.fail "(empty type)"
end

module Binding_aware_pattern_model = struct
  include
    [%lvca.abstract_syntax_module
    {|
string : *
primitive : *
list : * -> *

pattern :=
  | Operator(string; list scope)
  | Primitive(primitive)
  | Var(string)

scope := Scope(list string; pattern)
|}
    , { string = "Primitive.String"
      ; primitive = "Primitive.All"
      ; list = "List_model.List"
      }]

  let rec into tm =
    let info = Provenance.calculated_here [%here] [ Binding_aware_pattern.info tm ] in
    match tm with
    | Binding_aware_pattern.Operator (_, str, scopes) ->
      let scopes = scopes |> List.map ~f:scope |> List_model.of_list in
      Pattern.Operator (info, (info, str), scopes)
    | Primitive (_, prim) -> Primitive (info, (info, prim))
    | Var (_, str) -> Var (info, (info, str))

  and scope (Binding_aware_pattern.Scope (names, pat)) =
    Scope.Scope (Provenance.of_here [%here], List_model.of_list names, into pat)
  ;;

  let rec out tm =
    let info = Provenance.calculated_here [%here] [ Pattern.info tm ] in
    match tm with
    | Pattern.Operator (_, (_, str), scopes) ->
      let scopes = scopes |> List_model.to_list |> List.map ~f:scope in
      Binding_aware_pattern.Operator (info, str, scopes)
    | Primitive (_, (_, prim)) -> Primitive (info, prim)
    | Var (_, (_, str)) -> Var (info, str)

  and scope (Scope.Scope (_, names, pat)) =
    Binding_aware_pattern.Scope (List_model.to_list names, out pat)
  ;;
end

module Sort_model =
[%lvca.abstract_syntax_module
{|
string : *

sort :=
  | Ap(string; ap_list)
  | Name(string)

ap_list :=
  | Nil()
  | Cons(sort; ap_list)
|}
, { string = "Primitive.String" }]

module Sort = struct
  include Nominal.Convertible.Extend (Sort_model.Sort)

  let rec into tm =
    let info = Provenance.calculated_here [%here] [ Lvca_syntax.Sort.info tm ] in
    match tm with
    | Lvca_syntax.Sort.Ap (_, name, ap_list) ->
      Sort_model.Sort.Ap (info, (info, name), into_ap_list ap_list)
    | Name (_, name) -> Name (info, (info, name))

  and into_ap_list tm =
    let info = Provenance.calculated_here [%here] [ Lvca_syntax.Sort.Ap_list.info tm ] in
    match tm with
    | Lvca_syntax.Sort.Nil _ -> Sort_model.Ap_list.Nil info
    | Cons (_, sort, ap_list) -> Cons (info, into sort, into_ap_list ap_list)
  ;;

  let rec out tm =
    let info = Provenance.calculated_here [%here] [ Sort_model.Sort.info tm ] in
    match tm with
    | Sort_model.Sort.Ap (_, (_, name), ap_list) ->
      Lvca_syntax.Sort.Ap (info, name, out_ap_list ap_list)
    | Name (_, (_, name)) -> Name (info, name)

  and out_ap_list tm =
    let info = Provenance.calculated_here [%here] [ Sort_model.Ap_list.info tm ] in
    match tm with
    | Sort_model.Ap_list.Nil _ -> Lvca_syntax.Sort.Nil info
    | Cons (_, sort, ap_list) -> Cons (info, out sort, out_ap_list ap_list)
  ;;
end

module Type = struct
  include
    [%lvca.abstract_syntax_module
    {|
sort : *

ty := Sort(sort) | Arrow(ty; ty)
      |}, { sort = "Sort_model.Sort" }]

  include Nominal.Convertible.Extend (Ty)
  include Ty

  let pp ppf =
    let rec go need_parens ppf = function
      | Arrow (_, t1, t2) ->
        Fmt.pf
          ppf
          (if need_parens then "@[<hv>(%a -> %a)@]" else "%a -> %a")
          (go true)
          t1
          (go false)
          t2
      | Sort (_, s) -> Lvca_syntax.Sort.pp ppf (Sort.out s)
    in
    go false ppf
  ;;

  module Parse = struct
    open Lvca_parsing
    module Ws = C_comment_parser

    let arrow s1 s2 = Arrow (Provenance.of_here [%here], s1, s2)

    let rec of_list = function
      | [] ->
        Lvca_util.invariant_violation
          ~here:[%here]
          "of_list must be called with a non-empty list"
      | [ sort ] -> sort
      | sort :: sorts -> arrow sort (of_list sorts)
    ;;

    let t =
      fix (fun t ->
          let atom =
            Ws.parens t
            <|> (Lvca_syntax.Sort.parse
                >>| fun sort ->
                let sort = Sort.into sort in
                Sort (Sort.info sort, sort))
          in
          sep_by1 (Ws.string "->") atom >>| of_list)
      <?> "core type"
    ;;

    let%test_module "parsing" =
      (module struct
        let parse = parse_string t >> Result.ok_or_failwith
        let here = Provenance.of_here [%here]
        let bool = Sort_model.Sort.mk_Name ~info:here (here, "bool")
        let ( = ) = equivalent

        let list_bool =
          Sort_model.Sort.mk_Ap
            ~info:here
            (here, "list")
            Sort_model.Ap_list.(mk_Cons ~info:here bool (mk_Nil ~info:here))
        ;;

        let%test _ = parse "bool" = Sort (here, bool)
        let%test _ = parse "list bool" = Sort (here, list_bool)
      end)
    ;;

    let%test_module "of_list" =
      (module struct
        let here = Provenance.of_here [%here]
        let s = Sort_model.Sort.mk_Name ~info:here (here, "s")
        let mk_Sort s = mk_Sort ~info:here s

        let%test _ = of_list [ mk_Sort s ] = mk_Sort s
        let%test _ = of_list [ mk_Sort s; mk_Sort s ] = arrow (mk_Sort s) (mk_Sort s)

        let%test _ =
          of_list [ mk_Sort s; mk_Sort s; mk_Sort s ]
          = arrow (mk_Sort s) (arrow (mk_Sort s) (mk_Sort s))
        ;;

        let%test _ =
          of_list [ mk_Sort s; mk_Sort s; mk_Sort s; mk_Sort s ]
          = arrow (mk_Sort s) (arrow (mk_Sort s) (arrow (mk_Sort s) (mk_Sort s)))
        ;;

        let%test _ =
          of_list [ of_list [ mk_Sort s; mk_Sort s ]; mk_Sort s ]
          = arrow (arrow (mk_Sort s) (mk_Sort s)) (mk_Sort s)
        ;;

        let%test _ =
          of_list [ mk_Sort s; of_list [ mk_Sort s; mk_Sort s ] ]
          = arrow (mk_Sort s) (arrow (mk_Sort s) (mk_Sort s))
        ;;

        let%test _ =
          of_list [ arrow (mk_Sort s) (mk_Sort s); arrow (mk_Sort s) (mk_Sort s) ]
          = arrow (arrow (mk_Sort s) (mk_Sort s)) (arrow (mk_Sort s) (mk_Sort s))
        ;;
      end)
    ;;
  end

  let parse = Parse.t
end

module Lang = struct
  include
    [%lvca.abstract_syntax_module
    {|
ty : *
nominal : *
list : * -> *
option : * -> *
binding_aware_pattern : * -> *
string : *

letrec_row := Letrec_row(ty; term)

term :=
  | Embedded(nominal)
  | Ap(term; list term)
  | Case(term; list case_scope)
  | Lambda(ty; term. term)
  | Let(term; option ty; term. term)
  // let rec defines a group (represented as a list but unordered) of definitions at once
  | Let_rec(list letrec_row; (list empty)[term]. term)
  | Subst(term. term; term)

case_scope := Case_scope(binding_aware_pattern; term)
|}
    , { ty = "Type"
      ; nominal = "Nominal.Term"
      ; list = "List_model.List"
      ; option = "Option_model.Option"
      ; binding_aware_pattern = "Binding_aware_pattern_model.Pattern"
      ; string = "Primitive.String"
      ; empty = "Empty"
      }]

  module Term = struct
    include Nominal.Convertible.Extend (Term)
    include Term
  end

  module Case_scope = struct
    include Nominal.Convertible.Extend (Case_scope)
    include Case_scope
  end
end

let extract_vars_from_empty_list_pattern pat =
  let rec go = function
    | Pattern.Operator (_, "Nil", []) -> []
    | Operator (_, "Cons", [ Var (_, name); pats ]) -> name :: go pats
    | _ -> Lvca_util.invariant_violation ~here:[%here] "Invalid empty list pattern"
  in
  go pat
;;

module Pp = struct
  let braces, list, any, pf, sp = Fmt.(braces, list, any, pf, sp)

  (* TODO: add parse <-> pretty tests *)

  let rec term : Lang.Term.t Fmt.t =
   fun ppf tm ->
    Provenance.open_stag ppf (Lang.Term.info tm);
    match tm with
    | Lang.Term.Term_var (_, v) -> Fmt.string ppf v
    | Embedded (_, tm) -> braces Nominal.Term.pp ppf tm
    | Lambda (_, ty, (Single_var.{ name; info = _ }, body)) ->
      pf ppf "@[<hv>\\@[<hv>(%s : %a)@] ->@ %a@]" name Type.pp ty term body
    (* TODO: parens if necessary *)
    | Ap (_, f, args) ->
      pf ppf "@[<h>%a@ @[<hov>%a@]@]" term f (list ~sep:sp term) (List_model.to_list args)
    | Case (_, arg, cases') ->
      pf
        ppf
        "@[<hv>match %a with {%t%a@ }@]"
        term
        arg
        (* Before `|`, emit a single space if on the same line, or two when broken *)
        (Format.pp_print_custom_break ~fits:("", 1, "") ~breaks:("", 2, "| "))
        cases
        cases'
    | Let (_, tm, ty, (Single_var.{ name; info = _ }, body)) -> let_ ppf tm ty name body
    | Let_rec (_, rows, (binders, rhs)) ->
      let binders = extract_vars_from_empty_list_pattern binders in
      let rows = List_model.to_list rows in
      let pp_bound_row ppf (var_name, Lang.Letrec_row.Letrec_row (_, ty, body)) =
        pf ppf "%s@ :@ %a@ =@ %a" var_name Type.pp ty term body
      in
      (match List.zip binders rows with
      | Unequal_lengths ->
        Lvca_util.invariant_violation
          ~here:[%here]
          "invalid set of letrec binders (must be the same number of terms)"
      | Ok bound_rows ->
        pf
          ppf
          "@[let rec@ %a@ in@ %a]"
          (list pp_bound_row ~sep:(any "@ and@ "))
          bound_rows
          term
          rhs)
    | Subst (_, (Single_var.{ name; info = _ }, body), arg) ->
      let formatter =
        match body with
        | Embedded _ | Subst _ -> pf ppf "@[%a[%s := %a]@]"
        | _ -> pf ppf "@[@[(%a)@][%s := %a]@]"
      in
      formatter term body name term arg;
      Provenance.close_stag ppf (Lang.Term.info tm)

  and let_ ppf tm ty name body =
    let pp_ty ppf = function
      | Option_model.Option.Some (_, ty) -> pf ppf ": %a" Type.pp ty
      | None _ -> ()
    in
    pf ppf "@[let %s%a =@ %a in@ @[%a@]@]" name pp_ty ty term tm term body

  and cases ppf x = list ~sep:(any "@;<1 2>| ") case_scope ppf (List_model.to_list x)

  and case_scope ppf (Lang.Case_scope.Case_scope (info, pat, body)) =
    Provenance.open_stag ppf info;
    pf
      ppf
      "@[%a@ -> %a@]"
      Binding_aware_pattern.pp
      (Binding_aware_pattern_model.out pat)
      term
      body;
    Provenance.close_stag ppf info
  ;;
end

module Parse = struct
  open Lvca_parsing
  module Ws = C_comment_parser
  open Lang

  let reserved =
    Lvca_util.String.Set.of_list [ "let"; "rec"; "and"; "in"; "match"; "with" ]
  ;;

  let char_p = Char.(fun c -> is_alpha c || is_digit c || c = '_' || c = '\'' || c = '.')

  let identifier =
    Ws.(identifier' ~char_p ())
    >>= fun ident ->
    if Set.mem reserved ident
    then fail (Printf.sprintf "identifier: reserved word (%s)" ident)
    else return ident
  ;;

  let make_apps : Term.t list -> Term.t = function
    | [] -> Lvca_util.invariant_violation ~here:[%here] "must be a nonempty list"
    | [ x ] -> x
    | f :: args -> Ap (Provenance.of_here [%here], f, List_model.of_list args)
  ;;

  let rec make_empty_list_pattern vars =
    match vars with
    | [] -> Pattern.Operator (Provenance.of_here [%here], "Nil", [])
    | (v, pos) :: vars ->
      Operator
        ( Provenance.of_here [%here]
        , "Cons"
        , [ Var (v, pos); make_empty_list_pattern vars ] )
  ;;

  let term =
    fix (fun term ->
        let parse_prim =
          choice
            [ (Primitive_impl.All.parse >>| fun prim -> Nominal.Term.Primitive prim)
            ; Ws.braces term >>| Term.to_nominal
            ]
        in
        let atomic_term =
          choice
            ~failure_msg:
              "looking for a parenthesized term, identifier, or expression in braces"
            [ Ws.parens term
            ; (identifier
              >>~ fun range value -> Term.Term_var (Provenance.of_range range, value))
            ; Ws.braces (Nominal.Term.parse ~parse_prim)
              >>~ (fun range tm -> Term.Embedded (Provenance.of_range range, tm))
              <?> "quoted term"
            ]
        in
        let atomic_term' =
          atomic_term
          >>== fun { range = body_range; value = body } ->
          choice
            [ (Ws.brackets
                 (identifier
                 >>= fun name -> Ws.string ":=" >>= fun _ -> term >>| fun arg -> name, arg
                 )
              >>~ fun bracket_range (name, arg) ->
              let range = Opt_range.union bracket_range body_range in
              let info = Provenance.of_range range in
              Term.Subst
                (info, (Single_var.{ info (* TODO: wrong info *); name }, body), arg))
            ; return body
            ]
          (* TODO: add comments *)
        in
        let pattern =
          Binding_aware_pattern.parse >>| Binding_aware_pattern_model.into <?> "pattern"
        in
        let case_line =
          make3
            (fun ~info pat _ tm ->
              Case_scope.Case_scope (Provenance.of_range info, pat, tm))
            pattern
            (Ws.string "->")
            term
          <?> "case line"
        in
        let letrec_row =
          lift4
            (fun (var, var_pos) ty _ (rhs, rhs_pos) ->
              let range = Opt_range.union var_pos rhs_pos in
              ( (Provenance.of_range var_pos, var)
              , Letrec_row.Letrec_row (Provenance.of_range range, ty, rhs) ))
            (attach_pos identifier)
            (Ws.char ':' *> Type.parse)
            (Ws.char '=')
            (attach_pos term)
        in
        choice
          ~failure_msg:"looking for a lambda, let, match, or application"
          [ Ws.char '\\'
            >>== (fun { range = lam_loc; _ } ->
                   lift3
                     (fun (((name, name_loc), ty), parens_loc) _ body ->
                       let range = Opt_range.union lam_loc parens_loc in
                       let info = Provenance.of_range range in
                       Term.Lambda
                         ( info
                         , ty
                         , (Single_var.{ info = Provenance.of_range name_loc; name }, body)
                         ))
                     (attach_pos
                        (Ws.parens
                           (lift3
                              (fun ident _ ty -> ident, ty)
                              (attach_pos identifier)
                              (Ws.char ':')
                              Type.Parse.t)))
                     (Ws.string "->")
                     term)
            <?> "lambda"
          ; No_junk.string "let"
            <* Ws.junk1
            >>== (fun { range = let_pos; _ } ->
                   option' (No_junk.string "rec" <* Ws.junk1)
                   >>= function
                   | Some _rec ->
                     lift3
                       (fun (rows, rows_pos) _ (rhs, rhs_pos) ->
                         let info =
                           Provenance.of_range (Opt_range.union rows_pos rhs_pos)
                         in
                         let binders, rows = List.unzip rows in
                         let rows = List_model.of_list rows in
                         let binders = make_empty_list_pattern binders in
                         Term.Let_rec (info, rows, (binders, rhs)))
                       (attach_pos (sep_by1 (Ws.string "and") letrec_row))
                       (Ws.string "in")
                       (attach_pos term)
                   | None ->
                     lift4
                       (fun (name, name_pos) ty _eq tm _in (body, body_pos) ->
                         let info =
                           Provenance.of_range (Opt_range.union let_pos body_pos)
                         in
                         Term.Let
                           ( info
                           , tm
                           , ty
                           , ( Single_var.{ name; info = Provenance.of_range name_pos }
                             , body ) ))
                       (attach_pos identifier)
                       (option
                          (Option_model.Option.None (Provenance.of_here [%here]))
                          (Ws.char ':' *> Type.Parse.t
                          >>| fun tm ->
                          Option_model.Option.Some (Provenance.of_here [%here], tm)))
                       (Ws.string "=")
                       term
                     <*> Ws.string "in"
                     <*> attach_pos term)
            <?> "let"
          ; Ws.string "match"
            >>== (fun { range = match_pos; _ } ->
                   lift3
                     (fun tm _with (lines, lines_pos) ->
                       let pos = Opt_range.union match_pos lines_pos in
                       Term.Case (Provenance.of_range pos, tm, lines))
                     term
                     (Ws.string "with")
                     (attach_pos
                        (Ws.braces
                           (option '|' (Ws.char '|') *> sep_by (Ws.char '|') case_line)
                        >>| List_model.of_list)))
            <?> "match"
          ; many1 atomic_term' >>| make_apps <?> "application"
          ])
    <?> "core term"
  ;;
end

module Term = struct
  include Nominal.Convertible.Extend (Lang.Term)

  let parse_concrete = Parse.term
  let pp_concrete = Pp.term
  let pp_concrete ppf t = pp_concrete ppf t
end

module Module = struct
  type t =
    { externals : (string * Type.t) list
    ; defs : (string * Lang.Term.t) list
    }

  let pp ppf { externals; defs } =
    let open Fmt in
    let pp_externals ppf = function
      | [] -> ()
      | lst ->
        Fmt.pf ppf "%a;" (list ~sep:semi (pair ~sep:(any " : ") string Type.pp)) lst
    in
    let pp_def ppf (name, sort_def) = pf ppf "%s := %a" name Pp.term sort_def in
    pf ppf "%a@,%a" pp_externals externals (list pp_def) defs
  ;;

  let parse =
    let open Lvca_parsing in
    let module Ws = C_comment_parser in
    let external_decl =
      lift4
        (fun ident _ ty _ -> ident, ty)
        Ws.identifier
        (Ws.string ":")
        Type.Parse.t
        (Ws.string ";")
    in
    let def =
      lift3 (fun ident _ tm -> ident, tm) Ws.identifier (Ws.string ":=") Parse.term
    in
    lift2 (fun externals defs -> { externals; defs }) (many external_decl) (many1 def)
  ;;
end

type eval_env = Term.t String.Map.t

let preimage _ = failwith "TODO"
let reverse _tm _cases = failwith "TODO"

type check_env =
  { type_env : Type.t String.Map.t
  ; syntax : Abstract_syntax.t
  }

module Check_error' = struct
  type t =
    | Cant_infer_case
    | Cant_infer_lambda
    | Var_not_found
    | Operator_not_found
    | Mismatch of Type.t

  let pp ppf tm ty_opt = function
    | Cant_infer_case -> Fmt.pf ppf "can't infer cases"
    | Cant_infer_lambda -> Fmt.pf ppf "can't infer lambdas"
    | Var_not_found ->
      (match tm with
      | Lang.Term.Embedded (_, Nominal.Term.Var (_, name)) ->
        Fmt.pf ppf "variable %s not found" name
      | _ ->
        Lvca_util.invariant_violation
          ~here:[%here]
          Fmt.(str "expected Var (got %a)" Pp.term tm))
    | Operator_not_found -> Fmt.pf ppf "operator not found"
    | Mismatch ty ->
      (match ty_opt with
      | None -> Lvca_util.invariant_violation ~here:[%here] "expected Some ty'"
      | Some ty' -> Fmt.pf ppf "%a != %a" Type.pp ty Type.pp ty')
  ;;
end

module Check_error = struct
  type t =
    { env : check_env
    ; tm : Lang.Term.t
    ; ty : Type.t
    ; error : Check_error'.t
    }

  let pp ppf { env = _; tm; ty; error } = Check_error'.pp ppf tm (Some ty) error
end

module Infer_error = struct
  type t =
    { env : check_env
    ; tm : Lang.Term.t
    ; error : Check_error'.t
    }

  let pp ppf { env = _; tm; error } = Check_error'.pp ppf tm None error
end

let here = Provenance.of_here [%here]
let nominal_ty = Type.Sort (here, Name (here, (here, "nominal")))

let rec check ({ type_env; syntax } as env) tm ty =
  let check_ty ty' =
    if Type.equivalent ty ty'
    then None
    else Some Check_error.{ env; tm; ty; error = Mismatch ty' }
  in
  match tm with
  | Lang.Term.Embedded _ -> check_ty nominal_ty
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
  | Let (_, tm, ty', (Single_var.{ name; info = _ }, body)) ->
    let inferred_tm_ty =
      match ty' with None _ -> infer env tm | Some (_, tm_ty) -> Ok tm_ty
    in
    (match inferred_tm_ty with
    | Ok tm_ty ->
      let type_env = Map.set type_env ~key:name ~data:tm_ty in
      check { env with type_env } body ty
    | Error Infer_error.{ env; tm; error } -> Some Check_error.{ env; tm; ty; error })
  | Let_rec (_, rows, (binders, body)) ->
    (match check_binders env rows binders with
    | Error err -> Some err
    | Ok type_env -> check { env with type_env } body ty)
  | Subst (info, binding, arg) ->
    let info = Provenance.calculated_here [%here] [ info ] in
    check env (Let (info, arg, None info, binding)) ty
  | Term_var _ ->
    (match infer env tm with
    | Error { env; tm; error } -> Some { env; tm; ty; error }
    | Ok ty' -> check_ty ty')

and infer ({ type_env; syntax = _ } as env) tm =
  match tm with
  | Lang.Term.Embedded _ -> Ok nominal_ty
  | Ap (_, f, args) ->
    let _arg_tys = args |> List_model.to_list |> List.map ~f:(infer env) |> Result.all in
    (* XXX add arg tys to env *)
    infer env f
  | Case _ -> Error { env; tm; error = Cant_infer_case }
  | Lambda _ -> Error { env; tm; error = Cant_infer_lambda }
  | Let (_, tm, ty, (Single_var.{ name; info = _ }, body)) ->
    let%bind tm_ty = match ty with None _ -> infer env tm | Some (_, ty) -> Ok ty in
    let type_env = Map.set type_env ~key:name ~data:tm_ty in
    infer { env with type_env } body
  | Let_rec (_, rows, (binders, body)) ->
    (match check_binders env rows binders with
    | Error { env; tm; ty = _; error } -> Error { env; tm; error }
    | Ok type_env -> infer { env with type_env } body)
  | Subst (info, binding, arg) ->
    let info = Provenance.calculated_here [%here] [ info ] in
    infer env (Let (info, arg, None info, binding))
  | Term_var (_, name) ->
    (match Map.find type_env name with
    | None -> Error { env; tm; error = Var_not_found }
    | Some ty' -> Ok ty')

and check_binders ({ type_env; syntax = _ } as env) rows binders =
  let binders = extract_vars_from_empty_list_pattern binders in
  let rows = List_model.to_list rows in
  let defns =
    match List.zip binders rows with
    | Unequal_lengths ->
      Lvca_util.invariant_violation ~here:[%here] "Binder / row mismatch"
    | Ok defns -> defns
  in
  let type_env =
    List.fold
      defns
      ~init:type_env
      ~f:(fun type_env (name, Lang.Letrec_row.Letrec_row (_, ty, _)) ->
        Map.set type_env ~key:name ~data:ty)
  in
  let defn_check_error =
    List.find_map defns ~f:(fun (_name, Letrec_row (_, ty, tm)) ->
        check { env with type_env } tm ty)
  in
  match defn_check_error with None -> Ok type_env | Some err -> Error err
;;

let merge_pattern_context
    : Nominal.Term.t String.Map.t option list -> Nominal.Term.t String.Map.t option
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
      |> String.Map.strict_unions
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
    if Primitive.All.(l1 = l2) then Some String.Map.empty else None
  | tm, Var (_, v) -> Some (String.Map.of_alist_exn [ v, tm ])
  | _ -> None

and match_pattern_scope
    (Binding_aware_pattern.Scope (_XXXnames, pat))
    (Nominal.Scope.Scope (_XXXpats, body))
  =
  match_pattern body pat
;;

let find_match (v : Nominal.Term.t)
    : Lang.Case_scope.t list -> (Term.t * Term.t String.Map.t) option
  =
  List.find_map ~f:(fun (Lang.Case_scope.Case_scope (_, pat, rhs)) ->
      let pat = Binding_aware_pattern_model.out pat in
      match match_pattern v pat with
      | None -> None
      | Some bindings ->
        let here = Provenance.of_here [%here] in
        let bindings = Map.map bindings ~f:(fun v -> Lang.Term.Embedded (here, v)) in
        Some (rhs, bindings))
;;

type eval_error = string * Lang.Term.t

let eval_char_bool_fn eval_nominal_in_ctx name f (ctx : eval_env) tm c =
  let true_tm info = Nominal.Term.Operator (info, "true", []) in
  let false_tm info = Nominal.Term.Operator (info, "false", []) in
  let%bind c = eval_nominal_in_ctx ctx c in
  match c with
  | Nominal.Term.Primitive (info, Char c) ->
    let info = Provenance.calculated_here [%here] [ info ] in
    Ok (if f c then true_tm info else false_tm info)
  | _ -> Error (Printf.sprintf "Invalid argument to %s" name, tm)
;;

type eval_result = (Nominal.Term.t, string * Term.t) Result.t

let rec eval_in_ctx (ctx : eval_env) tm : eval_result =
  match tm with
  | Lang.Term.Term_var (_, v) ->
    (match Map.find ctx v with
    | Some (Embedded (_, v)) -> Ok v
    | Some tm -> Error ("TODO", tm)
    | None -> Error ("Unbound variable " ^ v, tm))
  | Case (info, tm, branches) ->
    let%bind tm_val = eval_in_ctx ctx tm in
    (match find_match tm_val (List_model.to_list branches) with
    | None ->
      let info =
        Provenance.calculated_here
          [%here]
          [ info; Term.info tm; List_model.List.info branches ]
      in
      Error ("no match found in case", Embedded (info, tm_val))
    | Some (branch, bindings) ->
      eval_in_ctx (Lvca_util.Map.union_right_biased ctx bindings) branch)
  | Ap (_, Lambda (_, _ty, (Single_var.{ name; info = _ }, body)), Cons (_, arg, Nil _))
    ->
    let%bind arg_val = eval_in_ctx ctx arg in
    let here = Provenance.of_here [%here] in
    let data = Lang.Term.Embedded (here, arg_val) in
    eval_in_ctx (Map.set ctx ~key:name ~data) body
  | Ap (_, Term_var (_, name), args) ->
    if Map.mem ctx name
    then failwith "TODO"
    else eval_primitive eval_in_ctx eval_nominal_in_ctx ctx tm name args
  | Embedded (_, tm) ->
    let free_vars = Nominal.Term.free_vars tm in
    let restricted_ctx = Map.filter_keys ctx ~f:(fun key -> Set.mem free_vars key) in
    let ctx =
      Map.map restricted_ctx ~f:(function
          | Embedded (_, v) -> v
          | _ -> Lvca_util.invariant_violation ~here:[%here] "TODO")
    in
    Ok (Nominal.Term.subst_all ctx tm)
  | Let_rec (_, rows, (binders, body)) ->
    let binders = extract_vars_from_empty_list_pattern binders in
    let rows = List_model.to_list rows in
    (match List.zip binders rows with
    | Unequal_lengths ->
      Lvca_util.invariant_violation
        ~here:[%here]
        "invalid set of letrec binders (must be the same number of terms)"
    | Ok bound_rows ->
      let ctx =
        List.fold bound_rows ~init:ctx ~f:(fun ctx (name, Letrec_row (_, _ty, body)) ->
            Map.set ctx ~key:name ~data:body)
      in
      eval_in_ctx ctx body)
  | Let (_, tm, _, (Single_var.{ name; info = _ }, body))
  | Subst (_, (Single_var.{ name; _ }, body), tm) ->
    let%bind tm_val = eval_in_ctx ctx tm in
    let here = Provenance.of_here [%here] in
    let data = Lang.Term.Embedded (here, tm_val) in
    eval_in_ctx (Map.set ctx ~key:name ~data) body
  | _ -> Error ("Found a term we can't evaluate", tm)

and eval_nominal_in_ctx (ctx : eval_env) tm : eval_result =
  match tm with
  | Nominal.Term.Var (info, v) ->
    (match Map.find ctx v with
    | Some (Embedded (_, v)) -> Ok v
    | Some _ -> Lvca_util.invariant_violation ~here:[%here] "TODO"
    | None ->
      Error
        ( "Unbound variable " ^ v
        , Lang.Term.Embedded (Provenance.calculated_here [%here] [ info ], tm) ))
  | _ -> Ok tm

and eval_primitive eval_in_ctx eval_nominal_in_ctx (ctx : eval_env) tm name args =
  let open Nominal.Term in
  let%bind args =
    args |> List_model.to_list |> List.map ~f:(eval_in_ctx ctx) |> Result.all
  in
  match name, args with
  | "rename", [ v1; v2; tm' ] ->
    let%bind v1 = eval_nominal_in_ctx ctx v1 in
    let%bind v2 = eval_nominal_in_ctx ctx v2 in
    let%map v1, v2 =
      match v1, v2 with
      | Primitive (_info1, String v1), Primitive (_info2, String v2) -> Ok (v1, v2)
      | _ -> Error ("Invalid arguments to rename", tm)
    in
    Nominal.Term.rename v1 v2 tm'
  | "add", [ a; b ] ->
    let%bind a = eval_nominal_in_ctx ctx a in
    let%bind b = eval_nominal_in_ctx ctx b in
    (match a, b with
    | Primitive (info, Integer a), Primitive (_binfo, Integer b) ->
      (* XXX can't reuse info *)
      Ok (Nominal.Term.Primitive (info, Integer Z.(a + b)))
    | _ -> Error ("Invalid arguments to add", tm))
  | "sub", [ a; b ] ->
    let%bind a = eval_nominal_in_ctx ctx a in
    let%bind b = eval_nominal_in_ctx ctx b in
    (match a, b with
    | Primitive (info, Integer a), Primitive (_binfo, Integer b) ->
      Ok (Nominal.Term.Primitive (info, Integer Z.(a - b)))
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
    Error
      ( Fmt.str
          "Unknown function (%s), or wrong number of arguments (got [%a])"
          name
          Fmt.(list Nominal.Term.pp)
          args
      , tm )
;;

let eval core = eval_in_ctx String.Map.empty core

let parse_exn =
  Lvca_parsing.(parse_string (whitespace *> Parse.term)) >> Result.ok_or_failwith
;;

let none = Option_model.Option.None here
let list xs = List_model.of_list xs
let pat_var name = Binding_aware_pattern_model.Pattern.Var (here, (here, name))

let no_bind_pat_var name =
  Binding_aware_pattern_model.Scope.Scope (here, list [], pat_var name)
;;

let ignored = pat_var "_"

let bpat_operator tag children =
  Binding_aware_pattern_model.Pattern.Operator (here, (here, tag), list children)
;;

let operator tag children = Nominal.Term.Operator (here, tag, children)
let ap f a = Lang.Term.Ap (here, f, list a)
let var name = Lang.Term.Term_var (here, name)
let single_var name = Single_var.{ name; info = here }
let case tm case_scopes = Lang.Term.Case (here, tm, list case_scopes)
let case_scope bpat tm = Lang.Case_scope.Case_scope (here, bpat, tm)
let term tm = Lang.Term.Embedded (here, tm)

let%test_module "Parsing" =
  (module struct
    module Term = Lang.Term
    module Case_scope = Lang.Case_scope

    let ( = ) = Term.equivalent
    let one = Nominal.Term.Primitive (here, Integer (Z.of_int 1))
    let scope : Nominal.Term.t -> Nominal.Scope.t = fun body -> Scope ([], body)

    let%test _ = parse_exn "{1}" = Term.Embedded (here, one)
    let%test _ = parse_exn "{true()}" = Embedded (here, operator "true" [])
    let%test _ = parse_exn "not x" = ap (var "not") [ var "x" ]

    let%test _ =
      parse_exn "let str = string_of_chars chars in {var(str)}"
      = Term.Let
          ( here
          , ap (var "string_of_chars") [ var "chars" ]
          , none
          , ( single_var "str"
            , Embedded (here, operator "var" [ scope (Nominal.Term.Var (here, "str")) ])
            ) )
    ;;

    let%test _ =
      parse_exn {|\(x : bool) -> x|}
      = Lambda
          (here, Type.Sort (here, Name (here, (here, "bool"))), (single_var "x", var "x"))
    ;;

    let%test _ =
      parse_exn {|match x with { _ -> {1} }|}
      = case (var "x") [ case_scope ignored (Embedded (here, one)) ]
    ;;

    let%test _ = parse_exn {|match empty with { }|} = Case (here, var "empty", Nil here)

    let%test _ =
      parse_exn {|match x with { | _ -> {1} }|}
      = case (var "x") [ case_scope ignored (Embedded (here, one)) ]
    ;;

    let%test _ =
      parse_exn {|match x with { true() -> {false()} | false() -> {true()} }|}
      = case
          (var "x")
          [ case_scope (bpat_operator "true" []) (Embedded (here, operator "false" []))
          ; case_scope (bpat_operator "false" []) (Embedded (here, operator "true" []))
          ]
    ;;

    let%test _ =
      parse_exn "let x = {true()} in not x"
      = Let
          ( here
          , Embedded (here, operator "true" [])
          , none
          , (single_var "x", ap (var "not") [ var "x" ]) )
    ;;

    let%test _ =
      let (_ : Term.t) = parse_exn {|fail {"some reason for failing"}|} in
      true
    ;;

    let%test _ =
      let (_ : Term.t) =
        parse_exn {|match c with { 'c' -> {true()} | _ -> {false()} }|}
      in
      true
    ;;

    let%test _ =
      let (_ : Term.t) = parse_exn "{some(1)}" in
      true
    ;;

    let%test _ =
      let (_ : Term.t) = parse_exn "{some({{1}})}" in
      true
    ;;

    let%test _ =
      let (_ : Term.t) = parse_exn "{some({let x = {1} in {some({x})}})}" in
      true
    ;;

    let%test _ =
      let (_ : Term.t) = parse_exn "body[f := reduce arg]" in
      true
    ;;

    let%test _ =
      let (_ : Term.t) = parse_exn {|\(tm : lam) -> tm[x := {Var("y")}]|} in
      true
    ;;

    let%test _ =
      let tm =
        {|
        \(tm: term) -> match tm with {
          | Lam(_) -> tm
          | App(f; arg) -> match reduce f with {
            | Lam(x. body) -> body[f := reduce arg]
            | f' -> {App(f'; {reduce arg})}
          }
          | Real_expr(expr) -> expr
        }
        |}
      in
      let (_ : Term.t) = parse_exn tm in
      true
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

    let c_var name = Term.Term_var (here, name)
    let t_operator tag children = Nominal.Term.Operator (here, tag, children)
    let meaning x = ap (c_var "meaning") [ x ]
    let ty = Type.Sort (here, Name (here, (here, "ty")))

    let dynamics =
      Term.Lambda
        ( here
        , ty
        , ( single_var "tm"
          , Case
              ( here
              , c_var "tm"
              , list
                  [ case_scope (bpat_operator "true" []) (term (t_operator "true" []))
                  ; case_scope (bpat_operator "false" []) (term (t_operator "false" []))
                  ; case_scope
                      (bpat_operator
                         "ite"
                         [ no_bind_pat_var "t1"
                         ; no_bind_pat_var "t2"
                         ; no_bind_pat_var "t3"
                         ])
                      (case
                         (meaning (c_var "t1"))
                         [ case_scope (bpat_operator "true" []) (meaning (c_var "t2"))
                         ; case_scope (bpat_operator "false" []) (meaning (c_var "t3"))
                         ])
                  ; case_scope
                      (bpat_operator "ap" [ no_bind_pat_var "f"; no_bind_pat_var "arg" ])
                      (ap (meaning @@ c_var "f") [ meaning (c_var "arg") ])
                  ; case_scope
                      (bpat_operator "fun" [ no_bind_pat_var "scope" ])
                      (term
                         (t_operator
                            "lambda"
                            [ scope @@ t_operator "list" []
                            ; scope @@ Var (here, "scope")
                            ]))
                  ] ) ) )
    ;;

    let%test "dynamics as expected" =
      let ( = ) = Lang.Term.equivalent in
      parse_exn dynamics_str = dynamics
    ;;
  end)
;;

let%test_module "Core eval" =
  (module struct
    let eval_str str =
      let result =
        match str |> parse_exn |> eval with
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

    let%expect_test _ =
      eval_str "x[x := {1}]";
      [%expect {| 1 |}]
    ;;

    let%expect_test _ =
      eval_str "(sub x {2})[x := {1}]";
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

    let term = mk_test Parse.term Pp.term
    let ty = mk_test Type.Parse.t Type.pp
    let module' = mk_test Module.parse Module.pp

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
      term "(sub x {2})[x := {1}]";
      [%expect {| (sub x {2})[x := {1}] |}]
    ;;

    let%expect_test _ =
      term "let rec x : foo = y in x";
      [%expect {| let rec x : foo = y in x] |}]
    ;;

    let%expect_test _ =
      term "let rec x : foo = y and y : foo = x in x";
      [%expect {| let rec x : foo = y and y : foo = x in x] |}]
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
      let defn = parse_exn dynamics_str in
      let core = parse_exn str in
      match eval (ap defn [ core ]) with
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

let%test_module "Checking / inference" =
  (module struct
    open Abstract_syntax
    module Term = Lang.Term

    let parse_type str =
      Lvca_parsing.parse_string Type.Parse.t str |> Result.ok_or_failwith
    ;;

    let externals = [ "option", Kind.Kind (here, 2) ]

    let sort_defs =
      [ ( "bool"
        , Sort_def.Sort_def
            ( []
            , [ Operator_def.Operator_def (here, "true", Arity (here, []))
              ; Operator_def.Operator_def (here, "false", Arity (here, []))
              ] ) )
      ; ( "list"
        , let a = Lvca_syntax.Sort.Name (here, "a") in
          Sort_def.Sort_def
            ( [ "a", Some (Kind.Kind (here, 1)) ]
            , [ Operator_def.Operator_def (here, "nil", Arity (here, []))
              ; Operator_def.Operator_def
                  ( here
                  , "cons"
                  , Arity
                      ( here
                      , [ Valence.Valence ([], a)
                        ; Valence.Valence
                            ( []
                            , Lvca_syntax.Sort.Ap
                                (here, "list", Lvca_syntax.Sort.Ap_list.of_list [ a ]) )
                        ] ) )
              ] ) )
      ]
    ;;

    let syntax = Abstract_syntax.{ externals; sort_defs }

    let check ?(type_env = String.Map.empty) ty_str tm_str =
      let tm = parse_exn tm_str in
      let ty = parse_type ty_str in
      let ctx = { type_env; syntax } in
      match check ctx tm ty with
      | Some err -> Check_error.pp Fmt.stdout err
      | None -> Fmt.pr "checked"
    ;;

    let infer ?(type_env = String.Map.empty) tm_str =
      let tm = parse_exn tm_str in
      let ctx = { type_env; syntax } in
      match infer ctx tm with
      | Error err -> Infer_error.pp Fmt.stdout err
      | Ok ty -> Fmt.pr "inferred: %a" Type.pp ty
    ;;

    let%expect_test _ =
      check "nominal" "{true()}";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check "nominal" "{2}";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check "nominal" "{2}";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      infer "{2}";
      [%expect {| inferred: nominal |}]
    ;;

    let%expect_test _ =
      infer "{true()}";
      [%expect {| inferred: nominal |}]
    ;;

    let%expect_test _ =
      check "a -> a" "{nil()}";
      [%expect {| nominal != a -> a |}]
    ;;

    let%expect_test _ =
      check "list integer" "{cons(1; nil())}";
      [%expect {| nominal != list integer |}]
    ;;

    let%expect_test _ =
      check "bool" "let rec x : bool = y and y : bool = x in x";
      [%expect {| checked |}]
    ;;

    let type_env =
      String.Map.of_alist_exn
        [ "x", Type.Sort (here, Sort_model.Sort.Name (here, (here, "a"))) ]
    ;;

    let%expect_test _ =
      infer ~type_env "x";
      [%expect {| inferred: a |}]
    ;;

    let%expect_test _ =
      infer ~type_env "{x}";
      [%expect {| inferred: nominal |}]
    ;;

    let%expect_test _ =
      check ~type_env "bool" "{x}";
      [%expect {| nominal != bool |}]
    ;;

    let%expect_test _ =
      check ~type_env "integer" "{x}";
      [%expect {| nominal != integer |}]
    ;;
  end)
;;
