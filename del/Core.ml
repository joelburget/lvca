open Base
open Lvca_provenance
open Lvca_syntax
open Lvca_util
open Result.Let_syntax
module Format = Stdlib.Format

let reserved =
  Lvca_util.String.Set.of_list [ "let"; "rec"; "and"; "in"; "match"; "with"; "unquote" ]
;;

let var_identifier = Lvca_parsing.C_comment_parser.lower_identifier reserved

module Type = struct
  module Kernel =
  [%lvca.abstract_syntax_module
  {|
sort : *
list : * -> *

ty := Sort(sort) | Arrow(ty; ty);
  |}
  , { sort = "Sort_model"; list = "List_model" }]

  include Nominal.Convertible.Extend (Kernel.Ty)
  include Kernel.Ty

  let rec pp need_parens ppf = function
    | Arrow (_, t1, t2) ->
      Fmt.pf
        ppf
        (if need_parens then "@[<hv>(%a -> %a)@]" else "%a -> %a")
        (pp true)
        t1
        (pp false)
        t2
    | Sort (_, s) -> Lvca_syntax.Sort.pp ppf (Sort_model.out s)
  ;;

  let pp = pp false

  module Parse = struct
    open Lvca_parsing
    open C_comment_parser

    let arrow s1 s2 = Arrow (Provenance.of_here [%here], s1, s2)

    let rec of_list = function
      | [] ->
        Lvca_util.invariant_violation
          [%here]
          "of_list must be called with a non-empty list"
      | [ sort ] -> sort
      | sort :: sorts -> arrow sort (of_list sorts)
    ;;

    let ty =
      fix (fun ty ->
          let atom =
            parens ty
            <|> (Lvca_syntax.Sort.parse reserved
                >>| fun sort ->
                let sort = Sort_model.into sort in
                Sort (Sort_model.info sort, sort))
          in
          sep_by1 (string "->") atom >>| of_list)
      <?> "core type"
    ;;

    let%test_module "parsing / printing" =
      (module struct
        let parse = parse_string ty >> Result.ok_or_failwith
        let go = parse >> Fmt.pr "%a\n" pp

        let%expect_test _ =
          go "bool";
          go "list bool";
          [%expect {|
            bool
            list bool
          |}]
        ;;
      end)
    ;;

    let%test_module "of_list" =
      (module struct
        let here = Provenance.of_here [%here]
        let s = mk_Sort ~info:here (Sort_model.mk_Name ~info:here (here, "s"))
        let ( = ) = equivalent ~info_eq:(fun _ _ -> true)

        let%test _ = of_list [ s ] = s
        let%test _ = of_list [ s; s ] = arrow s s
        let%test _ = of_list [ s; s; s ] = arrow s (arrow s s)
        let%test _ = of_list [ s; s; s; s ] = arrow s (arrow s (arrow s s))
        let%test _ = of_list [ of_list [ s; s ]; s ] = arrow (arrow s s) s
        let%test _ = of_list [ s; of_list [ s; s ] ] = arrow s (arrow s s)
        let%test _ = of_list [ arrow s s; arrow s s ] = arrow (arrow s s) (arrow s s)
      end)
    ;;
  end

  let parse = Parse.ty
end

module Term_syntax = struct
  include
    [%lvca.abstract_syntax_module
    {|
ty : *
list : * -> *
option : * -> *
binding_aware_pattern : * -> *
pattern : *
primitive : *
string : *
empty : *

term :=
  | Primitive(primitive)
  | Operator(string; list operator_scope)
  | Ap(term; term)
  | Case(term; list case_scope)
  | Lambda(ty; term. term)
  | Let(term; option ty; term. term)
  // let rec defines a group (represented as a list but unordered) of definitions at once
  | Let_rec(list letrec_row; (list empty)[term]. term)
  | Subst(term. term; term)
  | Quote(term)
  | Unquote(term)
  ;

letrec_row := Letrec_row(ty; term);
operator_scope := Operator_scope(list pattern; term);
case_scope := Case_scope(binding_aware_pattern; term);
|}
    , { ty = "Type"
      ; list = "List_model"
      ; option = "Option_model.Option"
      ; binding_aware_pattern = "Binding_aware_pattern"
      ; empty = "Empty"
      ; pattern = "Pattern_model.Pattern"
      ; primitive = "Primitive.All"
      ; string = "Primitive.String"
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

module Value_syntax = struct
  include
    [%lvca.abstract_syntax_module
    {|
ty : *
list : * -> *
pattern : *
primitive : *
string : *

value :=
  | Primitive(primitive)
  | Operator(string; list operator_scope)
  | Lambda(ty; neutral. value)
  | Neutral(neutral)
  ;

neutral := Ap(neutral; value);
operator_scope := Operator_scope(list pattern; value);
|}
    , { ty = "Type"
      ; list = "List_model"
      ; pattern = "Pattern_model.Pattern"
      ; primitive = "Primitive.All"
      ; string = "Primitive.String"
      }]
end

module Pp = struct
  let list, any, pf, semi, sp = Fmt.(list, any, pf, semi, sp)
  let case_sep = any "@;<1 2>| "

  let extract_aps tm =
    let rec go = function
      | Term_syntax.Term.Ap (_, (Ap _ as t1), t2) ->
        let f, args = go t1 in
        f, t2 :: args
      | Ap (_, f, t2) -> f, [ t2 ]
      | _ -> Lvca_util.invariant_violation [%here] "Expected an Ap"
    in
    let f, args = go tm in
    f, List.rev args
  ;;

  let rec term : Term_syntax.Term.t Fmt.t =
   fun ppf tm ->
    Provenance.open_stag ppf (Term_syntax.Term.info tm);
    match tm with
    | Term_syntax.Term.Term_var (_, v) -> Fmt.string ppf v
    | Primitive (_, tm) -> Primitive.All.pp ppf tm
    | Operator (_, (_, tag), subtms) ->
      let subtms = List_model.to_list subtms in
      pf ppf "@[<hv>%s(%a)@]" tag (list ~sep:semi operator_scope) subtms
    | Lambda (_, ty, (Single_var.{ name; info = _ }, body)) ->
      pf ppf "@[<hv>\\@[<hv>(%s : %a)@] ->@ %a@]" name Type.pp ty term body
    (* TODO: parens if necessary *)
    | Ap _ ->
      let f, args = extract_aps tm in
      pf ppf "@[<h>%a@ @[<hov>%a@]@]" term f (list ~sep:sp term) args
    | Case (_, arg, cases') ->
      let cases' = List_model.to_list cases' in
      (match cases' with
      | [] -> pf ppf "match %a with {}" term arg
      | [ case ] ->
        pf
          ppf
          "@[<hv 0>match %a with {%t@[<hv 0>%a@]@ }@]"
          term
          arg
          (* Before `|`, emit a single space if on the same line, or two when broken *)
          (Format.pp_print_custom_break ~fits:("", 1, "") ~breaks:("", 2, "| "))
          case_scope
          case
      (* Always split cases with more than one branch for readability *)
      | _ -> pf ppf "@[<v 0>match %a with {%a%a@ }@]" term arg case_sep () cases cases')
    | Let (_, tm, ty, (Single_var.{ name; info = _ }, body)) -> let_ ppf tm ty name body
    | Let_rec (_, rows, (binders, rhs)) ->
      let binders = List_model.extract_vars_from_empty_pattern binders in
      let rows = List_model.to_list rows in
      let pp_bound_row ppf (var_name, Term_syntax.Letrec_row.Letrec_row (_, ty, body)) =
        pf ppf "%s@ :@ %a@ =@ %a" var_name Type.pp ty term body
      in
      (match List.zip binders rows with
      | Unequal_lengths ->
        Lvca_util.invariant_violation
          [%here]
          "invalid set of letrec binders (must be the same number of terms)"
      | Ok bound_rows ->
        pf
          ppf
          "@[let rec@ %a@ in@ %a@]"
          (list pp_bound_row ~sep:(any "@ and@ "))
          bound_rows
          term
          rhs)
    | Subst (_, (Single_var.{ name; info = _ }, body), arg) ->
      let formatter =
        match body with
        | Primitive _ | Subst _ -> pf ppf "@[%a[%s := %a]@]"
        | _ -> pf ppf "@[@[(%a)@][%s := %a]@]"
      in
      formatter term body name term arg;
      Provenance.close_stag ppf (Term_syntax.Term.info tm)
    | Quote (_, tm) -> pf ppf "@[{%a}@]" term tm
    | Unquote (_, tm) -> pf ppf "@[unquote %a@]" term tm

  and let_ ppf tm ty name body =
    let pp_ty ppf = function
      | Option_model.Option.Some (_, ty) -> pf ppf ": %a" Type.pp ty
      | None _ -> ()
    in
    pf ppf "@[let %s%a =@ %a in@ @[%a@]@]" name pp_ty ty term tm term body

  and cases ppf = list ~sep:case_sep case_scope ppf

  and case_scope ppf (Term_syntax.Case_scope.Case_scope (info, pat, body)) =
    Provenance.open_stag ppf info;
    pf ppf "@[%a@ -> %a@]" Binding_aware_pattern.pp pat term body;
    Provenance.close_stag ppf info

  and operator_scope
      ppf
      (Term_syntax.Operator_scope.Operator_scope (_info, bindings, body))
    =
    let any, list, pf = Fmt.(any, list, pf) in
    let bindings = bindings |> List_model.to_list |> List.map ~f:Pattern_model.out in
    match bindings with
    | [] -> term ppf body
    | _ -> pf ppf "%a.@ %a" (list ~sep:(any ".@ ") Pattern.pp) bindings term body
  ;;

  let rec value ppf = function
    | Value_syntax.Value.Primitive (_, tm) -> Primitive.All.pp ppf tm
    | Operator (_, (_, tag), subtms) ->
      let subtms = List_model.to_list subtms in
      pf ppf "@[<hv>%s(%a)@]" tag (list ~sep:semi operator_scope') subtms
    | Lambda (_, ty, (Single_var.{ name; info = _ }, body)) ->
      pf ppf "@[<hv>\\@[<hv>(%s : %a)@] ->@ %a@]" name Type.pp ty value body
    | Neutral (_, n) -> neutral ppf n

  and neutral ppf = function
    | Value_syntax.Neutral.Ap (_, n, v) -> pf ppf "%a %a" neutral n value v
    | Neutral_var (_, name) -> pf ppf "%s" name

  and operator_scope'
      ppf
      (Value_syntax.Operator_scope.Operator_scope (_info, bindings, body))
    =
    let any, list, pf = Fmt.(any, list, pf) in
    let bindings = bindings |> List_model.to_list |> List.map ~f:Pattern_model.out in
    match bindings with
    | [] -> value ppf body
    | _ -> pf ppf "%a.@ %a" (list ~sep:(any ".@ ") Pattern.pp) bindings value body
  ;;
end

(*
module To_nominal = struct
  open Result.Let_syntax

  let rec term tm =
    match tm with
    | Term_syntax.Term.Primitive (info, (_, v)) -> Ok (Nominal.Term.Primitive (info, v))
    | Operator (info, (_, name), subterms) ->
      let%map subterms =
        subterms |> List_model.to_list |> List.map ~f:operator_scope |> Result.all
      in
      Nominal.Term.Operator (info, name, subterms)
    | _ -> Error ("TODO: term_to_nominal", tm)

  and operator_scope (Term_syntax.Operator_scope.Operator_scope (_, patterns, tm)) =
    let patterns = patterns |> List_model.to_list |> List.map ~f:Pattern_model.out in
    let%map tm = term tm in
    Nominal.Scope.Scope (patterns, tm)
  ;;
end
   *)

module Of_nominal = struct
  let rec term = function
    | Nominal.Term.Operator (info, name, scopes) ->
      let scopes = scopes |> List.map ~f:operator_scope |> List_model.of_list in
      Term_syntax.Term.Operator (info, (info, name), scopes)
    | Primitive (info, p) -> Primitive (info, (info, p))
    | Var (info, name) -> Term_var (info, name)

  and operator_scope (Nominal.Scope.Scope (patterns, tm)) =
    let patterns = patterns |> List.map ~f:Pattern_model.into |> List_model.of_list in
    Term_syntax.Operator_scope.Operator_scope
      (Provenance.of_here [%here], patterns, term tm)
  ;;

  let rec value = function
    | Nominal.Term.Operator (info, name, scopes) ->
      let scopes = scopes |> List.map ~f:operator_scope' |> List_model.of_list in
      Value_syntax.Value.Operator (info, (info, name), scopes)
    | Primitive (info, p) -> Primitive (info, (info, p))
    | Var (info, name) -> Neutral (info, Neutral_var (info, name))

  and operator_scope' (Nominal.Scope.Scope (patterns, tm)) =
    let patterns = patterns |> List.map ~f:Pattern_model.into |> List_model.of_list in
    Value_syntax.Operator_scope.Operator_scope
      (Provenance.of_here [%here], patterns, value tm)
  ;;
end

(* module Operator_scope = struct end *)

module Parse = struct
  open Lvca_parsing
  open C_comment_parser
  open Term_syntax

  let make_aps : Term.t list -> Term.t = function
    | [] -> Lvca_util.invariant_violation [%here] "must be a nonempty list"
    | [ x ] -> x
    | f :: args ->
      List.fold_left args ~init:f ~f:(fun f arg ->
          Term_syntax.Term.Ap (Provenance.of_here [%here], f, arg))
  ;;

  let pattern = Binding_aware_pattern.parse reserved <?> "pattern"

  let atomic_term term =
    choice
      ~failure_msg:"looking for a parenthesized expression or nominal term"
      [ parens term
      ; (braces term >>~ fun range tm -> Term.Quote (Provenance.of_range range, tm))
      ; (var_identifier
        >>~ fun range ident -> Term.Term_var (Provenance.of_range range, ident))
      ; Nominal.Term.parse' reserved >>| Of_nominal.term
      ]
  ;;

  let atomic_term' term =
    atomic_term term
    >>== fun { range = body_range; value = body } ->
    choice
      [ (brackets
           (attach_pos var_identifier
           >>= fun (name, range) ->
           let name = Single_var.{ info = Provenance.of_range range; name } in
           string ":=" >>= fun _ -> term >>| fun arg -> name, arg)
        >>~ fun bracket_range (name, arg) ->
        let range = Opt_range.union bracket_range body_range in
        let info = Provenance.of_range range in
        Term.Subst (info, (name, body), arg))
      ; return body
      ]
    <?> "atomic term (with possible subsitution)"
  ;;

  let case_line term =
    make3
      (fun ~info pat _ tm -> Case_scope.Case_scope (Provenance.of_range info, pat, tm))
      pattern
      (string "->")
      term
    <?> "case line"
  ;;

  let letrec_row term =
    lift4
      (fun (var, var_pos) ty _ (rhs, rhs_pos) ->
        let range = Opt_range.union var_pos rhs_pos in
        ( (Provenance.of_range var_pos, var)
        , Letrec_row.Letrec_row (Provenance.of_range range, ty, rhs) ))
      (attach_pos var_identifier)
      (char ':' *> Type.parse)
      (char '=')
      (attach_pos term)
    <?> "letrec row"
  ;;

  let parse_let term =
    keyword "let"
    >>== (fun { range = let_pos; _ } ->
           option' (keyword "rec")
           >>= function
           | Some _rec ->
             lift3
               (fun (rows, rows_pos) _ (rhs, rhs_pos) ->
                 let info = Provenance.of_range (Opt_range.union rows_pos rhs_pos) in
                 let binders, rows = List.unzip rows in
                 let rows = List_model.of_list rows in
                 let binders = List_model.make_empty_pattern binders in
                 Term.Let_rec (info, rows, (binders, rhs)))
               (attach_pos (sep_by1 (keyword "and") (letrec_row term)))
               (keyword "in")
               (attach_pos term)
           | None ->
             lift4
               (fun (name, name_pos) ty _eq tm _in (body, body_pos) ->
                 let info = Provenance.of_range (Opt_range.union let_pos body_pos) in
                 Term.Let
                   ( info
                   , tm
                   , ty
                   , (Single_var.{ name; info = Provenance.of_range name_pos }, body) ))
               (attach_pos var_identifier)
               (option
                  (Option_model.Option.None (Provenance.of_here [%here]))
                  (char ':' *> Type.parse
                  >>| fun tm -> Option_model.Option.Some (Provenance.of_here [%here], tm)
                  ))
               (char '=')
               term
             <*> keyword "in"
             <*> attach_pos term)
    <?> "let"
  ;;

  let parse_lambda term =
    lift4
      (fun (_, lam_loc) (((name, name_loc), ty), parens_loc) _ body ->
        let range = Opt_range.union lam_loc parens_loc in
        let info = Provenance.of_range range in
        Term.Lambda
          (info, ty, (Single_var.{ info = Provenance.of_range name_loc; name }, body)))
      (attach_pos (char '\\'))
      (attach_pos
         (parens
            (lift3
               (fun ident _ ty -> ident, ty)
               (attach_pos var_identifier)
               (char ':')
               Type.parse)))
      (string "->")
      term
    <?> "lambda"
  ;;

  let parse_match term =
    lift4
      (fun (_, match_pos) tm _with (lines, lines_pos) ->
        let pos = Opt_range.union match_pos lines_pos in
        Term.Case (Provenance.of_range pos, tm, lines))
      (attach_pos (keyword "match"))
      term
      (keyword "with")
      (attach_pos
         (braces (option '|' (char '|') *> sep_by (char '|') (case_line term))
         >>| List_model.of_list))
    <?> "match"
  ;;

  let parse_unquote term =
    lift2
      (fun (_, unquote_pos) (tm, tm_pos) ->
        let pos = Opt_range.union unquote_pos tm_pos in
        Term.Unquote (Provenance.of_range pos, tm))
      (attach_pos (keyword "unquote"))
      (attach_pos term)
  ;;

  let term =
    fix (fun term ->
        choice
          ~failure_msg:"looking for a lambda, let, match, or application"
          [ parse_lambda term
          ; parse_let term
          ; parse_match term
          ; parse_unquote term
          ; many1 (atomic_term' term) >>| make_aps <?> "application"
          ])
    <?> "core term"
  ;;
end

module Value = struct
  include Nominal.Convertible.Extend (Value_syntax.Value)

  let of_nominal' = Of_nominal.value
end

module Term = struct
  include Nominal.Convertible.Extend (Term_syntax.Term)

  let parse_concrete = Parse.term
  let pp_concrete = Pp.term
  let pp_concrete ppf t = pp_concrete ppf t
  let of_nominal' = Of_nominal.term

  let rec of_value = function
    | Value_syntax.Value.Primitive (i, p) -> Term_syntax.Term.Primitive (i, p)
    | Operator (i, name, scopes) ->
      let scopes =
        List_model.map
          scopes
          ~f:(fun (Value_syntax.Operator_scope.Operator_scope (info, pats, v)) ->
            Term_syntax.Operator_scope.Operator_scope
              (Provenance.calculated_here [%here] [ info ], pats, of_value v))
      in
      Operator (Provenance.calculated_here [%here] [ i ], name, scopes)
    | Lambda (i, ty, (binder, v)) ->
      Lambda (Provenance.calculated_here [%here] [ i ], ty, (binder, of_value v))
    | Neutral (_, n) -> of_neutral n

  and of_neutral = function
    | Ap (i, n, v) ->
      Ap (Provenance.calculated_here [%here] [ i ], of_neutral n, of_value v)
    | Neutral_var (i, name) -> Term_var (Provenance.calculated_here [%here] [ i ], name)
  ;;
end

module Module = struct
  type t =
    { externals : (string * Type.t) list
    ; defs : (string * Term_syntax.Term.t) list
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
    let open C_comment_parser in
    let external_decl =
      lift4
        (fun ident _ ty _ -> ident, ty)
        var_identifier
        (string ":")
        Type.parse
        (string ";")
    in
    let def =
      lift3 (fun ident _ tm -> ident, tm) var_identifier (string ":=") Parse.term
    in
    lift2 (fun externals defs -> { externals; defs }) (many external_decl) (many1 def)
  ;;
end

type eval_env = Value.t String.Map.t
type eval_error = string * Term_syntax.Term.t

let preimage _ = failwith "TODO"
let reverse _tm _cases = failwith "TODO"

type type_env = Type.t String.Map.t

type check_env =
  { type_env : type_env
  ; syntax : Abstract_syntax.t
  }

module Check_error' = struct
  type t =
    | Cant_infer_case
    | Cant_infer_lambda
    | Var_not_found
    | Operator_not_found
    | Mismatch of Type.t * Type.t
    | Binding_pattern_check of string
    | Overapplication
    | Message of string
    | Pattern_check_failure of Pattern.t Check_failure.t
    | Binding_pattern_check_failure of Binding_aware_pattern.t Check_failure.t
    | Eval_error of eval_error

  let pp tm ppf = function
    | Cant_infer_case -> Fmt.pf ppf "can't infer cases"
    | Cant_infer_lambda -> Fmt.pf ppf "can't infer lambdas"
    | Var_not_found ->
      (match tm with
      | Term_syntax.Term.Term_var (_, name) -> Fmt.pf ppf "variable %s not found" name
      | _ ->
        Lvca_util.invariant_violation [%here] Fmt.(str "expected Var (got %a)" Pp.term tm))
    | Operator_not_found -> Fmt.pf ppf "operator not found"
    | Mismatch (ty, ty') -> Fmt.pf ppf "%a != %a" Type.pp ty' Type.pp ty
    | Binding_pattern_check str -> Fmt.pf ppf "%s" str
    | Overapplication -> Fmt.pf ppf "non-function applied to arguments"
    | Message msg -> Fmt.pf ppf "%s" msg
    | Pattern_check_failure err -> Check_failure.pp Pattern.pp ppf err
    | Binding_pattern_check_failure err ->
      Check_failure.pp Binding_aware_pattern.pp ppf err
    | Eval_error (msg, tm) ->
      Fmt.pf ppf "evaluation error (%s) in term: %a" msg Term.pp tm
  ;;
end

module Check_error = struct
  type t =
    { env : check_env
    ; tm : Term_syntax.Term.t
    ; ty : Type.t
    ; error : Check_error'.t
    }

  let pp ppf { env = _; tm; ty = _; error } = Check_error'.pp tm ppf error
end

module Infer_error = struct
  type t =
    { env : check_env
    ; tm : Term_syntax.Term.t
    ; error : Check_error'.t
    }

  let pp ppf { env = _; tm; error } = Check_error'.pp tm ppf error
end

let here = Provenance.of_here [%here]
let nominal_ty = Type.Sort (here, Name (here, (here, "nominal")))

let check_binding_pattern
    syntax
    (pat : Binding_aware_pattern.t)
    (sort : Lvca_syntax.Sort.t)
    : (type_env, Check_error'.t) Result.t
  =
  let mk_ty sort = Type.Sort (Provenance.of_here [%here], Sort_model.into sort) in
  match Binding_aware_pattern.check Primitive_impl.All.check syntax sort pat with
  | Ok captures ->
    Ok
      (Map.map captures ~f:(function
          | Bound_var sort | Bound_term sort -> mk_ty sort
          | Bound_pattern _ -> failwith "TODO: check_binding_pattern Bound_pattern"))
  | Error check_failure -> Error (Binding_pattern_check_failure check_failure)
;;

module Primitive_types = struct
  let here = Provenance.of_here [%here]
  let sort s = Type.Sort (Lvca_syntax.Sort.info s, Sort_model.into s)
  let sort' name = sort (Lvca_syntax.Sort.Name (here, name))
  let char_name = Lvca_syntax.Sort.Name (here, "char")
  let bool = sort' "bool"
  let char = sort' "char"
  let float = sort' "float"
  let int32 = sort' "int32"
  let integer = sort' "integer"
  let string = sort' "string"

  let funs =
    let arr t1 t2 = Type.Arrow (here, t1, t2) in
    let list s = sort (Lvca_syntax.Sort.Ap (here, "list", [ s ])) in
    let binary_integer = arr integer (arr integer integer) in
    String.Map.of_alist_exn
      [ "rename", arr string (arr string (arr nominal_ty nominal_ty))
      ; "var", arr string nominal_ty
      ; "add", binary_integer
      ; "sub", binary_integer
      ; "string_of_chars", arr (list char_name) string
      ; "is_digit", arr char bool
      ; "is_lowercase", arr char bool
      ; "is_uppercase", arr char bool
      ; "is_alpha", arr char bool
      ; "is_alphanum", arr char bool
      ; "is_whitespace", arr char bool
      ]
  ;;
end

let merge_pattern_context
    : Value.t String.Map.t option list -> Value.t String.Map.t option
  =
 fun ctxs ->
  if List.for_all ctxs ~f:Option.is_some
  then
    Some
      (ctxs
      |> List.map
           ~f:
             (Lvca_util.Option.get_invariant [%here] (fun () ->
                  "we just checked all is_some"))
      |> String.Map.strict_unions
      |> function
      | `Duplicate_key k ->
        Lvca_util.invariant_violation
          [%here]
          (Printf.sprintf "multiple variables with the same name (%s) in one pattern" k)
      | `Ok m -> m)
  else None
;;

let rec match_pattern pat v =
  match v, pat with
  | ( Value_syntax.Value.Operator (_, (_, tag1), vals)
    , Binding_aware_pattern.Operator (_, tag2, pats) ) ->
    let vals = List_model.to_list vals in
    if String.(tag1 = tag2)
    then (
      match List.map2 pats vals ~f:match_pattern_scope with
      | Ok results -> merge_pattern_context results
      | Unequal_lengths -> None)
    else None
  | Primitive (_, l1), Primitive l2 ->
    if Primitive.All.(l1 = l2) then Some String.Map.empty else None
  | tm, Var (_, v) -> Some (String.Map.singleton v tm)
  | _ -> None

and match_pattern_scope
    (Binding_aware_pattern.Scope (_, pat))
    (Value_syntax.Operator_scope.Operator_scope (_, _, body))
  =
  (* XXX: what to do with binder patterns? *)
  match_pattern pat body
;;

let find_match (v : Value.t)
    : Term_syntax.Case_scope.t list -> (Term.t * Value.t String.Map.t) option
  =
  List.find_map ~f:(fun (Term_syntax.Case_scope.Case_scope (_, pat, rhs)) ->
      match match_pattern pat v with None -> None | Some bindings -> Some (rhs, bindings))
;;

type eval_result = (Value.t, string * Term.t) Result.t

(* Get the representation for a value. *)
module Quote = struct
  let nonbinding_scope info body =
    Value_syntax.Operator_scope.Operator_scope (info, List_model.Nil info, body)
  ;;

  let rec list f =
    let open Value_syntax.Value in
    let info = Provenance.of_here [%here] in
    function
    | [] -> Operator (info, (info, "Nil"), List_model.Nil info)
    | x :: xs ->
      Operator
        ( Provenance.of_here [%here]
        , (info, "Cons")
        , [ f x; list f xs ] |> List.map ~f:(nonbinding_scope info) |> List_model.of_list
        )
  ;;

  let rec pattern =
    let open Value_syntax.Value in
    function
    | Pattern_model.Pattern.Operator (info, (_, name), pats) ->
      let info = Provenance.calculated_here [%here] [ info ] in
      let name_prim = Primitive (info, (info, Primitive_impl.All_plain.String name)) in
      let scopes = pats |> List_model.to_list |> list pattern in
      let scopes =
        List_model.of_list
          [ nonbinding_scope info name_prim; nonbinding_scope info scopes ]
      in
      Operator (info, (info, "Operator"), scopes)
    | Primitive (info, prim) ->
      let info = Provenance.calculated_here [%here] [ info ] in
      let prim = Primitive (info, prim) in
      let vs = List_model.of_list [ nonbinding_scope info prim ] in
      Operator (info, (info, "Primitive"), vs)
    | Var (info, (_, name)) ->
      let name_prim = Primitive (info, (info, Primitive_impl.All_plain.String name)) in
      Operator
        (info, (info, "Var"), List_model.of_list [ nonbinding_scope info name_prim ])
  ;;

  let rec value =
    let open Value_syntax.Value in
    function
    | Primitive (info, prim) ->
      let info = Provenance.calculated_here [%here] [ info ] in
      let prim = Primitive (info, prim) in
      let vs = List_model.of_list [ nonbinding_scope info prim ] in
      Operator (info, (info, "Primitive"), vs)
    | Operator (info, (_, name), scopes) ->
      let info = Provenance.calculated_here [%here] [ info ] in
      let name_prim = Primitive (info, (info, Primitive_impl.All_plain.String name)) in
      let scopes = scopes |> List_model.to_list |> list operator_scope in
      let scopes =
        List_model.of_list
          [ nonbinding_scope info name_prim; nonbinding_scope info scopes ]
      in
      Operator (info, (info, "Operator"), scopes)
    | Lambda _ -> failwith "TODO: quote Lambda"
    | Neutral (_, n) -> neutral n

  and neutral = function
    | Ap _ -> failwith "TODO: quote Ap"
    | Neutral_var _ -> failwith "TODO: quote Ap"

  and operator_scope (Operator_scope (info, pats, v)) =
    let info = Provenance.calculated_here [%here] [ info ] in
    let v = value v in
    let pats = pats |> List_model.to_list |> list pattern in
    Operator
      ( info
      , (info, "Operator_scope")
      , List_model.of_list [ nonbinding_scope info pats; nonbinding_scope info v ] )
  ;;
end

(* Splice in a term *)
module Unquote = struct
  let rec list go = function
    | Value_syntax.Value.Primitive _ | Lambda _ ->
      Error "Primitive and Lambda are invalid arguments to unquote"
    | Operator (info, (_, name), vs) ->
      let vs = List_model.to_list vs in
      let info = Provenance.calculated_here [%here] [ info ] in
      (match name, vs with
      | "Nil", [] -> Ok (List_model.Nil info)
      | "Nil", _ -> Error "Invalid arguments to Nil in unquote"
      | "Cons", [ Operator_scope (_, Nil _, x); Operator_scope (_, Nil _, xs) ] ->
        let%bind x = go x in
        let%map xs = list go xs in
        List_model.Cons (info, x, xs)
      | "Cons", _ -> Error "Invalid arguments to Cons in unquote"
      | _ -> Error "Invalid operator for list unquote")
    | Neutral _ -> failwith "TODO: unquote neutral"
  ;;

  let option go = function
    | Value_syntax.Value.Primitive _ | Lambda _ ->
      Error "Primitive and Lambda are invalid arguments to unquote"
    | Operator (info, (_, name), vs) ->
      let vs = List_model.to_list vs in
      let info = Provenance.calculated_here [%here] [ info ] in
      (match name, vs with
      | "None", [] -> Ok (Option_model.Option.None info)
      | "None", _ -> Error "Invalid arguments to None in unquote"
      | "Some", [ x ] ->
        let%map x = go x in
        Option_model.Option.Some (info, x)
      | "Some", _ -> Error "Invalid arguments to Some in unquote"
      | _ -> Error "Invalid operator for option unquote")
    | Neutral _ -> failwith "TODO: unquote neutral"
  ;;

  let ty _ = failwith "TODO: unquote ty"
  let operator_scope _ = failwith "TODO: unquote operator_scope"
  let case_scope _ = failwith "TODO: unquote case_scope"
  let letrec_row _ = failwith "TODO: unquote letrec_row"

  let rec term =
    let open Term_syntax.Term in
    function
    | Value_syntax.Value.Primitive _ | Lambda _ ->
      Error "Primitive and Lambda are invalid arguments to unquote"
    | Operator (info, (_, name), vs) ->
      let vs = List_model.to_list vs in
      let info = Provenance.calculated_here [%here] [ info ] in
      (match name, vs with
      | "Primitive", [ Operator_scope (_, Nil _, Primitive (_, p)) ] ->
        Ok (Primitive (info, p))
      | ( "Operator"
        , [ Operator_scope
              (_, Nil _, Primitive (_, (s_info, Primitive_impl.All_plain.String name)))
          ; Operator_scope (_, Nil _, scopes)
          ] ) ->
        let s_info = Provenance.calculated_here [%here] [ s_info ] in
        let%map scopes = list operator_scope scopes in
        Operator (info, (s_info, name), scopes)
      | "Ap", [ Operator_scope (_, Nil _, t1); Operator_scope (_, Nil _, t2) ] ->
        let%bind t1 = term t1 in
        let%map t2 = term t2 in
        Ap (info, t1, t2)
      | "Case", [ Operator_scope (_, Nil _, tm); Operator_scope (_, Nil _, scopes) ] ->
        let%bind tm = term tm in
        let%map scopes = list case_scope scopes in
        Case (info, tm, scopes)
      | "Lambda", [ ty'; _ ] ->
        let%map ty' = ty ty' in
        Lambda (info, ty', failwith "TODO: unquote lambda")
      | "Let", [ Operator_scope (_, Nil _, tm); Operator_scope (_, Nil _, opt_ty); _ ] ->
        let%bind tm = term tm in
        let%map opt_ty = option ty opt_ty in
        Let (info, tm, opt_ty, failwith "TODO: unquote let")
      | "Let_rec", [ Operator_scope (_, Nil _, rows); _ ] ->
        let%map rows = list letrec_row rows in
        Let_rec (info, rows, failwith "TODO: unquote let_rec")
      | "Subst", [ Operator_scope (_, Nil _, tm); _ ] ->
        let%map tm = term tm in
        Subst (info, failwith "TODO: unquote subst", tm)
      | "Quote", [ Operator_scope (_, Nil _, tm) ] ->
        let%map tm = term tm in
        Quote (info, tm)
      | "Unquote", [ Operator_scope (_, Nil _, tm) ] ->
        let%map tm = term tm in
        Quote (info, tm)
      | ( ( "Primitive" | "Operator" | "Ap" | "Case" | "Lambda" | "Let" | "Let_rec"
          | "Subst" | "Quote" | "Unquote" )
        , _ ) ->
        Error "invalid arguments to an operator"
      | _ -> Error (Fmt.str "invalid operator name to unquote: %s" name))
    | Neutral _ -> failwith "TODO: unquote neutral"
  ;;

  let value _ = failwith "TODO"
end

module Properties = struct
  let quote_unquote v =
    let v' = v |> Unquote.value |> Quote.value in
    match Value.equivalent v v' with
    | true -> Property_result.Ok
    | false -> Failed (Fmt.str "%a <> %a" Value.pp v' Value.pp v)
  ;;
end

let rec eval_in_ctx (ctx : eval_env) tm : eval_result =
  match tm with
  | Term_syntax.Term.Term_var (_, v) ->
    (match Map.find ctx v with
    | Some v -> Ok v
    | None -> Error ("Unbound variable " ^ v, tm))
  | Case (_, tm, branches) ->
    let%bind tm_val = eval_in_ctx ctx tm in
    (match find_match tm_val (List_model.to_list branches) with
    | None -> Error ("no match found in case", failwith "TODO: eval_in_ctx error 1")
    | Some (branch, bindings) ->
      eval_in_ctx (Lvca_util.Map.union_right_biased ctx bindings) branch)
  | Ap (_, Lambda (_, _ty, (Single_var.{ name; info = _ }, body)), arg) ->
    let%bind arg_val = eval_in_ctx ctx arg in
    eval_in_ctx (Map.set ctx ~key:name ~data:arg_val) body
  | Ap (_info, Term_var (_, name), _args) ->
    (match Map.find ctx name with
    | Some (Lambda (_info, _ty, _)) -> failwith "TODO: ap Lambda"
    | None | Some _ -> Error ("Expected a lambda", failwith "TODO: eval_in_ctx error 2"))
  | Let_rec (_, rows, (binders, body)) ->
    let binders = List_model.extract_vars_from_empty_pattern binders in
    let rows = List_model.to_list rows in
    (match List.zip binders rows with
    | Unequal_lengths ->
      Lvca_util.invariant_violation
        [%here]
        "invalid set of letrec binders (must be the same number of terms)"
    | Ok bound_rows ->
      let ctx =
        List.fold bound_rows ~init:ctx ~f:(fun ctx (name, Letrec_row (_, ty, _body)) ->
            let data =
              Value_syntax.Value.Lambda
                (Provenance.of_here [%here], ty, failwith "TODO: eval_in_ctx letrec")
            in
            Map.set ctx ~key:name ~data)
      in
      eval_in_ctx ctx body)
  | Let (_info, tm, _, (Single_var.{ name; info = _ }, body))
  | Subst (_info, (Single_var.{ name; _ }, body), tm) ->
    let%bind v = eval_in_ctx ctx tm in
    eval_in_ctx (Map.set ctx ~key:name ~data:v) body
  | Operator (info, name, operator_scopes) ->
    let%map vs =
      operator_scopes
      |> List_model.to_list
      |> List.map ~f:(fun (Operator_scope (info, (* XXX: ignoring binders *) _, tm)) ->
             eval_in_ctx ctx tm |> Result.map ~f:(fun v -> info, v))
      |> Result.all
    in
    let vs =
      vs
      |> List.map ~f:(fun (info, tm) ->
             let info = Provenance.calculated_here [%here] [ info ] in
             Value_syntax.Operator_scope.Operator_scope (info, Nil info, tm))
      |> List_model.of_list
    in
    Value_syntax.Value.Operator (Provenance.calculated_here [%here] [ info ], name, vs)
  | Primitive (info, p) ->
    Ok (Value_syntax.Value.Primitive (Provenance.calculated_here [%here] [ info ], p))
  | Quote (_, tm) -> eval_in_ctx ctx tm |> Result.map ~f:Quote.value
  | Unquote (_, tm) ->
    let%bind tm_val = eval_in_ctx ctx tm in
    (match Unquote.term tm_val with
    | Ok tm -> eval_in_ctx ctx tm
    | Error msg -> Error (msg, tm))
  | _ -> Error ("Found a term we can't evaluate", tm)
;;

let eval core = eval_in_ctx String.Map.empty core

(*
let eval_char_bool_fn name f tm c =
  let open Value_syntax.Value in
  let true_tm info = Operator (info, (info, "True"), List_model.of_list []) in
  let false_tm info = Operator (info, (info, "False"), List_model.of_list []) in
  match c with
  | Primitive (info, (_, Char c)) ->
    let info = Provenance.calculated_here [%here] [ info ] in
    Ok (if f c then true_tm info else false_tm info)
  | _ -> Error (Printf.sprintf "Invalid argument to %s" name, tm)
;;

and eval_builtin eval_in_ctx (ctx : eval_env) tm name args =
  let open Value_syntax.Value in
  let%bind args =
    args |> List_model.to_list |> List.map ~f:(eval_in_ctx ctx) |> Result.all
  in
  match name, args with
  (*
  | "rename", [ v1; v2; tm' ] ->
    let%map v1, v2 =
      match v1, v2 with
      | Primitive (_info1, (_, String v1)), Primitive (_info2, (_, String v2)) ->
        Ok (v1, v2)
      | _ -> Error ("Invalid arguments to rename", tm)
    in
  Nominal.Term.rename v1 v2 tm'
     *)
  (*
  | "var", [ str_tm ] ->
    (match str_tm with
    | Primitive (info, (_, String name)) -> Ok (Nominal.Term.Var (info, name))
    | _ -> Error ("expected a string", tm))
         *)
  | "add", [ a; b ] ->
    (match a, b with
    | Primitive (ainfo, (_, Integer a)), Primitive (binfo, (_, Integer b)) ->
      let info = Provenance.calculated_here [%here] [ ainfo; binfo ] in
      Ok (Primitive (info, (info, Integer Z.(a + b))))
    | _ -> Error ("Invalid arguments to add", tm))
  | "sub", [ a; b ] ->
    (match a, b with
    | Primitive (ainfo, (_, Integer a)), Primitive (binfo, (_, Integer b)) ->
      let info = Provenance.calculated_here [%here] [ ainfo; binfo ] in
      Ok (Primitive (info, (info, Integer Z.(a - b))))
    | _ -> Error ("Invalid arguments to add", tm))
  | "string_of_chars", [ char_list ] ->
    (match char_list with
    | Operator (info, (_, "list"), chars) ->
      chars
      |> List_model.to_list
      |> List.map ~f:(function
             | Operator_scope (_, _, Value_syntax.Value.Primitive (_, (_, Char c))) ->
               Ok c
             | _ -> Error (Fmt.str "string_of_chars `%a`" Pp.value tm))
      |> Result.all
      |> Result.map ~f:(fun cs ->
             Value_syntax.Value.Primitive (info, (info, String (String.of_char_list cs))))
      |> Result.map_error ~f:(fun msg -> msg, tm)
    | _ -> Error ("expected a list of characters", tm))
  | "is_digit", [ c ] -> eval_char_bool_fn "is_digit" Char.is_digit tm c
  | "is_lowercase", [ c ] -> eval_char_bool_fn "is_lowercase" Char.is_lowercase tm c
  | "is_uppercase", [ c ] -> eval_char_bool_fn "is_uppercase" Char.is_uppercase tm c
  | "is_alpha", [ c ] -> eval_char_bool_fn "is_alpha" Char.is_alpha tm c
  | "is_alphanum", [ c ] -> eval_char_bool_fn "is_alphanum" Char.is_alphanum tm c
  | "is_whitespace", [ c ] -> eval_char_bool_fn "is_whitespace" Char.is_whitespace tm c
  | _ ->
    Error
      ( Fmt.str
          "Unknown function (%s), or wrong number of arguments (got [%a])"
          name
          Fmt.(list Pp.value)
          args
      , tm )
   *)

let rec check ({ type_env; syntax } as env) tm ty =
  let check_ty ty' =
    if Type.equivalent ty ty'
    then None
    else Some Check_error.{ env; tm; ty; error = Mismatch (ty, ty') }
  in
  match tm with
  | Term_syntax.Term.Primitive (_, (_, p)) ->
    let open Primitive_types in
    let sort =
      match p with
      | Primitive_impl.All_plain.String _ -> string
      | Float _ -> float
      | Char _ -> char
      | Integer _ -> integer
      | Int32 _ -> int32
    in
    check_ty sort
  | Operator (_, (_, operator_name), scopes) ->
    (match ty with
    | Arrow _ ->
      Some Check_error.{ env; tm; ty; error = Message "Expected sort, found arrow" }
    | Sort (_, expected_sort) ->
      let expected_sort = Sort_model.out expected_sort in
      let lookup_operator = Abstract_syntax.lookup_operator syntax in
      let sort_name, sort_args = Sort.split expected_sort in
      (match lookup_operator sort_name operator_name with
      | Error lookup_err ->
        let error =
          Check_error'.Message
            (Fmt.str
               "failed to find operator %s in sort %s: %a"
               operator_name
               sort_name
               Abstract_syntax.Lookup_error.pp
               lookup_err)
        in
        Some { env; tm; ty; error }
      | Ok (sort_vars, Operator_def (_, _, arity)) ->
        (* TODO: kind check *)
        let sort_vars = List.map sort_vars ~f:Tuple2.get1 in
        let sort_env = String.Map.of_alist_exn (List.zip_exn sort_vars sort_args) in
        let concrete_arity = Arity.instantiate sort_env arity in
        check_slots env tm ty concrete_arity scopes))
  | Ap (_, f, arg) ->
    (match infer env f with
    | Ok f_ty ->
      (match check_arg env tm f_ty arg with Error err -> Some err | Ok _ -> None)
    | Error Infer_error.{ env; tm; error } -> Some { env; tm; ty; error })
  | Case (_, tm, branches) ->
    (match infer env tm with
    | Error { env; tm; error } -> Some { env; tm; ty; error }
    | Ok (Type.Arrow _) -> failwith "TODO: check Case"
    | Ok (Sort (_, sort)) ->
      let sort = Sort_model.out sort in
      branches
      |> List_model.to_list
      |> List.find_map ~f:(fun (Term_syntax.Case_scope.Case_scope (_, pat, rhs)) ->
             match check_binding_pattern syntax pat sort with
             | Ok type_env -> check { env with type_env } rhs ty
             | Error error -> Some { env; tm; ty; error }))
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
  | Quote _ -> check_ty nominal_ty
  | Unquote (_, tm) ->
    (match ty with
    | Arrow _ ->
      Some Check_error.{ env; tm; ty; error = Message "Expected sort, found arrow" }
    | Sort (_, _sort) ->
      (match eval tm with
      | Ok v ->
        (match Unquote.term v with
        | Ok tm -> check env tm ty
        | Error msg -> Some Check_error.{ env; tm; ty; error = Eval_error (msg, tm) })
      | Error err -> Some Check_error.{ env; tm; ty; error = Eval_error err }))

and check_slots env tm ty (Arity (_, valences)) scopes =
  let scopes = List_model.to_list scopes in
  match List.zip scopes valences with
  | Unequal_lengths ->
    let str, list, comma = Fmt.(str, list, comma) in
    let msg =
      str
        "Wrong number of subterms (%u) for this arity (%a)"
        (List.length scopes)
        (list ~sep:comma Valence.pp)
        valences
    in
    Some { env; tm; ty; error = Check_error'.Message msg }
  | Ok scope_valences ->
    List.find_map scope_valences ~f:(fun (scope, valence) ->
        check_scope env tm ty valence scope)

and check_scope
    ({ type_env; syntax } as env)
    tm
    ty
    valence
    (Operator_scope (_, binders, body))
  =
  let binders = List_model.to_list binders in
  let (Valence (binder_sorts, body_sort)) = valence in
  match List.zip binder_sorts binders with
  | Unequal_lengths ->
    let msg =
      Fmt.str
        "Wrong number of binders (%u) for this valence (%a) (expected %u)"
        (List.length binders)
        Valence.pp
        valence
        (List.length binder_sorts)
    in
    Some { env; tm; ty; error = Check_error'.Message msg }
  | Ok binders ->
    let convert_err =
      Result.map_error ~f:(fun err -> Check_error'.Pattern_check_failure err)
    in
    let binders_env =
      List.map binders ~f:(fun (slot, pat) ->
          let pat = Pattern_model.out pat in
          match slot, pat with
          | Sort_binding sort, Var (_, _v) ->
            Pattern.check syntax ~pattern_sort:sort ~var_sort:sort pat |> convert_err
          | Sort_binding _sort, _ ->
            Error
              (Check_error'.Message "Fixed-valence binders must all be vars (no patterns)")
          | Sort_pattern { pattern_sort; var_sort }, _ ->
            Pattern.check syntax ~pattern_sort ~var_sort pat |> convert_err)
    in
    (* Check the body with the new binders environment *)
    (match Result.all binders_env with
    | Error error -> Some { env; tm; ty; error }
    | Ok binders_env ->
      (match String.Map.strict_unions binders_env with
      | `Ok binders_env (* check every term in body for an error *) ->
        let mk_ty sort = Type.Sort (Provenance.of_here [%here], Sort_model.into sort) in
        let binders_env = Map.map binders_env ~f:mk_ty in
        let type_env = Map.union_right_biased type_env binders_env in
        check { type_env; syntax } body (mk_ty body_sort)
      | `Duplicate_key k ->
        let error =
          Check_error'.Message
            (* TODO: should this definitely not be allowed? Seems okay. *)
            (Printf.sprintf
               "Did you mean to bind the same variable (%s) twice in the same set of \
                patterns? That's not allowed!"
               k)
        in
        Some { env; tm; ty; error }))

and infer ({ type_env; syntax } as env) tm =
  match tm with
  | Term_syntax.Term.Primitive (_, (_, p)) ->
    let open Primitive_types in
    let sort =
      match p with
      | Primitive_impl.All_plain.String _ -> string
      | Float _ -> float
      | Char _ -> char
      | Integer _ -> integer
      | Int32 _ -> int32
    in
    Ok sort
  | Operator (_, (_, operator_name), _scopes) ->
    (match Abstract_syntax.find_operator syntax operator_name with
    | Error find_err ->
      let error =
        Check_error'.Message
          (Fmt.str
             "failed to find operator %s: %a"
             operator_name
             Abstract_syntax.Find_error.pp
             find_err)
      in
      Error { env; tm; error }
    | Ok (sort_name, sort_def, _op_def) ->
      (match sort_def with
      | Sort_def ([], _, _) ->
        let here = Provenance.of_here [%here] in
        let sort = Sort.Name (here, sort_name) in
        let ty = Type.Sort (here, Sort_model.into sort) in
        (match check env tm ty with
        | None -> Ok ty
        | Some { env; tm; ty = _; error } -> Error { env; tm; error })
      | _ -> failwith "TODO: infer types with sort vars"))
  | Ap (_, f, arg) ->
    (match infer env f with
    | Ok f_ty ->
      (match check_arg env tm f_ty arg with
      | Ok remainder -> Ok remainder
      | Error { env; tm; ty = _; error } -> Error { env; tm; error })
    | Error err -> Error err)
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
    | None ->
      (match Map.find Primitive_types.funs name with
      | None -> Error { env; tm; error = Var_not_found }
      | Some ty' -> Ok ty')
    | Some ty' -> Ok ty')
  | Quote _ -> Ok nominal_ty
  | Unquote (_, tm) ->
    (match eval tm with
    | Ok v ->
      (match Unquote.term v with
      | Ok tm -> infer env tm
      | Error msg -> Error Infer_error.{ env; tm; error = Eval_error (msg, tm) })
    | Error err -> Error Infer_error.{ env; tm; error = Eval_error err })

and check_arg env tm ty arg =
  match ty with
  | Type.Arrow (_, t1, t2) ->
    (match check env arg t1 with None -> Ok t2 | Some err -> Error err)
  | Sort _ -> Error Check_error.{ env; tm; ty; error = Overapplication }

and check_binders ({ type_env; syntax = _ } as env) rows binders =
  let binders = List_model.extract_vars_from_empty_pattern binders in
  let rows = List_model.to_list rows in
  let defns =
    match List.zip binders rows with
    | Unequal_lengths -> Lvca_util.invariant_violation [%here] "Binder / row mismatch"
    | Ok defns -> defns
  in
  let type_env =
    List.fold
      defns
      ~init:type_env
      ~f:(fun type_env (name, Term_syntax.Letrec_row.Letrec_row (_, ty, _)) ->
        Map.set type_env ~key:name ~data:ty)
  in
  let defn_check_error =
    List.find_map defns ~f:(fun (_name, Letrec_row (_, ty, tm)) ->
        check { env with type_env } tm ty)
  in
  match defn_check_error with None -> Ok type_env | Some err -> Error err
;;

let parse_exn =
  Lvca_parsing.(parse_string (whitespace *> Parse.term)) >> Result.ok_or_failwith
;;

let%test_module "Parsing" =
  (module struct
    let parse_pretty str = str |> parse_exn |> Fmt.pr "%a@\n" Pp.term

    let%expect_test _ =
      parse_pretty "1";
      parse_pretty "True()";
      parse_pretty "not x";
      parse_pretty "Var(str)";
      parse_pretty "let x = x in x";
      parse_pretty "let str = string_of_chars chars in str";
      parse_pretty "let str = string_of_chars chars in Var(str)";
      parse_pretty "\\(x : bool) -> x";
      parse_pretty "match x with { _ -> 1 }";
      parse_pretty "match empty with { }";
      parse_pretty "match x with { | _ -> 1 }";
      parse_pretty "match x with { True() -> False() | False() -> True() }";
      parse_pretty "let x = True() in not x";
      parse_pretty {|fail "some reason for failing"|};
      parse_pretty "match c with { 'c' -> True() | _ -> False() }";
      parse_pretty "Some(1)";
      parse_pretty "let x = let x = 1 in Some(x) in Some(x)";
      parse_pretty "body[f := reduce arg]";
      parse_pretty {|\(tm : lam) -> tm[x := Var("y")]|};
      [%expect
        {|
      1
      True()
      not x
      Var(str)
      let x = x in x
      let str = string_of_chars chars in str
      let str = string_of_chars chars in Var(str)
      \(x : bool) -> x
      match x with { _ -> 1 }
      match empty with {}
      match x with { _ -> 1 }
      match x with {
        | True() -> False()
        | False() -> True()
      }
      let x = True() in not x
      fail "some reason for failing"
      match c with {
        | 'c' -> True()
        | _ -> False()
      }
      Some(1)
      let x = let x = 1 in Some(x) in Some(x)
      (body)[f := reduce arg]
      \(tm : lam) -> (tm)[x := Var("y")]
      |}]
    ;;

    let%expect_test _ =
      parse_pretty
        {|
        \(tm: term) -> match tm with {
          | Lam(_) -> tm
          | App(f; arg) -> match reduce f with {
            | Lam(x. body) -> body[f := reduce arg]
            | f' -> let arg = reduce arg in App(f'; arg)
          }
          | Real_expr(expr) -> expr
        }
      |};
      [%expect
        {|
      \(tm : term) ->
      match tm with {
        | Lam(_) -> tm
        | App(f; arg)
          -> match reduce f with {
               | Lam(x. body) -> (body)[f := reduce arg]
               | f' -> let arg = reduce arg in App(f'; arg)
             }
        | Real_expr(expr) -> expr
      }
      |}]
    ;;

    let%expect_test _ =
      parse_pretty
        {|
      \(tm : ty) -> match tm with {
        | True() -> True()
        | False() -> False()
        | Ite(t1; t2; t3) -> match meaning t1 with {
          | True()  -> meaning t2
          | False() -> meaning t3
        }
        | Ap(f; arg) -> (meaning f) (meaning arg)
        | Fun(scope) -> Lambda(List(); scope)
      }
      |};
      [%expect
        {|
      \(tm : ty) ->
      match tm with {
        | True() -> True()
        | False() -> False()
        | Ite(t1; t2; t3)
          -> match meaning t1 with {
               | True() -> meaning t2
               | False() -> meaning t3
             }
        | Ap(f; arg) -> meaning f meaning arg
        | Fun(scope) -> Lambda(List(); scope)
      }
      |}]
    ;;
  end)
;;

let%test_module "Core eval" =
  (module struct
    let eval_str str =
      let result =
        match str |> parse_exn |> eval with
        | Error (msg, tm) -> Fmt.str "%s: %a" msg Term_syntax.Term.pp tm
        | Ok result -> Fmt.to_to_string Pp.value result
      in
      Stdio.print_string result
    ;;

    let%expect_test _ =
      eval_str "1";
      [%expect {| 1 |}]
    ;;

    let%expect_test _ =
      eval_str "Foo(1)";
      [%expect {| Foo(1) |}]
    ;;

    let%expect_test _ =
      eval_str "True()";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "False()";
      [%expect {| False() |}]
    ;;

    let%expect_test _ =
      eval_str
        {|match True() with {
          | True() -> False()
          | False() -> True()
        }
      |};
      [%expect {| False() |}]
    ;;

    let%expect_test _ =
      eval_str {|(\(x: bool) -> x) True()|};
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "Add(1; 2)";
      [%expect {| 3 |}]
    ;;

    let%expect_test _ =
      eval_str "Sub(1; 2)";
      [%expect {| -1 |}]
    ;;

    let%expect_test _ =
      eval_str "Is_digit('9')";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "Is_digit('a')";
      [%expect {| False() |}]
    ;;

    let%expect_test _ =
      eval_str "Is_alpha('a')";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "Is_alpha('A')";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "Is_alpha('9')";
      [%expect {| False() |}]
    ;;

    let%expect_test _ =
      eval_str "Is_alphanum('9')";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "Is_alphanum('Z')";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "Is_alphanum('_')";
      [%expect {| False() |}]
    ;;

    let%expect_test _ =
      eval_str "Sub(1; 2)";
      [%expect {| -1 |}]
    ;;

    let%expect_test _ =
      eval_str "x[x := 1]";
      [%expect {| 1 |}]
    ;;

    let%expect_test _ =
      eval_str "Sub(x; 2)[x := 1]";
      [%expect {| -1 |}]
    ;;

    let%expect_test _ =
      eval_str "{False()}";
      [%expect {| Operator("False"; Nil()) |}]
    ;;

    let%expect_test _ =
      eval_str "{1}";
      [%expect {| Primitive(1) |}]
    ;;

    let%expect_test _ =
      eval_str "{Some(1)}";
      [%expect {| Operator("Some"; Cons(Primitive(1); Nil())) |}]
    ;;

    let%expect_test _ =
      eval_str "{sub 1 2}";
      [%expect {| Primitive(-1) |}]
    ;;

    let%expect_test _ =
      eval_str "{a}";
      [%expect {| Unbound variable a: a |}]
    ;;

    let%expect_test _ =
      eval_str "{Lam(a. a)}";
      [%expect {| Operator("Lam"; Cons(Scope(Cons(Var("a"); Nil()); Var("a")); Nil())) |}]
    ;;

    let%expect_test _ =
      eval_str "unquote {False()}";
      [%expect {| False() |}]
    ;;

    (* TODO
    let%expect_test _ =
      eval_str {|unquote (rename "foo" "bar" {Cons(foo; Nil())})|};
      [%expect {| Cons(bar; Nil()) |}]
    ;;

    let%expect_test _ =
      eval_str {|unquote (rename "foo" "bar" {Cons(baz; Nil())})|};
      [%expect {| Cons(baz; Nil()) |}]
    ;;
       *)
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
    let ty = mk_test Type.parse Type.pp
    let module' = mk_test Module.parse Module.pp

    let%expect_test _ =
      term ~width:20 "match True() with { True() -> False() | False() -> True() }";
      [%expect
        {|
        match True() with {
          | True()
            -> False()
          | False()
            -> True()
        } |}]
    ;;

    let%expect_test _ =
      term ~width:21 "match True() with { True() -> False() | False() -> True() }";
      [%expect
        {|
        match True() with {
          | True() -> False()
          | False() -> True()
        } |}]
    ;;

    let%expect_test _ =
      term ~width:23 "match x with { _ -> 1 }";
      [%expect {| match x with { _ -> 1 } |}]
    ;;

    let%expect_test _ =
      term ~width:22 "match x with { _ -> 1 }";
      [%expect {|
        match x with {
          | _ -> 1
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
      term ~width:20 "let x = True() in not x";
      [%expect {|
        let x = True() in
        not x |}]
    ;;

    let%expect_test _ =
      term ~width:20 "let x: bool = True() in not x";
      [%expect {|
        let x: bool =
        True() in not x |}]
    ;;

    let%expect_test _ =
      term "(sub x 2)[x := 1]";
      [%expect {| (sub x 2)[x := 1] |}]
    ;;

    let%expect_test _ =
      term "let rec x : foo = y in x";
      [%expect {| let rec x : foo = y in x |}]
    ;;

    let%expect_test _ =
      term "let rec x : foo = y and y : foo = x in x";
      [%expect {| let rec x : foo = y and y : foo = x in x |}]
    ;;

    let%expect_test _ =
      ty "string";
      [%expect "string"]
    ;;

    let%expect_test _ =
      ty "integer -> bool";
      [%expect "integer -> bool"]
    ;;

    let%expect_test _ =
      ty "(integer -> bool) -> string";
      [%expect "(integer -> bool) -> string"]
    ;;

    let%expect_test _ =
      ty "integer -> (bool -> string)";
      [%expect "integer -> bool -> string"]
    ;;

    let%expect_test _ =
      ty "(integer -> float) -> (bool -> string)";
      [%expect "(integer -> float) -> bool -> string"]
    ;;

    let%expect_test _ =
      module' {|go : string; x := "foo"|};
      [%expect {|
        go : string;
        x := "foo" |}]
    ;;
  end)
;;

(*
let%test_module "Core eval in dynamics" =
  (module struct
    module Term = Term_syntax.Term

    let eval_in dynamics_str str =
      let defn = parse_exn dynamics_str in
      let core = parse_exn str in
      let tm = Term_syntax.Term.Ap (here, defn, List_model.of_list [ core ]) in
      match eval tm with
      | Error (msg, tm) -> Fmt.str "%s: %a" msg Term.pp tm
      | Ok result -> Fmt.to_to_string Nominal.Term.pp result
    ;;

    let dynamics_str =
      {|\(tm : ty) -> match tm with {
  | True() -> True()
  | False() -> False()
  | Ite(t1; t2; t3) -> match meaning t1 with {
    | true()  -> meaning t2
    | false() -> meaning t3
  }
  | Ap(f; arg) -> (meaning f) (meaning arg)
  | Fun(scope) -> Lambda(List(); scope)
}
      |}
    ;;

    let%expect_test _ =
      Stdio.print_string @@ eval_in dynamics_str "True()";
      [%expect {| True() |}]
    ;;

    let id_dynamics = {|\(tm : ty) -> tm|}

    let%expect_test _ =
      Stdio.print_string @@ eval_in id_dynamics "True()";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      Stdio.print_string @@ eval_in id_dynamics "Lambda(tm. tm; List(Ty()))";
      [%expect {| Lambda(tm. tm; List(Ty())) |}]
    ;;
  end)
;;
   *)

let%test_module "Checking / inference" =
  (module struct
    module Term = Term_syntax.Term

    let parse_type str = Lvca_parsing.parse_string Type.parse str |> Result.ok_or_failwith
    let externals = [ "option", Kind.Kind (here, 2) ]

    let sort_defs =
      [ ( "bool"
        , Sort_def.Sort_def
            ( []
            , [ Operator_def.Operator_def (here, "True", Arity (here, []))
              ; Operator_def.Operator_def (here, "False", Arity (here, []))
              ]
            , [ "x" ] ) )
      ; ( "list"
        , let a = Lvca_syntax.Sort.Name (here, "a") in
          Sort_def.Sort_def
            ( [ "a", Some (Kind.Kind (here, 1)) ]
            , [ Operator_def.Operator_def (here, "Nil", Arity (here, []))
              ; Operator_def.Operator_def
                  ( here
                  , "Cons"
                  , Arity
                      ( here
                      , [ Valence.Valence ([], a)
                        ; Valence.Valence ([], Lvca_syntax.Sort.Ap (here, "list", [ a ]))
                        ] ) )
              ]
            , [ "x" ] ) )
      ]
    ;;

    let syntax = Abstract_syntax.{ externals; sort_defs }

    let check ?(type_env = String.Map.empty) ty_str tm_str =
      let tm = parse_exn tm_str in
      let ty = parse_type ty_str in
      let ctx = { type_env; syntax } in
      match check ctx tm ty with
      | Some err -> Check_error.pp Fmt.stdout err
      | None -> Fmt.pr "checked\n"
    ;;

    let infer ?(type_env = String.Map.empty) tm_str =
      let tm = parse_exn tm_str in
      let ctx = { type_env; syntax } in
      match infer ctx tm with
      | Error err -> Infer_error.pp Fmt.stdout err
      | Ok ty -> Fmt.pr "inferred: %a\n" Type.pp ty
    ;;

    let%expect_test _ =
      check "bool" "True()";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check "nominal" "True()";
      [%expect
        {|
        failed to find operator True in sort nominal: sort not found (options: {bool,
        list}) |}]
    ;;

    let%expect_test _ =
      check "integer" "2";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check "integer" "2";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      infer "2";
      [%expect {| inferred: integer |}]
    ;;

    let%expect_test _ =
      infer "True()";
      [%expect {| inferred: bool |}]
    ;;

    let%expect_test _ =
      check "a -> a" "Nil()";
      [%expect {| Expected sort, found arrow |}]
    ;;

    let%expect_test _ =
      check "list integer" "Cons(1; Nil())";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check "bool" "let rec x : bool = y and y : bool = x in x";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check
        "integer"
        {|match True() with {
          | True() -> 1
          | False() -> 2
        }|};
      [%expect {| checked |}]
    ;;

    let type_env =
      String.Map.of_alist_exn
        [ "x", Type.Sort (here, Sort_model.mk_Name ~info:here (here, "a")) ]
    ;;

    let%expect_test _ =
      infer ~type_env "x";
      [%expect {| inferred: a |}]
    ;;

    let%expect_test _ =
      check ~type_env "bool" "x";
      [%expect {| a != bool |}]
    ;;

    let%expect_test _ =
      check ~type_env "nominal" {|{True()}|};
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check ~type_env "bool" {|unquote {True()}|};
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check ~type_env "list integer" {|unquote (rename "foo" "bar" {Cons(foo; Nil())})|};
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check ~type_env "integer" {|add 1 2|};
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      infer ~type_env {|add 1|};
      [%expect {| inferred: integer -> integer |}]
    ;;

    let%expect_test _ =
      infer ~type_env {|sub|};
      infer ~type_env {|string_of_chars|};
      infer ~type_env {|var|};
      infer ~type_env {|is_digit|};
      infer ~type_env {|is_lowercase|};
      infer ~type_env {|is_uppercase|};
      infer ~type_env {|is_alpha|};
      infer ~type_env {|is_alphanum|};
      infer ~type_env {|is_whitespace|};
      [%expect
        {|
        inferred: integer -> integer -> integer
        inferred: list char -> string
        inferred: string -> nominal
        inferred: char -> bool
        inferred: char -> bool
        inferred: char -> bool
        inferred: char -> bool
        inferred: char -> bool
        inferred: char -> bool |}]
    ;;
  end)
;;
