open Base
open Lvca_provenance
open Lvca_syntax
open Lvca_util
open Result.Let_syntax
module Format = Stdlib.Format

let to_pattern_exn tm =
  tm
  |> Nominal.Term.to_pattern
  |> Result.map_error ~f:(fun _ -> "failed to convert term to pattern")
  |> Result.ok_or_failwith
;;

let reserved =
  Lvca_util.String.Set.of_list [ "let"; "rec"; "and"; "in"; "match"; "with"; "forall" ]
;;

let var_identifier = Lvca_parsing.C_comment_parser.lower_identifier reserved

let extract_vars_from_empty_list_pattern pat =
  let rec go = function
    | Pattern.Operator (_, "Nil", []) -> []
    | Operator (_, "Cons", [ Var (_, name); pats ]) -> name :: go pats
    | _ -> Lvca_util.invariant_violation ~here:[%here] "Invalid empty list pattern"
  in
  go pat
;;

let rec make_empty_list_pattern vars =
  match vars with
  | [] -> Pattern.Operator (Provenance.of_here [%here], "Nil", [])
  | (v, pos) :: vars ->
    Operator
      (Provenance.of_here [%here], "Cons", [ Var (v, pos); make_empty_list_pattern vars ])
;;

module Type = struct
  include
    [%lvca.abstract_syntax_module
    {|
sort : *

// we allow quantifiers only on the outside
ty := Forall((list empty)[quantified_ty]. quantified_ty)

// everything inside the quantifiers
quantified_ty :=
  | Sort(sort)
  | Arrow(quantified_ty; quantified_ty)
      |}
    , { sort = "Sort_model.Sort"; empty = "Empty" }]

  let rec pp_quantified_ty need_parens ppf = function
    | Quantified_ty.Arrow (_, t1, t2) ->
      Fmt.pf
        ppf
        (if need_parens then "@[<hv>(%a -> %a)@]" else "%a -> %a")
        (pp_quantified_ty true)
        t1
        (pp_quantified_ty false)
        t2
    | Sort (_, s) -> Lvca_syntax.Sort.pp ppf (Sort_model.Sort.out s)
    | Quantified_ty_var (_, name) -> Fmt.pf ppf "%s" name
  ;;

  let pp_ty ppf (Ty.Forall (_, (names, quantified_ty))) =
    let names = extract_vars_from_empty_list_pattern names in
    Fmt.pf
      ppf
      "forall %a. %a"
      Fmt.(list string)
      names
      (pp_quantified_ty false)
      quantified_ty
  ;;

  module Parse = struct
    open Lvca_parsing
    open C_comment_parser
    open Ty
    open Quantified_ty

    let arrow s1 s2 = Arrow (Provenance.of_here [%here], s1, s2)

    let rec of_list = function
      | [] ->
        Lvca_util.invariant_violation
          ~here:[%here]
          "of_list must be called with a non-empty list"
      | [ sort ] -> sort
      | sort :: sorts -> arrow sort (of_list sorts)
    ;;

    let quantified_ty =
      fix (fun quantified_ty ->
          let atom =
            parens quantified_ty
            <|> (Lvca_syntax.Sort.parse reserved
                >>| fun sort ->
                let sort = Sort_model.Sort.into sort in
                Sort (Sort_model.Sort.info sort, sort))
          in
          sep_by1 (string "->") atom >>| of_list)
      <?> "core type"
    ;;

    let here = Provenance.of_here [%here]

    let ty =
      choice
        ~failure_msg:"quantified or unquantified type"
        [ lift4
            (fun (_forall, pos0) names _dot (body, pos1) ->
              let names =
                List.map names ~f:(fun (name, pos) -> Provenance.of_range pos, name)
              in
              let range = Opt_range.union pos0 pos1 in
              Ty.Forall (Provenance.of_range range, (make_empty_list_pattern names, body)))
            (attach_pos (keyword "forall"))
            (many (attach_pos var_identifier))
            (char '.')
            (attach_pos quantified_ty)
        ; (quantified_ty
          >>~ fun range ty ->
          Ty.Forall (Provenance.of_range range, ([%lvca.pattern "Nil()"], ty)))
        ]
      <?> "type"
    ;;

    let%test_module "parsing / printing" =
      (module struct
        let parse = parse_string ty >> Result.ok_or_failwith
        let go = parse >> Fmt.pr "%a\n" pp_ty

        let%expect_test _ =
          go "bool";
          go "list bool";
          [%expect {|
            bool
            list bool
          |}]
        ;;

        let%expect_test _ =
          go "forall a. a";
          go "forall a b. a";
          go "forall a. forall b. a";
          [%expect]
        ;;
      end)
    ;;

    let%test_module "of_list" =
      (module struct
        let here = Provenance.of_here [%here]
        let s = Sort_model.Sort.mk_Name ~info:here (here, "s")
        let mk_Sort s = mk_Sort ~info:here s
        let ( = ) = Quantified_ty.equivalent ~info_eq:(fun _ _ -> true)

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

  let rec chop_trailing n ty =
    match n, ty with
    | 0, _ -> ty
    | _, Quantified_ty.Arrow (_, _, t) -> chop_trailing (n - 1) t
    | _, _ -> Lvca_util.invariant_violation ~here:[%here] "expected an arrow"
  ;;

  module Quantified_ty = struct
    include Nominal.Convertible.Extend (Quantified_ty)
    include Quantified_ty

    let parse = Parse.quantified_ty
  end

  module Ty = struct
    include Nominal.Convertible.Extend (Ty)
    include Ty

    let parse = Parse.ty
  end
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

letrec_row := Letrec_row(ty; term)

term :=
  | Nominal(nominal)
  | Ap(term; list term)
  | Case(term; list case_scope)
  | Lambda(ty; term. term)
  | Let(term; option ty; term. term)
  // let rec defines a group (represented as a list but unordered) of definitions at once
  | Let_rec(list letrec_row; (list empty)[term]. term)
  | Subst(term. term; term)

case_scope := Case_scope(binding_aware_pattern; term)
|}
    , { ty = "Type.Ty"
      ; nominal = "Nominal.Term"
      ; list = "List_model.List"
      ; option = "Option_model.Option"
      ; binding_aware_pattern = "Binding_aware_pattern_model.Pattern"
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

module Pp = struct
  let list, any, pf, sp = Fmt.(list, any, pf, sp)

  let rec term : Lang.Term.t Fmt.t =
   fun ppf tm ->
    Provenance.open_stag ppf (Lang.Term.info tm);
    match tm with
    | Lang.Term.Term_var (_, v) -> Fmt.string ppf v
    | Nominal (_, tm) -> Nominal.Term.pp ppf tm
    | Lambda (_, ty, (Single_var.{ name; info = _ }, body)) ->
      pf ppf "@[<hv>\\@[<hv>(%s : %a)@] ->@ %a@]" name Type.Ty.pp ty term body
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
        pf ppf "%s@ :@ %a@ =@ %a" var_name Type.Ty.pp ty term body
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
        | Nominal (_, Primitive _) | Subst _ -> pf ppf "@[%a[%s := %a]@]"
        | _ -> pf ppf "@[@[(%a)@][%s := %a]@]"
      in
      formatter term body name term arg;
      Provenance.close_stag ppf (Lang.Term.info tm)

  and let_ ppf tm ty name body =
    let pp_ty ppf = function
      | Option_model.Option.Some (_, ty) -> pf ppf ": %a" Type.Ty.pp ty
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
  open C_comment_parser
  open Lang

  let make_apps : Term.t list -> Term.t = function
    | [] -> Lvca_util.invariant_violation ~here:[%here] "must be a nonempty list"
    | [ x ] -> x
    | f :: args -> Ap (Provenance.of_here [%here], f, List_model.of_list args)
  ;;

  let pattern =
    Binding_aware_pattern.parse reserved
    >>| Binding_aware_pattern_model.into
    <?> "pattern"
  ;;

  let atomic_term term =
    choice
      ~failure_msg:"looking for a parenthesized expression or nominal term"
      [ parens term
      ; Nominal.Term.parse' reserved
        >>~ (fun range tm ->
              match tm with
              (* TODO: this special case feels wrong *)
              | Var (info, name) -> Term.Term_var (info, name)
              | _ -> Nominal (Provenance.of_range range, tm))
        <?> "nominal term / variable"
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
      (char ':' *> Type.Ty.parse)
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
                 let binders = make_empty_list_pattern binders in
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
                  (char ':' *> Type.Ty.parse
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
               Type.Ty.parse)))
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

  let term =
    fix (fun term ->
        choice
          ~failure_msg:"looking for a lambda, let, match, or application"
          [ parse_lambda term
          ; parse_let term
          ; parse_match term
          ; many1 (atomic_term' term) >>| make_apps <?> "application"
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
    { externals : (string * Type.Ty.t) list
    ; defs : (string * Lang.Term.t) list
    }

  let pp ppf { externals; defs } =
    let open Fmt in
    let pp_externals ppf = function
      | [] -> ()
      | lst ->
        Fmt.pf ppf "%a;" (list ~sep:semi (pair ~sep:(any " : ") string Type.Ty.pp)) lst
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
        Type.Ty.parse
        (string ";")
    in
    let def =
      lift3 (fun ident _ tm -> ident, tm) var_identifier (string ":=") Parse.term
    in
    lift2 (fun externals defs -> { externals; defs }) (many external_decl) (many1 def)
  ;;
end

type eval_env = Term.t String.Map.t

let preimage _ = failwith "TODO"
let reverse _tm _cases = failwith "TODO"

type type_env = Type.Ty.t String.Map.t

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
    | Mismatch of Type.Ty.t * Type.Ty.t
    | Binding_pattern_check of string
    | Overapplication

  let pp ppf tm = function
    | Cant_infer_case -> Fmt.pf ppf "can't infer cases"
    | Cant_infer_lambda -> Fmt.pf ppf "can't infer lambdas"
    | Var_not_found ->
      (match tm with
      | Lang.Term.Nominal (_, Nominal.Term.Var (_, name)) ->
        Fmt.pf ppf "variable %s not found" name
      | _ ->
        Lvca_util.invariant_violation
          ~here:[%here]
          Fmt.(str "expected Var (got %a)" Pp.term tm))
    | Operator_not_found -> Fmt.pf ppf "operator not found"
    | Mismatch (ty, ty') -> Fmt.pf ppf "%a != %a" Type.Ty.pp ty' Type.Ty.pp ty
    | Binding_pattern_check str -> Fmt.pf ppf "%s" str
    | Overapplication -> Fmt.pf ppf "non-function applied to arguments"
  ;;
end

module Check_error = struct
  type t =
    { env : check_env
    ; tm : Lang.Term.t
    ; ty : Type.Ty.t
    ; error : Check_error'.t
    }

  let pp ppf { env = _; tm; ty = _; error } = Check_error'.pp ppf tm error
end

module Infer_error = struct
  type t =
    { env : check_env
    ; tm : Lang.Term.t
    ; error : Check_error'.t
    }

  let pp ppf { env = _; tm; error } = Check_error'.pp ppf tm error
end

let here = Provenance.of_here [%here]
let nominal_ty = Type.Quantified_ty.Sort (here, Name (here, (here, "nominal")))

let check_binding_pattern
    syntax
    (pat : Binding_aware_pattern.t)
    (sort : Lvca_syntax.Sort.t)
    : (type_env, Check_error'.t) Result.t
  =
  let lookup_operator = Abstract_syntax.lookup_operator syntax in
  let here = Provenance.of_here [%here] in
  let rec check (sort : Lvca_syntax.Sort.t) pat =
    let sort' = Sort_model.Sort.into sort in
    match pat with
    | Binding_aware_pattern.Var (_, name) when Lvca_util.String.is_ignore name ->
      Ok String.Map.empty
    | Var (_, name) ->
      Ok (String.Map.singleton name (Type.Quantified_ty.Sort (here, sort')))
    | Primitive prim ->
      (match Primitive_impl.All.check prim sort with
      | None -> Ok String.Map.empty
      | Some msg -> Error (Check_error'.Binding_pattern_check msg))
    | Operator (_, op_name, subpats) ->
      let sort_name, sort_args = Lvca_syntax.Sort.split sort in
      (match lookup_operator sort_name op_name with
      | None ->
        Error
          (Check_error'.Binding_pattern_check
             (Printf.sprintf
                "Pattern.check: failed to find operator %s in sort %s"
                op_name
                sort_name))
      | Some (sort_vars, Operator_def (_, _, arity)) ->
        (* TODO: kind check *)
        let sort_vars = List.map sort_vars ~f:Tuple2.get1 in
        let sort_env = String.Map.of_alist_exn (List.zip_exn sort_vars sort_args) in
        check_slots (Arity.instantiate sort_env arity) subpats)
  and check_slots (Arity (_, _)) _ = failwith "TODO" in
  check sort pat
;;

let primitive_types =
  let here = Provenance.of_here [%here] in
  let arr t1 t2 = Type.Quantified_ty.Arrow (here, t1, t2) in
  let sort s =
    Type.Quantified_ty.Sort (Lvca_syntax.Sort.info s, Sort_model.Sort.into s)
  in
  let int = sort (Lvca_syntax.Sort.Name (here, "int")) in
  let string = sort (Lvca_syntax.Sort.Name (here, "string")) in
  let bool = sort (Lvca_syntax.Sort.Name (here, "bool")) in
  let char' = Lvca_syntax.Sort.Name (here, "char") in
  let char = sort char' in
  let list s =
    sort (Lvca_syntax.Sort.Ap (here, "list", Lvca_syntax.Sort.Ap_list.of_list [ s ]))
  in
  let binary_int = arr int (arr int int) in
  let forall names body =
    let names =
      names
      |> List.map ~f:(fun name -> Nominal.Term.Var (here, name))
      |> List_model.of_list
      |> List_model.List.to_nominal Nominal.Term.to_nominal
      |> to_pattern_exn
    in
    Type.Ty.Forall (here, (names, body))
  in
  let a = Type.Ty.Ty_var (here, "a") in
  String.Map.of_alist_exn
    [ "rename", forall [] (arr string (arr string nominal_ty))
    ; "var", forall [] (arr string nominal_ty)
    ; "quote", forall [ "a" ] (arr a nominal_ty)
    ; "antiquote", forall [ "a" ] (arr nominal_ty a)
    ; "add", forall [] binary_int
    ; "sub", forall [] binary_int
    ; "string_of_chars", forall [] (arr (list char') string)
    ; "is_digit", forall [] (arr char bool)
    ; "is_lowercase", forall [] (arr char bool)
    ; "is_uppercase", forall [] (arr char bool)
    ; "is_alpha", forall [] (arr char bool)
    ; "is_alphanum", forall [] (arr char bool)
    ; "is_whitespace", forall [] (arr char bool)
    ]
;;

let rec check ({ type_env; syntax } as env) tm ty =
  let check_ty ty' =
    if Type.Ty.equivalent ty ty'
    then None
    else Some Check_error.{ env; tm; ty; error = Mismatch (ty, ty') }
  in
  match tm with
  | Lang.Term.Nominal _ -> check_ty nominal_ty
  | Ap (_, f, args) ->
    (match infer env f with
    | Ok f_ty -> check_args env tm f_ty (List_model.to_list args)
    | Error Infer_error.{ env; tm; error } -> Some { env; tm; ty; error })
  | Case (_, tm, branches) ->
    (match infer env tm with
    | Error { env; tm; error } -> Some { env; tm; ty; error }
    | Ok (Type.Quantified_ty.Arrow _ | Forall _ | Ty_var _) -> failwith "TODO"
    | Ok (Sort (_, sort)) ->
      let sort = Sort_model.Sort.out sort in
      branches
      |> List_model.to_list
      |> List.find_map ~f:(fun (Lang.Case_scope.Case_scope (_, pat, rhs)) ->
             match
               check_binding_pattern syntax (Binding_aware_pattern_model.out pat) sort
             with
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

and infer ({ type_env; syntax = _ } as env) tm =
  match tm with
  | Lang.Term.Nominal _ -> Ok nominal_ty
  | Ap (_, f, args) ->
    (match infer env f with
    | Ok f_ty ->
      (match check_args env tm f_ty (List_model.to_list args) with
      | None -> Ok (Type.chop_trailing (args |> List_model.to_list |> List.length) f_ty)
      | Some { env; tm; ty = _; error } -> Error { env; tm; error })
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
      (match Map.find primitive_types name with
      | None -> Error { env; tm; error = Var_not_found }
      | Some ty' -> Ok ty')
    | Some ty' -> Ok ty')

and check_args env tm ty args =
  match ty, args with
  | Type.Quantified_ty.Arrow (_, t1, t2), arg :: args ->
    Option.first_some (check env arg t1) (check_args env tm t2 args)
  | Arrow _, [] -> None (* under-applied is okay *)
  | Sort _, [] -> None
  | Sort _, _ -> Some Check_error.{ env; tm; ty; error = Overapplication }
  | Forall _, _ -> Some Check_error.{ env; tm; ty; error = failwith "TODO" }
  | Ty_var _, _ -> failwith "TODO"

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
        let bindings = Map.map bindings ~f:(fun v -> Lang.Term.Nominal (here, v)) in
        Some (rhs, bindings))
;;

type eval_error = string * Lang.Term.t

let eval_char_bool_fn eval_nominal_in_ctx name f (ctx : eval_env) tm c =
  let true_tm info = Nominal.Term.Operator (info, "True", []) in
  let false_tm info = Nominal.Term.Operator (info, "False", []) in
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
    | Some (Nominal (_, v)) -> Ok v
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
      Error ("no match found in case", Nominal (info, tm_val))
    | Some (branch, bindings) ->
      eval_in_ctx (Lvca_util.Map.union_right_biased ctx bindings) branch)
  | Ap (_, Lambda (_, _ty, (Single_var.{ name; info = _ }, body)), Cons (_, arg, Nil _))
    ->
    let%bind arg_val = eval_in_ctx ctx arg in
    let here = Provenance.of_here [%here] in
    let data = Lang.Term.Nominal (here, arg_val) in
    eval_in_ctx (Map.set ctx ~key:name ~data) body
  | Ap (info, Term_var (_, name), args) ->
    (match Map.find ctx name with
    | None -> eval_primitive eval_in_ctx eval_nominal_in_ctx ctx tm name args
    | Some tm ->
      let here = Provenance.calculated_here [%here] [ info ] in
      eval_in_ctx ctx (Ap (here, tm, args)))
  | Nominal (_, tm) ->
    let free_vars = Nominal.Term.free_vars tm in
    let restricted_ctx = Map.filter_keys ctx ~f:(fun key -> Set.mem free_vars key) in
    let ctx =
      Map.map restricted_ctx ~f:(function
          | Nominal (_, v) -> v
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
  | Let (info, tm, _, (Single_var.{ name; info = _ }, body))
  | Subst (info, (Single_var.{ name; _ }, body), tm) ->
    let%bind tm_val = eval_in_ctx ctx tm in
    let here = Provenance.calculated_here [%here] [ info ] in
    let data = Lang.Term.Nominal (here, tm_val) in
    eval_in_ctx (Map.set ctx ~key:name ~data) body
  | _ -> Error ("Found a term we can't evaluate", tm)

and eval_nominal_in_ctx (ctx : eval_env) tm : eval_result =
  match tm with
  | Nominal.Term.Var (info, v) ->
    (match Map.find ctx v with
    | Some (Nominal (_, v)) -> Ok v
    | Some _ -> Lvca_util.invariant_violation ~here:[%here] "TODO"
    | None ->
      Error
        ( "Unbound variable " ^ v
        , Lang.Term.Nominal (Provenance.calculated_here [%here] [ info ], tm) ))
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
  | "var", [ str_tm ] ->
    (match str_tm with
    | Primitive (info, String name) -> Ok (Nominal.Term.Var (info, name))
    | _ -> Error ("expected a string", tm))
  (* TODO | "quote", [ tm ] -> Ok (Lang.Term.to_nominal tm) *)
  (* | "antiquote", [ tm ] -> failwith "TODO" *)
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

let%test_module "Parsing" =
  (module struct
    let parse_pretty str = str |> parse_exn |> Fmt.pr "%a\n" Pp.term

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
      parse_pretty "let x = 1 in Some(x) in Some(x)}";
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
      match empty with { }
      match x with { | _ -> 1 }
      match x with { True() -> False() | False() -> True() }
      let x = True() in not x
      fail "some reason for failing"
      match c with { 'c' -> True() | _ -> False() }
      Some(1)
      let x = 1 in Some(x) in Some(x)}
      body[f := reduce arg]
      \(tm : lam) -> tm[x := Var("y")]
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
      \(tm: term) -> match tm with {
        | Lam(_) -> tm
        | App(f; arg) -> match reduce f with {
          | Lam(x. body) -> body[f := reduce arg]
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
          | true()  -> meaning t2
          | false() -> meaning t3
        }
        | ap(f; arg) -> (meaning f) (meaning arg)
        | fun(scope) -> Lambda(List(); scope)
      }
      |};
      [%expect
        {|
      \(tm : ty) -> match tm with {
        | True() -> True()
        | False() -> False()
        | Ite(t1; t2; t3) -> match meaning t1 with {
          | true()  -> meaning t2
          | false() -> meaning t3
        }
        | ap(f; arg) -> (meaning f) (meaning arg)
        | fun(scope) -> Lambda(List(); scope)
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
        | Error (msg, tm) -> Fmt.str "%s: %a" msg Lang.Term.pp tm
        | Ok result -> Fmt.to_to_string Nominal.Term.pp result
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
      eval_str "add 1 2";
      [%expect {| 3 |}]
    ;;

    let%expect_test _ =
      eval_str "sub 1 2";
      [%expect {| -1 |}]
    ;;

    let%expect_test _ =
      eval_str "is_digit '9'";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "is_digit 'a'";
      [%expect {| False() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alpha 'a'";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alpha 'A'";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alpha '9'";
      [%expect {| False() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alphanum '9'";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alphanum 'Z'";
      [%expect {| True() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alphanum '_'";
      [%expect {| False() |}]
    ;;

    let%expect_test _ =
      eval_str "sub 1 2";
      [%expect {| -1 |}]
    ;;

    let%expect_test _ =
      eval_str "x[x := 1]";
      [%expect {| 1 |}]
    ;;

    let%expect_test _ =
      eval_str "(sub x 2)[x := 1]";
      [%expect {| -1 |}]
    ;;

    let%expect_test _ =
      eval_str {|rename "foo" "bar" Cons(foo; Nil())|};
      [%expect {| Cons(bar; Nil()) |}]
    ;;

    let%expect_test _ =
      eval_str {|rename "foo" "bar" Cons(baz; Nil())|};
      [%expect {| Cons(baz; Nil()) |}]
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
    let ty = mk_test Type.Ty.parse Type.Ty.pp
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
      module' {|go : string; x := "foo"|};
      [%expect {|
        go : string;
        x := "foo" |}]
    ;;
  end)
;;

let%test_module "Core eval in dynamics" =
  (module struct
    module Term = Lang.Term

    let eval_in dynamics_str str =
      let defn = parse_exn dynamics_str in
      let core = parse_exn str in
      let tm = Lang.Term.Ap (here, defn, List_model.of_list [ core ]) in
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
  | ap(f; arg) -> (meaning f) (meaning arg)
  | fun(scope) -> Lambda(List(); scope)
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

let%test_module "Checking / inference" =
  (module struct
    module Term = Lang.Term

    let parse_type str =
      Lvca_parsing.parse_string Type.Ty.parse str |> Result.ok_or_failwith
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
      | None -> Fmt.pr "checked\n"
    ;;

    let infer ?(type_env = String.Map.empty) tm_str =
      let tm = parse_exn tm_str in
      let ctx = { type_env; syntax } in
      match infer ctx tm with
      | Error err -> Infer_error.pp Fmt.stdout err
      | Ok ty -> Fmt.pr "inferred: %a\n" Type.Ty.pp ty
    ;;

    let%expect_test _ =
      check "nominal" "True()";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check "nominal" "2";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check "nominal" "2";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      infer "2";
      [%expect {| inferred: nominal |}]
    ;;

    let%expect_test _ =
      infer "True()";
      [%expect {| inferred: nominal |}]
    ;;

    let%expect_test _ =
      check "a -> a" "Nil()";
      [%expect {| nominal != a -> a |}]
    ;;

    let%expect_test _ =
      check "list integer" "Cons(1; Nil())";
      [%expect {| nominal != list integer |}]
    ;;

    let%expect_test _ =
      check "bool" "let rec x : bool = y and y : bool = x in x";
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check
        "bool"
        {|match True() with {
          | True() -> 1
          | False() -> 2
        }|};
      [%expect {| checked |}]
    ;;

    let type_env =
      String.Map.of_alist_exn
        [ ( "x"
          , Type.Quantified_ty.Sort (here, Sort_model.Sort.mk_Name ~info:here (here, "a"))
          )
        ]
    ;;

    let%expect_test _ =
      infer ~type_env "x";
      [%expect {| inferred: a |}]
    ;;

    let%expect_test _ =
      infer ~type_env "x";
      [%expect {| inferred: nominal |}]
    ;;

    let%expect_test _ =
      check ~type_env "bool" "x";
      [%expect {| nominal != bool |}]
    ;;

    let%expect_test _ =
      check ~type_env "integer" "x";
      [%expect {| nominal != integer |}]
    ;;

    let%expect_test _ =
      check ~type_env "list integer" {|rename "foo" "bar" Cons(foo; Nil())|};
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      check ~type_env "integer" {|add 1 2|};
      [%expect {| checked |}]
    ;;

    let%expect_test _ =
      infer ~type_env {|add 1|};
      [%expect {| int -> int |}]
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
        inferred: int -> int -> int
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
