(** A small "core" language. *)

open Base
open Lvca_syntax
open AbstractSyntax
module Format = Caml.Format

type is_rec =
  | Rec
  | NoRec

type 'a term =
  | Term of ('a, Primitive.t) Nominal.term
  (* plus, core-specific ctors *)
  | CoreApp of 'a term * 'a term
  | Case of 'a term * 'a core_case_scope list
  | Lambda of sort * 'a core_scope
  | Let of is_rec * 'a term * 'a core_scope (** Lets bind only a single variable *)

and 'a core_scope = Scope of string * 'a term

and 'a core_case_scope = CaseScope of ('a, Primitive.t) Pattern.t * 'a term

let rec erase : 'a term -> unit term = function
  | Term tm -> Term (Nominal.erase tm)
  | CoreApp (t1, t2) -> CoreApp (erase t1, erase t2)
  | Case (tm, scopes) -> Case (erase tm, List.map scopes ~f:erase_case_scope)
  | Lambda (sort, core_scope) -> Lambda (sort, erase_core_scope core_scope)
  | Let (is_rec, tm, core_scope) -> Let (is_rec, erase tm, erase_core_scope core_scope)

and erase_core_scope : 'a core_scope -> unit core_scope =
 fun (Scope (name, tm)) -> Scope (name, erase tm)

and erase_case_scope : 'a core_case_scope -> unit core_case_scope =
 fun (CaseScope (pat, tm)) -> CaseScope (Pattern.erase pat, erase tm)
;;

module PP = struct
  let braces, list, any, pf, sp = Fmt.(braces, list, any, pf, sp)

  (* TODO: add parse <-> pretty tests *)

  (** @raise InvariantViolation *)
  let rec pp ppf = function
    | Term (Var (_, v)) -> pf ppf "%s" v (* XXX *)
    | Term tm -> pf ppf "%a" (braces (Nominal.pp_term Primitive.pp)) tm
    | Lambda (sort, Scope (name, body)) ->
      pf ppf "\\(%s : %a) ->@ %a" name pp_sort sort pp body
    (* TODO: parens if necessary *)
    | CoreApp _ as app -> pp_app ppf app
    | Case (arg, case_scopes) ->
      pf
        ppf
        "@[<hv>match %a with {%t%a@ }@]"
        pp
        arg
        (* Before `|`, emit a single space if on the same line, or two when broken *)
        (Format.pp_print_custom_break ~fits:("", 1, "") ~breaks:("", 2, "| "))
        (list ~sep:(any "@;<1 2>| ") pp_core_case_scope)
        case_scopes
    | Let (is_rec, tm, Scope (name, body)) ->
      pf
        ppf
        "@[let %s%s =@ %a in@ @[%a@]@]"
        (match is_rec with Rec -> "rec " | NoRec -> "")
        name
        pp
        tm
        pp
        body

  and pp_core_case_scope : Format.formatter -> 'a core_case_scope -> unit =
   fun ppf (CaseScope (pat, body)) ->
    pf ppf "@[%a@ -> %a@]" (Pattern.pp Primitive.pp) pat pp body

  (* Flatten all arguments into one box *)
  and pp_app ppf app =
    let rec go = function
      | CoreApp (f_args, final_arg) -> go f_args @ [ final_arg ]
      | tm -> [ tm ]
    in
    match go app with
    | [] -> Lvca_util.invariant_violation "pp_app: must be at least one argument"
    | f :: args -> pf ppf "@[<h>%a@ @[<hov>%a@]@]" pp f (list ~sep:sp pp) args
  ;;
end

let pp : Format.formatter -> 'a term -> unit = PP.pp
let to_string : 'a term -> string = fun tm -> Format.asprintf "%a" pp tm

type import =
  { imported_symbols : (string * string option) list
  ; location : string
  }

type 'a defn = Defn of import list * 'a term

let pp_import_symbol : Format.formatter -> string * string option -> unit =
 fun ppf (name1, name2) ->
  match name2 with
  | None -> Fmt.string ppf name1
  | Some name2' -> Fmt.pf ppf "%s as %s" name1 name2'
;;

let pp_import : Format.formatter -> import -> unit =
 fun ppf { imported_symbols; location } ->
  Fmt.pf
    ppf
    "import { %a } from \"%s\""
    (Fmt.list ~sep:(Fmt.any ", ") pp_import_symbol)
    imported_symbols
    location
;;

let pp_defn : Format.formatter -> 'a defn -> unit =
 fun ppf (Defn (imports, defn)) ->
  List.iter imports ~f:(fun import ->
      pp_import ppf import;
      Format.pp_force_newline ppf ());
  pp ppf defn
;;

let defn_to_string : 'a defn -> string = fun defn -> Format.asprintf "%a" pp_defn defn
let erase_defn (Defn (imports, tm)) = Defn (imports, erase tm)

let merge_results
    :  ('a, Primitive.t) Nominal.term Lvca_util.String.Map.t option list
    -> ('a, Primitive.t) Nominal.term Lvca_util.String.Map.t option
  =
 fun results ->
  if List.for_all results ~f:Option.is_some
  then
    Some
      (results
      |> List.map
           ~f:(Lvca_util.Option.get_invariant (fun () -> "we just checked all is_some"))
      |> Lvca_util.String.Map.strict_unions
      |> function
      | `Duplicate_key k ->
        Lvca_util.invariant_violation
          (Printf.sprintf "multiple variables with the same name (%s) in one pattern" k)
      | `Ok m -> m)
  else None
;;

let rec match_pattern
    :  ('a, Primitive.t) Nominal.term -> ('b, Primitive.t) Pattern.t
    -> ('a, Primitive.t) Nominal.term Lvca_util.String.Map.t option
  =
 fun v pat ->
  match v, pat with
  | Operator (_, tag1, vals), Operator (_, tag2, pats) ->
    if String.(tag1 = tag2)
    then (
      match
        List.map2 pats vals ~f:(fun pats -> function
          | Scope ([], body_tms) ->
            (match List.map2 body_tms pats ~f:match_pattern with
            | Ok results -> merge_results results
            | Unequal_lengths -> None)
          | _ -> None)
      with
      | Ok results -> merge_results results
      | Unequal_lengths -> None)
    else None
  | Primitive (_, l1), Primitive (_, l2) ->
    if Primitive.(l1 = l2) then Some Lvca_util.String.Map.empty else None
  | _, Ignored _ -> Some Lvca_util.String.Map.empty
  | tm, Var (_, v) -> Some (Lvca_util.String.Map.of_alist_exn [ v, tm ])
  | _ -> None
;;

let find_core_match
    :  ('a, Primitive.t) Nominal.term -> 'b core_case_scope list
    -> ('b term * ('a, Primitive.t) Nominal.term Lvca_util.String.Map.t) option
  =
 fun v branches ->
  branches
  |> List.find_map ~f:(fun (CaseScope (pat, rhs)) ->
         match match_pattern v pat with
         | None -> None
         | Some bindings -> Some (rhs, bindings))
;;

type eval_error = string * unit term

exception EvalExn of string * unit term

let rec eval_ctx_exn
    :  ('a, Primitive.t) Nominal.term Lvca_util.String.Map.t -> 'a term
    -> ('a, Primitive.t) Nominal.term
  =
 fun ctx tm ->
  match tm with
  | Term (Var (_, v)) ->
    (match Map.find ctx v with
    | Some result -> result
    | None -> raise @@ EvalExn ("Unbound variable " ^ v, erase tm))
  | CoreApp (Lambda (_ty, Scope (name, body)), arg) ->
    let arg_val = eval_ctx_exn ctx arg in
    eval_ctx_exn (Map.set ctx ~key:name ~data:arg_val) body
  | Case (tm, branches) ->
    let tm_val = eval_ctx_exn ctx tm in
    (match find_core_match tm_val branches with
    | None -> raise @@ EvalExn ("no match found in case", erase @@ Term tm_val)
    | Some (branch, bindings) ->
      eval_ctx_exn (Lvca_util.Map.union_right_biased ctx bindings) branch)
  (* primitives *)
  (* TODO: or should this be an app? *)
  | Term (Operator (_, "add", [ Scope ([], [ a ]); Scope ([], [ b ]) ])) ->
    (match eval_ctx_exn' ctx a, eval_ctx_exn' ctx b with
    | Primitive (loc, PrimInteger a'), Primitive (_, PrimInteger b') ->
      (* XXX can't reuse loc *)
      Primitive (loc, PrimInteger Bigint.(a' + b'))
    | _ -> raise @@ EvalExn ("Invalid arguments to add", erase tm))
  | Term (Operator (_, "sub", [ Scope ([], [ a ]); Scope ([], [ b ]) ])) ->
    (match eval_ctx_exn' ctx a, eval_ctx_exn' ctx b with
    | Primitive (loc, PrimInteger a'), Primitive (_, PrimInteger b') ->
      Primitive (loc, PrimInteger Bigint.(a' - b'))
    | _ -> raise @@ EvalExn ("Invalid arguments to sub", erase tm))
  | Term tm -> tm
  | _ -> raise @@ EvalExn ("Found a term we can't evaluate", erase tm)

and eval_ctx_exn'
    :  ('a, Primitive.t) Nominal.term Lvca_util.String.Map.t
    -> ('a, Primitive.t) Nominal.term -> ('a, Primitive.t) Nominal.term
  =
 fun ctx tm ->
  match tm with
  | Var (_, v) ->
    (match Map.find ctx v with
    | Some result -> result
    | None -> raise @@ EvalExn ("Unbound variable " ^ v, erase @@ Term tm))
  | _ -> tm
;;

let eval_exn : 'a term -> ('a, Primitive.t) Nominal.term =
 fun core -> eval_ctx_exn Lvca_util.String.Map.empty core
;;

let eval : 'a term -> (('a, Primitive.t) Nominal.term, eval_error) Result.t =
 fun core -> try Ok (eval_exn core) with EvalExn (msg, tm) -> Error (msg, tm)
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Term = Nominal.Parse (Comment)
  module ParsePrimitive = Primitive.Parse (Comment)
  module Abstract = AbstractSyntax.Parse (Comment)
  open Parsers

  let reserved = Lvca_util.String.Set.of_list [ "let"; "rec"; "in"; "match"; "with" ]

  let identifier =
    identifier
    >>= fun ident -> if Set.mem reserved ident then fail "reserved word" else return ident
  ;;

  let make_apps : 'a term list -> 'a term = function
    | [] -> Lvca_util.invariant_violation "make_apps: must be a nonempty list"
    | [ x ] -> x
    | f :: args -> List.fold_left args ~init:f ~f:(fun f_app arg -> CoreApp (f_app, arg))
  ;;

  let term : OptRange.t term Parsers.t =
    fix (fun term ->
        let atomic_term =
          choice
            [ parens term
            ; (identifier >>|| fun ~pos ident -> Term (Var (pos, ident)), pos)
            ; braces (Term.t ParsePrimitive.t) >>| (fun tm -> Term tm) <?> "quoted term"
            ]
        in
        let pattern' =
          Term.t ParsePrimitive.t
          >>= fun tm ->
          match Nominal.to_pattern tm with
          | Ok pat -> return pat
          | Error scope ->
            fail ("Unexpected scope: " ^ Nominal.pp_scope_str Primitive.pp scope)
        in
        let pattern = pattern' <?> "pattern" in
        let case_line =
          lift3 (fun pat _ tm -> CaseScope (pat, tm)) pattern (string "->") term
          <?> "case line"
        in
        choice
          [ lift4
              (fun _ (name, sort) _ body -> Lambda (sort, Scope (name, body)))
              (char '\\')
              (parens
                 (lift3
                    (fun ident _ sort -> ident, sort)
                    identifier
                    (char ':')
                    Abstract.sort))
              (string "->")
              term
            <?> "lambda"
          ; lift4
              (fun _let is_rec name _eq tm _in body ->
                Let (is_rec, tm, Scope (name, body)))
              (string "let")
              (option NoRec (Fn.const Rec <$> string "rec"))
              identifier
              (string "=")
            <*> term
            <*> string "in"
            <*> term
            <?> "let"
          ; lift4
              (fun _match tm _with lines -> Case (tm, lines))
              (string "match")
              term
              (string "with")
              (braces (option '|' (char '|') *> sep_by1 (char '|') case_line))
            <?> "match"
          ; many1 atomic_term >>| make_apps <?> "application"
          ])
    <?> "core term"
  ;;

  let import_symbol : (string * string option) Parsers.t =
    lift2
      (fun ident as_ident -> ident, as_ident)
      identifier
      (option None ((fun x -> Some x) <$> string "as" *> identifier))
    <?> "import symbol"
  ;;

  let import : import Parsers.t =
    lift4
      (fun _import imported_symbols _from location -> { imported_symbols; location })
      (string "import")
      (braces (sep_by1 (char ',') import_symbol))
      (string "from")
      string_lit
    <?> "import"
  ;;

  let defn : OptRange.t defn Parsers.t =
    lift2 (fun imports tm -> Defn (imports, tm)) (many import) term <?> "core definition"
  ;;
end

let%test_module "Parsing" =
  (module struct
    module Parse = Parse (ParseUtil.NoComment)

    let parse str =
      ParseUtil.parse_string Parse.term str |> Result.ok_or_failwith |> erase
    ;;

    let ( = ) = Caml.( = )
    let one = Nominal.Primitive ((), Primitive.PrimInteger (Bigint.of_int 1))
    let var name = Nominal.Var ((), name)
    let ignored name = Pattern.Ignored ((), name)
    let operator tag children = Nominal.Operator ((), tag, children)

    let%test _ = parse "{1}" = Term one
    let%test _ = parse "{true()}" = Term (operator "true" [])
    let%test _ = parse "not x" = CoreApp (Term (var "not"), Term (var "x"))

    let%test _ =
      parse {|\(x : bool()) -> x|}
      = Lambda (SortAp ("bool", []), Scope ("x", Term (var "x")))
    ;;

    let%test _ =
      parse {|match x with { _ -> {1} }|}
      = Case (Term (var "x"), [ CaseScope (ignored "", Term one) ])
    ;;

    let%test _ =
      parse {|match x with { | _ -> {1} }|}
      = Case (Term (var "x"), [ CaseScope (ignored "", Term one) ])
    ;;

    let%test _ =
      parse {|match x with { true() -> {false()} | false() -> {true()} }|}
      = Case
          ( Term (var "x")
          , [ CaseScope (Operator ((), "true", []), Term (operator "false" []))
            ; CaseScope (Operator ((), "false", []), Term (operator "true" []))
            ] )
    ;;

    let%test _ =
      parse "let x = {true()} in not x"
      = Let
          ( NoRec
          , Term (operator "true" [])
          , Scope ("x", CoreApp (Term (var "not"), Term (var "x"))) )
    ;;
  end)
;;
