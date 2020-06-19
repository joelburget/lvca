(** A small "core" language. *)

open Base
open AbstractSyntax
open Binding
module Format = Caml.Format

type is_rec = Rec | NoRec

type term =
  | Term of Nominal.term
  (* plus, core-specific ctors *)
  | CoreApp of term * term
  | Case of term * core_case_scope list
  | Lambda of sort * core_scope
  | Let of is_rec * term * core_scope
  (** Lets bind only a single variable *)

and core_scope = Scope of string * term

and core_case_scope = CaseScope of Pattern.t * term

module PP = struct
  let list, any, pf, sp = Fmt.(list, any, pf, sp)

  (* TODO: add parse <-> pretty tests *)

  (** @raise InvariantViolation *)
  let rec pp
    = fun ppf -> function
      | Term (Var v) -> pf ppf "%s" v (* XXX *)
      | Term tm -> pf ppf "{%a}" Nominal.pp_term tm
      | Lambda (sort, Scope (name, body)) ->
        pf ppf "\\(%s : %a) ->@ %a"
        name
        pp_sort sort
        pp body
      (* TODO: parens if necessary *)
      | (CoreApp _ as app) -> pp_app ppf app
      | Case (arg, case_scopes)
      -> pf ppf
        "@[<hv>match %a with {%t%a@ }@]"
        pp arg
        (* Before `|`, emit a single space if on the same line, or two when broken *)
        (Format.pp_print_custom_break ~fits:("", 1, "") ~breaks:("", 2, "| "))
        (list ~sep:(any "@;<1 2>| ") pp_core_case_scope) case_scopes
      | Let (is_rec, tm, Scope (name, body))
      -> pf ppf "@[let %s%s =@ %a in@ @[%a@]@]"
         (match is_rec with Rec -> "rec " | NoRec -> "")
         name pp tm pp body

  and pp_core_case_scope : Format.formatter -> core_case_scope -> unit
    = fun ppf (CaseScope (pat, body))
    -> pf ppf "@[%a@ -> %a@]" Pattern.pp pat pp body

  (* Flatten all arguments into one box *)
  and pp_app = fun ppf app ->
    let rec go = function
      | CoreApp (f_args, final_arg) -> go f_args @ [final_arg]
      | tm -> [tm]
    in
    match go app with
      | [] -> Util.invariant_violation "pp_app: must be at least one argument"
      | f :: args ->
        pf ppf "@[<h>%a@ @[<hov>%a@]@]"
        pp f
        (list ~sep:sp pp) args
end

let pp : Format.formatter -> term -> unit
  = PP.pp

let to_string : term -> string
  = Format.asprintf "%a" pp

type import = AbstractSyntax.import

type core_defn = CoreDefn of import list * term

let pp_defn : Format.formatter -> core_defn -> unit
  = fun ppf (CoreDefn (imports, defn)) ->
    List.iter imports ~f:(fun import ->
      AbstractSyntax.pp_import ppf import;
      Format.pp_force_newline ppf ()
    );
    pp ppf defn

let pp_defn_str : core_defn -> string
  = Format.asprintf "%a" pp_defn

let rec match_pattern
  : Nominal.term -> Pattern.t -> Nominal.term Util.String.Map.t option
  = fun v pat -> match v, pat with
  | Operator (tag1, vals), Operator (tag2, pats) ->
    if String.(tag1 = tag2) && List.(Int.(length vals = length pats))
    then (
      let sub_results =
        List.map2_exn vals pats ~f:(fun core_scope (pat : Pattern.t) ->
            match core_scope, pat with
            | Scope ([], body), pat' -> match_pattern body pat'
            | _ -> None)
      in
      if List.for_all sub_results ~f:Option.is_some
      then
        Some
          (sub_results
          |> List.map ~f:(Util.Option.get_invariant
            (fun () -> "we just checked all is_some"))
          |> Util.String.Map.strict_unions
          |> function
            | `Duplicate_key k -> Util.invariant_violation (Printf.sprintf
                "multiple variables with the same name (%s) in one pattern"
                k
            )
            | `Ok m -> m
          )
      else None)
    else None
  | Primitive l1, Primitive l2
  -> if Primitive.(l1 = l2) then Some Util.String.Map.empty else None
  | _, Var "_" -> Some Util.String.Map.empty
  | tm, Var v -> Some (Util.String.Map.of_alist_exn [ v, tm ])
  | _ -> None
;;

let find_core_match
  : Nominal.term -> core_case_scope list -> (term * Nominal.term Util.String.Map.t) option
  = fun v branches ->
  branches
  |> List.find_map ~f:(function
         | CaseScope (pat, rhs) ->
           (match match_pattern v pat with
           | None -> None
           | Some bindings -> Some (rhs, bindings)))
;;

type eval_error = string * term

exception EvalExn of string * term

let eval_exn : term -> Nominal.term
  = fun core ->
    let rec go : Nominal.term Util.String.Map.t -> term -> Nominal.term =
     fun ctx tm -> match tm with
      | Term (Var v) -> (match Map.find ctx v with
        | Some result -> result
        | None -> raise @@ EvalExn ("Unbound variable " ^ v, tm))
      | CoreApp (Lambda (_ty, Scope (name, body)), arg) ->
        let arg_val = go ctx arg in
        go (Map.set ctx ~key:name ~data:arg_val) body
      | Case (tm, branches) ->
        (match find_core_match (go ctx tm) branches with
        | None -> raise @@ EvalExn ("no match found in case", tm)
        | Some (branch, bindings) -> go (Util.Map.union_right_biased ctx bindings) branch)

      (* primitives *)
      (* TODO: or should this be an app? *)
      | Term (Operator ("add", [ Scope ([], a); Scope ([], b) ])) ->
        (match go' ctx a, go' ctx b with
        | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
          Primitive (PrimInteger Bigint.(a' + b'))
        | _ -> raise @@ EvalExn ("Invalid arguments to add", tm))
      | Term (Operator ("sub", [ Scope ([], a); Scope ([], b) ])) ->
        (match go' ctx a, go' ctx b with
        | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
          Primitive (PrimInteger Bigint.(a' - b'))
        | _ -> raise @@ EvalExn ("Invalid arguments to sub", tm))

      | Term tm -> tm
      | _ -> raise @@ EvalExn ("Found a term we can't evaluate", tm)
    and go' : Nominal.term Util.String.Map.t -> Nominal.term -> Nominal.term
      = fun ctx tm -> match tm with
        | Var v -> (match Map.find ctx v with
          | Some result -> result
          | None -> raise @@ EvalExn ("Unbound variable " ^ v, Term tm))
        | _ -> tm

    in go Util.String.Map.empty core

let eval : term -> (Nominal.term, eval_error) Result.t =
  fun core -> try Ok (eval_exn core) with EvalExn (msg, tm) -> Error (msg, tm)
;;

module Parse (Comment : Util.Angstrom.Comment_int) = struct
  open Angstrom
  module Parsers = Util.Angstrom.Mk(Comment)
  module Term = Binding.Nominal.Parse(Comment)
  module Primitive = Primitive.Parse(Comment)
  module Abstract = AbstractSyntax.Parse(Comment)
  let braces, identifier, char, parens, string =
    Parsers.(braces, identifier, char, parens, string)

  let reserved = Util.String.Set.of_list ["rec"; "in"; "match"; "with"; "let"]

  let identifier = identifier >>= fun ident ->
    if Set.mem reserved ident
    then fail "reserved word"
    else return ident

  let make_apps : term list -> term
    = function
      | [] -> Util.invariant_violation "make_apps: must be a nonempty list"
      | [x] -> x
      | f :: args -> List.fold_left args
        ~init:f
        ~f:(fun f_app arg -> CoreApp (f_app, arg))

  let term : term Angstrom.t
    = fix (fun term ->

      let atomic_term = choice
        [ parens term
        ; identifier >>| (fun ident -> Term (Var ident))
        ; braces Term.t >>| (fun tm -> Term tm)
          <?> "quoted term"
        ]
      in

      let pattern = Nominal.to_pattern_exn <$> Term.t
        <?> "pattern"
      in

      let case_line = lift3
        (fun pat _ tm -> CaseScope (pat, tm))
        pattern
        (string "->")
        term
        <?> "case line"
      in

      choice
      [ lift4 (fun _ (name, sort) _ body -> Lambda (sort, Scope (name, body)))
        (char '\\')
        (parens (
          lift3 (fun ident _ sort -> ident, sort)
          identifier
          (char ':')
          Abstract.sort
        ))
        (string "->")
        term
        <?> "lambda"

      ; lift4
        (fun _let is_rec name _eq tm _in body -> Let (is_rec, tm, Scope (name, body)))
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

      ; many1 atomic_term >>| make_apps
        <?> "application"
      ]) <?> "core term"

  let core_defn : core_defn Angstrom.t
    = lift2 (fun imports tm -> CoreDefn (imports, tm))
      (many Abstract.import)
      term
      <?> "core definition"
end

let%test_module "Parsing" = (module struct
  module Parse = Parse(Util.Angstrom.NoComment)

  let parse str =
    match
      Angstrom.parse_string ~consume:All Parse.term str
    with
      | Ok tm -> tm
      | Error msg -> failwith msg

  let (=) = Caml.(=)

  let%test _ = parse "{true()}" = Term (Operator ("true", []))
  let%test _ = parse "not x" = CoreApp (Term (Var "not"), Term (Var "x"))

  let%test _ = parse {|\(x : bool()) -> x|} =
    Lambda (SortAp ("bool", []), Scope ("x", Term (Var "x")))

  let%test _ = parse {|match x with { _ -> {1} }|} = Case
    ( Term (Var "x")
    , [ CaseScope (Ignored "", Term (Primitive (PrimInteger (Bigint.of_int 1)))) ]
    )

  let%test _ = parse {|match x with { | _ -> {1} }|} = Case
    ( Term (Var "x")
    , [ CaseScope (Ignored "", Term (Primitive (PrimInteger (Bigint.of_int 1)))) ]
    )

  let%test _ = parse {|match x with { true() -> {false()} | false() -> {true()} }|} = Case
    ( Term (Var "x")
    , [ CaseScope (Operator ("true", []), Term (Operator ("false", [])))
      ; CaseScope (Operator ("false", []), Term (Operator ("true", [])))
      ]
    )

  let%test _ =
    parse "let x = {true()} in not x"
    =
    Let
      ( NoRec
      , Term (Operator ("true", []))
      , Scope ("x", CoreApp (Term (Var "not"), Term (Var "x")))
      )
end);;

(* module_to_term *)

(*
let rec term_of_core : term -> Nominal.term
  = function
  | Term tm -> Operator ("term", [Scope ([], tm)])
  | Lambda (sort, scope) -> Operator ("lambda",
    [ Scope ([], sort
        |> term_of_sort
        |> NonBinding.to_nominal)
    ; scope_of_core_scope scope
    ])
  | CoreApp (f, arg) -> Operator ("core_app",
    [ Scope ([], term_of_core f)
    ; Scope ([], term_of_core arg)
    ])
  | Case (tm, _branches) -> Operator ("case",
    [ Scope ([], term_of_core tm)
    (* TODO *)
    (* ; Scope ([], Sequence (List.map branches ~f:scope_of_core_case_scope)) *)
    ])
  | Let (is_rec, tm, body) ->
     let rec_tm = match is_rec with Rec -> "rec" | NoRec -> "norec" in
     Operator ("let",
    [ Scope ([], Operator (rec_tm, []))
    ; Scope ([], term_of_core tm)
    ; scope_of_core_scope body
    ])

and scope_of_core_scope : core_scope -> Nominal.scope
  = fun (Scope (name, body)) -> Scope ([Pattern.Var name], term_of_core body)

  (*
and scope_of_core_case_scope : core_case_scope -> Nominal.scope
  = fun (CaseScope (baw_pat, body)) -> failwith "TODO"
  *)

let module_to_term : core_defn -> Nominal.term
  = fun (CoreDefn (imports, defns)) -> Operator ("core_defn",
    [ Scope ([], Sequence (imports
        |> List.map ~f:AbstractSyntax.term_of_import
        |> List.map ~f:NonBinding.to_nominal
      ))
    ; Scope ([], Sequence (List.map defns
      ~f:(fun { name; ty; defn } -> Nominal.Operator ("core_defn",
        [ Scope ([], Primitive (PrimString name))
        ; Scope ([], ty |> term_of_sort |> NonBinding.to_nominal)
        ; Scope ([], term_of_core defn)
        ]))))
    ])
    *)
