(** A small "core" language. *)

open Base
open Lvca_syntax
module Format = Caml.Format

type is_rec =
  | Rec
  | NoRec

type 'a term =
  | Term of ('a, Primitive.t) Nominal.term
  (* plus, core-specific ctors *)
  | CoreApp of 'a * 'a term * 'a term
  | Case of 'a * 'a term * 'a core_case_scope list
  | Lambda of 'a * Sort.t * 'a core_scope
  | Let of 'a * is_rec * 'a term * 'a core_scope (** Lets bind only a single variable *)

and 'a core_scope = Scope of string * 'a term

and 'a core_case_scope = CaseScope of ('a, Primitive.t) Pattern.t * 'a term

let rec map_loc ~f = function
  | Term tm -> Term (Nominal.map_loc ~f tm)
  | CoreApp (loc, t1, t2) -> CoreApp (f loc, map_loc ~f t1, map_loc ~f t2)
  | Case (loc, tm, scopes) ->
    Case (f loc, map_loc ~f tm, List.map scopes ~f:(map_loc_case_scope ~f))
  | Lambda (loc, sort, core_scope) ->
    Lambda (f loc, sort, map_loc_core_scope ~f core_scope)
  | Let (loc, is_rec, tm, core_scope) ->
    Let (f loc, is_rec, map_loc ~f tm, map_loc_core_scope ~f core_scope)

and map_loc_core_scope ~f (Scope (name, tm)) = Scope (name, map_loc ~f tm)

and map_loc_case_scope ~f (CaseScope (pat, tm)) =
  CaseScope (Pattern.map_loc ~f pat, map_loc ~f tm)
;;

let location = function
  | Term tm -> Nominal.location tm
  | CoreApp (loc, _, _) | Case (loc, _, _) | Lambda (loc, _, _) | Let (loc, _, _, _) ->
    loc
;;

let erase tm = map_loc ~f:(fun _ -> ()) tm

module PP = struct
  let braces, list, any, pf, sp = Fmt.(braces, list, any, pf, sp)

  (* TODO: add parse <-> pretty tests *)

  (** @raise InvariantViolation *)
  let rec pp ppf = function
    | Term (Var (_, v)) -> pf ppf "%s" v (* XXX *)
    | Term tm -> pf ppf "%a" (braces (Nominal.pp_term Primitive.pp)) tm
    | Lambda (_, sort, Scope (name, body)) ->
      pf ppf "\\(%s : %a) ->@ %a" name Sort.pp sort pp body
    (* TODO: parens if necessary *)
    | CoreApp _ as app -> pp_app ppf app
    | Case (_, arg, case_scopes) ->
      pf
        ppf
        "@[<hv>match %a with {%t%a@ }@]"
        pp
        arg
        (* Before `|`, emit a single space if on the same line, or two when broken *)
        (Format.pp_print_custom_break ~fits:("", 1, "") ~breaks:("", 2, "| "))
        (list ~sep:(any "@;<1 2>| ") pp_core_case_scope)
        case_scopes
    | Let (_, is_rec, tm, Scope (name, body)) ->
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
      | CoreApp (_, f_args, final_arg) -> go f_args @ [ final_arg ]
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

type 'a n_term = ('a, Primitive.t) Nominal.term

let merge_results
    :  'a n_term Lvca_util.String.Map.t option list
    -> 'a n_term Lvca_util.String.Map.t option
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
    : 'a n_term -> ('b, Primitive.t) Pattern.t -> 'a n_term Lvca_util.String.Map.t option
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
    :  'a n_term -> 'b core_case_scope list
    -> ('b term * 'a n_term Lvca_util.String.Map.t) option
  =
 fun v branches ->
  branches
  |> List.find_map ~f:(fun (CaseScope (pat, rhs)) ->
         match match_pattern v pat with
         | None -> None
         | Some bindings -> Some (rhs, bindings))
;;

type 'a eval_error = string * 'a term

let true_tm loc = Nominal.Operator (loc, "true", [])
let false_tm loc = Nominal.Operator (loc, "false", [])

let rec eval_ctx
    : 'a n_term Lvca_util.String.Map.t -> 'a term -> ('a n_term, 'a eval_error) Result.t
  =
 fun ctx tm ->
  let open Result.Let_syntax in
  match tm with
  | Term (Var (_, v)) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, tm))
  | CoreApp (_, Lambda (_, _ty, Scope (name, body)), arg) ->
    let%bind arg_val = eval_ctx ctx arg in
    eval_ctx (Map.set ctx ~key:name ~data:arg_val) body
  | Case (_, tm, branches) ->
    let%bind tm_val = eval_ctx ctx tm in
    (match find_core_match tm_val branches with
    | None -> Error ("no match found in case", Term tm_val)
    | Some (branch, bindings) ->
      eval_ctx (Lvca_util.Map.union_right_biased ctx bindings) branch)
  (* primitives *)
  (* TODO: or should this be an app? *)
  | CoreApp (_, CoreApp (_, Term (Var (_, "add")), Term a), Term b) ->
    let%bind a_result = eval_ctx' ctx a in
    let%bind b_result = eval_ctx' ctx b in
    (match a_result, b_result with
    | Primitive (loc, PrimInteger a'), Primitive (_, PrimInteger b') ->
      (* XXX can't reuse loc *)
      Ok (Nominal.Primitive (loc, Primitive.PrimInteger Z.(a' + b')))
    | _ -> Error ("Invalid arguments to add", tm))
  | CoreApp (_, CoreApp (_, Term (Var (_, "sub")), Term a), Term b) ->
    let%bind a_result = eval_ctx' ctx a in
    let%bind b_result = eval_ctx' ctx b in
    (match a_result, b_result with
    | Primitive (loc, PrimInteger a'), Primitive (_, PrimInteger b') ->
      Ok (Nominal.Primitive (loc, Primitive.PrimInteger Z.(a' - b')))
    | _ -> Error ("Invalid arguments to sub", tm))
  | CoreApp (_, Term (Var (_, "string_of_chars")), char_list) ->
    let%bind char_list = eval_ctx ctx char_list in
    (match char_list with
    | Operator (loc, "list", [ Nominal.Scope ([], chars) ]) ->
      chars
      |> List.map ~f:(function
             | Nominal.Primitive (_, Primitive.PrimChar c) -> Ok c
             | tm ->
               Error
                 (Printf.sprintf
                    "string_of_chars `list(%s)`"
                    (Nominal.pp_term_str Primitive.pp tm)))
      |> Result.all
      |> Result.map ~f:(fun cs ->
             Nominal.Primitive (loc, Primitive.PrimString (String.of_char_list cs)))
      |> Result.map_error ~f:(fun msg -> msg, tm)
    | _ -> Error ("expected a list of characters", tm))
  | CoreApp (_, Term (Var (_, "var")), str_tm) ->
    let%bind str = eval_ctx ctx str_tm in
    (match str with
    | Primitive (loc, PrimString name) -> Ok (Nominal.Var (loc, name))
    | _ -> Error ("expected a string", tm))
  | CoreApp (_, Term (Var (_, "is_digit")), Term c) ->
    eval_char_bool_fn "is_digit" Char.is_digit ctx tm c
  | CoreApp (_, Term (Var (_, "is_lowercase")), Term c) ->
    eval_char_bool_fn "is_lowercase" Char.is_lowercase ctx tm c
  | CoreApp (_, Term (Var (_, "is_uppercase")), Term c) ->
    eval_char_bool_fn "is_uppercase" Char.is_uppercase ctx tm c
  | CoreApp (_, Term (Var (_, "is_alpha")), Term c) ->
    eval_char_bool_fn "is_alpha" Char.is_alpha ctx tm c
  | CoreApp (_, Term (Var (_, "is_alphanum")), Term c) ->
    eval_char_bool_fn "is_alphanum" Char.is_alphanum ctx tm c
  | CoreApp (_, Term (Var (_, "is_whitespace")), Term c) ->
    eval_char_bool_fn "is_whitespace" Char.is_whitespace ctx tm c
  | Term tm -> Ok (Nominal.subst_all ctx tm)
  | Let (_, _is_rec, tm, Scope (name, body)) ->
    let%bind tm_val = eval_ctx ctx tm in
    eval_ctx (Map.set ctx ~key:name ~data:tm_val) body
  | _ -> Error ("Found a term we can't evaluate", tm)

and eval_ctx'
    : 'a n_term Lvca_util.String.Map.t -> 'a n_term -> ('a n_term, 'a eval_error) Result.t
  =
 fun ctx tm ->
  match tm with
  | Var (_, v) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, Term tm))
  | _ -> Ok tm

and eval_char_bool_fn name f ctx tm c =
  let open Result.Let_syntax in
  let%bind c_result = eval_ctx' ctx c in
  match c_result with
  | Primitive (loc, PrimChar c') -> Ok (if f c' then true_tm loc else false_tm loc)
  | _ -> Error (Printf.sprintf "Invalid argument to %s" name, tm)
;;

let eval : 'a term -> ('a n_term, 'a eval_error) Result.t =
 fun core -> eval_ctx Lvca_util.String.Map.empty core
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Term = Nominal.Parse (Comment)
  module ParsePrimitive = Primitive.Parse (Comment)
  module Abstract = AbstractSyntax.Parse (Comment)
  module Sort = Sort.Parse (Comment)
  open Parsers

  let reserved = Lvca_util.String.Set.of_list [ "let"; "rec"; "in"; "match"; "with" ]

  let identifier =
    identifier
    >>= fun ident -> if Set.mem reserved ident then fail "reserved word" else return ident
  ;;

  let make_apps : 'a term list -> 'a term = function
    | [] -> Lvca_util.invariant_violation "make_apps: must be a nonempty list"
    | [ x ] -> x
    | f :: args ->
      List.fold_left args ~init:f ~f:(fun f_app arg ->
          let pos = OptRange.union (location f_app) (location arg) in
          CoreApp (pos, f_app, arg))
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
        let pattern =
          Term.t ParsePrimitive.t
          >>= fun tm ->
          match Nominal.to_pattern tm with
          | Ok pat -> return pat
          | Error scope ->
            fail ("Unexpected scope: " ^ Nominal.pp_scope_str Primitive.pp scope)
        in
        let pattern = pattern <?> "pattern" in
        let case_line =
          lift3 (fun pat _ tm -> CaseScope (pat, tm)) pattern (string "->") term
          <?> "case line"
        in
        choice
          [ lift4
              (fun (_, lam_loc) ((name, sort), parens_loc) _ body ->
                let loc = OptRange.union lam_loc parens_loc in
                Lambda (loc, sort, Scope (name, body)))
              (attach_pos (char '\\'))
              (attach_pos
                 (parens
                    (lift3 (fun ident _ sort -> ident, sort) identifier (char ':') Sort.t)))
              (string "->")
              term
            <?> "lambda"
          ; lift4
              (fun (_let, let_pos) is_rec name _eq tm _in (body, body_pos) ->
                let loc = OptRange.union let_pos body_pos in
                Let (loc, is_rec, tm, Scope (name, body)))
              (attach_pos (string "let"))
              (option NoRec (Fn.const Rec <$> string "rec"))
              identifier
              (string "=")
            <*> term
            <*> string "in"
            <*> attach_pos term
            <?> "let"
          ; lift4
              (fun (_match, match_pos) tm _with (lines, lines_pos) ->
                let pos = OptRange.union match_pos lines_pos in
                Case (pos, tm, lines))
              (attach_pos (string "match"))
              term
              (string "with")
              (attach_pos
                 (braces (option '|' (char '|') *> sep_by1 (char '|') case_line)))
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
    let one = Nominal.Primitive ((), Primitive.PrimInteger (Z.of_int 1))
    let var name = Term (Nominal.Var ((), name))
    let ignored name = Pattern.Ignored ((), name)
    let operator tag children = Nominal.Operator ((), tag, children)
    let app f a = CoreApp ((), f, a)

    let%test _ = parse "{1}" = Term one
    let%test _ = parse "{true()}" = Term (operator "true" [])
    let%test _ = parse "not x" = app (var "not") (var "x")

    let%test _ =
      parse "let str = string_of_chars chars in {var(str)}"
      = Let
          ( ()
          , NoRec
          , app (var "string_of_chars") (var "chars")
          , Scope
              ("str", Term (operator "var" Nominal.[ Scope ([], [ Var ((), "str") ]) ]))
          )
    ;;

    let%test _ =
      parse {|\(x : bool) -> x|} = Lambda ((), Sort.Name "bool", Scope ("x", var "x"))
    ;;

    let%test _ =
      parse {|match x with { _ -> {1} }|}
      = Case ((), var "x", [ CaseScope (ignored "", Term one) ])
    ;;

    let%test _ =
      parse {|match x with { | _ -> {1} }|}
      = Case ((), var "x", [ CaseScope (ignored "", Term one) ])
    ;;

    let%test _ =
      parse {|match x with { true() -> {false()} | false() -> {true()} }|}
      = Case
          ( ()
          , var "x"
          , [ CaseScope (Operator ((), "true", []), Term (operator "false" []))
            ; CaseScope (Operator ((), "false", []), Term (operator "true" []))
            ] )
    ;;

    let%test _ =
      parse "let x = {true()} in not x"
      = Let
          ( ()
          , NoRec
          , Term (operator "true" [])
          , Scope ("x", CoreApp ((), var "not", var "x")) )
    ;;
  end)
;;
