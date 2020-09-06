open Base
open Lvca_syntax

type 'a term =
  | Operator of 'a * string * 'a scope list
  | Bound of 'a * int * int
  (** Bound vars come via conversion of de Bruijn terms. *)
  | Free of 'a * string
  (** Free vars are used during typechecking. *)
  | Primitive of 'a * Primitive.t

and 'a scope = Scope of 'a Pattern.t list * 'a term list

let rec string_of_term = function
  | Operator (_, name, scopes) ->
    Printf.sprintf "%s(%s)" name
    (scopes |> List.map ~f:string_of_scope |> String.concat ~sep:"; ")
  | Bound (_, i, j) -> Printf.sprintf "%d, %d" i j
  | Free (_, str) -> str
  | Primitive (_, prim) -> Primitive.to_string prim

and string_of_scope (Scope (pats, tms)) =
  let string_of_terms tms = tms
    |> List.map ~f:string_of_term
    |> String.concat ~sep:", "
  in
  match pats with
  | [] -> string_of_terms tms
  | _ ->
    let pats' = pats |> List.map ~f:Pattern.to_string |> String.concat ~sep:". " in
    Printf.sprintf "%s. %s" pats' (string_of_terms tms)
;;

let location = function
  | Operator (loc, _, _)
  | Bound (loc, _, _)
  | Free (loc, _)
  | Primitive (loc, _)
  -> loc

type 'a typing_rule =
  { tm : 'a term
  ; ty : 'a term
  }

type 'a inference_rule = 'a typing_rule
type 'a checking_rule = 'a typing_rule

type 'a typing_clause =
  | InferenceRule of 'a inference_rule
  | CheckingRule of 'a checking_rule

type 'a hypothesis = 'a term Lvca_util.String.Map.t * 'a typing_clause

type 'a rule =
  { hypotheses : 'a hypothesis list
  ; name : string option
  ; conclusion : 'a hypothesis
  }

type 'a typing = Typing of 'a term * 'a term

type 'a t = 'a rule list

let rec erase_term
  = function
    | Operator (_, tag, scopes) -> Operator ((), tag, List.map scopes ~f:erase_scope)
    | Bound (_, i, j) -> Bound ((), i, j)
    | Free (_, name) -> Free ((), name)
    | Primitive (_, prim) -> Primitive ((), prim)

and erase_scope = fun (Scope (pats, tms)) ->
  Scope (List.map pats ~f:Pattern.erase, List.map tms ~f:erase_term)

let erase_typing_rule = fun { tm; ty } ->
  { tm = erase_term tm
  ; ty = erase_term ty
  }

let erase_typing_clause = function
  | InferenceRule rule -> InferenceRule (erase_typing_rule rule)
  | CheckingRule rule -> CheckingRule (erase_typing_rule rule)

let erase_hypothesis = fun (env, clause) ->
  Map.map env ~f:erase_term, erase_typing_clause clause

let erase_rule = fun { hypotheses; name; conclusion } ->
  { hypotheses = List.map hypotheses ~f:erase_hypothesis
  ; name
  ; conclusion = erase_hypothesis conclusion
  }

let erase_typing = fun (Typing (t1, t2)) -> Typing (erase_term t1, erase_term t2)

let erase = List.map ~f:erase_rule

(** Convert a de Bruijn term to a [term]. See also [to_de_bruijn_exn]. *)
let rec of_de_bruijn : 'a Binding.DeBruijn.term -> 'a term = function
  | Operator (loc, tag, scopes)
  -> Operator (loc, tag, List.map scopes ~f:scope_of_de_bruijn)
  | Var (loc, i, j) -> Bound (loc, i, j)
  | Primitive (loc, p) -> Primitive (loc, p)

and scope_of_de_bruijn : 'a Binding.DeBruijn.scope -> 'a scope =
  fun (Scope (pats, body)) -> Scope (pats, List.map body ~f:of_de_bruijn)
;;

exception FreeVar of string

(** Convert a [term] to a de Bruijn representation. See also [of_de_bruijn].
 @raise FreeVar *)
let rec to_de_bruijn_exn : 'a term -> 'a Binding.DeBruijn.term
  = function
    | Operator (loc, name, scopes) -> Operator (loc, name, List.map scopes ~f:to_scope)
    | Bound (loc, i, j) -> Var (loc, i, j)
    | Free (_, name) -> raise (FreeVar name)
    | Primitive (loc, prim) -> Primitive (loc, prim)

and to_scope : 'a scope -> 'a Binding.DeBruijn.scope
  = fun (Scope (pats, tms)) -> Scope (pats, List.map tms ~f:to_de_bruijn_exn)

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk(Comment)
  module Term = Binding.Nominal.Parse(Comment)
  let identifier, char, parens, string = Parsers.(identifier, char, parens, string)

  exception StaticsParseError of string

  type arrow_dir = LeftArr | RightArr

  (* TODO: I don't think this is right -- we never produce bound variables. I'm keeping it
   this way temporarily since it's how menhir parsing worked. *)
  let rec cvt_tm : 'a Binding.Nominal.term -> 'a term
    = function
      | Operator (loc, name, scopes) -> Operator (loc, name, List.map scopes ~f:cvt_scope)
      | Var (loc, name) -> Free (loc, name)
      | Primitive (loc, p) -> Primitive (loc, p)

  and cvt_scope : 'a Binding.Nominal.scope -> 'a scope
    = fun (Scope (pats, tms)) -> Scope (pats, List.map tms ~f:cvt_tm)

  let term : Range.t term Parsers.t
    = cvt_tm <$> Term.t
      <?> "term"

  let typing_clause : Range.t typing_clause Parsers.t
    = lift3
      (fun tm dir ty -> match dir with
        | LeftArr -> CheckingRule { tm; ty }
        | RightArr -> InferenceRule { tm; ty })
      term
      (choice
        [ string "<=" >>| (fun _ -> LeftArr)
        ; string "=>" >>| (fun _ -> RightArr)
        ])
      term
      <?> "typing clause"

  let typed_term : (string * Range.t term) Parsers.t
    = lift3
      (fun ident _ tm -> ident, tm)
      identifier
      (char ':')
      term
      <?> "typed term"

  let context : Range.t term Lvca_util.String.Map.t Parsers.t
    = (string "ctx" *>
      choice
        [ (char ',' >>= fun _ ->
          sep_by1 (char ',') typed_term >>= fun ctx_entries ->
          (match Lvca_util.String.Map.of_alist ctx_entries with
            | `Ok context -> return context
            | `Duplicate_key str
            -> raise (StaticsParseError (Printf.sprintf "duplicate name in context: %s"
            str))))
        ; return Lvca_util.String.Map.empty
        ]) <?> "context"

  let hypothesis : Range.t hypothesis Parsers.t
    = lift3 (fun ctx _ clause -> ctx, clause)
      context
      (string ">>")
      typing_clause
      <?> "hypothesis"

  let line : string option Parsers.t
    = lift3 (fun _ _ ident -> ident)
      (Angstrom.string "--") (* use Angstrom.string to prevent spaces here *)
      (many (char '-'))
      (option None ((fun ident -> Some ident) <$> parens identifier))
      <?> "line"

  let rule : Range.t rule Parsers.t
    = lift3 (fun hypotheses name conclusion -> { hypotheses; name; conclusion })
      (many hypothesis)
      line
      hypothesis
      <?> "typing rule"

  let t : Range.t rule list Parsers.t
    = many rule

  let whitespace_t = whitespace *> t
end

let%test_module "Parsing" = (module struct
  module Parse = Parse(ParseUtil.NoComment)

  let parse_with parser = ParseUtil.parse_string parser

  let (=) = Caml.(=)

  let%test _ = ParseUtil.parse_string Parse.typing_clause "tm => ty"
    |> Result.map ~f:erase_typing_clause
    = Ok (InferenceRule { tm = Free ((), "tm") ; ty = Free ((), "ty") })

  let%test _ =
    match
      ParseUtil.parse_string Parse.hypothesis "ctx >> t1 <= bool()"
    with
      | Error _ -> false
      | Ok (m, rule)
      -> Map.is_empty m &&
         erase_typing_clause rule =
           CheckingRule { tm = Free ((), "t1"); ty = Operator ((), "bool", []) }
end)

(* to_term: *)

  (*
open Binding

let rec term_to_term : term -> Nominal.term
  = fun tm -> match tm with
    | Operator (name, subtms)
    (* We can't translate directly to [Operator (name, ...)] because we need to
       use [Operator] for free and bound vars. *)
    -> Operator (name, List.map subtms ~f:scope_to_term)
    | Bound _ -> Lvca_util.invariant_violation (Printf.sprintf
      "term_to_term: Bound variables are not allowed in typing rules: %s"
      (string_of_term tm)
      )
    | Free name -> Var name
    | Primitive p -> Primitive p

and scope_to_term : scope -> Nominal.scope
  = fun (Scope (pats, tm)) -> Scope (pats, term_to_term tm)

let typing_rule_to_term : typing_rule -> Nominal.term
  = fun { tm; ty } -> Operator ("typing_rule",
    [ Scope ([], term_to_term tm)
    ; Scope ([], term_to_term ty)
    ])

let typing_clause_to_term : typing_clause -> Nominal.term
  = function
    | InferenceRule rule
    -> Operator ("inference_rule", [Scope ([], typing_rule_to_term rule)])
    | CheckingRule rule
    -> Operator ("checking_rule", [Scope ([], typing_rule_to_term rule)])

let hypothesis_to_term : hypothesis -> Nominal.term
  = fun (ctx, typing_clause) -> Operator ("hypothesis",
    [ Scope ([], Sequence (ctx
        |> Map.to_alist
        |> List.map ~f:(fun (name, tm) -> Nominal.Operator ("pair",
          [ Scope ([], Primitive (PrimString name))
          ; Scope ([], term_to_term tm)
          ]))
      ))
    ; Scope ([], typing_clause_to_term typing_clause)
    ])

let term_of_option : ('a -> Nominal.term) -> 'a option -> Nominal.term
  = fun f -> function
    | None -> Operator ("none", [])
    | Some a -> Operator ("some", [Scope ([], f a)])

let rule_to_term : rule -> Nominal.term
  = fun { hypotheses; name; conclusion } -> Operator ("rule",
    [ Scope ([], Sequence (List.map hypotheses ~f:hypothesis_to_term))
    ; Scope ([],
        term_of_option (fun name' -> Primitive (PrimString name')) name)
    ; Scope ([], hypothesis_to_term conclusion)
    ])

let to_term : rule list -> Nominal.term
  = fun rules -> Sequence (List.map rules ~f:rule_to_term)
  *)
