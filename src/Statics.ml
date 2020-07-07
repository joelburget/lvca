open Base

type term =
  | Operator of string * scope list
  | Bound of int * int
  (** Bound vars come via conversion of de Bruijn terms. *)
  | Free of string
  (** Free vars are used during typechecking. *)
  | Primitive of Primitive.t

and scope = Scope of Pattern.t list * term list

let rec string_of_term = function
  | Operator (name, scopes) ->
    Printf.sprintf "%s(%s)" name
    (scopes |> List.map ~f:string_of_scope |> String.concat ~sep:"; ")
  | Bound (i, j) -> Printf.sprintf "%d, %d" i j
  | Free str -> str
  | Primitive prim -> Primitive.to_string prim

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

type typing_rule =
  { tm : term
  ; ty : term
  }

type inference_rule = typing_rule
type checking_rule = typing_rule

type typing_clause =
  | InferenceRule of inference_rule
  | CheckingRule of checking_rule

type hypothesis = term Util.String.Map.t * typing_clause

type rule =
  { hypotheses : hypothesis list
  ; name : string option
  ; conclusion : hypothesis
  }

type typing = Typing of term * term

type t = rule list

(** Convert a de Bruijn term to a [term]. See also [to_de_bruijn_exn]. *)
let rec of_de_bruijn : Binding.DeBruijn.term -> term = function
  | Operator (tag, scopes) -> Operator (tag, List.map scopes ~f:scope_of_de_bruijn)
  | Var (i, j) -> Bound (i, j)
  | Primitive p -> Primitive p

and scope_of_de_bruijn : Binding.DeBruijn.scope -> scope =
  fun (Scope (pats, body)) -> Scope (pats, List.map body ~f:of_de_bruijn)
;;

exception FreeVar of string

(** Convert a [term] to a de Bruijn representation. See also [of_de_bruijn].
 @raise FreeVar *)
let rec to_de_bruijn_exn : term -> Binding.DeBruijn.term
  = function
    | Operator (name, scopes) -> Operator (name, List.map scopes ~f:to_scope)
    | Bound (i, j) -> Var (i, j)
    | Free name -> raise (FreeVar name)
    | Primitive prim -> Primitive prim

and to_scope : scope -> Binding.DeBruijn.scope
  = fun (Scope (pats, tms)) -> Scope (pats, List.map tms ~f:to_de_bruijn_exn)

module Parse (Comment : Util.Angstrom.Comment_int) = struct
  open Angstrom
  module Parsers = Util.Angstrom.Mk(Comment)
  module Term = Binding.Nominal.Parse(Comment)
  let identifier, char, parens, string = Parsers.(identifier, char, parens, string)

  exception StaticsParseError of string

  type arrow_dir = LeftArr | RightArr

  (* TODO: I don't think this is right -- we never produce bound variables. I'm keeping it
   this way temporarily since it's how menhir parsing worked. *)
  let rec cvt_tm : Binding.Nominal.term -> term
    = function
      | Operator (name, scopes) -> Operator (name, List.map scopes ~f:cvt_scope)
      | Var name -> Free name
      | Primitive p -> Primitive p

  and cvt_scope : Binding.Nominal.scope -> scope
    = fun (Scope (pats, tms)) -> Scope (pats, List.map tms ~f:cvt_tm)

  let term : term Angstrom.t
    = cvt_tm <$> Term.t
      <?> "term"

  let typing_clause : typing_clause Angstrom.t
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

  let typed_term : (string * term) Angstrom.t
    = lift3
      (fun ident _ tm -> ident, tm)
      identifier
      (char ':')
      term
      <?> "typed term"

  let context : term Util.String.Map.t Angstrom.t
    = (string "ctx" *>
      choice
        [ (char ',' >>= fun _ ->
          sep_by1 (char ',') typed_term >>= fun ctx_entries ->
          (match Util.String.Map.of_alist ctx_entries with
            | `Ok context -> return context
            | `Duplicate_key str
            -> raise (StaticsParseError (Printf.sprintf "duplicate name in context: %s"
            str))))
        ; return Util.String.Map.empty
        ]) <?> "context"

  let hypothesis : hypothesis Angstrom.t
    = lift3 (fun ctx _ clause -> ctx, clause)
      context
      (string ">>")
      typing_clause
      <?> "hypothesis"

  let line : string option Angstrom.t
    = lift3 (fun _ _ ident -> ident)
      (Angstrom.string "--") (* use Angstrom.string to prevent spaces here *)
      (many (char '-'))
      (option None ((fun ident -> Some ident) <$> parens identifier))
      <?> "line"

  let rule : rule Angstrom.t
    = lift3 (fun hypotheses name conclusion -> { hypotheses; name; conclusion })
      (many hypothesis)
      line
      hypothesis
      <?> "typing rule"

  let t : rule list Angstrom.t
    = many rule
end

let%test_module "Parsing" = (module struct
  module Parse = Parse(Util.Angstrom.NoComment)

  let parse_with parser = Angstrom.parse_string ~consume:All parser

  let (=) = Caml.(=)

  let%test _ = parse_with Parse.typing_clause "tm => ty" =
    Ok (InferenceRule { tm = Free "tm" ; ty = Free "ty" })

  let%test _ =
    match
      parse_with Parse.hypothesis "ctx >> t1 <= bool()"
    with
      | Error _ -> false
      | Ok (m, rule)
      -> Map.is_empty m &&
         rule = CheckingRule { tm = Free "t1"; ty = Operator ("bool", []) }
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
    | Bound _ -> Util.invariant_violation (Printf.sprintf
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
