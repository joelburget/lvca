open Base
open Lvca_syntax

type 'info term = ('info, Primitive.t) BindingAwarePattern.t

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

let erase_typing_rule { tm; ty } =
  { tm = BindingAwarePattern.erase tm; ty = BindingAwarePattern.erase ty }
;;

let erase_typing_clause = function
  | InferenceRule rule -> InferenceRule (erase_typing_rule rule)
  | CheckingRule rule -> CheckingRule (erase_typing_rule rule)
;;

let erase_hypothesis (env, clause) =
  Map.map env ~f:BindingAwarePattern.erase, erase_typing_clause clause
;;

let erase_rule { hypotheses; name; conclusion } =
  { hypotheses = List.map hypotheses ~f:erase_hypothesis
  ; name
  ; conclusion = erase_hypothesis conclusion
  }
;;

let erase_typing (Typing (t1, t2)) =
  Typing (BindingAwarePattern.erase t1, BindingAwarePattern.erase t2)
;;

let erase = List.map ~f:erase_rule

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  (* module Term = Nominal.Parse (Comment) *)
  module Pattern = BindingAwarePattern.Parse (Comment)
  module ParsePrimitive = Primitive.Parse (Comment)

  let identifier, char, parens, string = Parsers.(identifier, char, parens, string)

  exception StaticsParseError of string

  type arrow_dir =
    | LeftArr
    | RightArr

  let term : OptRange.t term Parsers.t = Pattern.t ParsePrimitive.t <?> "term"

  let typing_clause : OptRange.t typing_clause Parsers.t =
    lift3
      (fun tm dir ty ->
        match dir with
        | LeftArr -> CheckingRule { tm; ty }
        | RightArr -> InferenceRule { tm; ty })
      term
      (choice [ (string "<=" >>| fun _ -> LeftArr); (string "=>" >>| fun _ -> RightArr) ])
      term
    <?> "typing clause"
  ;;

  let typed_term : (string * OptRange.t term) Parsers.t =
    lift3 (fun ident _ tm -> ident, tm) identifier (char ':') term <?> "typed term"
  ;;

  let context : OptRange.t term Lvca_util.String.Map.t Parsers.t =
    string "ctx"
    *> choice
         [ (char ','
           >>= fun _ ->
           sep_by1 (char ',') typed_term
           >>= fun ctx_entries ->
           match Lvca_util.String.Map.of_alist ctx_entries with
           | `Ok context -> return context
           | `Duplicate_key str ->
             raise
               (StaticsParseError (Printf.sprintf "duplicate name in context: %s" str)))
         ; return Lvca_util.String.Map.empty
         ]
    <?> "context"
  ;;

  let hypothesis : OptRange.t hypothesis Parsers.t =
    lift3 (fun ctx _ clause -> ctx, clause) context (string ">>") typing_clause
    <?> "hypothesis"
  ;;

  let bar =
    Angstrom.(
      lift3
        (fun start result finish -> result, Some Range.{ start; finish })
        pos
        (string "--") (* use Angstrom.string to prevent spaces here *)
        pos)
  ;;

  let line : string option Parsers.t =
    lift3
      (fun _ _ ident -> ident)
      bar
      (many (char '-'))
      (option None ((fun ident -> Some ident) <$> parens identifier))
    <?> "line"
  ;;

  let rule : OptRange.t rule Parsers.t =
    lift3
      (fun hypotheses name conclusion -> { hypotheses; name; conclusion })
      (many hypothesis)
      line
      hypothesis
    <?> "typing rule"
  ;;

  let t : OptRange.t rule list Parsers.t = many rule
  let whitespace_t = ParseUtil.whitespace *> t
end

let%test_module "Parsing" =
  (module struct
    module Parse = Parse (ParseUtil.NoComment)

    let ( = ) = Caml.( = )

    let%test _ =
      ParseUtil.parse_string Parse.typing_clause "tm => ty"
      |> Result.map ~f:erase_typing_clause
      = Ok (InferenceRule { tm = Var ((), "tm"); ty = Var ((), "ty") })
    ;;

    let%test _ =
      match ParseUtil.parse_string Parse.hypothesis "ctx >> t1 <= bool()" with
      | Error _ -> false
      | Ok (m, rule) ->
        Map.is_empty m
        && erase_typing_clause rule
           = CheckingRule { tm = Var ((), "t1"); ty = Operator ((), "bool", []) }
    ;;
  end)
;;
