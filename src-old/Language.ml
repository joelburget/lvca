module Result = Base.Result
module String = Util.String

module type Description = sig
  val abstract_syntax : string
  val concrete_syntax : string
  val statics : string
  val expr_name : string
end

exception InferenceError

module type S = sig
  val abstract_syntax : AbstractSyntax.t
  (* val concrete_syntax : ConcreteSyntaxDescription.t *)
  val statics : Statics.rule list
  val check : Statics.typing -> unit
  val infer : Binding.Nominal.term -> Binding.Nominal.term
  (*
  val parse_concrete : string
    -> ( ConcreteSyntax.formatted_tree
       , (LexerUtil.lex_error, LrParsing.parse_error) Base.Either.t
       ) Base.Result.t
  val to_ast
    :  ConcreteSyntax.formatted_tree
    -> (Binding.Nominal.term, string) Base.Result.t
  val of_ast : Binding.Nominal.term -> ConcreteSyntax.formatted_tree
  *)
end

module Make(X : Description) : S = struct
  let abstract_syntax : AbstractSyntax.t =
    match Parsing.AbstractSyntax.parse X.abstract_syntax with
      | Ok tm -> tm
      | Error err -> failwith (ParseError.to_string err)
  ;;

  (*
  let concrete_syntax =
    let pre_terminal_rules, sort_rules =
      match Parsing.ConcreteSyntax.parse X.concrete_syntax with
        | Ok tm -> tm
        | Error err -> failwith (ParseError.to_string err)
    in
    ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
  ;;
  *)

  let statics = match Parsing.Statics.parse X.statics with
    | Ok statics -> statics
    | Error err -> failwith (ParseError.to_string err)
  ;;

  let bidirectional_env = Bidirectional.
    { rules = statics
    ; var_types = String.Map.empty
    }

  let check : Statics.typing -> unit
    = Bidirectional.check bidirectional_env
  ;;

  (** Infer the type of a term or raise [InferenceError]
   @raise [FreeVar]
   @raise [InferenceError]
   *)
  let infer : Binding.Nominal.term -> Binding.Nominal.term
    = fun tm -> tm
      |> Binding.DeBruijn.from_nominal
      |> Result.ok_or_failwith
      |> Statics.Types.of_de_bruijn
      |> Bidirectional.infer bidirectional_env
      |> Statics.Types.to_de_bruijn_exn
      |> Binding.DeBruijn.to_nominal
      |> Util.Option.get_or_raise InferenceError
  ;;

  (*
  let parse_concrete = ConcreteSyntax.parse concrete_syntax X.expr_name
  ;;

  let to_ast = ConcreteSyntax.to_ast concrete_syntax
  ;;

  let expr_sort = AbstractSyntax.Types.SortAp (X.expr_name, [||])

  let of_ast = ConcreteSyntax.of_ast abstract_syntax.sort_defs
    concrete_syntax expr_sort X.expr_name 80
  ;;
  *)
end
