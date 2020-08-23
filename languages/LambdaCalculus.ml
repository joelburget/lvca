open Base
open Lvca_syntax
module Format = Caml.Format

type 'a term = 'a Binding.Nominal.term

module AngstromParse(Comment : ParseUtil.Angstrom.Comment_int) = struct
  module Parsers = ParseUtil.Angstrom.Mk(Comment)
  let chainl1, whitespace = ParseUtil.Angstrom.(chainl1, whitespace)
  let char, string, integer_lit, parens = Parsers.(char, string, integer_lit, parens)
  let choice, fix, lift3, lift4, pos, return, (>>=), (>>|), (<|>), ( *> ) =
    Angstrom.(choice, fix, lift3, lift4, pos, return, (>>=), (>>|), (<|>), ( *> ))

  let location = Binding.Nominal.location

  let var : Range.t term Angstrom.t
    = Parsers.identifier >>| (fun (name, range) -> Binding.Nominal.Var (range, name))

  (* Precedence
     0: lam (right-associative)
     1: app (left-associative)
   *)

  let t : Range.t term Angstrom.t
    = fix (fun t ->
      let atom = var <|> parens t in

      let lam : Range.t term Angstrom.t
        = pos >>= fun start ->
          lift4 (fun _lam (name, name_range) _arr body -> Binding.Nominal.Operator
          ( (let Range.{ finish; _ } = location body in Range.{ start; finish })
          , "lam"
          , [ Scope ([Pattern.Var (name_range, name)], [body]) ]
          ))
          (char '\\')
          Parsers.identifier
          (string "->")
          t
      in

      let app = return
        (fun x y -> Binding.Nominal.Operator
          ( Range.(location x <> location y)
          , "app"
          , [Scope ([], [x]); Scope ([], [y])]
          ))
      in

      chainl1 (atom <|> lam) app)

  let whitespace_t = whitespace *> t
end
;;

module ParseNoComment = AngstromParse(ParseUtil.Angstrom.NoComment)

let pp : Range.t term Fmt.t =

  let rec pp' prec ppf tm =
    Format.pp_open_stag ppf (Format.String_tag (Binding.Nominal.hash tm));
    Format.pp_open_stag ppf (Range.Stag (Binding.Nominal.location tm));
    begin
      match tm with
        | Binding.Nominal.Operator (_, "app", [Scope ([], [a]); Scope ([], [b])]) ->
          begin
            if prec > 1
            then Fmt.pf ppf "(%a %a)" (pp' 1) a (pp' 2) b
            else Fmt.pf ppf "%a %a" (pp' 1) a (pp' 2) b
          end;
        | Var (_, name) -> Fmt.pf ppf "%s" name
        | Operator (_, "lam", [Scope ([Pattern.Var (_range, name)], [body])]) ->
          begin
            if prec > 0
            then Fmt.pf ppf {|(\%s -> %a)|} name (pp' 0) body
            else Fmt.pf ppf {|\%s -> %a|} name (pp' 0) body
          end;
        | tm ->
          Fmt.failwith "Invalid Lambda term %a" Binding.Nominal.pp_term tm
    end;
    Format.pp_close_stag ppf (); (* range tag *)
    Format.pp_close_stag ppf (); (* hash tag *)

  in pp' 0

let%test_module "Lambda Calculus" = (module struct
  let parse str = Angstrom.parse_string ~consume:All ParseNoComment.whitespace_t str
  let pretty_parse str = match parse str with
    | Error str -> Caml.print_string str
    | Ok tm -> Fmt.pr "%a" pp tm

  let%expect_test _ = pretty_parse "a"; [%expect{| a |}]
  let%expect_test _ = pretty_parse {|\a -> a|}; [%expect{| \a -> a |}]
  let%expect_test _ = pretty_parse {|f g|}; [%expect{| f g |}]
  let%expect_test _ = pretty_parse {|f g x|}; [%expect{| f g x |}]
  let%expect_test _ = pretty_parse {|(f g) (h x)|}; [%expect{| f g (h x) |}]
  let%expect_test _ =
    pretty_parse {|\f -> \g -> \x -> f (g x)|};
    [%expect{| \f -> \g -> \x -> f (g x) |}]
  (* let%expect_test _ = pretty_parse {|\x -> x z \y -> x y|}; [%expect{|\x -> x z \y -> x y|}] *)
end)
;;
