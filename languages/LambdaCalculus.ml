open Base
open Lvca_syntax

type 'a term = 'a Binding.Nominal.term

module AngstromParse(Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk(Comment)
  open Parsers

  let location = Binding.Nominal.location

  let t_var : OptRange.t term Parsers.t
    = Parsers.identifier >>||
      (fun ~pos name -> Binding.Nominal.Var (pos, name), pos)

  let p_var : OptRange.t Pattern.t Parsers.t
    = Parsers.identifier >>|| (fun ~pos name -> Pattern.Var (pos, name), pos)

  (* Precedence
     0: lam (right-associative)
     1: app (left-associative)
   *)

  let t : OptRange.t term Parsers.t
    = fix (fun t ->
      let atom = t_var <|> parens t in

      let lam : OptRange.t term Parsers.t
        = pos >>= fun start ->
          lift4
            (fun _lam var _arr body ->
              let range = OptRange.extend_to (location body) start in
              let tm = Binding.Nominal.Operator
                ( range
                , "lam"
                , [ Scope ([var], [body]) ]
                )
              in tm)
            (char '\\')
            p_var
            (string "->")
            t
      in

      let f (x, rng1) (y, rng2) =
        let range = OptRange.(rng1 <> rng2) in
        let tm = Binding.Nominal.Operator
          ( range
          , "app"
          , [Scope ([], [x]); Scope ([], [y])]
          )
        in
        tm, range
      in

      let atom_or_lam = attach_pos (atom <|> lam) in
      atom_or_lam >>= fun init ->
      many atom_or_lam >>| fun atoms ->
      List.fold atoms ~init ~f |> fst
    )

  let whitespace_t = junk *> t
end
;;

module ParseNoComment = AngstromParse(ParseUtil.NoComment)

let pp : OptRange.t term Fmt.t =

  let rec pp' prec ppf tm =
    let module Format = Caml.Format in
    Format.pp_open_stag ppf (Format.String_tag (Binding.Nominal.hash tm));
    Format.pp_open_stag ppf (OptRange.Stag (Binding.Nominal.location tm));
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
  let () = Caml.Format.set_tags false
  let parse str = Angstrom.parse_string ~consume:All ParseNoComment.whitespace_t str
  let pretty_parse str = match parse str with
    | Error str -> Caml.print_string str
    | Ok (tm, _rng) -> Fmt.pr "%a" pp tm

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
