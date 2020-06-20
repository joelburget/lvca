open Base
open Lvca
module Format = Caml.Format

let abstract_syntax_str = {|
import { char } from "lvca/builtin"
import { term as n_term } from "lvca/term"
import { term as c_term } from "lvca/core"

parsers := parsers(parser()*)

parser :=
  // primitive parsers
  | char(c_term())
  | string(c_term())
  | satisfy(char(). c_term())
  | named(c_term(); parser())
  | fail(c_term())

  // combinators
  | option(parser())
  | count(c_term(); parser())
  | many(parser())
  | many1(parser())
  | fix(parser(). parser())

  // alternative
  | alt(parser(); parser())

  // monad
  | return(c_term())

  | left(parser(); parser())
  | right(parser(); parser())
  // is sequence a better name?
  | lift_n(n_term()*. c_term(); parsers())
|};;

module ParseAbstract = AbstractSyntax.Parse(Util.Angstrom.CComment)

let abstract_syntax : AbstractSyntax.t = abstract_syntax_str
  |> Angstrom.parse_string ~consume:All
    Angstrom.(Util.Angstrom.whitespace *> ParseAbstract.t)
  |> Result.ok_or_failwith

type c_term = Core.term
type n_term = Binding.Nominal.term

type t =
  (* primitive parsers *)
  | Char of c_term
  | String of c_term
  | Satisfy of string * c_term
  | Named of c_term * t
  | Fail of c_term

  (* combinators *)
  | Option of t
  | Count of c_term * t
  | Many of t
  | Many1 of t
  | Fix of string * t

  (* alternative *)
  | Alt of t * t

  | Return of c_term

  (* There's no need for ap (<*>), since lift_n fills the role of sequencing effects *)
  | Left of t * t
  | Right of t * t

  | LiftN of string list * c_term * t list

let rec pp : t Fmt.t (* Format.formatter -> t -> unit *)
  = fun ppf ->
    let term = Core.pp in
    let pf = Fmt.pf in
    function
    | Char tm -> pf ppf "char %a" term tm
    | String tm -> pf ppf "string %a" term tm
    | Satisfy (name, tm) -> pf ppf "satisfy (\\%s. %a)" name term tm
    | Named (tm, t) -> pf ppf "named %a %a" term tm pp t
    | Fail tm -> pf ppf "fail %a" term tm
    | Option t -> pf ppf "option %a" pp t
    | Count (t, p) -> pf ppf "count %a %a" term t pp p
    | Many t -> pf ppf "many %a" pp t
    | Many1 t -> pf ppf "many1 %a" pp t
    | Fix (name, t) -> pf ppf "fix (%s -> %a)" name pp t
    | Alt (t1, t2) -> pf ppf "alt %a %a" pp t1 pp t2
    | Return tm -> pf ppf "return %a" term tm
    | Left (t1, t2) -> pf ppf "%a <* %a" pp t1 pp t2
    | Right (t1, t2) -> pf ppf "%a *> %a" pp t1 pp t2
    | LiftN (names, p, ps) -> pf ppf "lift (\\%a. %a) [%a]"
      Fmt.(list ~sep:(any ".@ ") string) names
      term p
      Fmt.(list ~sep:comma pp) ps

module Parse(Comment : Util.Angstrom.Comment_int) = struct
  module Parsers = Util.Angstrom.Mk(Comment)

  let char_lit, parens, string = Parsers.(char_lit, parens, string)
  let choice, fix, lift3, many1, return, ( *> ), (<$>), (<?>), (>>=) =
    Angstrom.(choice, fix, lift3, many1, return, ( *> ), (<$>), (<?>), (>>=))

  (* Note: "many1" must occur before "many", etc.
   * Eventually I would like to move to a representation without this limitation.
   *)
  let keywords : string list =
    [ "char"; "string"; "satisfy"; "option"; "many1"; "many"; "alt";
      "named"; "fail"; "return" ]

  let keyword : string Angstrom.t
    = keywords |> List.map ~f:string |> choice

  let operators : string list = [ "<*"; "*>" ]

  let operator : string Angstrom.t
    = operators |> List.map ~f:string |> choice

  type atom =
    | Keyword of string
    | Name of string
    | Operator of string
    | Parser of t
    | Term of c_term

  let pp_atom : atom Fmt.t (* Format.formatter -> atom -> unit*)
    = fun ppf ->
      let pf = Fmt.pf in
      function
      | Keyword str -> pf ppf "%s" str
      | Name str -> pf ppf "%s" str
      | Operator str -> pf ppf "%s" str
      | Parser p -> pf ppf "(%a)" pp p
      | Term tm -> Core.pp ppf tm

  let t : c_term Angstrom.t -> t Angstrom.t
    = fun term -> fix (fun parser ->

      let atomic_t = choice
        [ (fun k -> Keyword k) <$> keyword
        ; (fun n -> Name n) <$> Parsers.identifier
        ; (fun o -> Operator o) <$> operator
        ; (fun p -> Parser p) <$> parens parser
        ; (fun t -> Term t) <$> term
        ]
      in

      many1 atomic_t >>= fun atoms -> match atoms with
        | [ Keyword "char"; Term tm ] -> return (Char tm)
        | [ Keyword "string"; Term tm ] -> return (String tm)
        (* TODO: punctuation *)
        | [ Keyword "satisfy"; Name name; Term tm ] -> return (Satisfy (name, tm))
        | [ Keyword "named"; Term tm; Parser p ] -> return (Named (tm, p))
        | [ Keyword "fail"; Term tm ] -> return (Fail tm)
        | [ Keyword "option"; Parser p ] -> return (Option p)
        (* TODO: count *)
        | [ Keyword "many"; Parser p ] -> return (Many p)
        | [ Keyword "many1"; Parser p ] -> return (Many1 p)
        (* TODO: fix *)
        | [ Keyword "alt"; Parser p1; Parser p2 ] -> return (Alt (p1, p2))
        (* TODO: choice *)
        | [ Keyword "return"; Term tm ] -> return (Return tm)
        | [ Parser p1; Operator "<*"; Parser p2 ] -> return (Left (p1, p2))
        | [ Parser p1; Operator "*>"; Parser p2 ] -> return (Right (p1, p2))
        (* TODO: liftn *)
        | _ -> Angstrom.fail (Fmt.str "%a" (Fmt.list pp_atom ~sep:Fmt.sp) atoms)

    ) <?> "parser"
end;;

let scope_list : n_term list -> Binding.Nominal.scope list
  = List.map ~f:(fun tm -> Binding.Nominal.Scope ([], tm))

let mk_list : n_term list -> n_term
  = fun lst -> Binding.Nominal.Operator ("list", scope_list lst)

let mk_some : n_term -> n_term
  = fun tm -> Binding.Nominal.Operator ("some", [Scope ([], tm)])

(* Translate our parser type into an angstrom parser *)
let translate : t -> n_term Angstrom.t
  = let open Angstrom in
    let mk_err () = failwith "TODO: error" in

    let rec translate' ctx = function
    | Char tm ->
      let c = match Core.eval_ctx_exn ctx tm with
        | Primitive (PrimChar c) -> c
        | _ -> mk_err ()
      in
      char c >>| (fun c -> Binding.Nominal.Primitive (PrimChar c))
        <?> (Printf.sprintf {|char '%s'|} (String.make 1 c))
    | String tm ->
      let str = match Core.eval_ctx_exn ctx tm with
        | Primitive (PrimString str) -> str
        | _ -> mk_err ()
      in
      string str >>| (fun str -> Binding.Nominal.Primitive (PrimString str))
        <?> (Printf.sprintf {|string "%s"|} str)
    | Satisfy (name, tm) ->
      let f c =
        let ctx' = Map.set ctx ~key:name ~data:(Primitive (PrimChar c)) in
        match Core.eval_ctx_exn ctx' tm with
          | Operator ("true", []) -> true
          | _ -> false
      in
      satisfy f >>| fun c -> Binding.Nominal.Primitive (PrimChar c)
    | Named (tm, p) -> (match Core.eval_ctx_exn ctx tm with
      | Primitive (PrimString name) -> translate' ctx p <?> name
      | _ -> mk_err ())
    | Fail tm -> (match Core.eval_ctx_exn ctx tm with
      | Primitive (PrimString msg) -> fail msg
      | _ -> mk_err ())
    | Option p -> option None ((fun tm -> Some tm) <$> translate' ctx p) >>| (function
      | Some tm -> mk_some tm
      | None -> Operator ("none", []))
    | Count (n_tm, p) ->
      let n = match Core.eval_ctx_exn ctx n_tm with
        | Primitive (PrimInteger i) -> (match Bigint.to_int i with
          | Some n -> n
          | None -> mk_err ()
        )
        | _ -> mk_err ()
      in
      count n (translate' ctx p) >>| mk_list
    | Many t -> many (translate' ctx t) >>| mk_list
      <?> "many"
    | Many1 t -> many1 (translate' ctx t) >>| mk_list
      <?> "many1"
    (* TODO: do we even want explicit fix? or should this be done implicitly? *)
    (* | Fix (name, p) -> fix (fun p' -> translate' (Map.set ctx ~key:name ~data:p') p) *)
    | Fix _ -> failwith "TODO"
    | Alt (p1, p2) -> translate' ctx p1 <|> translate' ctx p2
      <?> "alt"
    | Return tm -> (match tm with
      | Term tm -> return tm
      | _ -> mk_err ())
    | Left (p1, p2) -> translate' ctx p1 <* translate' ctx p2
    | Right (p1, p2) -> translate' ctx p1 *> translate' ctx p2
    | LiftN (_names, _p, _ps) -> failwith "TODO"
    in

    translate' Util.String.Map.empty

let parse : t -> string -> (n_term, string) Result.t
  = fun parser -> Angstrom.parse_string ~consume:All (translate parser)

let%test_module "Parsing" = (module struct
  (* module ParseTerm = Binding.Nominal.Parse(Util.Angstrom.CComment) *)
  module ParseCore = Core.Parse(Util.Angstrom.CComment)
  module ParseParser = Parse(Util.Angstrom.CComment)

  let parse' : string -> string -> unit
    = fun parser_str str ->
      match
        Angstrom.parse_string ~consume:All (ParseParser.t ParseCore.term) parser_str
      with
        | Error msg -> Caml.print_string ("failed to parse parser desc: " ^ msg)
        | Ok parser -> (match parse parser str with
          | Error msg -> Caml.print_string ("failed to parse: " ^ msg)
          | Ok tm -> Binding.Nominal.pp_term Caml.Format.std_formatter tm)

  let%expect_test _ =
    parse' {|string {"str"}|} "str";
    [%expect{| "str" |}]

  let%expect_test _ =
    parse' {|string {"str"}|} "foo";
    [%expect{| failed to parse: string "str": string |}]

  let%expect_test _ =
    parse' {|many1 (string {"str"})|} "strstrstr";
    [%expect{| list("str"; "str"; "str") |}]

  let%expect_test _ =
    (* TODO: proper precedence: remove parens *)
    parse' {|(string {"str"}) *> (string {"foo"})|} "strfoo";
    [%expect{| "foo" |}]

  let%expect_test _ =
    (* TODO: proper precedence: remove parens *)
    parse' {|(string {"str"}) <* (string {"foo"})|} "strfoo";
    [%expect{| "str" |}]

    (* TODO: proper syntax for binding *)
  let sat_parser = {|satisfy x (match x with {
      | 'c' -> {true()}
      | _ -> {false()}
    })
    |}
  let%expect_test _ = parse' sat_parser "c"; [%expect{| 'c' |}]
  let%expect_test _ = parse' sat_parser "d"; [%expect{| failed to parse: : satisfy: 'd' |}]
end);;
