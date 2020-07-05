open Base
open Lvca
module Format = Caml.Format

let abstract_syntax_str = {|
import { char, string } from "lvca/builtin"
import { term as n_term } from "lvca/term"
import { term as c_term } from "lvca/core"

parsers := parsers(parser()*)

parser :=
  // primitive parsers
  | char(char())
  | string(string())
  | satisfy(char(). c_term())
  | fail(c_term())

  | let(parser(); parser(). parser())

  // combinators
  | option(parser())
  | count(parser(); c_term())
  | many(parser())
  | many1(parser())
  | fix(parser(). parser())

  // alternative
  | alt(parser(); parser())

  // monad
  | return(c_term())

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
  | Char of char
  | String of string
  | Satisfy of string * c_term
  | Fail of c_term

  | Let of string * t * t

  (* combinators *)
  | Option of t
  | Count of t * c_term
  | Many of t
  | Many1 of t
  | Fix of string * t

  (* alternative *)
  | Alt of t * t

  | Return of c_term

  | LiftN of string list * c_term * t list

  | Identifier of string

let rec pp : t Fmt.t (* Format.formatter -> t -> unit *)
  = fun ppf ->
    let core = Core.pp in
    let pf = Fmt.pf in
    function
    | Char char -> pf ppf "'%c'" char
    | String str -> pf ppf {|"%s"|} str
    | Satisfy (name, tm) -> pf ppf "satisfy (\\%s. %a)" name core tm
    | Let (name, named, body) -> pf ppf "let %s = %a in %a" name pp named pp body
    | Fail tm -> pf ppf "fail %a" core tm
    | Option t -> pf ppf "%a?" pp t
    | Count (p, t) -> pf ppf "%a{%a}" pp p core t
    | Many t -> pf ppf "%a*" pp t
    | Many1 t -> pf ppf "%a+" pp t
    | Fix (name, t) -> pf ppf "fix (%s -> %a)" name pp t
    | Alt (t1, t2) -> pf ppf "alt %a %a" pp t1 pp t2
    | Return tm -> pf ppf "return %a" core tm
    | LiftN (names, p, ps) -> pf ppf "lift (\\%a. %a) [%a]"
      Fmt.(list ~sep:(any ".@ ") string) names
      core p
      Fmt.(list ~sep:comma pp) ps
    | Identifier name -> pf ppf "%s" name

module Parse(Comment : Util.Angstrom.Comment_int) = struct
  module Parsers = Util.Angstrom.Mk(Comment)

  let char, char_lit, string_lit, parens, string =
    Parsers.(char, char_lit, string_lit, parens, string)
  let choice, fail, fix, lift2, lift3, many1, option, return, ( <* ), ( *> ), (<$>),
    (<?>), (>>=), (>>|) =
    Angstrom.(choice, fail, fix, lift2, lift3, many1, option, return, ( <* ), ( *> ),
      (<$>), (<?>), (>>=), (>>|))

  let keywords : string list = [ "satisfy"; "let"; "in"; "fail" ]

  let keyword : string Angstrom.t
    = keywords |> List.map ~f:string |> choice

  let operators : string list = [ "?"; "*"; "+"; "|"; "=" ]

  let operator : string Angstrom.t
    = operators |> List.map ~f:string |> choice

  let t : c_term Angstrom.t -> t Angstrom.t
    = fun term -> fix (fun parser ->
      let parse_token = choice
        [ (fun c -> Char c) <$> char_lit
        ; (fun s -> String s) <$> string_lit
        ; parens parser
        ; (fun name -> Identifier name) <$> Parsers.identifier
        ]
      in

      choice
        [ string "let" *> (lift3
          (fun name bound body -> Let (name, bound, body))
          Parsers.identifier
          (string "=" *> parser)
          (string "in" *> parser))
        ; string "satisfy" *>
          (parens
            (lift3
              (fun name _arr tm -> Satisfy (name, tm))
              Parsers.identifier
              (string "->")
              term))
        ; string "fail" *> term >>| (fun tm -> Fail tm)
        ; string "fix" *> (parens
          (lift3
            (fun name _arr tm -> Fix (name, tm))
            Parsers.identifier
            (string "->")
            parser)
          )
        (* TODO: sequence *)
        (* ; string "sequence" *)
        ; parse_token >>= fun tok -> option tok (choice
          [ char '?' >>| (fun _ -> Option tok)
          ; char '*' >>| (fun _ -> Many tok)
          ; char '+' >>| (fun _ -> Many1 tok)
          ; char '|' *> (parser >>| fun rhs -> Alt (tok, rhs))
          ])
        ]

    ) <?> "parser"
end;;

let scope_list : n_term list -> Binding.Nominal.scope list
  = List.map ~f:(fun tm -> Binding.Nominal.Scope ([], tm))

let mk_list : n_term list -> n_term
  = fun lst -> Binding.Nominal.Operator ("list", scope_list lst)

let mk_some : n_term -> n_term
  = fun tm -> Binding.Nominal.Operator ("some", [Scope ([], tm)])

type ctx_entry =
  | BoundChar of char
  | BoundParser of t

(* TODO: this is hacky *)
let thin_ctx : ctx_entry Util.String.Map.t -> n_term Util.String.Map.t
  = Map.filter_map ~f:(function
    | BoundChar c -> Some (Binding.Nominal.Primitive (PrimChar c))
    | BoundParser _ -> None
  )

(* Translate our parser type into an angstrom parser *)
let translate : t -> n_term Angstrom.t
  = let open Angstrom in
    let mk_err () = failwith "TODO: error" in

    let rec translate' ctx = function
    | Char c ->
      char c >>| (fun c -> Binding.Nominal.Primitive (PrimChar c))
        <?> (Printf.sprintf {|char '%s'|} (String.make 1 c))
    | String str ->
      string str >>| (fun str -> Binding.Nominal.Primitive (PrimString str))
        <?> (Printf.sprintf {|string "%s"|} str)
    | Satisfy (name, tm) ->
      let f c =
        let ctx' = Map.set ctx ~key:name ~data:(BoundChar c) in
        match Core.eval_ctx_exn (thin_ctx ctx') tm with
          | Operator ("true", []) -> true
          | _ -> false
      in
      satisfy f >>| fun c -> Binding.Nominal.Primitive (PrimChar c)
    | Let (name, named, body) ->
      let ctx' = Map.set ctx ~key:name ~data:(BoundParser named) in
      translate' ctx' body <?> name
    | Fail tm -> (match Core.eval_ctx_exn (thin_ctx ctx) tm with
      | Primitive (PrimString msg) -> fail msg
      | _ -> mk_err ())
    | Option p -> option None ((fun tm -> Some tm) <$> translate' ctx p) >>| (function
      | Some tm -> mk_some tm
      | None -> Operator ("none", []))
    | Count (p, n_tm) ->
      let n = match Core.eval_ctx_exn (thin_ctx ctx) n_tm with
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
    | LiftN (_names, _p, _ps) -> failwith "TODO"
    | Identifier name -> (match Map.find ctx name with
      | Some (BoundParser p) -> translate' ctx p <?> name
      | None
      | Some (BoundChar _) -> mk_err ()
      )
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
    parse' {|"str"|} "str";
    [%expect{| "str" |}]

  let%expect_test _ =
    parse' {|"str"|} "foo";
    [%expect{| failed to parse: string "str": string |}]

  let%expect_test _ =
    parse' {|"str"*|} "strstrstr";
    [%expect{| list("str"; "str"; "str") |}]

  let%expect_test _ =
    parse' {|"str"+|} "strstrstr";
    [%expect{| list("str"; "str"; "str") |}]

  let%expect_test _ =
    parse' {|"str" | "foo"|} "str";
    [%expect{| "str" |}]

  let%expect_test _ =
    parse' {|"str" | "foo"|} "foo";
    [%expect{| "foo" |}]

  let sat_parser = {|satisfy (x -> match x with {
      | 'c' -> {true()}
      | _ -> {false()}
    })
    |}

  let%expect_test _ = parse' sat_parser "c"; [%expect{| 'c' |}]
  let%expect_test _ = parse' sat_parser "d"; [%expect{| failed to parse: : satisfy: 'd' |}]

  let%expect_test _ =
    parse' {|let x = "str" in x|} "str";
    [%expect{| "str" |}]

  let%expect_test _ =
    parse' {|fail {"reason"}|} "str";
    (* TODO: nicer formatting *)
    [%expect{| failed to parse: : reason |}]
end);;
