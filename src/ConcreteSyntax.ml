module Result = Belt.Result
module BL = Belt.List
module BA = Belt.Array
module Nominal = Binding.Nominal
open Types
open Types.ConcreteSyntaxDescription
module AA = Util.ArrayApplicative(struct type t = string end)
open AA
module MS = Belt.Map.String
module MI = Belt.Map.Int
let (find, traverse_list_result) = Util.(find, traverse_list_result)

type prim_ty =
  | Integer
  | String

type node_type =
  | Operator  of string
  | Var
  | Sequence
  | Primitive of prim_ty

type terminal_capture =
  { content         : string;
    leading_trivia  : string;
    trailing_trivia : string;
  }

type nonterminal_capture = tree

(* Inspired by:
 * - https://github.com/apple/swift/tree/master/lib/Syntax
 * - https://github.com/dotnet/roslyn/wiki/Roslyn-Overview#syntax-trees
 *
 * Rules of trivia (same as for swift):
 * - A token owns all of its trailing trivia up to, but not including, the
 *   next newline character.
 * - Looking backward in the text, a token owns all of the leading trivia up
 *   to and including the first newline character.
 *
 * In other words, a contiguous stretch of trivia between two tokens is split
 * on the leftmost newline.
 *)
and tree =
  { sort            : sort;
    node_type       : node_type;
    leading_trivia  : string;
    trailing_trivia : string;
    children        : (terminal_capture, nonterminal_capture) Either.t array;
  }

(* tree equality mod trivia *)
let rec equivalent t1 t2 =
  t1.sort = t2.sort &&
  t1.node_type = t2.node_type &&
  BA.(every (zipBy t1.children t2.children equivalent')) (fun b -> b)

and equivalent' child1 child2 = match child1, child2 with
  | Left   tc1, Left   tc2 -> tc1 = tc2
  | Right ntc1, Right ntc2 -> equivalent ntc1 ntc2
  |          _,          _ -> false

let find_operator_match
  (matches: operator_match list list)
  (opname : string)
  : operator_match
  = let maybe_match = find
      (* TODO now need to match *)
      (fun (OperatorMatch { term_pattern }) -> match term_pattern with
        | TermPattern (opname', _) -> opname' = opname
        | ParenthesizingPattern _  -> false
      )
      (BL.flatten matches)
    in
    match maybe_match with
      | Some m -> m
      | None -> failwith "TODO: default match"

type subterm_result =
  | NotFound
  | FoundCapture
  | FoundTerm   of int * Nominal.term
  | FoundBinder of string

(* Find a subterm or binder given a term pattern template and the index of the
 * subterm / binder we're looking for *)
let rec find_subtm' slot_num token_ix scopes term_pattern
  = match scopes, term_pattern with
    | _, [] -> NotFound
    | Nominal.Scope (binders, body) :: scopes', NumberedScopePattern (binder_nums, body_num) :: pattern_scopes
    ->
      let binder_matches = binders
        |. BL.zip binder_nums
        |> find (fun (_, num) -> num = token_ix)
      in
      (match binder_matches with
      | Some (name, _ix) -> FoundBinder name
      | None -> if token_ix = body_num
        then FoundTerm (slot_num, body)
        else find_subtm' (slot_num + 1) token_ix scopes' pattern_scopes
      )
    | _, _ -> failwith "invariant violation: mismatched scopes / term patterns"

let find_subtm
  : int -> Nominal.scope list -> numbered_scope_pattern list -> subterm_result
  = find_subtm' 0

(** The current term and sort don't match *)
exception BadSortTerm of sort * Nominal.term

exception BadRules of string

(** raised from of_ast when we need to emit a token but don't have a capture,
 * and the terminal match is a regex, not a string literal. This could actually
 * be a form of BadRules *)
exception CantEmitTokenRegex of string * regex

let regex_is_literal : regex -> string option = function
  | [ReString str] -> Some str
  | _              -> None

(* TODO: do we need to insert \b? *)
(* Escape special characters *)
let rec regex_piece_to_string : regex_piece -> string = function
  | ReString str   -> Js.String.(str
    |> replaceByRe [%re "/\\+/g"] "\\+"
    |> replaceByRe [%re "/\\*/g"] "\\*"
    |> replaceByRe [%re "/\\?/g"] "\\?"
    |> replaceByRe [%re "/\\-/g"] "\\-"
    |> replaceByRe [%re "/\\(/g"] "\\("
    |> replaceByRe [%re "/\\)/g"] "\\)"
  )
  | ReSet    str   -> "[" ^ str ^ "]"
  | ReStar   piece -> regex_piece_to_string piece ^ "*"
  | RePlus   piece -> regex_piece_to_string piece ^ "+"
  | ReOption piece -> regex_piece_to_string piece ^ "?"

let regex_to_string : regex -> string = fun re_parts -> re_parts
  |> List.map regex_piece_to_string
  |> String.concat ""

let mk_tree sort node_type children =
  { sort;
    node_type;
    leading_trivia  = "";
    trailing_trivia = "";
    children;
  }

(* Helper for use in of_ast *)
let mk_left content =
  Either.Left { leading_trivia = ""; content; trailing_trivia = "" }

(* TODO: handle non-happy cases *)
let rec of_ast
  (Language sorts as lang)
  ({ terminal_rules; sort_rules } as rules)
  current_sort
  tm
  = match current_sort, tm with
  | SortAp (sort_name, _), Nominal.Operator (op_name, scopes) ->
    let SortRule { operator_rules } = M.getExn sort_rules sort_name in
    (* TODO: remove possible exception. possible to have var-only sort? *)
    let OperatorMatch { tokens; term_pattern } (* TODO: variable rule? *)
      = find_operator_match operator_rules op_name
    in

    let find_subtm' = fun ix -> match term_pattern with
      | ParenthesizingPattern _
      -> FoundCapture

      | TermPattern (_term_name, numbered_scope_patterns)
      -> find_subtm ix scopes numbered_scope_patterns
    in

    (* Map each token to a subtree. For each token:
      * if it's a space, ignore it
      * if it's a terminal, print it
      * if it's a nonterminal, look up the subterm (by token number)
    *)
    let children = BL.(tokens
      |. mapWithIndex (fun token_ix token -> token_ix, token)
      |. keep (fun (_, tok) -> match tok with
        | Underscore -> false
        | _          -> true
        )
      |. toArray
      )
      |. BA.map (fun (token_ix, token) ->

      let token_ix' = token_ix + 1 in (* switch from 0- to 1-based indexing *)
      match find_subtm' token_ix', token with

        | FoundCapture, NonterminalName _sort
        -> assert false

        | FoundCapture, TerminalName name
        -> raise (BadRules ("capture found, terminal name: " ^ name))

        | FoundTerm (tm_ix, subtm), NonterminalName sort
        -> let SortDef (_, operator_defs) = M.getExn sorts sort in
           let some_operator = find
             (fun (OperatorDef (op_name', _)) -> op_name' = op_name)
             operator_defs
           in
           let valences = match some_operator with
             | Some (OperatorDef (_, Arity (_, valences))) -> valences
             | None -> assert false
           in
           let valence = BL.getExn valences tm_ix in
           let new_sort = (match valence with
             | FixedValence    (_, new_sort)
             | VariableValence (_, new_sort)
             -> new_sort
           ) in
           Either.Right (of_ast lang rules new_sort subtm)

        | FoundTerm (_tm_ix, subtm), TerminalName _name
        -> Right (of_ast lang rules current_sort subtm)

        | FoundBinder binder_name, TerminalName _name
        -> mk_left binder_name

        (* if the current token is a terminal, and we didn't capture a binder
         * or term, we just emit the contents of the token *)
        | NotFound, TerminalName name
        -> let terminal_rule = M.getExn terminal_rules name in
           (match regex_is_literal terminal_rule with
             | Some re_str -> mk_left re_str
             | None -> raise (CantEmitTokenRegex (name, terminal_rule))
           )

        (* if the current token is naming a nonterminal, there has to be a
         * subterm *)
        | FoundBinder binder_name, NonterminalName name
        -> raise (BadRules
          ("binder (" ^ binder_name ^ ") found, nonterminal name: " ^ name))
        | NotFound, NonterminalName name
        -> raise (BadRules ("subterm not found, nonterminal name: " ^ name))

        (* need to attach trivia to node *)
        | _, Underscore -> assert false
    ) in

    mk_tree current_sort (Operator op_name) children

  | _, Nominal.Var name
  -> mk_tree current_sort Var [| mk_left name |]

  | SortAp ("sequence", [|sort|]), Nominal.Sequence tms ->
    let children = tms
      |> List.map (fun tm -> Either.Right (of_ast lang rules sort tm))
      |> BL.toArray
    in
    mk_tree current_sort Sequence children

  | SortAp ("string", [||]), Nominal.Primitive (PrimString str) ->
    mk_tree current_sort (Primitive String) [| mk_left str |]

  | SortAp ("integer", [||]), Nominal.Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    mk_tree current_sort (Primitive Integer) [| mk_left str |]

  | _, _ -> raise (BadSortTerm (current_sort, tm))

let rec to_string { leading_trivia; children; trailing_trivia } =
  let children_str = children
    |> Array.map
      (function
      | Either.Left { leading_trivia; content; trailing_trivia }
      -> leading_trivia ^ content ^ trailing_trivia
      | Right nonterminal_capture    -> to_string nonterminal_capture
      )
    |> BL.fromArray
    |> String.concat ""
  in leading_trivia ^ children_str ^ trailing_trivia

(* Convert a concrete tree to an AST. We ignore trivia. *)
let rec to_ast lang { node_type; children; }
  : (Nominal.term, string) Result.t
  = match node_type, children with
    | Var, [| Left { content = name } |] -> Result.Ok (Nominal.Var name)
    | Sequence, _ -> Result.map
      (traverse_array_result
        (function
          | Either.Left _ -> Error "TODO: message"
          | Right child   -> to_ast lang child
        )
        children)
      (fun children' -> Nominal.Sequence (BL.fromArray children'))
    | Primitive prim_ty, [| Left { content = str } |] -> (match prim_ty with
      | String  -> Ok (Nominal.Primitive (PrimString str))
      | Integer ->
        try
          Ok (Nominal.Primitive (PrimInteger (Bigint.of_string str)))
        with
          _ -> Error "failed to read integer literal"
      )

    (* TODO: check validity *)
    | Operator op_name, _ ->
      let children' =
        BA.keepMap children (function
          | Left _      -> None
          | Right child -> Some (scope_to_ast lang child)
        )
        |> sequence_array_result
      in Result.map children'
           (fun children'' -> Nominal.Operator (op_name, BL.fromArray children''))

    | Primitive _, _
    | Var        , _
    -> assert false

and scope_to_ast lang ({ children } as tree)
  : (Nominal.scope, string) Result.t
  = match BL.fromArray (BA.reverse children) with
  | body :: binders -> (match to_ast lang {tree with children = [| body |]} with
    | Ok body' -> binders
      |> traverse_list_result
        (function
          | Either.Left { content = binder_name }
          -> Ok binder_name
          | Right _nonterminal_capture
          -> Error "expected binder name, got TODO"
        )
      |. Result.map
        (fun binders' -> Nominal.Scope (List.rev binders', body'))
    | Error err -> Error err
  )
  | [] -> Error "scope_to_ast called on no children"

exception NonMatchingFixities of string * string list
exception MixedFixities of bool * int

let to_grammar ({terminal_rules; sort_rules}: ConcreteSyntaxDescription.t)
  : LrParsing.grammar
      (* TODO: how to do string -> int mapping? *)
  = let terminal_key_arr = MS.keysToArray terminal_rules in
    let nonterminal_key_arr = MS.keysToArray sort_rules in
    let terminal_names = terminal_rules
        |. MS.keysToArray
        |. BA.mapWithIndex (fun i name -> name, i)
        |. MS.fromArray;
    in
    let nonterminal_names = sort_rules
        |. MS.keysToArray
        |. BA.mapWithIndex (fun i name -> name, i)
        |. MS.fromArray;
    in

    { nonterminals = sort_rules
        |. MS.valuesToArray
        |. BA.mapWithIndex (fun i (SortRule { operator_rules; variable }) ->
          let productions = operator_rules
            |. BL.map (fun operator_level -> operator_level |.
              BL.map (fun (OperatorMatch { tokens }) ->
                tokens |. BL.map (function
                  | TerminalName tn
                  -> LrParsing.Terminal (terminal_names |. MS.getExn tn)
                  | NonterminalName ntn
                  -> Nonterminal (nonterminal_names |. MS.getExn ntn)
                  | Underscore
                  -> failwith "TODO"
                )
              )
            )
            (* TODO: temporary? *)
            |. BL.flatten
          in
          (* production list *)
          (* production = symbol list *)
          i, { LrParsing.productions = productions }
        )
        |. MI.fromArray;
      num_terminals = MS.size terminal_rules;
      terminal_names; nonterminal_names;
    }

(* TODO: this is going to need the grammar *)
let symbol_info : LrParsing.symbol -> node_type * sort
  = failwith "TODO"

let tree_of_parse_result : string -> LrParsing.parse_result -> tree
  = fun str root ->
    let str_pos = ref 0 in
    let str_len = Js.String2.length str in
    (* TODO: update positions *)

    let get_trivia : int -> int -> string * string
      = fun start_pos end_pos ->
        (* look back consuming all whitespace to (and including) a newline *)
        let leading_trivia =
          str |. Js.String2.slice ~from:!str_pos ~to_:start_pos
        in

        (* look forward consuming all whitespace up to a newline *)
        str_pos := end_pos;
        let continue = ref true in
        while !continue do
          let got_space, got_newline = match Js.String2.charAt str !str_pos with
            | " "  -> true,  false
            | "\ " -> true,  false
            | "\n" -> false, true
            | _    -> false, false
          in
          continue := !str_pos < str_len && got_space;
          if !continue then str_pos := !str_pos + 1;
        done;

        let trailing_trivia =
          str |. Js.String2.slice ~from:end_pos ~to_:!str_pos
        in

        leading_trivia, trailing_trivia
    in

    let rec go_nt : LrParsing.parse_result -> tree
      = fun { symbol; children; start_pos; end_pos } ->
        let node_type, sort = symbol_info symbol in
        let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in

        let tokens : nonterminal_token list
          = failwith "TODO"
        in

        { sort; node_type; leading_trivia; trailing_trivia;
          children = children
            |. BL.zip tokens
            |. BL.map (function (parse_result, token) -> match token with
              | TerminalName tn -> Either.Left (go_t parse_result)
              (* TODO: I think we need to use the (non-)terminal name *)
              | NonterminalName ntn -> Right (go_nt parse_result)
              | Underscore -> failwith "TODO"
              (* (terminal_capture, nonterminal_capture) Either.t *)
            )
            |. BL.toArray
        }

        (*
        match symbol with
          | Terminal tn ->
            { sort; node_type; leading_trivia; trailing_trivia;
              children = [|
                Either.Left
                  { leading_trivia;
                    content = Js.String.slice str ~from:start_pos ~to_:end_pos;
                    trailing_trivia;
                  }
              |];
            }
          | Nonterminal ntn ->
              *)

    and go_t : LrParsing.parse_result -> terminal_capture
      = fun { start_pos; end_pos } ->
        let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in
        let content = Js.String.slice str ~from:start_pos ~to_:end_pos in
        { leading_trivia; content; trailing_trivia }

    in

    go_nt root

let lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer
  = fun { terminal_rules } -> terminal_rules
    |. M.map regex_to_string
    |. M.toArray
    |. BL.fromArray

let parse desc str =
  try
    let module Lr0' = LrParsing.Lr0(struct
      let grammar = to_grammar desc
    end) in
    let lexer = lexer_of_desc desc in
    match Lr0'.lex_and_parse lexer str with
      | Result.Ok result -> Result.Ok (tree_of_parse_result str result)
      | Result.Error (Either.Left { start_pos; end_pos; message })
      -> Error (Printf.sprintf
        "lexical error at characters %n - %n (%s):\n%s"
        start_pos
        end_pos
        (Js.String2.slice str ~from:start_pos ~to_:end_pos)
        message
      )
      | Result.Error (Either.Right (char_no, message)) -> Error (Printf.sprintf
        "parser error at character %n:\n%s"
        char_no
        message
      )
  with
    | NonMatchingFixities (sort_name, token_names) -> Result.Error
      ("In sort " ^ sort_name ^ ": all fixities in a precedence level must be the same fixity (this is a limitation of Bison-style parsers (Jison in particular). The operators identified by [" ^ String.concat ", " token_names ^ "] must all share the same fixity.")
    | MixedFixities (b, l) -> Error ("Found a mix of fixities -- all must be uniform " ^ string_of_bool b ^ " " ^ string_of_int l)
