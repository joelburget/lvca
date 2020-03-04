(* Heavily borrowing from "Strictly Pretty" *)
module Nominal = Binding.Nominal
open Types
open ConcreteSyntaxDescription
open! ConcreteSyntax_Private
open Core_kernel

(** The current term and sort don't match *)
exception BadSortTerm of sort * Nominal.term

exception BadRules of string

(** raised from [of_ast] when we need to emit a token but don't have a capture,
 and the terminal match is a regex, not a string literal. This could actually
 be a form of [BadRules] *)
exception CantEmitTokenRegex of string * Regex.t

type mode = Flat | Break

(** how many spaces to break
 * TODO other params from Oppen
 * TODO unused
 *)
type break_type = int

type box_open_info =
  { box_info : box_info
  ; tokens : doc Queue.t
  }

let mk_box_info' : (box_type * int list) option -> (box_info, string) Result.t
  = function
    | None -> mk_box_info HovBox []
    | Some (box_type, args) -> mk_box_info box_type args

(** Pretty-print an abstract term to a concrete syntax tree.

 @raise [UserError]
 @raise [InvariantViolation]
 @raise [BadRules]
*)
let rec term_to_tree
  : nonterminal_pointer -> ConcreteSyntaxDescription.t -> Nominal.term -> doc
  = fun nonterminal_pointer rules tm ->

    let NonterminalRule { operator_rules; _ } =
      current_nonterminal nonterminal_pointer
    in

    let { match_number; tokens = operator_match_tokens; subterms; _ } =
      find_operator_match nonterminal_pointer operator_rules tm
    in

    let tree_info = nonterminal_pointer.current_nonterminal, match_number in

    let terminal_rules_map = String.Map.of_alist_exn rules.terminal_rules in

    (* Use a stack of queues of tokens. The stack represents group structure
       ('[' and ']), while the queues represent tokens within those groups:
       - '[': push on the stack
       - ']': pop from the stack, enqueue at the new current level
       - any other token: enqueue at the current level
     *)
    let token_stack = Stack.singleton
      { box_info = mk_box_info' None |> Result.ok_or_failwith
      ; tokens = Queue.create ()
      }
    in
    let emit tok =
      let { tokens; _ } = Stack.top_exn token_stack in
      Queue.enqueue tokens tok;
    in

    (* Map each token to a subtree. For each token:
       - if it's a space or terminal, print it
       - if it's a nonterminal, look up the subterm (by token number)
    *)
    operator_match_tokens
      |> index_tokens
      |> Fqueue.iter ~f:(fun (token_ix, token) ->

      match Int.Map.find subterms token_ix, token with

    (* if the current token is a terminal, and we didn't capture a binder
     * or term, we just emit the contents of the token *)
    | None, TerminalName name ->
      let terminal_rule = String.Map.find terminal_rules_map name
        |> Util.get_option' (fun () ->
            "term_to_tree: failed to get terminal rule " ^ name)
      in
      (match Regex.is_literal terminal_rule with
      | Some re_str -> emit (TerminalDoc (DocText re_str))
      | None -> raise (CantEmitTokenRegex (name, terminal_rule))
      )

    | Some (CapturedBinder (_current_sort, nonterminal_pointer', pat)),
      NonterminalName _nt_name
    -> emit
      (term_to_tree nonterminal_pointer' rules (Nominal.pattern_to_term pat))

    | Some (CapturedTerm (_current_sort, nonterminal_pointer', tm')),
      NonterminalName _nt_name
    -> emit (term_to_tree nonterminal_pointer' rules tm')

    | _, Underscore n -> emit (TerminalDoc (DocBreak n))

    | None, NonterminalName nt_name -> invariant_violation (Printf.sprintf
      "term_to_tree: failed to find token $%n (%s)" token_ix nt_name
    )

    | Some (CapturedTerm (_sort, _nt_ptr, Var v)), TerminalName _t_name
    -> emit (TerminalDoc (DocText v))
    | Some (CapturedTerm (_sort, _nt_ptr, Primitive p)), TerminalName _t_name
    -> emit (TerminalDoc (DocText (match p with
      | PrimString str -> str
      | PrimInteger i -> Bigint.to_string i)))

    | Some (CapturedBinder (_, _, Var name)), TerminalName _
    -> emit (TerminalDoc (DocText name))

    | Some (CapturedBinder _), TerminalName t_name
    | Some (CapturedTerm _), TerminalName t_name
    -> invariant_violation (Printf.sprintf
      "term_to_tree: unexpectedly directly captured a terminal (%s)" t_name
    )

    | _, OpenBox pre_box_info -> Stack.push token_stack
      (match mk_box_info' pre_box_info with
        | Ok box_info ->
          { box_info
          ; tokens = Queue.create ()
          }
          (* TODO: user error *)
        | Error msg -> failwith msg)
    | _, CloseBox ->
      let { box_info; tokens } = Stack.pop token_stack
        |> Util.get_option' (fun () -> "term_to_tree: Encountered an empty \
          stack on a close box -- this means there were more close boxes than \
          opens")
      in
      emit (DocGroup (Queue.to_list tokens, box_info))
    );

    (match Stack.to_list token_stack with
      (* Ignore box_info -- it's always None for the top level *)
      | [ { tokens; _ } ]
      -> NonterminalDoc (Queue.to_list tokens, tree_info)
      | _
      -> failwith "invariant violation: term_to_tree non-matching opening and \
        closing boxes")
;;

type fit_info = Fits of int | DoesntFit

type indentation = int

type space =
  | SSpace of int
  | SLine of int

(** A pre-formatted tree. It's been formatted (converted from [doc] but not yet
 normalized to a [formatted_tree] (see [normalize_nonterminal]
 *)
type pre_formatted =
  | Terminal of string
  | Nonterminal of pre_formatted_nonterminal
  | Space of space
  | Group of pre_formatted array

and pre_formatted_nonterminal =
  { children : pre_formatted array
  ; tree_info : tree_info
  }

(** [tree_fits w i m] checks whether a flat document fits completely into [w]
   characters.

   @param max_width The maximum printed column width.
   @param start_col The column printing starts at (indendation)
   @param mode The mode of the current group
   @return [Fits n]: The tree fits in the current width, ending at indentation
     level [n], [DoesntFit]: the tree doesn't fit in the current width
 *)
let rec tree_fits : int -> int -> mode -> doc -> fit_info
  = fun max_width start_col mode -> function
    | _ when start_col >= max_width -> DoesntFit
    | TerminalDoc (DocText str) ->
      let len = String.length str in
      let end_col = start_col + len in
      if end_col < max_width then Fits end_col else DoesntFit
    | TerminalDoc (DocBreak size) when Caml.(mode = Flat)
    -> if size < max_width then Fits start_col else DoesntFit
    | TerminalDoc (DocBreak _)
    -> failwith "impossible" (* TODO: raise InvariantViolation *)
    | NonterminalDoc (children, _)
    -> group_fits max_width start_col mode children
    | DocGroup (group, _box_info) -> group_fits max_width start_col Flat group

and group_fits max_width start_col mode children = List.fold_right
  ~f:(fun child -> function
    | DoesntFit -> DoesntFit
    | Fits col -> tree_fits max_width col mode child
  )
  ~init:(Fits start_col)
  children

let rec tree_format
  (* takes the starting column, returns the ending column *)
  : int -> indentation -> mode -> doc -> indentation * pre_formatted
  = fun max_width indentation mode -> function
  | TerminalDoc (DocText str)
  -> indentation + String.length str, Terminal str
  | TerminalDoc (DocBreak len) when Caml.(mode = Flat)
  -> indentation + len, Space (SSpace len)
  | TerminalDoc (DocBreak _) (* when mode = Break *)
  -> indentation, Space (SLine indentation)
  | NonterminalDoc (children, tree_info) ->
    let indentation', children' =
      group_format max_width indentation mode children
    in
    indentation', Nonterminal { children = children'; tree_info }
    (* XXX need to add indentation *)
  | DocGroup (group, _box_info) as doc ->
    let mode' = match tree_fits max_width indentation Flat doc with
      | DoesntFit -> Break
      | Fits _ -> Flat
    in
    let indentation', group' = group_format max_width indentation mode' group in
    indentation', Group group'

and group_format max_width indentation mode children =
  let children' = Queue.create () in
  let indentation' = List.fold_left
    ~f:(fun indentation' child ->
      let indentation'', child' =
        tree_format max_width indentation' mode child
      in
      Queue.enqueue children' child';
      indentation''
    )
    ~init:indentation
    children
  in
  indentation', Queue.to_array children'

(* Accumulate all leading trivia for each terminal. So, all whitespace leading
 * back to and including the first newline. *)
let walk_leading_trivia : pre_formatted_nonterminal -> string array
  = fun tree ->

    let accum = ref "" in
    let accumulating = ref true in
    let result = Queue.create () in

    let rec go_nt = fun { children; _ } -> Array.iter children ~f:go_pft

    and go_pft = (function
      (* Stop accumulating when we hit a token, clear accumulator *)
      | Terminal _ ->
        Queue.enqueue result !accum;
        accumulating := false;
        accum := ""
      | Space (SSpace n)
      -> if !accumulating then accum := !accum ^ String.make n ' '
      (* Start accumulating when we hit a newline.
       * Invariant relied upon: accum = "" if not accumulating
       *)
      | Space (SLine n) ->
        accumulating := true;
        accum := !accum ^ "\n" ^ String.make n ' '
      | Group children' -> Array.iter children' ~f:go_pft
      | Nonterminal nt -> go_nt nt
    )
    in

    go_nt tree;
    Queue.to_array result

(* Accumulate all trailing trivia up to, but not including the next newline.
 *
 * Important: returns trivia in reverse order!
 *)
let walk_trailing_trivia : pre_formatted_nonterminal -> string array
  = fun tree ->

    let reverse_iter = Util.Array.reverse_iter in

    let accum = ref "" in
    let result = Queue.create () in

    (* Traverse all children in reverse *)
    let rec go_nt = fun { children; _ } -> reverse_iter children ~f:go_pft

    and go_pft = function
      | Terminal _ ->
        ignore (Queue.enqueue result !accum : unit);
        accum := ""
      | Space (SSpace n) -> accum := !accum ^ String.make n ' '
      (* Every time we hit a newline, clear the accumulator. *)
      | Space (SLine _) -> accum := ""
      | Group children' -> reverse_iter children' ~f:go_pft
      | Nonterminal nt -> go_nt nt
    in

    go_nt tree;

    Queue.to_array result

(* Traverse the pre-formatted tree, normalizing spacing.
 *)
let normalize_nonterminal : pre_formatted_nonterminal -> formatted_tree
  = fun tree ->

    let forward_trivia = tree |> walk_leading_trivia in
    let reverse_trivia = tree |> walk_trailing_trivia |> Util.Array.reverse in
    let overall_ix = ref 0 in

    (* Note that for each nonterminal [go_nt] we create a flat list of children
     * [formatted_tree_children]. Initially empty, it's modified by [go_pft].
     *)
    let rec go_nt
      : pre_formatted_nonterminal -> formatted_tree
      = fun { children; tree_info } ->
      let formatted_tree_children = Queue.create () in
      Array.iter children ~f:(go_pft formatted_tree_children);
      { children = Queue.to_array formatted_tree_children
      ; tree_info
      }

    and go_pft
      : formatted_capture Queue.t -> pre_formatted -> unit
      = fun formatted_tree_children -> function
      | Terminal content ->
        let leading_trivia = forward_trivia.(!overall_ix) in
        let trailing_trivia = reverse_trivia.(!overall_ix) in
        overall_ix := !overall_ix + 1;
        Queue.enqueue formatted_tree_children
          (TerminalCapture { content; leading_trivia; trailing_trivia })
      | Space _ -> ()
      | Nonterminal pfnt ->
        Queue.enqueue formatted_tree_children
          (NonterminalCapture (go_nt pfnt))
      | Group group_children
      -> Array.iter group_children ~f:(go_pft formatted_tree_children)

    in go_nt tree

let%test_module "normalize_nonterminal" =
  (module struct
    let (=) = Caml.(=)

    let terminal_capture leading_trivia content trailing_trivia =
      TerminalCapture { content; leading_trivia; trailing_trivia }

    let%test _ =
      let formatted_tree = normalize_nonterminal
        { children = [| Group [|
          Space (SSpace 1);
          Terminal "\\";
          Terminal "x";
          Space (SSpace 1);
          Terminal "->";
          Space (SSpace 1);
          Group [|
            Nonterminal
              { children = [| Terminal "x" ; Space (SSpace 1) |]
              ; tree_info = "tm", 0
              };
            Space (SSpace 1);
          |];
          Space (SSpace 1);
        |] |]
        ; tree_info = "tm", 1
        }
      in
      let expected : formatted_tree =
        { children = [|
          terminal_capture " " "\\" "";
          terminal_capture "" "x" " ";
          terminal_capture "" "->" " ";
          NonterminalCapture
            { children = [| terminal_capture "" "x" "   " |]
            ; tree_info = "tm", 0
            }
        |]
        ; tree_info = "tm", 1
        }
      in

      formatted_tree = expected
  end)

(**
 @raise [UserError]
 @raise [InvariantViolation]
 @raise [BadRules]
*)
let of_ast
  :  ConcreteSyntaxDescription.t
  -> string
  -> int
  -> Binding.Nominal.term
  -> formatted_tree
  = fun desc start_nonterminal width tm ->
    let nonterminal_pointer =
      { nonterminals = desc.nonterminal_rules
      ; current_nonterminal = start_nonterminal
      ; bound_sorts = String.Map.empty
      }
    in
    let doc = term_to_tree nonterminal_pointer desc tm in
    let _, pre_formatted = tree_format width 0 Flat doc in
    match pre_formatted with
      | Nonterminal pre_formatted_nonterminal
      -> normalize_nonterminal pre_formatted_nonterminal
      | _ -> failwith "invariant violation"

let%test_module "tree_format" =
  (module struct
    let hovbox = HovBox (1, 2, 0)
    let (^|) x y = [x; TerminalDoc (DocBreak 1); y]
    let binop left op right = DocGroup
      ( [ DocGroup
          ( [ TerminalDoc (DocText left);
              TerminalDoc (DocBreak 1);
              TerminalDoc (DocText op)
            ]
          , hovbox
          );
          TerminalDoc (DocBreak 1);
          TerminalDoc (DocText right);
        ]
      , hovbox
      )
    let cond = binop "a" "==" "b"
    let expr1 = binop "a" "<<" "2"
    let expr2 = binop "a" "+" "b"
    let ifthen c e1 e2 = NonterminalDoc
      ( [ DocGroup
          ( [ DocGroup (TerminalDoc (DocText "if") ^| c, hovbox);
              TerminalDoc (DocBreak 1);
              DocGroup (TerminalDoc (DocText "then") ^| e1, hovbox);
              TerminalDoc (DocBreak 1);
              DocGroup (TerminalDoc (DocText "else") ^| e2, hovbox);
            ]
          , hovbox
          )
        ]
      , ("expr", 1)
      )
    let doc = ifthen cond expr1 expr2

    let run width =
      let _, pre_formatted = tree_format width 0 Flat doc in
      match pre_formatted with
        | Nonterminal pre_formatted_nonterminal
        -> print_string (to_string (normalize_nonterminal pre_formatted_nonterminal))
      | _ -> failwith "invariant violation"

    (* Test we can replicate the results from "strictly pretty" *)
    let%expect_test "width 32" =
      run 32;
      [%expect{| if a == b then a << 2 else a + b |}]

    let%expect_test "width 15" =
      run 15;
      [%expect]

    let%expect_test "width 10" =
      run 10;
      [%expect]

    (* TODO: 8, 7, 6 *)
  end)
