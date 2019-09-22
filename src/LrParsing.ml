module A = Belt.Array
module L = Belt.List
module M = Belt.Map.Int
module MM = Belt.MutableMap.Int
module SI = Belt.Set.Int
module SS = Belt.Set.String
module MSI = Belt.MutableSet.Int
module Result = Belt.Result

(* TODO:
  * augment grammar
 *)

type terminal_num = int (* TODO *)
type nonterminal_num = int

type symbol =
  | Terminal    of terminal_num
  | Nonterminal of nonterminal_num

module SymbolCmp = Belt.Id.MakeComparable(struct
  type t = terminal_num * symbol
  let cmp (a0, a1) (b0, b1) =
    match Pervasives.compare a0 b0 with
      | 0 -> Pervasives.compare a1 b1
      | c -> c
end)

type production = symbol list

type production_num = int

(** A production with a dot at some position.
 *
 * Note: Menhir encodes this as a single integer.
 *)
type item_view =
  (** The number of one of the productions of the underlying grammar *)
  { production_num: production_num;
  (** The position of the dot *)
    position: int;
  }

(* An LR(0) item encodes a pair of integers, namely the index of the
   production and the index of the bullet in the production's
   right-hand side. *)

(* Both integers are packed into a single integer, using 8 bits for
   the bullet position and the remaining 24 bits for the
   production index. *)

(* Note that this implies some maximums:
   * The maximum length of a production is 255
   * The maximum number of different productions is 16777215
 *)
type item = int

type item_set = SI.t

(* TODO: test *)
let view_item : item -> item_view
  = fun item ->
    { production_num = item land 0x00ffffff;
      position = (item land 0xff000000) lsr 24
    }

let mk_item' : int -> int -> item
  = fun production_num position -> (position lsl 24) lor production_num

let mk_item : item_view -> item
  = fun { production_num; position } ->  mk_item' production_num position

type configuration_set =
  { kernel_items : item_set;    (* set of items *)
    nonkernel_items : item_set; (* set of nonterminals *)
  }

type nonterminal =
  { (* nonterminal_num: nonterminal_num; *)
    productions: production list;
  }

type state = int

(* By convention:
  * A non-augmented grammar has keys starting at 1 (start)
  * An augmented grammar has key 0 (augmented start)
 *)
type grammar = {
  nonterminals : nonterminal M.t;
  num_terminals : int
}

type action =
  | Shift  of state
  | Reduce of nonterminal_num (* Q: nonterminal_num = state? *)
  | Accept
  | Error

type action_table = state -> terminal_num -> action
type goto_table = state -> symbol -> state

type parser_tables = action_table * goto_table

module ComparableSet = Belt.Id.MakeComparable(struct
  type t = SI.t
  let cmp = SI.cmp
end)

type int_set_set = (SI.t, ComparableSet.identity) Belt.Set.t
type mutable_int_set_set = (SI.t, ComparableSet.identity) Belt.MutableSet.t

module type GRAMMAR = sig
  val grammar : grammar
end

(* TODO: remove exns *)
module Lr0 (G : GRAMMAR) = struct

  let production_map : production MM.t
    = MM.make ()

  let production_nonterminal_map : nonterminal_num MM.t
    = MM.make ()

  (* Map from a nonterminal num to the set of productions it holds *)
  let nonterminal_production_map : MSI.t MM.t
    = MM.make ()

  let production_cnt = ref 0
  let () = M.forEach G.grammar.nonterminals
    (fun nt_num { productions } ->
      nonterminal_production_map |. MM.set nt_num (MSI.make ());
      L.forEach productions (fun production ->
        let production_num = !production_cnt in
        production_cnt := production_num + 1;
        production_map |. MM.set production_num production;
        production_nonterminal_map |. MM.set production_num nt_num;
        let prod_set = nonterminal_production_map |. MM.getExn nt_num in
        prod_set |. MSI.add production_num
      )
    )

  (* TODO: clarify if this is for the augmented grammar or not *)
  let number_of_nonterminals : int
    = M.size G.grammar.nonterminals

  let get_nonterminal_num : production_num -> nonterminal_num
    = MM.getExn production_nonterminal_map

  let get_nonterminal : production_num -> nonterminal
    = fun pn -> M.getExn G.grammar.nonterminals @@ get_nonterminal_num pn

  (** The closure of an item set. CPTT fig 4.32. *)
  (* We could save memory by not even calculating nonkernel items here *)
  let closure : item_set -> configuration_set
    = fun initial_items ->
      let added = Bitstring.alloc number_of_nonterminals false in
      let kernel_items = initial_items in (* XXX wrong *)
      let nonkernel_items = MSI.make () in
      let nt_stack = MSI.make () in

      (* Create the set (nt_stack) of nonterminals to look at *)
      SI.forEach initial_items (fun item ->
        let { production_num; position } = view_item item in
        let production = MM.getExn production_map production_num in
        (* first symbol right of the dot *)
        match production |. L.get position with
          | Some (Nonterminal nt) ->
              nt_stack |. MSI.add nt
          | _              -> ()
        );

      while not (MSI.isEmpty nt_stack) do
        let nonterminal_num = match MSI.minimum nt_stack with
          | Some nonterminal_num -> nonterminal_num
          | None -> failwith "invariant violation: the set is not empty!"
        in
        nt_stack |. MSI.remove nonterminal_num;
        if not (added |. Bitstring.getExn nonterminal_num) then (
          added |. Bitstring.setExn nonterminal_num true;
          let production_set =
            MM.getExn nonterminal_production_map nonterminal_num
          in
          MSI.forEach production_set (fun production_num ->
            nonkernel_items |. MSI.add (mk_item' production_num 0)
          );
          let { productions } = M.getExn G.grammar.nonterminals nonterminal_num in
          L.forEach productions (fun production ->
            match production with
              | Terminal _         :: _ -> ()
              | Nonterminal new_nt :: _ -> nt_stack |. MSI.add new_nt
              | _                       -> failwith "Empty production"
          )
        )
      done;
      { kernel_items = initial_items;
        nonkernel_items = nonkernel_items |. MSI.toArray |. SI.fromArray;
      }

  let simplify_config_set : configuration_set -> item_set
    = fun { kernel_items; nonkernel_items } ->
      SI.union kernel_items nonkernel_items

  let closure' : item_set -> item_set
    = fun items -> simplify_config_set @@ closure items

  (* Examine for items with the nonterminal immediately to the right of the
   * dot. Move the dot over the nonterminal and take the closure of those sets.
   *)
  let goto_kernel : item_set -> symbol -> item_set
    = fun item_set symbol ->
      let result = MSI.make () in
      SI.forEach item_set (fun item ->
        let { production_num; position } = view_item item in
        let production = production_map |. MM.getExn production_num in
        match L.get production position with
          | Some next_symbol ->
            if symbol = next_symbol then (
              result |. MSI.add (mk_item' production_num (position + 1))
            )
          | _ -> ()
      );
      result |. MSI.toArray |. SI.fromArray

  let goto : item_set -> symbol -> configuration_set
    = fun item_set symbol -> closure @@ goto_kernel item_set symbol

  (** Compute the canonical collection of sets of LR(0) items. CPTT fig 4.33. *)
  let items : mutable_int_set_set
    = let augmented_start = SI.fromArray
          [| mk_item {production_num = 0; position = 0} |]
      in
      let ca = closure' augmented_start in
      let c = Belt.MutableSet.fromArray [| ca |] ~id:(module ComparableSet) in
      let continue = ref true in
      while !continue do
        continue := false;
        (* for each set of items i in c: *)
        Belt.MutableSet.forEach c @@ fun i ->
          let grammar_symbols = L.concat
            (L.makeBy G.grammar.num_terminals (fun n -> Terminal n))
            (L.makeBy number_of_nonterminals (fun n -> Nonterminal n))
          in
          (* for each grammar symbol x: *)
          L.forEach grammar_symbols @@ fun x ->
          (* M.forEach G.grammar @@ fun x _ -> *)
            let goto_i_x = simplify_config_set @@ goto i x in
            (* if GOTO(i, x) is not empty and not in c: *)
            if not (SI.isEmpty goto_i_x) && not (Belt.MutableSet.has c goto_i_x)
            then (
              c |. Belt.MutableSet.add goto_i_x;
              continue := true
            )
      done;
      c

  let items' : item_set M.t
    = items
    |. Belt.MutableSet.toArray
    |. A.mapWithIndex (fun i item_set -> i, item_set)
    |. M.fromArray

  let state_to_item_set : state -> item_set
    = M.getExn items'

  let item_set_to_state : item_set -> state
    = fun item_set ->
    let state, _ = items'
      |. M.findFirstBy (fun k item_set' -> item_set' = item_set)
      |. Belt.Option.getExn
    in state

  let in_first_cache : (terminal_num * symbol, bool, SymbolCmp.identity) Belt.Map.t
    = Belt.Map.make ~id:(module SymbolCmp)

  let in_first : terminal_num -> symbol -> bool
    =
      (* We use this stack (which is a set) to track which nonterminals we're
       * currently looking through, to prevent loops *)
      let stack = MSI.make () in
      let rec in_first' t_num sym =
        match in_first_cache |. Belt.Map.get (t_num, sym) with
        | Some result -> result
        | None -> (match sym with
          | Terminal t_num'    -> t_num' = t_num
          | Nonterminal nt_num ->
          let { productions } = G.grammar.nonterminals |. M.getExn nt_num in
          let result, _ = Util.fold_right
            (fun (symbol, (already_found, all_derive_empty)) ->
              let found_it = productions |. L.some (function
                | Terminal t_num' :: _ -> t_num' = t_num
                | (Nonterminal nt_num as nt) :: _ ->
                  if stack |. MSI.has nt_num
                  then false
                  else (
                    stack |. MSI.add nt_num;
                    let result = in_first' t_num nt in
                    stack |. MSI.remove nt_num;
                    result
                )
                | [] -> false
              )
              in
              (* TODO: update to allow empty productions *)
              let this_derives_empty = false in
              ( already_found || found_it
              , all_derive_empty && this_derives_empty
              )
            )
            productions
            (false, true)
          in
          in_first_cache |. Belt.Map.set (t_num, sym) result;
          result
        )
      in in_first'

  let in_first_str : terminal_num -> symbol list -> bool
    = fun t_num str -> match str with
      (* TODO: update to allow empty productions *)
      | x :: _ -> in_first t_num x
      | _ -> false

  let end_marker : terminal_num
    = 0

  exception FoundInFollow

  (* nts_visited (which is a set) tracks the nonterminals we've looked in
   *)

  (* find all `t`s in the production, then look at their following
   * sentences *)
  let rec in_follow''
    : SI.t -> terminal_num -> nonterminal_num -> production -> bool
    = fun nts_visited t_num nt_num -> function
      | Terminal _ :: rest -> in_follow'' nts_visited t_num nt_num rest
      (* TODO: update to allow empty productions *)
      | Nonterminal nt'_num :: rest ->
        (nt'_num = nt_num && in_first_str t_num rest) ||
        in_follow'' nts_visited t_num nt_num rest
      | [] -> false

  and in_follow' : SI.t -> terminal_num -> nonterminal_num -> bool
    = fun nts_visited t_num nt_num ->
      if nts_visited |. SI.has nt_num then false else
      (* treat the augmenting production specially (rule 1):
         $ is in Follow(S)
       *)
      if nt_num = 0 then
        t_num = end_marker
      else
        (* TODO: update to allow empty productions *)
        try
          let nts_visited' = nts_visited |. SI.add nt_num in
          production_map |. MM.forEach (fun prod_num production ->
            (* First look for this terminal following nt directly (rule 2) *)
            if in_follow'' nts_visited' t_num nt_num production
              then raise FoundInFollow;

            (* Then look for this terminal following nonterminals which end in
               nt. (rule 3)
               IE, if there is a production A -> xB, then everything in
               Follow(A) is in Follow(B)
             *)
            let nt_num' = production_nonterminal_map |. MM.getExn prod_num in
            match Util.unsnoc production with
              | _, Nonterminal last
              -> if last = nt_num && in_follow' nts_visited' t_num nt_num'
                then raise FoundInFollow;
              | _ -> ()
          );
          false
        with
          FoundInFollow -> true

  let in_follow : terminal_num -> nonterminal_num -> bool
    = in_follow' (SI.fromArray [||])

  let slr_tables : parser_tables
    = let goto_table state nt =
        item_set_to_state
          @@ simplify_config_set
          @@ goto (state_to_item_set state) nt
      in

      let action_table = fun state terminal_num ->
        let item_set = state_to_item_set state in

        let item_set_l = SI.toList item_set in

        (* If [A -> xs . a ys] is in I_i and GOTO(I_i, a) = I_j, set
         * ACTION[i, a] to `shift j` *)
        let shift_action = item_set_l
          |. Util.find_by (fun item ->
            let { production_num; position } = view_item item in
            let symbols = production_map |. MM.getExn production_num in
            match symbols |. L.get position  with
              | Some ((Terminal a) as next_symbol) ->
                Some (Shift (goto_table state next_symbol))
              | _ -> None
          )
        in
        (* If [A -> xs .] is in I_i, set ACTION[i, a] to `Reduce A -> a` for
         * all a in FOLLOW(A) *)
        let reduce_action = item_set_l
          |. Util.find_by (fun item ->
            let { production_num; position } = view_item item in
            if production_num = terminal_num && position = 1
            (* XXX check in follow set *)
            then Some (Reduce
              (production_nonterminal_map |. MM.getExn production_num))
            else None
          )
        in
        (* If [S' -> S .] is in I_i, set ACTION[i, $] to `accept` *)
        let accept_action =
          (* XXX check is $ *)
          if item_set |. SI.has (mk_item' 0 1) then Some Accept else None
        in
        match shift_action, reduce_action, accept_action with
          | Some act, None, None -> act
          | None, Some act, None -> act
          | None, None, Some act -> act
          | None, None, None     -> Error
      in

      (action_table, goto_table)
end

(* lalr table construction (both action and goto tables) *)
let lalr_tables : grammar -> parser_tables
  = fun grammar ->

    (* the sets of LR(1) items *)
    let item_sets : item M.t = failwith "TODO"
    in

    (* for each core, find all sets having that core, and replace by their
     * union *)
    let item_sets' = failwith "TODO"
    in

    let action_table = failwith "TODO"
    in

    let goto_table = failwith "TODO"
    in

    (action_table, goto_table)

(* TODO *)
type parse_error = Lex.position * string

exception ParseFinished
exception ParseFailed of parse_error
exception PopFailed

let pop_exn : 'a array -> 'a
  = fun arr -> match Js.Array2.pop arr with
    | None -> raise PopFailed
    | Some a -> a

let parse : parser_tables -> Lex.token array -> (nonterminal, parse_error) Result.t
  = fun (action_table, goto_table) toks ->
    let stack = ref [0] in
    try
      while true do
        let s :: _ = !stack in
        let a = ref @@ pop_exn toks in
        let a' = failwith "TODO" a in
        match action_table s a' with
          | Shift t ->
              stack := t :: !stack;
              a := pop_exn toks
          | Reduce t ->
              (); (* pop symbols off the stack *)
              (match !stack with
                (* push GOTO[t, A] onto the stack *)
                | t :: stack' -> stack := goto_table t a' :: stack'
                | [] -> failwith "invariant violation: reduction with empty stack"
              );
              () (* output production *)
          | Accept -> raise ParseFinished
          | Error -> raise (ParseFailed (failwith "TODO"))
          ;
      done;
      failwith "can't make it here"
    with
      | ParseFinished -> failwith "TODO"
      | ParseFailed parse_error -> Result.Error parse_error

(* TODO: menhir compatibility?

module MenhirBasics = struct
  exception Error

  type token = Lex.token
end

module Tables = struct
  include MenhirBasics

  let token2terminal : token -> int = failwith "TODO"

  (* This is the integer code for the error pseudo-token. *)
  and error_terminal = 0

  (* TODO: change if tokens start holding values *)
  and token2value : token -> Obj.t = fun _ -> Obj.repr ()
  and default_reduction = failwith "TODO"
  and error = failwith "TODO"
  and start = 1
  and action =  failwith "TODO"
  and lhs =  failwith "TODO"
  and goto = failwith "TODO"
  and semantic_action = failwith "TODO"
  and trace = failwith "TODO"
  (* TODO *)
end

module MenhirParser = struct
  include MenhirBasics

  let top_term : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> nonterminal
    = failwith "TODO"

  module MenhirInterpreter = struct
    module ET = MenhirLib.TableInterpreter.MakeEngineTable (Tables)

    module TI = MenhirLib.Engine.Make (ET)

    include TI
  end

  module Incremental = struct
    let top_term : Lexing.position -> nonterminal MenhirInterpreter.checkpoint
      = failwith "TODO"
  end
end

*)
