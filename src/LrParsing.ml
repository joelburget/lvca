module A = Belt.Array
module L = Belt.List
module M = Belt.Map.Int
module MS = Belt.Map.String
module MM = Belt.MutableMap
module MMI = Belt.MutableMap.Int
module S = Belt.Set
module SI = Belt.Set.Int
module SS = Belt.Set.String
module MSet = Belt.MutableSet
module MSI = Belt.MutableSet.Int
module Result = Belt.Result
module MStack = Belt.MutableStack
module MQueue = Belt.MutableQueue
let (get_option', invariant_violation) = Util.(get_option', invariant_violation)

(* by convention, we reserve 0 for the `$` terminal, and number the rest
 * contiguously from 1 *)
type terminal_num = int

(* by convention, we reserve 0 for the root nonterminal, and number the rest
 * contiguously from 1 *)
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
 * We encode this as a single integer -- see `item`, `mk_item`, `mk_item'`, and
 * `view_item`.
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

type lookahead_item =
  { item : item;
    (* the set of terminals that can follow this item *)
    lookahead_set : SI.t;
  }

module LookaheadItemCmp = Belt.Id.MakeComparable(struct
  type t = lookahead_item
  let cmp { item = i1; lookahead_set = s1 } { item = i2; lookahead_set = s2 } =
    match Pervasives.compare i1 i2 with
      | 0 -> SI.cmp s1 s2
      | c -> c
end)

module LookaheadClosureItemCmp = Belt.Id.MakeComparable(struct
  type t = nonterminal_num * terminal_num
  let cmp = Pervasives.compare
end)

(* TODO: do we use this? *)
type lookahead_item_set = (lookahead_item, LookaheadItemCmp.identity) S.t

let lookahead_item_set_from_array : lookahead_item array -> lookahead_item_set
  = S.fromArray ~id:(module LookaheadItemCmp)

let view_item : item -> item_view
  = fun item ->
    { production_num = item land 0x00ffffff;
      position = (item land 0xff000000) lsr 24
    }

let mk_item' : int -> int -> item
  = fun production_num position -> (position lsl 24) lor production_num

type item_set = SI.t

let mk_item : item_view -> item
  = fun { production_num; position } ->  mk_item' production_num position

type configuration_set =
  { kernel_items : item_set;    (* set of items *)
    nonkernel_items : item_set; (* set of nonterminals *)
  }

let simplify_config_set : configuration_set -> item_set
  = fun { kernel_items; nonkernel_items } ->
    SI.union kernel_items nonkernel_items

(* TODO: do we use this? *)
type lookahead_configuration_set =
  { kernel_items : lookahead_item_set;    (* set of items *)
    nonkernel_items : lookahead_item_set; (* set of nonterminals *)
  }

let simplify_lookahead_config_set
  : lookahead_configuration_set -> lookahead_item_set
  = fun { kernel_items; nonkernel_items } ->
    S.union kernel_items nonkernel_items

type nonterminal =
  { (* nonterminal_num: nonterminal_num; *)
    productions: production list;
  }

(** An LR(0) state number *)
type state = int

(* By convention:
  * A non-augmented grammar has keys starting at 1 (start)
  * An augmented grammar has key 0 (augmented start)
 *)
type grammar = {
  nonterminals : nonterminal Belt.Map.Int.t;
  terminal_nums : (string * terminal_num) array;
  nonterminal_nums : (string * nonterminal_num) array;
}

type action =
  | Shift  of state
  | Reduce of production_num
  | Accept
  | Error

(* Our action / goto table formulations are lazy (not actually tables). Tables
 * can be computed with `full_action_table` / `full_goto_table` *)
type lr0_action_table = state -> terminal_num -> action
type lr0_goto_table = state -> symbol -> state option

module ComparableIntSet = Belt.Id.MakeComparable(struct
  type t = SI.t
  let cmp = SI.cmp
end)

(* A mutable set of int sets. This is used to represent the set of LR(0) items.
 * Each int set represents a set of encoded items.
 *)
type mutable_lr0_item_set = (SI.t, ComparableIntSet.identity) MSet.t

module LookaheadItemSetCmp = Belt.Id.MakeComparable(struct
  type t = lookahead_item_set
  let cmp = S.cmp
end)

(* A mutable set of lookahead item sets. This is used to represent the set of
 * LR(1) items. Each set represents a set of encoded items with lookaheads.
 *)
type mutable_lookahead_item_set =
  (lookahead_item_set, LookaheadItemSetCmp.identity) MSet.t

type parse_error = int (* character number *) * string

type parse_result =
  { production : (terminal_num, production_num) Either.t;
    children : parse_result list;
    start_pos : int; (* inclusive *)
    end_pos : int; (* exclusive *)
  }

let rec parse_result_to_string : parse_result -> string
  = fun { production; children } -> Printf.sprintf "%s[%s]"
    (match production with
      | Left n -> "t" ^ string_of_int n
      | Right n -> "n" ^ string_of_int n
    )
    (Util.stringify_list parse_result_to_string ", " children)

exception ParseFinished
exception ParseFailed of parse_error
exception PopFailed of int

type do_trace =
  | DoTrace
  | DontTrace

let pop_front_exn : int -> 'a MQueue.t -> 'a
  = fun position arr -> match MQueue.pop arr with
    | None -> raise (PopFailed position)
    | Some a -> a

module type GRAMMAR = sig
  (* An *augmented* grammar, or else this won't work right *)
  val grammar : grammar
end

module type LR0 = sig
  (* TODO: fill in the rest of the signature *)
  val production_map : production MMI.t
  val production_nonterminal_map : nonterminal_num MMI.t
end

(* Used for action table entries. Compare with string_of_action. *)
let action_abbrev : action -> string
  = function
  | Shift state -> "s" ^ string_of_int state
  | Reduce prod -> "r" ^ string_of_int prod
  | Accept      -> "acc"
  | Error       -> ""

let string_of_stack : state array -> string
  = fun states -> states
    |. A.map string_of_int
    |. Js.Array2.joinWith " "

(* TODO: where to put this? *)
let string_of_tokens : Lex.token array -> string
  = fun toks -> toks
      |. A.map (fun { name } -> name)
      |. Js.Array2.joinWith " "

(* TODO: remove exns *)
module Lr0 (G : GRAMMAR) = struct

  (* Map from production number to the actual production *)
  let production_map : production Belt.MutableMap.Int.t
    = MMI.make ()

  (* Map from production number to the number of the nonterminal it belongs to
   *)
  let production_nonterminal_map : nonterminal_num Belt.MutableMap.Int.t
    = MMI.make ()

  (* Map from a nonterminal num to the set of productions it holds *)
  let nonterminal_production_map : MSI.t Belt.MutableMap.Int.t
    = MMI.make ()

  (* number of nonterminals in the passed-in grammar (which ought to be
   * augmented) *)
  let number_of_nonterminals : int
    = M.size G.grammar.nonterminals

  let number_of_terminals : int
    = A.length G.grammar.terminal_nums

  let terminal_names : string Belt.Map.Int.t
    = G.grammar.terminal_nums
      |. A.map (fun (name, num) -> (num, name))
      |. M.fromArray

  let nonterminal_names : string Belt.Map.Int.t
    = G.grammar.nonterminal_nums
      |. A.map (fun (name, num) -> (num, name))
      |. M.fromArray

  let terminal_nums : int Belt.Map.String.t
    = MS.fromArray G.grammar.terminal_nums

  let nonterminal_nums : int Belt.Map.String.t
    = MS.fromArray G.grammar.nonterminal_nums

  let rec nonterminal_first_set : SI.t -> MSI.t -> nonterminal_num -> unit
    = fun already_seen_nts result nt ->

      let productions : MSI.t = nonterminal_production_map
        |. MMI.get nt
        |> get_option' "TODO"
        |. MSI.copy
      in

      while not (MSI.isEmpty productions) do
        let production_num = productions
          |. MSI.minimum
          |> get_option' "TODO"
        in
        MSI.remove productions production_num;
        let production = production_map
          |. MMI.get production_num
          |> get_option' "TODO"
        in
        first_set' already_seen_nts result production
      done;

  and first_set' : SI.t -> MSI.t -> production -> unit
    = fun already_seen_nts result -> function
    | [] -> failwith
      "invariant violation: first_set must be called with a non-empty list"
    | Terminal num :: _ -> MSI.add result num
    (* TODO: update when we allow empty productions *)
    | Nonterminal num :: _
    -> if not (already_seen_nts |. SI.has num)
       then nonterminal_first_set already_seen_nts result num

  let first_set : production -> SI.t
    = fun prod ->
      let result = MSI.make () in
      let already_seen_nts = SI.fromArray [||] in
      first_set' already_seen_nts result prod;
      result |. MSI.toArray |. SI.fromArray

  let string_of_terminal : terminal_num -> string
    = fun t_num -> terminal_names
      |. M.get t_num
      |> get_option' (Printf.sprintf
        "string_of_symbol: failed to get terminal %n"
        t_num
      )

  let string_of_nonterminal : nonterminal_num -> string
    = fun nt_num -> nonterminal_names
      |. M.get nt_num
      |> get_option' (Printf.sprintf
        "string_of_symbol: failed to get nonterminal %n"
        nt_num
      )

  let string_of_symbol : symbol -> string
    = function
    | Terminal t_num -> string_of_terminal t_num
    | Nonterminal nt_num -> string_of_nonterminal nt_num

  let string_of_item : item -> string
    = fun item ->
      let { production_num; position } = view_item item in
      let production = production_map
        |. MMI.get production_num
        |> get_option' (Printf.sprintf
          "Lr0 string_of_item: unable to find production %n in production_map"
          production_num
        )
      in
      let pieces = [||] in
      L.forEachWithIndex production (fun i symbol ->
        if position = i then (let _ = Js.Array2.push pieces "." in ());
        Js.Array2.push pieces (string_of_symbol symbol);
      );

      if position = L.length production then
        (let _ = Js.Array2.push pieces "." in ());

      let nt_num = production_nonterminal_map
        |. MMI.get production_num
        |> get_option' (Printf.sprintf
          "Lr0 string_of_item: unable to find production %n in production_nonterminal_map"
          production_num
        )
      in

      let nt_name = nonterminal_names
        |. M.get nt_num
        |> get_option' (Printf.sprintf
          "Lr0 string_of_production: unable to find nonterminal %n in nonterminal_names"
          production_num
        )
      in

      (* output if trailing *)
      Printf.sprintf "%s -> %s" nt_name (Js.Array2.joinWith pieces " ")

  let string_of_item_set : ?sep:string -> item_set -> string
    = fun ?(sep=" ") item_set -> item_set
      |. SI.toArray
      |. A.map string_of_item
      |. Js.Array2.joinWith sep

  (* TODO: move to module *)
  let string_of_lookahead_item = fun { item; lookahead_set } ->
    let lookahead_nums = lookahead_set
      |. SI.toArray
      |. Belt.Array.map (fun t_num -> Belt.Map.Int.get terminal_names t_num
        |> get_option' (Printf.sprintf
          "Lr0 string_of_lookahead_item: unable to find terminal %n in terminal_names"
          t_num
        )
      )
      |. Js.Array2.joinWith "/"
    in
    Printf.sprintf "[%s, %s]" (string_of_item item) lookahead_nums

  let string_of_lookahead_item_set = fun lookahead_item_set ->
    lookahead_item_set
      |. S.toArray
      |. Belt.Array.map string_of_lookahead_item
      |. Js.Array2.joinWith "\n"

  let string_of_production : production_num -> string
    = fun production_num ->

      let production = production_map
        |. MMI.get production_num
        |> get_option' (Printf.sprintf
          "Lr0 string_of_production: unable to find production %n in production_map"
          production_num
        )
      in

      let nt_num = production_nonterminal_map
        |. MMI.get production_num
        |> get_option' (Printf.sprintf
          "Lr0 string_of_production: unable to find production %n in production_nonterminal_map"
          production_num
        )
      in

      let rhs = production
        |. L.map string_of_symbol
        |. L.toArray
        |. Js.Array2.joinWith " "
      in

      let nt_name = nonterminal_names
        |. M.get nt_num
        |> get_option' (Printf.sprintf
          "Lr0 string_of_production: unable to find nonterminal %n in nonterminal_names"
          production_num
        )
      in

      Printf.sprintf "%s -> %s" nt_name rhs

  (* Used for logging actions. Compare with action_abbrev. *)
  let string_of_action : action -> string
    = function
    | Shift state -> "shift to " ^ string_of_int state
    | Reduce prod -> "reduce by " ^ string_of_production prod
    | Accept      -> "accept"
    | Error       -> "error"

  let production_cnt = ref 0
  let () = M.forEach G.grammar.nonterminals
    (fun nt_num { productions } ->
      nonterminal_production_map |. MMI.set nt_num (MSI.make ());
      L.forEach productions (fun production ->
        let production_num = !production_cnt in
        production_cnt := production_num + 1;
        production_map |. MMI.set production_num production;
        production_nonterminal_map |. MMI.set production_num nt_num;
        let prod_set = nonterminal_production_map
          |. MMI.get nt_num
          |> get_option'
            ("Lr0 preprocessing -- unable to find nonterminal " ^
              string_of_int nt_num)
        in
        (* Printf.printf "Adding prod %n to nt %n set\n" production_num nt_num; *)
        prod_set |. MSI.add production_num
      )
    )

  let get_nonterminal_num : production_num -> nonterminal_num
    = fun p_num -> production_nonterminal_map
        |. MMI.get p_num
        |> get_option' ("get_nonterminal_num: couldn't find production " ^
          string_of_int p_num)

  let get_nonterminal : production_num -> nonterminal
    = fun pn -> get_option'
        ("get_nonterminal: couldn't find production " ^ string_of_int pn) @@
      M.get G.grammar.nonterminals @@
      get_nonterminal_num pn

  let lr1_closure' : lookahead_item_set -> lookahead_configuration_set
    = fun initial_items ->

      (* Map from nonterminal number to a (mutable) set of tokens in the
       * lookahead set
       *)
      let nonkernel_items = MMI.make () in

      (* Set of (nonterminal, terminal that might be in its lookahead set) to
       * consider.
       *)
      let stack = MStack.make () in

      (* For each initial item, add an entry to the stack containing a
       * nonterminal and lookahead set to consider.
       * TODO: should this be a set of lookahead tokens instead of a single one?
       *)
      S.forEach initial_items (fun lookahead_item ->
        let { item; lookahead_set } = lookahead_item in
        let { production_num; position } = view_item item in
        let production = production_map
            |. MMI.get production_num
            |> get_option' (Printf.sprintf
              "lr0_closure': couldn't find production %n"
              production_num
            )
        in
        (* Printf.printf "considering lookahead item %s\n" (string_of_lookahead_item lookahead_item); *)
        (* Printf.printf "it has %n items in its lookahead set\n" (SI.size lookahead_set); *)
        lookahead_set |. SI.forEach (fun lookahead_terminal_num ->
          (* first symbol right of the dot *)
          match L.get production position with
            | Some (Nonterminal nt) ->
              let first_set' = match L.get production (position + 1) with
                | None -> first_set [Terminal lookahead_terminal_num]
                | Some (Nonterminal nt') -> first_set [Nonterminal nt'; Terminal lookahead_terminal_num]
              in
              (*
              Printf.printf "its first_set is %n items: [%s]\n"
                (SI.size first_set')
                (first_set' |. SI.toList |> Util.stringify_list string_of_terminal ", ");
                *)
              SI.forEach
                first_set'
                (fun new_lookahead ->
                  (*
                  Printf.printf "stack <- nonterminal %s, lookahead %s\n"
                    (string_of_nonterminal nt)
                    (string_of_terminal new_lookahead);
                    *)
                  MStack.push stack (nt, new_lookahead)
                )
            | _                     -> ()
        )
      );

      (* Consider every (nonterminal, lookahead terminal pair), adding it to
       * the nonkernel_items set if appropriate, and adding new entries to the
       * stack if appropriate
       *)
      while not (MStack.isEmpty stack) do
        let (nonterminal_num, lookahead) =
          get_option' "the set is not empty!" @@ MStack.pop stack
        in

        (*
        Printf.printf "stack -> nonterminal %s, lookahead %s\n"
          (string_of_nonterminal nonterminal_num)
          (string_of_terminal lookahead);
          *)

        let is_added = MMI.has nonkernel_items nonterminal_num &&
          nonkernel_items |. MMI.getExn nonterminal_num |. MSI.has lookahead
        in

        if not is_added then (
          (*
          Printf.printf "adding nonterminal %s, lookahead %s\n"
            (string_of_nonterminal nonterminal_num) (string_of_terminal lookahead);
            *)
          (match MMI.get nonkernel_items nonterminal_num with
            | None -> MMI.set nonkernel_items nonterminal_num
              (MSI.fromArray [| lookahead |])
            | Some old_set -> MSI.add old_set lookahead);

          let { productions } = M.get G.grammar.nonterminals nonterminal_num
            |> get_option' (Printf.sprintf
            "lr0_closure': unable to find nonterminal %n in G.grammar.nonterminals"
            nonterminal_num
            )
          in

          (* Printf.printf "productions: %n\n" (Belt.List.length productions); *)

          L.forEach productions (function
            | Terminal _         :: _ -> ()
            | Nonterminal new_nt :: rest ->
              (* XXX do we need to modify lookahead? *)
              let first_set' = match rest with
                | [] -> first_set [Terminal lookahead]
                | symbol :: _ -> first_set [symbol; Terminal lookahead]
              in
              SI.forEach first_set' (fun new_lookahead ->
                (*
                Printf.printf "stack <- nonterminal %s, lookahead %s\n"
                  (string_of_nonterminal new_nt)
                  (string_of_terminal new_lookahead);
                  *)
                MStack.push stack (new_nt, new_lookahead)
              );
            | _                       -> failwith "Empty production"
          )
        )
      done;

      { kernel_items = initial_items;
        nonkernel_items = nonkernel_items
          |. MMI.toArray
          |. A.map (fun (nonterminal_num, mut_lookahead_set) ->
            let production_set =
              MMI.get nonterminal_production_map nonterminal_num
                |> get_option' (Printf.sprintf
                "lr0_closure': unable to find nonterminal %n nonterminal_production_map"
                nonterminal_num
                )
            in

            production_set
              |. MSI.toArray
              |. A.map (fun production_num ->
                let item = mk_item { production_num; position = 0 } in
                let lookahead_set = mut_lookahead_set
                  |. MSI.toArray
                  |. SI.fromArray
                in
                { item; lookahead_set }
              )
          )
          |. A.concatMany
          |. lookahead_item_set_from_array;
      }

  (** The closure of an item set. CPTT fig 4.32. *)
  let lr0_closure' : item_set -> configuration_set
    = fun initial_items ->
      let added = Bitstring.alloc number_of_nonterminals false in
      let nonkernel_items = MSI.make () in
      let nt_stack = MSI.make () in

      (* Create the set (nt_stack) of nonterminals to look at *)
      SI.forEach initial_items (fun item ->
        let { production_num; position } = view_item item in
        let production =
          get_option' (Printf.sprintf
            "lr0_closure': couldn't find production %n"
            production_num
          )
          @@ MMI.get production_map production_num
        in
        (* first symbol right of the dot *)
        match L.get production position with
          | Some (Nonterminal nt) -> MSI.add nt_stack nt
          | _                     -> ()
        );

      while not (MSI.isEmpty nt_stack) do
        let nonterminal_num =
          get_option' "the set is not empty!" @@ MSI.minimum nt_stack
        in
        MSI.remove nt_stack nonterminal_num;
        let is_added = added
          |. Bitstring.get nonterminal_num
          |> get_option' (Printf.sprintf
            "lr0_closure': couldn't find nonterminal %n in added (nonterminal count %n)"
            nonterminal_num
            (Bitstring.length added)
          )
        in
        if not is_added then (
          Bitstring.setExn added nonterminal_num true;
          let production_set =
            MMI.get nonterminal_production_map nonterminal_num
              |> get_option' (Printf.sprintf
              "lr0_closure': unable to find nonterminal %n nonterminal_production_map"
              nonterminal_num
              )
          in
          MSI.forEach production_set (fun production_num ->
            MSI.add nonkernel_items (mk_item' production_num 0)
          );
          let { productions } = M.get G.grammar.nonterminals nonterminal_num
            |> get_option' (Printf.sprintf
            "lr0_closure': unable to find nonterminal %n in G.grammar.nonterminals"
            nonterminal_num
            )
          in
          L.forEach productions (fun production ->
            match production with
              | Terminal _         :: _ -> ()
              | Nonterminal new_nt :: _ -> MSI.add nt_stack new_nt
              | _                       -> failwith "Empty production"
          )
        )
      done;
      { kernel_items = initial_items;
        nonkernel_items = nonkernel_items |. MSI.toArray |. SI.fromArray;
      }

  (* closure returning an item set (rather than a configuration set) *)
  let lr0_closure : item_set -> item_set
    = fun items -> simplify_config_set @@ lr0_closure' items

  let lr1_closure : lookahead_item_set -> lookahead_item_set
    = fun items -> simplify_lookahead_config_set @@ lr1_closure' items

  (* Quoting CPTT:
   *
   * > Intuitively, the GOTO function is used to defined the transitions in the
   * > LR(0) automaton for a grammar. The states of the automaton correspond to
   * > sets of items, and GOTO(I, X) specifies the transition from the state
   * > for I under input X.
   *
   * Examine for items with the nonterminal immediately to the right of the
   * dot. Move the dot over the nonterminal.
   *)
  let lr0_goto_kernel : item_set -> symbol -> item_set
    = fun item_set symbol ->
      let result = MSI.make () in
      SI.forEach (lr0_closure item_set) (fun item ->
        let { production_num; position } = view_item item in
        let production = production_map
          |. MMI.get production_num
          |> get_option' (Printf.sprintf
            "lr0_goto_kernel: unable to find production %n in production_map"
            production_num
          )
        in
        match L.get production position with
          | Some next_symbol ->
            if symbol = next_symbol then
              result |. MSI.add (mk_item' production_num (position + 1))
          | _ -> ()
      );
      result |. MSI.toArray |. SI.fromArray

  let lr1_goto_kernel : lookahead_item_set -> symbol -> lookahead_item_set
    = fun item_set symbol ->
      let result = MSet.make ~id:(module LookaheadItemCmp) in
      S.forEach (lr1_closure item_set) (fun { item; lookahead_set } ->
        let { production_num; position } = view_item item in
        let production = production_map
          |. MMI.get production_num
          |> get_option' (Printf.sprintf
            "lr0_goto_kernel: unable to find production %n in production_map"
            production_num
          )
        in
        match L.get production position with
          | Some next_symbol ->
            if symbol = next_symbol then
              result |. MSet.add
                { item = mk_item' production_num (position + 1);
                  lookahead_set;
                }
          | _ -> ()
      );
      result |. MSet.toArray |. lookahead_item_set_from_array

  (* A list of all grammar symbols (terminals and nonterminals) *)
  let grammar_symbols = L.concat
    (L.makeBy number_of_terminals    (fun n -> Terminal n))
    (L.makeBy number_of_nonterminals (fun n -> Nonterminal n))

  (** Compute the canonical collection of sets of LR(0) items. CPTT fig 4.33. *)
  let mutable_lr0_items : mutable_lr0_item_set
    = let augmented_start = SI.fromArray
          [| mk_item {production_num = 0; position = 0} |]
      in
      let c = MSet.fromArray [| augmented_start |] ~id:(module ComparableIntSet)
      in

      (* iterate through every set of items in the collection, compute the GOTO
       * kernel of each item set, and add any new sets. `continue` is set to
       * `true` if we find a new item set, indicating we need to loop again. *)
      (* TODO: don't examine item sets we've already used *)
      let continue = ref true in
      while !continue do
        continue := false;
        (* for each set of items in c: *)
        MSet.forEach c @@ fun items ->
          (* for each grammar symbol: *)
          L.forEach grammar_symbols @@ fun symbol ->
            let goto_items_symbol = lr0_goto_kernel items symbol in
            (* if GOTO(items, symbol) is not empty and not in c: *)
            if not (SI.isEmpty goto_items_symbol) &&
               not (MSet.has c goto_items_symbol)
            then (
              MSet.add c goto_items_symbol;
              continue := true
            )
      done;
      c

  let mutable_lr1_items : mutable_lookahead_item_set
    = let augmented_start : lookahead_item_set = lookahead_item_set_from_array
        [|
           { item = mk_item {production_num = 0; position = 0};
             lookahead_set = SI.fromArray [| 0 |];
           }
        |]
      in
      let c = MSet.fromArray [| augmented_start |]
                             ~id:(module LookaheadItemSetCmp)
      in
      let continue = ref true in
      while !continue do
        continue := false;
        (* for each set of items in c: *)
        MSet.forEach c @@ fun items ->
          (* for each grammar symbol: *)
          L.forEach grammar_symbols @@ fun symbol ->
            let goto_items_symbol = lr1_goto_kernel items symbol in
            (* if GOTO(items, symbol) is not empty and not in c: *)
            if not (S.isEmpty goto_items_symbol) &&
               not (MSet.has c goto_items_symbol)
            then (
              MSet.add c goto_items_symbol;
              continue := true
            )
      done;
      c

  let lr0_items : item_set M.t
    = mutable_lr0_items
    |. MSet.toArray
    |. A.mapWithIndex (fun i item_set -> i, item_set)
    |. M.fromArray

  let lr1_items : lookahead_item_set M.t
    = mutable_lr1_items
    |. MSet.toArray
    |. A.mapWithIndex (fun i lookahead_item_set -> i, lookahead_item_set)
    |. M.fromArray

  let state_to_item_set : state -> item_set
    = fun state -> lr0_items
      |. M.get state
      |> get_option'
        ("state_to_item_set -- couldn't find state " ^ string_of_int state)

  let item_set_to_state : item_set -> state
    = fun item_set ->
    let state, _ = lr0_items
      |. M.findFirstBy (fun _ item_set' ->
        SI.toArray item_set' = SI.toArray item_set
      )
      |> get_option' (Printf.sprintf
        "item_set_to_state -- couldn't find item_set (%s) (options: %s)"
        (string_of_item_set item_set)
        (lr0_items
          |. M.valuesToArray
          |. A.map string_of_item_set
          |. Js.Array2.joinWith ", ")
      )
    in state

  let augmented_state : state
    = item_set_to_state @@ SI.fromArray [| mk_item' 0 0 |]

  let in_first_cache : (terminal_num * symbol, bool, SymbolCmp.identity) MM.t
    = MM.make ~id:(module SymbolCmp)

  let in_first : terminal_num -> symbol -> bool
    =
      (* We use this stack (which is a set) to track which nonterminals we're
       * currently looking through, to prevent loops *)
      let stack = MSI.make () in
      let rec in_first' t_num sym =
        match in_first_cache |. MM.get (t_num, sym) with
        | Some result -> result
        | None -> (match sym with
          | Terminal t_num'    -> t_num' = t_num
          | Nonterminal nt_num ->
          let { productions } = G.grammar.nonterminals
            |. M.get nt_num
            |> get_option' (Printf.sprintf
            "Lr0 in_first: unable to find nonterminal %n in G.grammar.nonterminals"
            nt_num
            )
          in
          let result, _ = Util.fold_right
            (* XXX doesn't use symbol? *)
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
          in_first_cache |. MM.set (t_num, sym) result;
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
          production_map |. MMI.forEach (fun prod_num production ->
            (* First look for this terminal following nt directly (rule 2) *)
            if in_follow'' nts_visited' t_num nt_num production
              then raise FoundInFollow;

            (* Then look for this terminal following nonterminals which end in
               nt. (rule 3)
               IE, if there is a production A -> xB, then everything in
               Follow(A) is in Follow(B)
             *)
            let nt_num' = production_nonterminal_map
              |. MMI.get prod_num
              |> get_option' (Printf.sprintf
              "Lr0 in_follow': unable to find nonterminal %n in production_nonterminal_map"
              prod_num
              )
            in
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

  (* This is the GOTO function operating on states. See `lr0_goto_kernel` for
   * the version operating on item set.
   *)
  let lr0_goto_table state nt =
    try
      Some (item_set_to_state @@ lr0_goto_kernel (state_to_item_set state) nt)
    with
      (* TODO: this shouldn't catch all invariant violations *)
      Util.InvariantViolation _ -> None

  let lr0_action_table state terminal_num =
    let item_set = lr0_closure @@ state_to_item_set state in

    let item_set_l = SI.toList item_set in

    (* If [A -> xs . a ys] is in I_i and GOTO(I_i, a) = I_j, set
     * ACTION[i, a] to `shift j` *)
    let shift_action = item_set_l
      |. Util.find_by (fun item ->
        let { production_num; position } = view_item item in
        let symbols = production_map
          |. MMI.get production_num
          |> get_option' (Printf.sprintf
          "Lr0 shift_action: unable to find production %n in production_map"
          production_num
          )
        in
        match symbols |. L.get position  with
          | Some (Terminal t_num as next_symbol) ->
            if t_num = terminal_num
              then lr0_goto_table state next_symbol
                |. Belt.Option.map (fun x -> Shift x)
              else None
          | _ -> None
      )
    in

    (* If [A -> xs .] is in I_i, set ACTION[i, a] to `Reduce A -> a` for
     * all a in FOLLOW(A) *)
    let reduce_action = item_set_l
      |. Util.find_by (fun item ->
        let { production_num; position } = view_item item in
        let nt_num = production_nonterminal_map
          |. MMI.get production_num
          |> get_option' (Printf.sprintf
          "Lr0 shift_action: unable to find production %n in production_nonterminal_map"
          production_num
          )
        in
        let production = production_map
          |. MMI.get production_num
          |> get_option' (Printf.sprintf
          "Lr0 shift_action: unable to find production %n in production_map"
          production_num
          )
        in
        if position = L.length production &&
           in_follow terminal_num nt_num &&
           (* Accept in this case (end marker on the augmented nonterminal) --
              don't reduce. *)
           nt_num != 0
          then Some (Reduce production_num)
          else None
      )
    in

    (* If [S' -> S .] is in I_i, set ACTION[i, $] to `accept` *)
    let accept_action =
      if terminal_num = end_marker && item_set |. SI.has (mk_item' 0 1)
        then Some Accept
        else None
    in

    (* We should always have exactly one action, otherwise it's a
     * shift-reduce(-accept) conflict.
     *)
    match shift_action, reduce_action, accept_action with
      | Some act,     None,     None
      |     None, Some act,     None
      |     None,     None, Some act -> act
      |        _,        _,        _ -> Error

  (* TODO: is this right? *)
  let states : state array =
    A.makeBy (M.size lr0_items) Util.id
  let terminals : terminal_num array =
    (* Add one to include the `$` terminal *)
    A.makeBy (number_of_terminals + 1) Util.id
  let nonterminals : nonterminal_num array =
    A.makeBy (MS.size terminal_nums) Util.id

  let full_lr0_action_table : unit -> action array array
    = fun () -> states |. A.map (fun state ->
      terminals |. A.map (lr0_action_table state)
    )

  let full_lr0_goto_table : unit -> (symbol * state option) array array
    = fun () -> states
      |. A.map (fun state ->
      nonterminals
        |. A.map (fun nt -> Nonterminal nt)
        |. A.map (fun sym ->
            sym, lr0_goto_table state sym
        )
    )

  let token_to_terminal
    : Lex.token -> terminal_num
    = fun { name } -> terminal_nums
        |. MS.get name
        |> get_option' (Printf.sprintf
        "Lr0 token_to_terminal: unable to find name %s in terminal_nums"
        name
        )

  let token_to_symbol
    : Lex.token -> symbol
    = fun { name } ->
      let t_match = terminal_nums |. MS.get name in
      let nt_match = nonterminal_nums |. MS.get name in
      match t_match, nt_match with
        | Some t_num, None -> Terminal t_num
        | None, Some nt_num -> Nonterminal nt_num
        | None, None -> failwith
          ("Failed to find a terminal or nonterminal named " ^ name)
        | Some _, Some _ -> failwith
          ("Found both a terminal *and* nonterminal with name " ^ name
          ^ " (this should never happen)")

  let string_of_symbols : parse_result array -> string
    = fun parse_results -> parse_results
      |. A.map (fun { production } -> match production with
        | Left terminal_num -> terminal_names
          |. M.get terminal_num
          |> get_option' (Printf.sprintf
            "string_of_symbols: failed to get terminal %n"
            terminal_num
          )
        | Right production_num ->
          let nt_num = production_nonterminal_map
            |. MMI.get production_num
            |> get_option' (Printf.sprintf
              "Lr0 string_of_item: unable to find production %n in production_nonterminal_map"
              production_num
            )
          in

          let nt_name = nonterminal_names
            |. M.get nt_num
            |> get_option' (Printf.sprintf
              "Lr0 string_of_production: unable to find nonterminal %n in nonterminal_names"
              production_num
            )

          in nt_name
      )
      |. Js.Array2.joinWith " "

  (* This is the main parsing function: CPTT Algorithm 4.44 / Figure 4.36. *)
  let parse_trace
    : do_trace (* trace or not *)
    -> Lex.token MQueue.t
    -> (parse_result, parse_error) Result.t *
       (action * state array * parse_result array * Lex.token array) array
    = fun do_trace toks ->
      (* Re stack / results:
       * These are called `stack` and `symbols` in CPTT. Their structure
       * mirrors one another: there is a 1-1 correspondence between states in
       * `stack` and symbols in `results`, expept that `stack` always has
       * `augmented_state`, at the bottom of its stack.
       *)
      let stack : state MStack.t = MStack.make () in
      MStack.push stack augmented_state;
      let results : parse_result MStack.t = MStack.make () in
      let trace = MQueue.make () in
      try
        let a = ref @@ pop_front_exn 0 toks in
        while true do
          (* let x be the state on top of the stack *)
          let s = match MStack.top stack with
            | Some s' -> s'
            | None -> failwith "invariant violation: empty stack"
          in
          let tok = !a in
          let terminal_num = token_to_terminal tok in
          let action = lr0_action_table s terminal_num in
          if do_trace = DoTrace then
            MQueue.add trace
              ( action,
                Util.array_of_stack stack,
                Util.array_of_stack results,
                MQueue.toArray toks
              );
          match action with
            | Shift t ->
                MStack.push stack t;
                MStack.push results
                  { production = Either.Left terminal_num;
                    children = [];
                    start_pos = tok.start;
                    end_pos = tok.finish;
                  };
                a := pop_front_exn tok.start toks;
            | Reduce production_num ->
                let pop_count = production_map
                  |. MMI.get production_num
                  |> get_option' (Printf.sprintf
                    "Lr0 parse_trace: unable to find production %n in production_map"
                    production_num
                  )
                  |. L.length
                in
                (* pop symbols off the stack *)
                let children : parse_result list ref = ref [] in
                let start_pos : int ref = ref 0 in
                let end_pos : int ref = ref 0 in
                for i = 1 to pop_count do
                  let _ = MStack.pop stack in
                  match MStack.pop results with
                    | Some child -> (
                      children := child :: !children;
                      (* Note: children appear in reverse *)
                      if i = pop_count then start_pos := child.start_pos;
                      if i = 1 then end_pos := child.end_pos;
                    )
                    | None -> failwith
                      "invariant violation: popping from empty stack"
                done;

                let nt_num = production_nonterminal_map
                  |. MMI.get production_num
                  |> get_option' (Printf.sprintf
                    "Lr0 parse_trace: unable to find production %n in production_nonterminal_map"
                    production_num
                  )
                in
                (match MStack.top stack with
                  | Some t -> (match lr0_goto_table t (Nonterminal nt_num) with
                      | None -> failwith
                        "invariant violation: invalid GOTO transition"
                      | Some state -> MStack.push stack state
                  )
                  | None -> failwith "invariant violation: peeking empty stack"
                );

                MStack.push results
                  { production = Either.Right production_num;
                    children = !children;
                    start_pos = !start_pos;
                    end_pos = !end_pos;
                  };
            | Accept -> raise ParseFinished
            | Error ->
                raise (ParseFailed
                  ( tok.start
                  (* TODO: give a decent error message *)
                  , Printf.sprintf
                    "parse failed -- no valid transition on this token (%s)"
                    tok.name
                  ))
            ;
        done;
        failwith "invariant violation: can't make it here"
      with
        | ParseFinished -> (match MStack.size results with
          | 1 -> (match MStack.top results with
            | Some result -> Result.Ok result, MQueue.toArray trace
            | None -> failwith "invariant violation: no result"
            )
          | 0 -> failwith "invariant violation: no result"
          | n -> failwith (Printf.sprintf
            "invariant violation: multiple results (%n)"
            n
          )
        )
        | ParseFailed parse_error -> (Error parse_error, MQueue.toArray trace)
        | PopFailed pos
        -> (Error (pos, "parsing invariant violation -- pop failed"), MQueue.toArray trace)

  let parse : Lex.token MQueue.t -> (parse_result, parse_error) Result.t
    = fun toks -> match parse_trace DontTrace toks with result, _ -> result

  let lex_and_parse : Lex.lexer -> string
    -> (parse_result, (Lex.lex_error, parse_error) Either.t) Result.t
    = fun lexer input -> match Lex.lex lexer input with
      | Error error -> Error (Left error)
      | Ok tokens ->
        let len = String.length input in
        let tokens' = tokens
          |. Belt.Array.keep (fun token -> token.name != "SPACE")
          |. MQueue.fromArray
        in
        (* TODO: name might not always be "$" *)
        MQueue.add tokens' { name = "$"; start = len; finish = len };
        parse tokens' |. Util.map_error (fun err -> Either.Right err)

end
