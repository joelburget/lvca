open Core_kernel
module MMI = Core_kernel.Int.Table
module MSet = Hash_set
module MSI = Int.Hash_set
module MStack = Core_kernel.Stack
module MQueue = Core_kernel.Queue
let get_option, get_option', invariant_violation =
  Util.(get_option, get_option', invariant_violation)
module Lex = Placemat.Lex

exception NoItemSet of (unit -> string)

(* by convention, we reserve 0 for the `$` terminal, and number the rest
 * contiguously from 1 *)
type terminal_num = int
  [@@deriving sexp, compare]

(* by convention, we reserve 0 for the root nonterminal, and number the rest
 * contiguously from 1 *)
type nonterminal_num = int
  [@@deriving sexp, compare]

type symbol =
  | Terminal    of terminal_num
  | Nonterminal of nonterminal_num
  [@@deriving sexp, compare]

module SymbolCmp = Comparable.Make(struct
  type t = terminal_num * symbol [@@deriving sexp, compare]
end)

(** A state number *)
type state = int

type production = symbol list

type production_num = int

(** A production with a dot at some position.
 *
 * We encode this as a single integer -- see [item], [mk_item], [mk_item'], and
 * [view_item].
 *
 * See also [level_view].
*)
type item_view =
  { production_num: production_num
  (** The number of one of the productions of the underlying grammar *)
  ; position: int
  (** The position of the dot *)
  }

(** An LR(0) item encodes a pair of integers, namely the index of the
 * production and the index of the bullet in the production's
 * right-hand side.
 *
 * Both integers are packed into a single integer, using 8 bits for
 * the bullet position and the remaining 24 bits for the
 * production index.
 *
 * Note that this implies some maximums:
 * The maximum length of a production is 255
 * The maximum number of different productions is 16777215
*)
type item = int

let view_item : item -> item_view
  = fun item ->
    { production_num = item land 0x00ffffff;
      position = (item land 0xff000000) lsr 24
    }

let mk_item' : int -> int -> item
  = fun production_num position -> (position lsl 24) lor production_num

type item_set = Int.Set.t

let mk_item : item_view -> item
  = fun { production_num; position } -> mk_item' production_num position

type configuration_set =
  { kernel_items : item_set    (* set of items *)
  ; nonkernel_items : item_set (* set of nonterminals *)
  }

let simplify_config_set : configuration_set -> item_set
  = fun { kernel_items; nonkernel_items } ->
    Int.Set.union kernel_items nonkernel_items

type nonterminal =
  { (* nonterminal_num: nonterminal_num; *)
    productions: production list;
  }

type prec_level = int

(* By convention:
 * A non-augmented grammar has keys starting at 1 (start)
 * An augmented grammar has key 0 (augmented start)
*)
type grammar = {
  nonterminals : nonterminal Int.Map.t;
  terminal_nums : (string * terminal_num) array;
  (* TODO: better name: nonterminal_info? *)
  nonterminal_nums : (string * nonterminal_num) array;
}

type conflict_type = ShiftReduce | ReduceReduce

type action =
  | Shift  of state
  | Reduce of production_num
  | Accept
  | Error of conflict_type option

(* Our action / goto table formulations are lazy (not actually tables). Tables
 * can be computed with `full_action_table` / `full_goto_table` *)
type lr0_action_table = state -> terminal_num -> action
type lr0_goto_table = state -> symbol -> state option

module ComparableIntSet = Comparable.Make(struct
  type t = Int.Set.t [@@deriving sexp, compare]
end)

(* A mutable set of int sets. This is used to represent the set of LR(0) items.
 * Each int set represents a set of encoded items.
*)
type mutable_lr0_item_set = Int.Set.t MSet.t

type item_set_set = (item_set, ComparableIntSet.comparator_witness) Set.t

type parse_error = int (* character number *) * string

type parse_result =
  { production : (terminal_num, production_num) Either.t;
    children : parse_result list;
    start_pos : int; (* inclusive *)
    end_pos : int; (* exclusive *)
  }

let rec parse_result_to_string : parse_result -> string
  = fun { production; children; _ } -> Printf.sprintf "%s[%s]"
                                      (match production with
                                       | First n -> "t" ^ string_of_int n
                                       | Second n -> "n" ^ string_of_int n
                                      )
                                      (Util.stringify_list parse_result_to_string ", " children)

exception ParseFinished
exception ParseFailed of parse_error
exception PopFailed of int

type do_trace =
  | DoTrace
  | DontTrace

type trace_line =
  { action : action ;
    stack : state array;
    results : parse_result array;
    input : Lex.token array;
  }

let pop_front_exn : int -> 'a MQueue.t -> 'a
  = fun position arr -> match MQueue.dequeue arr with
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
  val string_of_symbol : symbol -> string
  val string_of_terminal : terminal_num -> string
  val string_of_production_num : production_num -> string
  val string_of_production : production -> string
  val string_of_nonterminal : nonterminal -> string
  val states : state array
end

(* Used for action table entries. Compare with string_of_action. *)
let action_abbrev : action -> string
  = function
    | Shift state -> "s" ^ string_of_int state
    | Reduce prod -> "r" ^ string_of_int prod
    | Accept      -> "acc"
    | Error None  -> ""
    | Error (Some ShiftReduce) -> "s/r"
    | Error (Some ReduceReduce) -> "r/r"

let string_of_stack : state array -> string
  = fun states -> states
                  |> Array.map ~f:string_of_int
                  |> String.concat_array ~sep:" "

(* TODO: remove exns *)
module Lr0 (G : GRAMMAR) = struct

  (* Map from production number to the actual production *)
  let production_map : production Core_kernel.Int.Table.t
    = MMI.create ()

  (* Map from production number to the number of the nonterminal it belongs to
  *)
  let production_nonterminal_map : nonterminal_num Core_kernel.Int.Table.t
    = MMI.create ()

  (* Map from a nonterminal num to the set of productions it holds *)
  let nonterminal_production_map : MSI.t Core_kernel.Int.Table.t
    = MMI.create ()

  (* number of nonterminals in the passed-in grammar (which ought to be
   * augmented) *)
  let number_of_nonterminals : int
    = Int.Map.length G.grammar.nonterminals

  let number_of_terminals : int
    = Array.length G.grammar.terminal_nums

  let terminal_names : string Int.Map.t
    = G.grammar.terminal_nums
      |> Array.map ~f:(fun (name, num) -> num, name)
      |> Array.to_list
      |> Int.Map.of_alist
      |> (function
        | `Ok map -> map
        | `Duplicate_key num -> Util.invariant_violation (Printf.sprintf
          "terminal_names: duplicate terminal number: %n" num
        )
      )

  let terminal_nums : int String.Map.t
    = String.Map.from_array G.grammar.terminal_nums

  let nonterminal_names : string Int.Map.t
    = G.grammar.nonterminal_nums
      |> Array.map ~f:Tuple2.swap
      |> Int.Map.from_array

  let nonterminal_nums : nonterminal_num String.Map.t
    = String.Map.from_array G.grammar.nonterminal_nums

  let string_of_nonterminal_num : nonterminal_num -> string
    = fun nt_num -> nonterminal_names
      |> Int.Map.find nt_num
      |> get_option' (fun () -> Printf.sprintf
         "string_of_nonterminal_num: failed to get nonterminal %n from \
         nonterminal_names"
         nt_num
      )

  let rec nonterminal_first_set : Int.Set.t -> MSI.t -> nonterminal_num -> unit
    = fun already_seen_nts result nt ->
      let productions : MSI.t = nonterminal_production_map
        |> MMI.find nt
        |> get_option' (fun () -> Printf.sprintf
          "nonterminal_first_set->already_seen_nts: Couldn't find nonterminal \
          %s in nonterminal_production_map"
          (string_of_nonterminal_num nt)
        )
        |> MSI.copy
      in

      while not (MSI.is_empty productions) do
        let production_num = productions
          |. MSI.minimum
          |> get_option' (fun () ->
            "nonterminal_first_set->already_seen_nts: minimum of productions \
            must exist"
          )
        in
        MSI.remove productions production_num;
        let production = production_map
          |> MMI.find production_num
          |> get_option' (fun () -> Printf.sprintf
            "nonterminal_first_set->already_seen_nts: Couldn't find production \
            %n in production_map"
            production_num
          )
        in
        first_set' already_seen_nts result production
      done;

  and first_set' : Int.Set.t -> MSI.t -> production -> unit
    = fun already_seen_nts result -> function
      | [] -> failwith
                "invariant violation: first_set must be called with a non-empty list"
      | Terminal num :: _ -> MSI.add result num
      (* TODO: update when we allow empty productions *)
      | Nonterminal num :: _
        -> if not (Int.Set.has already_seen_nts ~data:num)
        then nonterminal_first_set (Int.Set.add already_seen_nts ~data:num) result num

  let first_set : production -> Int.Set.t
    = function
      | [] -> Int.Set.empty
      | prod ->
        let result = MSI.make () in
        let already_seen_nts = Int.Set.empty in
        first_set' already_seen_nts result prod;
        result |> MSI.to_list |> Int.Set.of_list

  let string_of_terminal : terminal_num -> string
    = fun t_num -> terminal_names
                   |> Int.Map.find t_num
                   |> get_option' (fun () -> Printf.sprintf
                                     "string_of_symbol: failed to get terminal %n"
                                     t_num
                                  )

  let string_of_symbol : symbol -> string
    = function
      | Terminal t_num -> string_of_terminal t_num
      | Nonterminal nt_num -> string_of_nonterminal_num nt_num

  let string_of_item : item -> string
    = fun item ->
      let { production_num; position } = view_item item in
      let production = production_map
                       |> MMI.find production_num
                       |> get_option' (fun () -> Printf.sprintf
                                         "Lr0 string_of_item: unable to find production %n in production_map"
                                         production_num
                                      )
      in
      let pieces = MQueue.create () in
      List.iteri production ~f:(fun i symbol ->
        if position = i then MQueue.enqueue pieces ".";
        MQueue.enqueue pieces (string_of_symbol symbol);
      );

      if position = List.length production then
        MQueue.enqueue pieces ".";

      let nt_num = production_nonterminal_map
                   |> MMI.find production_num
                   |> get_option' (fun () -> Printf.sprintf
                                     "Lr0 string_of_item: unable to find production %n in production_nonterminal_map"
                                     production_num
                                  )
      in

      let nt_name = nonterminal_names
                    |> Int.Map.find nt_num
                    |> get_option' (fun () -> Printf.sprintf
                                      "Lr0 string_of_item: unable to find nonterminal %n in nonterminal_names"
                                      production_num
                                   )
      in

      (* output if trailing *)
      Printf.sprintf "%s -> %s" nt_name
        (String.concat_array (MQueue.to_array pieces) ~sep:" ")

  let string_of_item_set : ?sep:string -> item_set -> string
    = fun ?(sep=" ") item_set -> match Int.Set.length item_set with
      | 0 -> "empty"
      | _ -> item_set
        |> Int.Set.to_list
        |> Util.stringify_list string_of_item sep

  let string_of_production : production -> string
    = fun production -> production
                        |> List.map ~f:string_of_symbol
                        |> Array.of_list
                        |> String.concat_array ~sep:" "

  let string_of_production_num : production_num -> string
    = fun production_num ->

      let nt_num = production_nonterminal_map
                   |> MMI.find production_num
                   |> get_option' (fun () -> Printf.sprintf
                                     "Lr0 string_of_production: unable to find production %n in production_nonterminal_map"
                                     production_num
                                  )
      in

      let production = production_map
                       |> MMI.find production_num
                       |> get_option' (fun () -> Printf.sprintf
                                         "Lr0 string_of_production_num: unable to find production %n in production_map"
                                         production_num
                                      )
      in

      let nt_name = nonterminal_names
                    |> Int.Map.find nt_num
                    |> get_option' (fun () -> Printf.sprintf
                                      "Lr0 string_of_production: unable to find nonterminal %n in nonterminal_names"
                                      production_num
                                   )
      in

      Printf.sprintf "%s -> %s" nt_name (string_of_production production)

  let string_of_nonterminal : nonterminal -> string
    = fun { productions } ->
      Util.stringify_list string_of_production "\n" productions

  (* Used for logging actions. Compare with action_abbrev. *)
  let string_of_action : action -> string
    = function
      | Shift state -> "shift to " ^ string_of_int state
      | Reduce prod -> "reduce by " ^ string_of_production_num prod
      | Accept      -> "accept"
      | Error None  -> "error"
      | Error (Some ReduceReduce) -> "error (reduce/reduce conflict)"
      | Error (Some ShiftReduce) -> "error (shift/reduce conflict)"

  let production_cnt = ref 0
  let () = Int.Map.for_each G.grammar.nonterminals
             ~f:(fun nt_num { productions } ->
                nonterminal_production_map
                  |> MMI.add ~key:nt_num ~data:(MSI.make ());
                List.for_each productions ~f:(fun production ->
                  let production_num = !production_cnt in
                  incr production_cnt;
                  production_map
                    |> MMI.add ~key:production_num ~data:production;
                  production_nonterminal_map
                    |> MMI.add ~key:production_num ~data:nt_num;
                  let prod_set = nonterminal_production_map
                                 |> MMI.find nt_num
                                 |> get_option' (fun () ->
                                      "Lr0 preprocessing -- unable to find nonterminal " ^
                                       string_of_int nt_num)
                  in
                  prod_set |. MSI.add production_num
                )
             )

  let get_nonterminal_num : production_num -> nonterminal_num
    = fun p_num -> production_nonterminal_map
                   |> MMI.find p_num
                   |> get_option' (fun () ->
                     "get_nonterminal_num: couldn't find production " ^
                     string_of_int p_num)

  let get_nonterminal : production_num -> nonterminal
    = fun pn -> G.grammar.nonterminals
      |> Int.Map.find (get_nonterminal_num pn)
      |> get_option' (fun () ->
                  "get_nonterminal: couldn't find production " ^ string_of_int pn)

  (** The closure of an item set. CPTT fig 4.32. *)
  let lr0_closure' : item_set -> configuration_set
    = fun initial_items ->
      let added = Bitstring.alloc number_of_nonterminals false in
      let kernel_items = MSI.make () in
      let nonkernel_items = MSI.make () in
      let nt_set = MSI.make () in

      (* Create the set (nt_set) of nonterminals to look at. Add each initial
       * item to the kernel or nonkernel set. *)
      Int.Set.for_each initial_items ~f:(fun item ->
        let { production_num; position } = view_item item in

        if production_num = 0 || position > 0
        then MSI.add kernel_items item
        else MSI.add nonkernel_items item;

        let production = production_map
                         |> MMI.find production_num
                         |> get_option' (fun () -> Printf.sprintf
                                           "lr0_closure': couldn't find production %n"
                                           production_num
                                        )
        in
        (* first symbol right of the dot. if it's a nonterminal, look at it *)
        match List.nth production position with
        | Some (Nonterminal nt) -> MSI.add nt_set nt
        | _                     -> ()
      );

      (* Examine each accessible nonterminal, adding its initial items as
       * nonkernel items. *)
      while not (MSI.is_empty nt_set) do
        let nonterminal_num =
          get_option' (fun () -> "the set is not empty!") @@ MSI.minimum nt_set
        in
        MSI.remove nt_set nonterminal_num;
        let is_alrady_added = added
                              |. Bitstring.get nonterminal_num
                              |> get_option' (fun () -> Printf.sprintf
                                                "lr0_closure': couldn't find nonterminal %n in added (nonterminal count %n)"
                                                nonterminal_num
                                                (Bitstring.length added)
                                             )
        in
        if not is_alrady_added then (
          Bitstring.set_exn added nonterminal_num true;
          let production_num_set = nonterminal_production_map
                                   |> MMI.find nonterminal_num
                                   |> get_option' (fun () -> Printf.sprintf
                                                     "lr0_closure': unable to find nonterminal %n nonterminal_production_map"
                                                     nonterminal_num
                                                  )
          in
          MSI.for_each production_num_set (fun production_num ->
            MSI.add nonkernel_items (mk_item' production_num 0)
          );
          let { productions } = Int.Map.find G.grammar.nonterminals nonterminal_num
                                |> get_option' (fun () -> Printf.sprintf
                                                  "lr0_closure': unable to find nonterminal %n in G.grammar.nonterminals"
                                                  nonterminal_num
                                               )
          in
          List.for_each productions ~f:(fun production ->
            match production with
            | Terminal _         :: _ -> ()
            | Nonterminal new_nt :: _ -> MSI.add nt_set new_nt
            | _                       -> failwith "Empty production"
          )
        )
      done;

      { kernel_items = kernel_items |> MSI.to_list |> Int.Set.of_list;
        nonkernel_items = nonkernel_items |> MSI.to_list |> Int.Set.of_list;
      }

  (* closure returning an item set (rather than a configuration set) *)
  let lr0_closure : item_set -> item_set
    = fun items -> simplify_config_set @@ lr0_closure' items

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
      Int.Set.for_each (lr0_closure item_set) ~f:(fun item ->
        let { production_num; position } = view_item item in
        let production = production_map
                         |> MMI.find production_num
                         |> get_option' (fun () -> Printf.sprintf
                                           "lr0_goto_kernel: unable to find production %n in production_map"
                                           production_num
                                        )
        in
        match List.nth production position with
        | Some next_symbol ->
          if symbol = next_symbol then
            MSI.add result (mk_item' production_num (position + 1))
        | _ -> ()
      );
      result |> MSI.to_list |> Int.Set.of_list

  (* A list of all grammar symbols (terminals and nonterminals) *)
  let grammar_symbols = List.append
    (List.init number_of_terminals ~f:(fun n -> Terminal n))
    (List.init number_of_nonterminals ~f:(fun n -> Nonterminal n))

  (** Compute the canonical collection of sets of LR(0) items. CPTT fig 4.33. *)
  let mutable_lr0_items : mutable_lr0_item_set
    = let augmented_start = Int.Set.of_list
                              [ mk_item {production_num = 0; position = 0} ]
    in
    (* canonical collection of sets *)
    let c =
      MSet.from_array [| augmented_start |] ~id:(module ComparableIntSet)
    in
    (* set of item sets we've added to c but not yet explored *)
    let active_set = ref @@ MSet.copy c in

    (* iterate through every set of items in the collection, compute the GOTO
     * kernel of each item set, and add any new sets. `continue` is set to
     * `true` if we find a new item set, indicating we need to loop again. *)
    while not (MSet.is_empty !active_set) do
      let new_active_set = MSet.from_array [||] ~id:(module ComparableIntSet)
      in
      (* for each set of items in the active set: *)
      MSet.for_each !active_set ~f:(fun items ->
        (* for each grammar symbol: *)
        List.for_each grammar_symbols ~f:(fun symbol ->
          let goto_result = lr0_goto_kernel items symbol in
          (* if GOTO(items, symbol) is not empty and not in c: *)
          if not (Int.Set.isEmpty goto_result) &&
             not (MSet.has c ~data:goto_result)
          then (
            MSet.add c ~data:goto_result;
            MSet.add new_active_set ~data:goto_result;
          )
        )
      );
      active_set := new_active_set;
    done;
    c

  let lr0_items : item_set Int.Map.t
    = mutable_lr0_items
      |> MSet.to_array
      |> Array.mapi ~f:(fun i item_set -> i, item_set)
      |> Int.Map.from_array

  let state_to_item_set : state -> item_set
    = fun state -> lr0_items
                   |> Int.Map.find state
                   |> get_option' (fun () -> Printf.sprintf
                                     "state_to_item_set -- couldn't find state %n (%n item sets)"
                                     state
                                     (Int.Map.length lr0_items)
                                  )

  (** raises [NoItemSet] *)
  let item_set_to_state : item_set -> state
    = fun item_set ->
      let state, _ = lr0_items
                     |> Int.Map.find_first_by
                       ~f:(fun _ item_set' -> item_set' = item_set)
                     |> get_option (NoItemSet
                       (fun () -> Printf.sprintf
                          "item_set_to_state -- couldn't find item_set (%s) (options: %s)"
                          (string_of_item_set item_set)
                          (lr0_items
                            |> Int.Map.to_list
                            |> Util.stringify_list
                              (fun (_, item_set) -> string_of_item_set item_set)
                              ", ")
                          ))
      in state

  let augmented_state : state
    = item_set_to_state @@ Int.Set.of_list [ mk_item' 0 0 ]

  let end_marker : terminal_num
    = 0

  let rec follow' : Int.Set.t -> nonterminal_num -> Int.Set.t
    = fun nts_visited nt_num -> if nt_num = 0
    (* Rule 1 from the CPTT algorithm for FOLLOW(A):
     * $ is in FOLLOW(S), where S is the start symbol.
    *)
      then Int.Set.of_list [ end_marker ]

      (* For each production, accumulate the terminals it adds to the follow
       * set
      *)
      else production_map
           |> MMI.to_array
           |> Array.fold_left
                ~init:Int.Set.empty
                ~f:(fun (prod_num, production) follow_set ->
                   (* Rule 2 from the CPTT algorithm for FOLLOW(A):
                    * If there is a production A -> aBb, then everything in FIRST(b),
                    * except e, is in FOLLOW(B).
                   *)
                   let rule_2_follow_set = first_after_nt nt_num production in

                   let parent_nt = production_nonterminal_map
                                   |> MMI.find prod_num
                                   |> get_option' (fun () -> Printf.sprintf
                                                     "Lr0 follow': unable to find nonterminal %n in production_nonterminal_map"
                                                     prod_num
                                                  )
                   in

                   (* Rule 3 from the CPTT algorithm for FOLLOW(A):
                    * If there is a production A -> aB, or a production A -> aBb,
                    * where FIRST(b) contains e, then everything in FOLLOW(A) is in
                    * FOLLOW(B)
                   *)
                   let rule_3_follow_set = match Util.unsnoc production with
                     | _, Nonterminal nt_num'
                       when nt_num' = nt_num && not (Int.Set.has nts_visited ~data:nt_num)
                       -> follow' (Int.Set.add nts_visited ~data:nt_num) parent_nt
                     | _ -> Int.Set.empty
                   in

                   follow_set
                     |> Int.Set.union rule_2_follow_set
                     |> Int.Set.union rule_3_follow_set
                )

  (* Find all the terminals occuring in first sets directly after the
   * nonterminal *)
  and first_after_nt : nonterminal_num -> production -> Int.Set.t
    = fun nt_num -> function
      | Nonterminal nt_num' :: rest
        when nt_num' = nt_num
        -> Int.Set.union (first_set rest) (first_after_nt nt_num rest)
      | _ :: rest
        -> first_after_nt nt_num rest
      | []
        -> Int.Set.empty

  (* Compute the set of terminals that can appear immediately to the right of
   * nt in some production *)
  let follow_set : nonterminal_num -> Int.Set.t
    = follow' Int.Set.empty

  (* This is the GOTO function operating on states. See `lr0_goto_kernel` for
   * the version operating on item set.
   *
   * raises: [InvariantViolation]
  *)
  let lr0_goto_table state nt =
    try
      Some (item_set_to_state @@ lr0_goto_kernel (state_to_item_set state) nt)
    with
      NoItemSet _ -> None

  let lr0_action_table state terminal_num =
    let item_set = lr0_closure @@ state_to_item_set state in

    let item_set_l = Int.Set.to_list item_set in

    (* If [A -> xs . a ys] is in I_i and GOTO(I_i, a) = I_j, set
     * ACTION[i, a] to `shift j` *)
    let shift_action = item_set_l
                       |> List.find_map ~f:(fun item ->
                         let { production_num; position } = view_item item in
                         let symbols = production_map
                                       |> MMI.find production_num
                                       |> get_option' (fun () -> Printf.sprintf
                                                         "Lr0 shift_action: unable to find production %n in production_map"
                                                         production_num
                                                      )
                         in
                         match List.nth symbols position with
                         | Some (Terminal t_num as next_symbol) ->
                           if t_num = terminal_num
                           then lr0_goto_table state next_symbol
                             |> Option.map ~f:(fun x -> Shift x)
                           else None
                         | _ -> None
                       )
    in

    (* If [A -> xs .] is in I_i, set ACTION[i, a] to `Reduce A -> xs` for
     * all a in FOLLOW(A) *)
    let reduce_action = item_set_l
                        |> List.find_map ~f:(fun item ->
                          let { production_num; position } = view_item item in
                          let nt_num = production_nonterminal_map
                                       |> MMI.find production_num
                                       |> get_option' (fun () -> Printf.sprintf
                                                         "Lr0 shift_action: unable to find production %n in production_nonterminal_map"
                                                         production_num
                                                      )
                          in
                          let production = production_map
                                           |> MMI.find production_num
                                           |> get_option' (fun () -> Printf.sprintf
                                                             "Lr0 shift_action: unable to find production %n in production_map"
                                                             production_num
                                                          )
                          in
                          if position = List.length production &&
                             follow_set nt_num |> Int.Set.has ~data:terminal_num &&
                             (* Accept in this case (end marker on the augmented nonterminal) --
                                don't reduce. *)
                             nt_num <> 0
                          then Some (Reduce production_num)
                          else None
                        )
    in

    (* If [S' -> S .] is in I_i, set ACTION[i, $] to `accept` *)
    let accept_action =
      if terminal_num = end_marker && item_set |> Int.Set.has ~data:(mk_item' 0 1)
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
    |   Some _,   Some _,     None -> Error (Some ShiftReduce)
    |        _,        _,        _ -> Error None

  (* TODO: is this right? *)
  let states : state array = Base.Array.initialize
    ~length:(Int.Map.length lr0_items)
    ~f:Fn.id
  let terminals : terminal_num array = Base.Array.initialize
    ~length:number_of_terminals
    ~f:Fn.id
  let nonterminals : nonterminal_num array = Base.Array.initialize
    ~length:(String.Map.length nonterminal_nums)
    ~f:Fn.id

  let full_lr0_action_table : unit -> action array array
    = fun () -> states |> Array.map ~f:(fun state ->
      terminals |> Array.map ~f:(lr0_action_table state)
    )

  let full_lr0_goto_table : unit -> (symbol * state option) array array
    = fun () -> states
                |> Array.map ~f:(fun state ->
                  nonterminals
                  |> Array.map ~f:(fun nt ->
                    let sym = Nonterminal nt in
                    sym, lr0_goto_table state sym
                  )
                )

  let token_to_terminal
    : Lex.token -> terminal_num
    = fun { name } -> terminal_nums
                      |> String.Map.find name
                      |> get_option' (fun () -> Printf.sprintf
                                        "Lr0 token_to_terminal: unable to find name %s in terminal_nums"
                                        name
                                     )

  let token_to_symbol
    : Lex.token -> symbol
    = fun { name } ->
      let t_match = terminal_nums |> String.Map.find name in
      let nt_match = nonterminal_nums |> String.Map.find name in
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
                           |> Array.map ~f:(fun { production } -> match production with
                             | First terminal_num -> terminal_names
                                                    |> Int.Map.find terminal_num
                                                    |> get_option' (fun () -> Printf.sprintf
                                                                      "string_of_symbols: failed to get terminal %n"
                                                                      terminal_num
                                                                   )
                             | Second production_num ->
                               let nt_num = production_nonterminal_map
                                            |> MMI.find production_num
                                            |> get_option' (fun () -> Printf.sprintf
                                                              "Lr0 string_of_symbols: unable to find production %n in production_nonterminal_map"
                                                              production_num
                                                           )
                               in

                               let nt_name = nonterminal_names
                                             |> Int.Map.find nt_num
                                             |> get_option' (fun () -> Printf.sprintf
                                                               "Lr0 string_of_symbols: unable to find nonterminal %n in nonterminal_names"
                                                               production_num
                                                            )

                               in nt_name
                           )
                           |> String.concat_array ~sep:" "

  let string_of_trace_line = fun { action; stack; results; input } ->
    Printf.sprintf "action: %s\nstack: %s\nresults: %s\ninput: %s\n\n"
      (string_of_action action)
      (stack
       |> Array.map ~f:string_of_int
       |> String.concat_array ~sep:" ")
      (string_of_symbols results)
      (input
       |> Array.map ~f:(fun (tok : Lex.token) -> tok.name)
       |> String.concat_array ~sep:" ")

  (* This is the main parsing function: CPTT Algorithm 4.44 / Figure 4.36. *)
  let parse_trace_tables
    : lr0_action_table
      -> lr0_goto_table
      -> do_trace (* trace or not *)
      -> Lex.token MQueue.t
      -> (parse_error, parse_result) Result.t * trace_line array
    = fun lr0_action_table lr0_goto_table do_trace toks ->
      (* Re stack / results:
       * These are called `stack` and `symbols` in CPTT. Their structure
       * mirrors one another: there is a 1-1 correspondence between states in
       * `stack` and symbols in `results`, expept that `stack` always has
       * `augmented_state`, at the bottom of its stack.
      *)
      let stack : state MStack.t = MStack.create () in
      MStack.push stack augmented_state;
      let results : parse_result MStack.t = MStack.create () in
      let trace = MQueue.create () in
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
            MQueue.enqueue trace
              { action;
                stack = MStack.to_array stack;
                results = MStack.to_array results;
                input = MQueue.to_array toks;
              };
          match action with
          | Shift t ->
            MStack.push stack t;
            MStack.push results
              { production = Either.First terminal_num;
                children = [];
                start_pos = tok.start;
                end_pos = tok.finish;
              };
            a := pop_front_exn tok.start toks;
          | Reduce production_num ->
            let pop_count = production_map
                            |> MMI.find production_num
                            |> get_option' (fun () -> Printf.sprintf
                                              "Lr0 parse_trace: unable to find production %n in production_map"
                                              production_num
                                           )
                            |> List.length
            in
            (* pop symbols off the stack *)
            let children : parse_result list ref = ref [] in
            let start_pos : int ref = ref 0 in
            let end_pos : int ref = ref 0 in
            for i = 1 to pop_count do
              ignore (MStack.pop stack : state);
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
                         |> MMI.find production_num
                         |> get_option' (fun () -> Printf.sprintf
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
              { production = Either.Second production_num;
                children = !children;
                start_pos = !start_pos;
                end_pos = !end_pos;
              };
          | Accept -> raise ParseFinished
          | Error (Some ReduceReduce) ->
            raise (ParseFailed
                     ( tok.start
                     , Printf.sprintf
                         "parse failed -- reduce/reduce conflict on this token (%s) from state %n"
                         tok.name
                         s
                     ))
          | Error (Some ShiftReduce) ->
            raise (ParseFailed
                     ( tok.start
                     , Printf.sprintf
                         "parse failed -- shift/reduce conflict on this token (%s) from state %n"
                         tok.name
                         s
                     ))
          | Error None ->
            raise (ParseFailed
                     ( tok.start
                     (* TODO: give a decent error message *)
                     , Printf.sprintf
                         "parse failed -- no valid transition on this token (%s) from state %n"
                         tok.name
                         s
                     ))
            ;
        done;
        failwith "invariant violation: can't make it here"

      with
      | ParseFinished -> (match MStack.length results with
        | 1 -> (match MStack.top results with
          | Some result -> Ok result, MQueue.to_array trace
          | None -> failwith "invariant violation: no result"
        )
        | 0 -> failwith "invariant violation: no result"
        | n -> failwith (Printf.sprintf
                           "invariant violation: multiple results (%n)"
                           n
                        )
      )
      | ParseFailed parse_error -> (Error parse_error, MQueue.to_array trace)
      | PopFailed pos
        -> (Error (pos, "parsing invariant violation -- pop failed"),
            MQueue.to_array trace)

  let parse_trace
    : do_trace (* trace or not *)
      -> Lex.token MQueue.t
      -> (parse_error, parse_result) Result.t * trace_line array
    = parse_trace_tables lr0_action_table lr0_goto_table

  let parse : Lex.token MQueue.t -> (parse_error, parse_result) Result.t
    = fun toks ->
      match parse_trace DontTrace toks with
        result, _ -> result

  let lex_and_parse : Lex.lexer -> string
    -> ((Lex.lex_error, parse_error) Either.t, parse_result) Result.t
    = fun lexer input -> match Lex.lex lexer input with
      | Error error -> Error (First error)
      | Ok tokens ->
        let len = String.length input in
        let tokens' = tokens
                      |> Array.filter
                        ~f:(fun (token : Lex.token) -> token.name <> "SPACE")
                      |> MQueue.from_array
        in
        (* TODO: name might not always be "$" *)
        MQueue.enqueue tokens' { name = "$"; start = len; finish = len };
        Result.map_error (parse tokens') ~f:(fun err -> Either.Second err)

end
