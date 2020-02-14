open Tablecloth
module MMI = Placemat.MutableMap.Int
module MSet = Placemat.MutableSet
module MSI = Placemat.MutableSet.Int
module MStack = Placemat.MutableStack
module MQueue = Placemat.MutableQueue
let get_option, get_option', invariant_violation =
  Util.(get_option, get_option', invariant_violation)

exception NoItemSet of (unit -> string)

(* by convention, we reserve 0 for the `$` terminal, and number the rest
 * contiguously from 1 *)
type terminal_num = int

(* by convention, we reserve 0 for the root nonterminal, and number the rest
 * contiguously from 1 *)
type nonterminal_num = int

type symbol =
  | Terminal    of terminal_num
  | Nonterminal of nonterminal_num

module SymbolCmp = Placemat.Id.MakeComparable(struct
    type t = terminal_num * symbol
    let cmp (a0, a1) (b0, b1) =
      match Pervasives.compare a0 b0 with
      | 0 -> Pervasives.compare a1 b1
      | c -> c
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
  (** The number of one of the productions of the underlying grammar *)
  { production_num: production_num
    (** The position of the dot *)
  ; position: int;
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

type item_set = IntSet.t

let mk_item : item_view -> item
  = fun { production_num; position } -> mk_item' production_num position

type configuration_set =
  { kernel_items : item_set    (* set of items *)
  ; nonkernel_items : item_set (* set of nonterminals *)
  }

let simplify_config_set : configuration_set -> item_set
  = fun { kernel_items; nonkernel_items } ->
    IntSet.union kernel_items nonkernel_items

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
  nonterminals : nonterminal IntDict.t;
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

module ComparableIntSet = Placemat.Id.MakeComparable(struct
    type t = IntSet.t
    let cmp = Placemat.IntSet.cmp
  end)

(* A mutable set of int sets. This is used to represent the set of LR(0) items.
 * Each int set represents a set of encoded items.
*)
type mutable_lr0_item_set = (IntSet.t, ComparableIntSet.identity) MSet.t

type item_set_set = (item_set, ComparableIntSet.identity) Placemat.Set.t

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
                  |> Placemat.String.concat_array ~sep:" "

(* TODO: remove exns *)
module Lr0 (G : GRAMMAR) = struct

  (* Map from production number to the actual production *)
  let production_map : production Placemat.MutableMap.Int.t
    = MMI.make ()

  (* Map from production number to the number of the nonterminal it belongs to
  *)
  let production_nonterminal_map : nonterminal_num Placemat.MutableMap.Int.t
    = MMI.make ()

  (* Map from a nonterminal num to the set of productions it holds *)
  let nonterminal_production_map : MSI.t Placemat.MutableMap.Int.t
    = MMI.make ()

  (* number of nonterminals in the passed-in grammar (which ought to be
   * augmented) *)
  let number_of_nonterminals : int
    = Placemat.IntDict.size G.grammar.nonterminals

  let number_of_terminals : int
    = Array.length G.grammar.terminal_nums

  let terminal_names : string IntDict.t
    = G.grammar.terminal_nums
      |> Array.map ~f:(fun (name, num) -> num, name)
      |> Placemat.IntDict.from_array

  let terminal_nums : int StrDict.t
    = Placemat.StrDict.from_array G.grammar.terminal_nums

  let nonterminal_names : string IntDict.t
    = G.grammar.nonterminal_nums
      |> Array.map ~f:Tuple2.swap
      |> Placemat.IntDict.from_array

  let nonterminal_nums : nonterminal_num StrDict.t
    = Placemat.StrDict.from_array G.grammar.nonterminal_nums

  let string_of_nonterminal_num : nonterminal_num -> string
    = fun nt_num -> nonterminal_names
      |> IntDict.get ~key:nt_num
      |> get_option' (fun () -> Printf.sprintf
         "string_of_nonterminal_num: failed to get nonterminal %n from \
         nonterminal_names"
         nt_num
      )

  let rec nonterminal_first_set : IntSet.t -> MSI.t -> nonterminal_num -> unit
    = fun already_seen_nts result nt ->
      let productions : MSI.t = nonterminal_production_map
        |. MMI.get nt
        |> get_option' (fun () -> Printf.sprintf
          "nonterminal_first_set->already_seen_nts: Couldn't find nonterminal \
          %s in nonterminal_production_map"
          (string_of_nonterminal_num nt)
        )
        |. MSI.copy
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
          |. MMI.get production_num
          |> get_option' (fun () -> Printf.sprintf
            "nonterminal_first_set->already_seen_nts: Couldn't find production \
            %n in production_map"
            production_num
          )
        in
        first_set' already_seen_nts result production
      done;

  and first_set' : IntSet.t -> MSI.t -> production -> unit
    = fun already_seen_nts result -> function
      | [] -> failwith
                "invariant violation: first_set must be called with a non-empty list"
      | Terminal num :: _ -> MSI.add result num
      (* TODO: update when we allow empty productions *)
      | Nonterminal num :: _
        -> if not (IntSet.has already_seen_nts ~value:num)
        then nonterminal_first_set (IntSet.add already_seen_nts ~value:num) result num

  let first_set : production -> IntSet.t
    = function
      | [] -> IntSet.empty
      | prod ->
        let result = MSI.make () in
        let already_seen_nts = IntSet.empty in
        first_set' already_seen_nts result prod;
        result |> MSI.to_list |> IntSet.from_list

  let string_of_terminal : terminal_num -> string
    = fun t_num -> terminal_names
                   |> IntDict.get ~key:t_num
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
                       |. MMI.get production_num
                       |> get_option' (fun () -> Printf.sprintf
                                         "Lr0 string_of_item: unable to find production %n in production_map"
                                         production_num
                                      )
      in
      let pieces = [||] in
      Placemat.List.for_each_with_index production ~f:(fun i symbol ->
        if position = i then (let _ = Js.Array2.push pieces "." in ());
        Js.Array2.push pieces (string_of_symbol symbol);
      );

      if position = List.length production then
        (let _ = Js.Array2.push pieces "." in ());

      let nt_num = production_nonterminal_map
                   |. MMI.get production_num
                   |> get_option' (fun () -> Printf.sprintf
                                     "Lr0 string_of_item: unable to find production %n in production_nonterminal_map"
                                     production_num
                                  )
      in

      let nt_name = nonterminal_names
                    |> IntDict.get ~key:nt_num
                    |> get_option' (fun () -> Printf.sprintf
                                      "Lr0 string_of_item: unable to find nonterminal %n in nonterminal_names"
                                      production_num
                                   )
      in

      (* output if trailing *)
      Printf.sprintf "%s -> %s" nt_name
        (Placemat.String.concat_array pieces ~sep:" ")

  let string_of_item_set : ?sep:string -> item_set -> string
    = fun ?(sep=" ") item_set -> match Placemat.IntSet.size item_set with
      | 0 -> "empty"
      | _ -> item_set
        |> IntSet.to_list
        |> Util.stringify_list string_of_item sep

  let string_of_production : production -> string
    = fun production -> production
                        |> List.map ~f:string_of_symbol
                        |> Array.from_list
                        |> Placemat.String.concat_array ~sep:" "

  let string_of_production_num : production_num -> string
    = fun production_num ->

      let nt_num = production_nonterminal_map
                   |. MMI.get production_num
                   |> get_option' (fun () -> Printf.sprintf
                                     "Lr0 string_of_production: unable to find production %n in production_nonterminal_map"
                                     production_num
                                  )
      in

      let production = production_map
                       |. MMI.get production_num
                       |> get_option' (fun () -> Printf.sprintf
                                         "Lr0 string_of_production_num: unable to find production %n in production_map"
                                         production_num
                                      )
      in

      let nt_name = nonterminal_names
                    |> IntDict.get ~key:nt_num
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
  let () = Placemat.IntDict.for_each G.grammar.nonterminals
             ~f:(fun nt_num { productions } ->
                nonterminal_production_map |. MMI.set nt_num (MSI.make ());
                Placemat.List.for_each productions ~f:(fun production ->
                  let production_num = !production_cnt in
                  incr production_cnt;
                  production_map |. MMI.set production_num production;
                  production_nonterminal_map |. MMI.set production_num nt_num;
                  let prod_set = nonterminal_production_map
                                 |. MMI.get nt_num
                                 |> get_option' (fun () ->
                                      "Lr0 preprocessing -- unable to find nonterminal " ^
                                       string_of_int nt_num)
                  in
                  prod_set |. MSI.add production_num
                )
             )

  let get_nonterminal_num : production_num -> nonterminal_num
    = fun p_num -> production_nonterminal_map
                   |. MMI.get p_num
                   |> get_option' (fun () ->
                     "get_nonterminal_num: couldn't find production " ^
                     string_of_int p_num)

  let get_nonterminal : production_num -> nonterminal
    = fun pn -> G.grammar.nonterminals
      |> IntDict.get ~key:(get_nonterminal_num pn)
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
      Placemat.IntSet.for_each initial_items ~f:(fun item ->
        let { production_num; position } = view_item item in

        if production_num = 0 || position > 0
        then MSI.add kernel_items item
        else MSI.add nonkernel_items item;

        let production = production_map
                         |. MMI.get production_num
                         |> get_option' (fun () -> Printf.sprintf
                                           "lr0_closure': couldn't find production %n"
                                           production_num
                                        )
        in
        (* first symbol right of the dot. if it's a nonterminal, look at it *)
        match List.get_at production ~index:position with
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
                                   |. MMI.get nonterminal_num
                                   |> get_option' (fun () -> Printf.sprintf
                                                     "lr0_closure': unable to find nonterminal %n nonterminal_production_map"
                                                     nonterminal_num
                                                  )
          in
          MSI.for_each production_num_set (fun production_num ->
            MSI.add nonkernel_items (mk_item' production_num 0)
          );
          let { productions } = IntDict.get G.grammar.nonterminals ~key:nonterminal_num
                                |> get_option' (fun () -> Printf.sprintf
                                                  "lr0_closure': unable to find nonterminal %n in G.grammar.nonterminals"
                                                  nonterminal_num
                                               )
          in
          Placemat.List.for_each productions ~f:(fun production ->
            match production with
            | Terminal _         :: _ -> ()
            | Nonterminal new_nt :: _ -> MSI.add nt_set new_nt
            | _                       -> failwith "Empty production"
          )
        )
      done;

      { kernel_items = kernel_items |> MSI.to_list |> IntSet.from_list;
        nonkernel_items = nonkernel_items |> MSI.to_list |> IntSet.from_list;
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
      Placemat.IntSet.for_each (lr0_closure item_set) ~f:(fun item ->
        let { production_num; position } = view_item item in
        let production = production_map
                         |. MMI.get production_num
                         |> get_option' (fun () -> Printf.sprintf
                                           "lr0_goto_kernel: unable to find production %n in production_map"
                                           production_num
                                        )
        in
        match List.get_at production ~index:position with
        | Some next_symbol ->
          if symbol = next_symbol then
            MSI.add result (mk_item' production_num (position + 1))
        | _ -> ()
      );
      result |> MSI.to_list |> IntSet.from_list

  (* A list of all grammar symbols (terminals and nonterminals) *)
  let grammar_symbols = List.append
    (Placemat.List.initialize
      ~length:number_of_terminals
      ~f:(fun n -> Terminal n))
    (Placemat.List.initialize
      ~length:number_of_nonterminals
      ~f:(fun n -> Nonterminal n))

  (** Compute the canonical collection of sets of LR(0) items. CPTT fig 4.33. *)
  let mutable_lr0_items : mutable_lr0_item_set
    = let augmented_start = IntSet.from_list
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
        Placemat.List.for_each grammar_symbols ~f:(fun symbol ->
          let goto_result = lr0_goto_kernel items symbol in
          (* if GOTO(items, symbol) is not empty and not in c: *)
          if not (IntSet.isEmpty goto_result) &&
             not (MSet.has c ~value:goto_result)
          then (
            MSet.add c ~value:goto_result;
            MSet.add new_active_set ~value:goto_result;
          )
        )
      );
      active_set := new_active_set;
    done;
    c

  let lr0_items : item_set IntDict.t
    = mutable_lr0_items
      |> MSet.to_array
      |> Placemat.Array.map_with_index ~f:(fun i item_set -> i, item_set)
      |> Placemat.IntDict.from_array

  let state_to_item_set : state -> item_set
    = fun state -> lr0_items
                   |> IntDict.get ~key:state
                   |> get_option' (fun () -> Printf.sprintf
                                     "state_to_item_set -- couldn't find state %n (%n item sets)"
                                     state
                                     (Placemat.IntDict.size lr0_items)
                                  )

  (** raises [NoItemSet] *)
  let item_set_to_state : item_set -> state
    = fun item_set ->
      let state, _ = lr0_items
                     |> Placemat.IntDict.find_first_by
                       ~f:(fun _ item_set' -> item_set' = item_set)
                     |> get_option (NoItemSet
                       (fun () -> Printf.sprintf
                          "item_set_to_state -- couldn't find item_set (%s) (options: %s)"
                          (string_of_item_set item_set)
                          (lr0_items
                            |> IntDict.to_list
                            |> Util.stringify_list
                              (fun (_, item_set) -> string_of_item_set item_set)
                              ", ")
                          ))
      in state

  let augmented_state : state
    = item_set_to_state @@ IntSet.from_list [ mk_item' 0 0 ]

  let end_marker : terminal_num
    = 0

  let rec follow' : IntSet.t -> nonterminal_num -> IntSet.t
    = fun nts_visited nt_num -> if nt_num = 0
    (* Rule 1 from the CPTT algorithm for FOLLOW(A):
     * $ is in FOLLOW(S), where S is the start symbol.
    *)
      then IntSet.from_list [ end_marker ]

      (* For each production, accumulate the terminals it adds to the follow
       * set
      *)
      else production_map
           |> MMI.to_array
           |> Array.fold_left
                ~initial:IntSet.empty
                ~f:(fun (prod_num, production) follow_set ->
                   (* Rule 2 from the CPTT algorithm for FOLLOW(A):
                    * If there is a production A -> aBb, then everything in FIRST(b),
                    * except e, is in FOLLOW(B).
                   *)
                   let rule_2_follow_set = first_after_nt nt_num production in

                   let parent_nt = production_nonterminal_map
                                   |. MMI.get prod_num
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
                       when nt_num' = nt_num && not (IntSet.has nts_visited ~value:nt_num)
                       -> follow' (IntSet.add nts_visited ~value:nt_num) parent_nt
                     | _ -> IntSet.empty
                   in

                   follow_set
                     |> IntSet.union rule_2_follow_set
                     |> IntSet.union rule_3_follow_set
                )

  (* Find all the terminals occuring in first sets directly after the
   * nonterminal *)
  and first_after_nt : nonterminal_num -> production -> IntSet.t
    = fun nt_num -> function
      | Nonterminal nt_num' :: rest
        when nt_num' = nt_num
        -> IntSet.union (first_set rest) (first_after_nt nt_num rest)
      | _ :: rest
        -> first_after_nt nt_num rest
      | []
        -> IntSet.empty

  (* Compute the set of terminals that can appear immediately to the right of
   * nt in some production *)
  let follow_set : nonterminal_num -> IntSet.t
    = follow' IntSet.empty

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

    let item_set_l = IntSet.to_list item_set in

    (* If [A -> xs . a ys] is in I_i and GOTO(I_i, a) = I_j, set
     * ACTION[i, a] to `shift j` *)
    let shift_action = item_set_l
                       |. Util.find_by (fun item ->
                         let { production_num; position } = view_item item in
                         let symbols = production_map
                                       |. MMI.get production_num
                                       |> get_option' (fun () -> Printf.sprintf
                                                         "Lr0 shift_action: unable to find production %n in production_map"
                                                         production_num
                                                      )
                         in
                         match List.get_at symbols ~index:position with
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
                        |. Util.find_by (fun item ->
                          let { production_num; position } = view_item item in
                          let nt_num = production_nonterminal_map
                                       |. MMI.get production_num
                                       |> get_option' (fun () -> Printf.sprintf
                                                         "Lr0 shift_action: unable to find production %n in production_nonterminal_map"
                                                         production_num
                                                      )
                          in
                          let production = production_map
                                           |. MMI.get production_num
                                           |> get_option' (fun () -> Printf.sprintf
                                                             "Lr0 shift_action: unable to find production %n in production_map"
                                                             production_num
                                                          )
                          in
                          if position = List.length production &&
                             follow_set nt_num |> IntSet.has ~value:terminal_num &&
                             (* Accept in this case (end marker on the augmented nonterminal) --
                                don't reduce. *)
                             nt_num != 0
                          then Some (Reduce production_num)
                          else None
                        )
    in

    (* If [S' -> S .] is in I_i, set ACTION[i, $] to `accept` *)
    let accept_action =
      if terminal_num = end_marker && item_set |> IntSet.has ~value:(mk_item' 0 1)
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
  let states : state array = Tablecloth.Array.initialize
    ~length:(Placemat.IntDict.size lr0_items)
    ~f:Util.id
  let terminals : terminal_num array = Tablecloth.Array.initialize
    ~length:number_of_terminals
    ~f:Util.id
  let nonterminals : nonterminal_num array = Tablecloth.Array.initialize
    ~length:(Placemat.StrDict.size nonterminal_nums)
    ~f:Util.id

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
                      |> StrDict.get ~key:name
                      |> get_option' (fun () -> Printf.sprintf
                                        "Lr0 token_to_terminal: unable to find name %s in terminal_nums"
                                        name
                                     )

  let token_to_symbol
    : Lex.token -> symbol
    = fun { name } ->
      let t_match = terminal_nums |> StrDict.get ~key:name in
      let nt_match = nonterminal_nums |> StrDict.get ~key:name in
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
                           |. Array.map ~f:(fun { production } -> match production with
                             | Left terminal_num -> terminal_names
                                                    |> IntDict.get ~key:terminal_num
                                                    |> get_option' (fun () -> Printf.sprintf
                                                                      "string_of_symbols: failed to get terminal %n"
                                                                      terminal_num
                                                                   )
                             | Right production_num ->
                               let nt_num = production_nonterminal_map
                                            |. MMI.get production_num
                                            |> get_option' (fun () -> Printf.sprintf
                                                              "Lr0 string_of_symbols: unable to find production %n in production_nonterminal_map"
                                                              production_num
                                                           )
                               in

                               let nt_name = nonterminal_names
                                             |> IntDict.get ~key:nt_num
                                             |> get_option' (fun () -> Printf.sprintf
                                                               "Lr0 string_of_symbols: unable to find nonterminal %n in nonterminal_names"
                                                               production_num
                                                            )

                               in nt_name
                           )
                           |> Placemat.String.concat_array ~sep:" "

  let string_of_trace_line = fun { action; stack; results; input } ->
    Printf.sprintf "action: %s\nstack: %s\nresults: %s\ninput: %s\n\n"
      (string_of_action action)
      (stack
       |> Array.map ~f:string_of_int
       |> Placemat.String.concat_array ~sep:" ")
      (string_of_symbols results)
      (input
       |> Array.map ~f:(fun (tok : Lex.token) -> tok.name)
       |> Placemat.String.concat_array ~sep:" ")

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
              { production = Either.Left terminal_num;
                children = [];
                start_pos = tok.start;
                end_pos = tok.finish;
              };
            a := pop_front_exn tok.start toks;
          | Reduce production_num ->
            let pop_count = production_map
                            |. MMI.get production_num
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
              { production = Either.Right production_num;
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
      | ParseFinished -> (match MStack.size results with
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
      | Error error -> Error (Left error)
      | Ok tokens ->
        let len = String.length input in
        let tokens' = tokens
                      |> Array.filter
                        ~f:(fun (token : Lex.token) -> token.name != "SPACE")
                      |> MQueue.from_array
        in
        (* TODO: name might not always be "$" *)
        MQueue.enqueue tokens' { name = "$"; start = len; finish = len };
        Util.map_error (parse tokens') ~f:(fun err -> Either.Right err)

end
