open Core_kernel
module MutableSet = Util.MutableSet

let get_option, get_option', invariant_violation =
  Util.(get_option, get_option', invariant_violation)
;;

exception NoItemSet of (unit -> string)

(* By convention, we reserve:
  - 0 for the [$] terminal
  - 1 for [SPACE]
  - 2 for the empty terminal
  - and number the rest contiguously from 3
 *)
type terminal_num = int [@@deriving sexp, compare]

let empty_terminal_num = 2

(* by convention, we reserve 0 for the root nonterminal, and number the rest
 * contiguously from 1 *)
type nonterminal_num = int [@@deriving sexp, compare]

type symbol =
  | Terminal of terminal_num
  | Nonterminal of nonterminal_num
[@@deriving sexp, compare]

(** A state number *)
type state = int [@@deriving sexp, compare]

type production = symbol list
type production_num = int

(** A production with a dot at some position. We encode this as a single integer --
    see [item], [mk_item], [mk_item'], and [view_item]. See also [level_view]. *)
type item_view =
  { production_num : production_num
        (** The number of one of the productions of the underlying grammar *)
  ; position : int (** The position of the dot *)
  }

(** An LR(0) item encodes a pair of integers, namely the index of the production and the
    index of the bullet in the production's right-hand side. Both integers are
    packed into a single integer, using 8 bits for the bullet position and the remaining
    24 bits for the production index. Note that this implies some maximums: The
    maximum length of a production is 255 The maximum number of different productions is
    16777215 *)
type item = int [@@deriving sexp, compare]

let view_item : item -> item_view =
 fun item ->
  { production_num = item land 0x00ffffff; position = (item land 0xff000000) lsr 24 }
;;

let mk_item' : int -> int -> item =
 fun production_num position -> (position lsl 24) lor production_num
;;

type item_set = Int.Set.t [@@deriving equal]

let mk_item : item_view -> item =
 fun { production_num; position } -> mk_item' production_num position
;;

module ConfigurationSet = struct
  module T = struct
    type t =
      { kernel_items : item_set (* set of items *)
      ; nonkernel_items : item_set (* set of nonterminals *)
      }
    [@@deriving equal]
  end

  include T
end

type configuration_set = ConfigurationSet.t

let simplify_config_set : configuration_set -> item_set =
 fun { kernel_items; nonkernel_items } -> Set.union kernel_items nonkernel_items
;;

type nonterminal =
  { (* nonterminal_num: nonterminal_num; *)
    productions : production list }

type prec_level = int

(* By convention:
 - A non-augmented grammar has keys starting at 1 (start)
 - An augmented grammar has key 0 (augmented start)
 *)
type grammar =
  { terminal_nums : (string * terminal_num) array
  ; nonterminals : (string * nonterminal_num * nonterminal) array
  }

type augmented_grammar = AugmentedGrammar of grammar
type nonaugmented_grammar = NonAugmentedGrammar of grammar

type conflict_type =
  | ShiftReduce
  | ReduceReduce

type action =
  | Shift of state
  | Reduce of production_num
  | Accept
  | Error of conflict_type option

(* Our action / goto table formulations are lazy (not actually tables). Tables
 * can be computed with `full_action_table` / `full_goto_table` *)
type lr0_action_table = state -> terminal_num -> action
type lr0_goto_table = state -> symbol -> state option

module IntSet = struct
  module T = struct
    type t = Int.Set.t [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

(* A mutable set of int sets. This is used to represent the set of LR(0) items.
 * Each int set represents a set of encoded items.
 *)
type mutable_lr0_item_set = (IntSet.t, IntSet.comparator_witness) MutableSet.t
type item_set_set = (item_set, IntSet.comparator_witness) Set.t
type parse_error = int (* character number *) * string

let error_to_string : parse_error -> string
  = fun (c, msg) -> Printf.sprintf "character %n: %s" c msg

type parse_result =
  { production : (terminal_num, production_num) Either.t
  ; children : parse_result list
  ; start_pos : int (* inclusive *)
  ; end_pos : int (* exclusive *)
  }

let rec parse_result_to_string : parse_result -> string =
 fun { production; children; _ } ->
  Printf.sprintf
    "%s[%s]"
    (match production with
    | First n -> "t" ^ string_of_int n
    | Second n -> "n" ^ string_of_int n)
    (children |> List.map ~f:parse_result_to_string |> String.concat ~sep:", ")
;;

exception ParseFinished
exception ParseFailed of parse_error
exception PopFailed of int

type do_trace =
  | DoTrace
  | DontTrace

type trace_line =
  { action : action
  ; stack : state array
  ; results : parse_result array
  ; input : Lex.token array
  }

let pop_front_exn : int -> 'a Queue.t -> 'a =
 fun position arr ->
  match Queue.dequeue arr with None -> raise (PopFailed position) | Some a -> a
;;

module type GRAMMAR = sig
  (* An *augmented* grammar, or else this won't work right *)
  val grammar : augmented_grammar
end

module type LR0 = sig
  (* TODO: fill in the rest of the signature *)
  val production_map : production Int.Table.t
  val production_nonterminal_map : nonterminal_num Int.Table.t
  val string_of_symbol : symbol -> string
  val string_of_terminal : terminal_num -> string
  val string_of_production_num : production_num -> string
  val string_of_production : production -> string
  val states : state array
end

(* Used for action table entries. Compare with string_of_action. *)
let action_abbrev : action -> string = function
  | Shift state -> "s" ^ string_of_int state
  | Reduce prod -> "r" ^ string_of_int prod
  | Accept -> "acc"
  | Error None -> ""
  | Error (Some ShiftReduce) -> "s/r"
  | Error (Some ReduceReduce) -> "r/r"
;;

let string_of_stack : state array -> string =
 fun states -> states |> Array.map ~f:string_of_int |> String.concat_array ~sep:" "
;;

(* TODO: remove exns *)
module Lr0 (G : GRAMMAR) = struct
  let AugmentedGrammar augmented_grammar = G.grammar

  let nonterminal_map : nonterminal Int.Map.t =
    augmented_grammar.nonterminals
    |> Array.map ~f:(fun (_name, num, nt) -> num, nt)
    |> Array.to_list
    |> Int.Map.of_alist_exn
  ;;

  (* Map from production number to the actual production *)
  let production_map : production Int.Table.t = Int.Table.create ()

  (* Map from production number to the number of the nonterminal it belongs to *)
  let production_nonterminal_map : nonterminal_num Int.Table.t = Int.Table.create ()

  (* Map from a nonterminal num to the set of productions it holds *)
  let nonterminal_production_map : MutableSet.Int.t Int.Table.t = Int.Table.create ()

  (* number of nonterminals in the passed-in grammar (which ought to be
   * augmented) *)
  let number_of_nonterminals : int = Array.length augmented_grammar.nonterminals
  let number_of_terminals : int = Array.length augmented_grammar.terminal_nums

  let terminal_names : string Int.Map.t =
    augmented_grammar.terminal_nums
    |> Array.map ~f:(fun (name, num) -> num, name)
    |> Array.to_list
    |> Int.Map.of_alist
    |> function
    | `Ok map -> map
    | `Duplicate_key num ->
      Util.invariant_violation
        (Printf.sprintf "terminal_names: duplicate terminal number: %n" num)
  ;;

  let terminal_nums : int String.Map.t =
    augmented_grammar.terminal_nums
    |> Array.to_list
    |> String.Map.of_alist
    |> function
    | `Ok map -> map
    | `Duplicate_key name ->
      Util.invariant_violation
        (Printf.sprintf "terminal_nums: duplicate terminal name: %s" name)
  ;;

  let nonterminal_names : string Int.Map.t =
    augmented_grammar.nonterminals
    |> Array.map ~f:(fun (name, num, _nt) -> num, name)
    |> Array.to_list
    |> Int.Map.of_alist
    |> function
    | `Ok map -> map
    | `Duplicate_key num ->
      Util.invariant_violation
        (Printf.sprintf "nonterminal_names: duplicate terminal number: %n" num)
  ;;

  let nonterminal_nums : nonterminal_num String.Map.t =
    augmented_grammar.nonterminals
    |> Array.map ~f:(fun (name, num, _nt) -> name, num)
    |> Array.to_list
    |> String.Map.of_alist
    |> function
    | `Ok map -> map
    | `Duplicate_key name ->
      Util.invariant_violation
        (Printf.sprintf "nonterminal_nums: duplicate terminal name: %s" name)
  ;;

  let string_of_nonterminal_num : nonterminal_num -> string =
   fun nt_num -> Map.find nonterminal_names nt_num
    |> get_option' (fun () ->
           Printf.sprintf
             "string_of_nonterminal_num: failed to get nonterminal %n from \
              nonterminal_names"
             nt_num)
 ;;

  let string_of_terminal : terminal_num -> string =
   fun t_num -> Map.find terminal_names t_num
    |> get_option' (fun () ->
           Printf.sprintf "string_of_terminal: failed to get terminal %n" t_num)
 ;;

  let string_of_symbol : symbol -> string = function
    | Terminal t_num -> string_of_terminal t_num
    | Nonterminal nt_num -> string_of_nonterminal_num nt_num
  ;;

  let string_of_item : item -> string =
   fun item ->
    let { production_num; position } = view_item item in
    let production =
      Hashtbl.find production_map production_num
      |> get_option' (fun () ->
             Printf.sprintf
               "Lr0 string_of_item: unable to find production %n in production_map"
               production_num)
    in
    let pieces = Queue.create () in
    List.iteri production ~f:(fun i symbol ->
        if position = i then Queue.enqueue pieces ".";
        Queue.enqueue pieces (string_of_symbol symbol));
    if position = List.length production then Queue.enqueue pieces ".";
    let nt_num =
      Hashtbl.find production_nonterminal_map production_num
      |> get_option' (fun () ->
             Printf.sprintf
               "Lr0 string_of_item: unable to find production %n in \
                production_nonterminal_map"
               production_num)
    in
    let nt_name =
      Map.find nonterminal_names nt_num
      |> get_option' (fun () ->
             Printf.sprintf
               "Lr0 string_of_item: unable to find nonterminal %n in nonterminal_names"
               production_num)
    in
    (* output if trailing *)
    Printf.sprintf
      "%s -> %s"
      nt_name
      (String.concat_array (Queue.to_array pieces) ~sep:" ")
 ;;

  let string_of_item_set : ?sep:string -> item_set -> string =
   fun ?(sep = " ") item_set ->
    match Set.length item_set with
    | 0 -> "empty"
    | _ -> item_set |> Set.to_list |> List.map ~f:string_of_item |> String.concat ~sep
 ;;

  let string_of_production : production -> string =
   fun production ->
    production
    |> List.map ~f:string_of_symbol
    |> Array.of_list
    |> String.concat_array ~sep:" "
 ;;

  let string_of_production_num : production_num -> string =
   fun production_num ->
    let nt_num =
      Hashtbl.find production_nonterminal_map production_num
      |> get_option' (fun () ->
             Printf.sprintf
               "Lr0 string_of_production: unable to find production %n in \
                production_nonterminal_map"
               production_num)
    in
    let production =
      Hashtbl.find production_map production_num
      |> get_option' (fun () ->
             Printf.sprintf
               "Lr0 string_of_production_num: unable to find production %n in \
                production_map"
               production_num)
    in
    let nt_name =
      Map.find nonterminal_names nt_num
      |> get_option' (fun () ->
             Printf.sprintf
               "Lr0 string_of_production: unable to find nonterminal %n in \
                nonterminal_names"
               production_num)
    in
    Printf.sprintf "%s -> %s" nt_name (string_of_production production)
 ;;

  let string_of_nonterminal : int ref -> nonterminal -> string =
   fun prod_num_counter { productions } -> productions
     |> List.map ~f:(fun prod ->
       let n = !prod_num_counter in
       incr prod_num_counter;
       Printf.sprintf "  %n: %s" n (string_of_production prod))
     |> String.concat ~sep:"\n"
 ;;

  let string_of_grammar : grammar -> string =
   fun { nonterminals; terminal_nums } ->
    let prod_num_counter = ref 0 in
    let nt_str =
      nonterminals
      |> Array.map ~f:(fun (name, num, nt) -> Printf.sprintf "%s (%n):\n%s"
        name num (string_of_nonterminal prod_num_counter nt))
      |> String.concat_array ~sep:"\n"
    in
    let t_str =
      terminal_nums
      |> Array.map ~f:(fun (name, num) -> Printf.sprintf "%s <-> %n" name num)
      |> String.concat_array ~sep:"\n"
    in
    nt_str ^ "\n\n" ^ t_str
 ;;

  (* Used for logging actions. Compare with action_abbrev. *)
  let string_of_action : action -> string = function
    | Shift state -> "shift to " ^ string_of_int state
    | Reduce prod -> "reduce by " ^ string_of_production_num prod
    | Accept -> "accept"
    | Error None -> "error"
    | Error (Some ReduceReduce) -> "error (reduce/reduce conflict)"
    | Error (Some ShiftReduce) -> "error (shift/reduce conflict)"
  ;;

  (* From CPTT section 4.4.2 *)
  let rec nonterminal_first_set : Int.Set.t -> nonterminal_num -> Int.Set.t =
   fun parent_nts nt ->
    let productions : MutableSet.Int.t =
      Hashtbl.find nonterminal_production_map nt
      |> get_option' (fun () ->
             Printf.sprintf
               "nonterminal_first_set: Couldn't find nonterminal %n in \
                nonterminal_production_map (keys [%s])"
               nt
               (Hashtbl.keys nonterminal_production_map
                 |> List.map ~f:string_of_int
                 |> String.concat ~sep:", ")
        )
      |> MutableSet.copy
    in

    let result = MutableSet.Int.create () in
    while not (MutableSet.is_empty productions) do
      let production_num = MutableSet.min_elt_exn productions in
      MutableSet.remove productions production_num;
      let production = Hashtbl.find production_map production_num
        |> get_option' (fun () -> Printf.sprintf
          "nonterminal_first_set: Couldn't find production %n in production_map"
          production_num)
      in
      Set.iter (first_set' parent_nts production) ~f:(MutableSet.add result)
    done;

    MutableSet.snapshot result

  (* From CPTT section 4.4.2 *)
  and first_set' : Int.Set.t -> production -> Int.Set.t =
    fun parent_nts -> function
    | []
    -> Int.Set.singleton empty_terminal_num
    | Terminal num :: _
    -> Int.Set.singleton num
    | Nonterminal num :: tail_production ->
      (* Guard against recurring back into the same nonterminal *)
      if Set.mem parent_nts num
      then Int.Set.empty
      else
        let nt_first_set =
          nonterminal_first_set (Set.add parent_nts num) num
        in

        (* If this nonterminal accepts empty productions, add the first set of
          the rest of the production *)
        if Set.mem nt_first_set empty_terminal_num
        then Set.union nt_first_set (first_set' parent_nts tail_production)
        else nt_first_set
  ;;

  (* The set of terminals that begin strings defived from this production. From
   CPTT section 4.4.2 *)
  let first_set : production -> Int.Set.t
    = first_set' Int.Set.empty
  ;;

  let production_cnt = ref 0

  let () =
    Array.iter augmented_grammar.nonterminals ~f:(fun (_, nt_num, { productions }) ->
        nonterminal_production_map
        |> Hashtbl.set ~key:nt_num ~data:(MutableSet.Int.create ());
        List.iter productions ~f:(fun production ->
            let production_num = !production_cnt in
            incr production_cnt;
            production_map |> Hashtbl.set ~key:production_num ~data:production;
            production_nonterminal_map |> Hashtbl.set ~key:production_num ~data:nt_num;
            let prod_set =
              Hashtbl.find nonterminal_production_map nt_num
              |> get_option' (fun () ->
                     "Lr0 preprocessing -- unable to find nonterminal "
                     ^ string_of_int nt_num)
            in
            MutableSet.add prod_set production_num))
  ;;

  let get_nonterminal_num : production_num -> nonterminal_num =
   fun p_num ->
    Hashtbl.find production_nonterminal_map p_num
    |> get_option' (fun () ->
           "get_nonterminal_num: couldn't find production " ^ string_of_int p_num)
 ;;

  let get_nonterminal : production_num -> nonterminal =
   fun pn ->
    Map.find nonterminal_map (get_nonterminal_num pn)
    |> get_option' (fun () ->
           "get_nonterminal: couldn't find production " ^ string_of_int pn)
 ;;

  (** The closure of an item set. CPTT fig 4.32. *)
  let lr0_closure' : item_set -> configuration_set =
   fun initial_items ->
    let added = Bitstring.alloc number_of_nonterminals false in
    let kernel_items = Int.Hash_set.create () in
    let nonkernel_items = Int.Hash_set.create () in
    let nt_set = MutableSet.Int.create () in
    (* Create the set (nt_set) of nonterminals to look at. Add each initial
     * item to the kernel or nonkernel set. *)
    Set.iter initial_items ~f:(fun item ->
        let { production_num; position } = view_item item in
        if production_num = 0 || position > 0
        then Hash_set.add kernel_items item
        else Hash_set.add nonkernel_items item;
        let production =
          Hashtbl.find production_map production_num
          |> get_option' (fun () ->
                 Printf.sprintf "lr0_closure': couldn't find production %n" production_num)
        in
        (* first symbol right of the dot. if it's a nonterminal, look at it *)
        match List.nth production position with
        | Some (Nonterminal nt) -> MutableSet.add nt_set nt
        | _ -> ());
    (* Examine each accessible nonterminal, adding its initial items as
     * nonkernel items. *)
    while not (MutableSet.is_empty nt_set) do
      let nonterminal_num =
        nt_set |> MutableSet.min_elt |> get_option' (fun () -> "the set is not empty!")
      in
      MutableSet.remove nt_set nonterminal_num;
      let is_alrady_added = Bitstring.get added nonterminal_num
        |> get_option' (fun () ->
               Printf.sprintf
                 "lr0_closure': couldn't find nonterminal %n in added (nonterminal count \
                  %n)"
                 nonterminal_num
                 (Bitstring.length added))
      in
      if not is_alrady_added
      then (
        Bitstring.set_exn added nonterminal_num true;
        let production_num_set =
          Hashtbl.find nonterminal_production_map nonterminal_num
          |> get_option' (fun () ->
                 Printf.sprintf
                   "lr0_closure': unable to find nonterminal %n \
                    nonterminal_production_map"
                   nonterminal_num)
        in
        MutableSet.iter production_num_set ~f:(fun production_num ->
            Hash_set.add nonkernel_items (mk_item' production_num 0));
        let { productions } =
          Map.find nonterminal_map nonterminal_num
          |> get_option' (fun () ->
                 Printf.sprintf
                   "lr0_closure': unable to find nonterminal %n in nonterminal_map"
                   nonterminal_num)
        in
        List.iter productions ~f:(function
          | Terminal _ :: _
          | []
          -> ()
          | Nonterminal new_nt :: _ -> MutableSet.add nt_set new_nt))
    done;
    { kernel_items = kernel_items |> Hash_set.to_list |> Int.Set.of_list
    ; nonkernel_items = nonkernel_items |> Hash_set.to_list |> Int.Set.of_list
    }
 ;;

  (* closure returning an item set (rather than a configuration set) *)
  let lr0_closure : item_set -> item_set =
   fun items -> simplify_config_set @@ lr0_closure' items
 ;;

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
  let lr0_goto_kernel : item_set -> symbol -> item_set =
   fun item_set symbol ->
    let result = Hash_set.create (module Int) in
    Set.iter (lr0_closure item_set) ~f:(fun item ->
        let { production_num; position } = view_item item in
        let production =
          Hashtbl.find production_map production_num
          |> get_option' (fun () ->
                 Printf.sprintf
                   "lr0_goto_kernel: unable to find production %n in production_map"
                   production_num)
        in
        match List.nth production position with
        | Some next_symbol ->
          if Caml.(symbol = next_symbol)
          then Hash_set.add result (mk_item' production_num (position + 1))
        | _ -> ());
    result |> Hash_set.to_list |> Int.Set.of_list
 ;;

  (* A list of all grammar symbols (terminals and nonterminals) *)
  let grammar_symbols =
    List.append
      (List.init number_of_terminals ~f:(fun n -> Terminal n))
      (List.init number_of_nonterminals ~f:(fun n -> Nonterminal n))
  ;;

  (** Compute the canonical collection of sets of LR(0) items. CPTT fig 4.33. *)
  let mutable_lr0_items : mutable_lr0_item_set =
    let augmented_start =
      Int.Set.of_list [ mk_item { production_num = 0; position = 0 } ]
    in
    (* canonical collection of sets *)
    let c = MutableSet.of_list (module IntSet) [ augmented_start ] in
    (* set of item sets we've added to c but not yet explored *)
    let active_set = ref @@ MutableSet.copy c in
    (* iterate through every set of items in the collection, compute the GOTO
     * kernel of each item set, and add any new sets. `continue` is set to
     * `true` if we find a new item set, indicating we need to loop again. *)
    while not (MutableSet.is_empty !active_set) do
      let new_active_set = MutableSet.of_list (module IntSet) [] in
      (* for each set of items in the active set: *)
      MutableSet.iter !active_set ~f:(fun items ->
          (* for each grammar symbol: *)
          List.iter grammar_symbols ~f:(fun symbol ->
              let goto_result = lr0_goto_kernel items symbol in
              (* if GOTO(items, symbol) is not empty and not in c: *)
              if (not (Set.is_empty goto_result))
                 && not (MutableSet.mem c goto_result)
              then (
                MutableSet.add c goto_result;
                MutableSet.add new_active_set goto_result)));
      active_set := new_active_set
    done;
    c
  ;;

  let lr0_items : item_set Int.Map.t =
    mutable_lr0_items
    |> MutableSet.to_list
    |> List.mapi ~f:(fun i item_set -> i, item_set)
    |> Int.Map.of_alist_exn
  ;;

  let state_to_item_set : state -> item_set =
   fun state ->
    lr0_items
    |> Fn.flip Map.find state
    |> get_option' (fun () ->
           Printf.sprintf
             "state_to_item_set -- couldn't find state %n (%n item sets)"
             state
             (Map.length lr0_items))
 ;;

  (** @raise [NoItemSet] *)
  let item_set_to_state : item_set -> state =
   fun item_set ->
    let state, _ =
      lr0_items
      |> Map.to_sequence
      |> Sequence.find ~f:(fun (_, item_set') -> Set.equal item_set' item_set)
      |> get_option
           (NoItemSet
              (fun () ->
                Printf.sprintf
                  "item_set_to_state -- couldn't find item_set (%s) (options: %s)"
                  (string_of_item_set item_set)
                  (lr0_items
                  |> Map.to_alist
                  |> List.map ~f:(fun (_, item_set) -> string_of_item_set item_set)
                  |> String.concat ~sep:", ")))
    in
    state
 ;;

  let augmented_state : state = item_set_to_state @@ Int.Set.of_list [ mk_item' 0 0 ]
  let end_marker : terminal_num = 0

  (* From CPTT section 4.4.2 *)
  let rec follow' : Int.Set.t -> nonterminal_num -> Int.Set.t =
   fun nts_visited nt_num ->
      (* Rule 2 from the CPTT algorithm for FOLLOW(A):
         If there is a production A -> aBb, then everything in FIRST(b),
         except e, is in FOLLOW(B).

         Rule 3 from the CPTT algorithm for FOLLOW(A):
         If there is a production A -> aB, or a production A -> aBb,
         where FIRST(b) contains e, then everything in FOLLOW(A) is in
         FOLLOW(B)
       *)
      let rec go parent_nt = function
        | [] ->
          (* Rule 1 from the CPTT algorithm for FOLLOW(A):
             $ is in FOLLOW(S), where S is the start symbol.
           *)
          if nt_num = 0
          then Int.Set.of_list [end_marker]
          else Int.Set.empty
        | Nonterminal nt_num' :: remaining_production
          when nt_num' = nt_num && not (Set.mem nts_visited nt_num)
        -> (match remaining_production with
          (* aB: everything in FOLLOW(A) is in FOLLOW(B) *)
          | [] -> follow' (Set.add nts_visited nt_num) parent_nt

          (* aBb *)
          | _ ->
            let b_first_set = first_set remaining_production in

            (* everything in FIRST(b), except e, is in FOLLOW(B) *)
            let rule_2_follow_set =
              Set.remove b_first_set empty_terminal_num
            in

            let rule_3_follow_set =
              (* if FIRST(b) contains e: *)
              if Set.mem b_first_set empty_terminal_num
              (* everything in FOLLOW(A) is in FOLLOW(B) *)
              then follow' (Set.add nts_visited nt_num) parent_nt
              else Int.Set.empty
            in

            go parent_nt remaining_production
              |> Set.union rule_2_follow_set
              |> Set.union rule_3_follow_set)
        | _ :: remaining_production
        -> go parent_nt remaining_production
      in

      (* For each production, accumulate the terminals it adds to the follow
         set
       *)
      production_map
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (prod_num, production) ->
        let parent_nt =
          Hashtbl.find production_nonterminal_map prod_num
          |> get_option' (fun () ->
                 Printf.sprintf
                   "Lr0 follow': unable to find nonterminal %n in \
                    production_nonterminal_map"
                   prod_num)
        in
        go parent_nt production)
      |> Int.Set.union_list
 ;;

  (* Compute the set of terminals that can appear immediately to the right of
   * nt in some production. From CPTT section 4.4.2. *)
  let follow_set : nonterminal_num -> Int.Set.t
    = follow' Int.Set.empty

  (* This is the GOTO function operating on states. See `lr0_goto_kernel` for
   * the version operating on item set.
   *
   * raises: [InvariantViolation]
   *)
  let lr0_goto_table state nt =
    try Some (item_set_to_state @@ lr0_goto_kernel (state_to_item_set state) nt) with
    | NoItemSet _ -> None
  ;;

  let lr0_action_table state terminal_num =
    let item_set = lr0_closure @@ state_to_item_set state in
    let item_set_l = Set.to_list item_set in
    (* If [A -> xs . a ys] is in I_i and GOTO(I_i, a) = I_j, set
     * ACTION[i, a] to `shift j` *)
    let shift_action =
      item_set_l
      |> List.find_map ~f:(fun item ->
             let { production_num; position } = view_item item in
             let symbols =
               Hashtbl.find production_map production_num
               |> get_option' (fun () ->
                      Printf.sprintf
                        "Lr0 shift_action: unable to find production %n in production_map"
                        production_num)
             in
             match List.nth symbols position with
             | Some (Terminal t_num as next_symbol) ->
               if t_num = terminal_num
               then lr0_goto_table state next_symbol |> Option.map ~f:(fun x -> Shift x)
               else None
             | _ -> None)
    in
    (* If [A -> xs .] is in I_i, set ACTION[i, a] to `Reduce A -> xs` for
     * all a in FOLLOW(A) *)
    let reduce_action =
      item_set_l
      |> List.find_map ~f:(fun item ->
             let { production_num; position } = view_item item in
             let nt_num =
               Hashtbl.find production_nonterminal_map production_num
               |> get_option' (fun () ->
                      Printf.sprintf
                        "Lr0 shift_action: unable to find production %n in \
                         production_nonterminal_map"
                        production_num)
             in
             let production =
               Hashtbl.find production_map production_num
               |> get_option' (fun () ->
                      Printf.sprintf
                        "Lr0 shift_action: unable to find production %n in production_map"
                        production_num)
             in
             if position = List.length production
                && Set.mem (follow_set nt_num) terminal_num
                && (* Accept in this case (end marker on the augmented nonterminal) --
                      don't reduce. *)
                nt_num <> 0
             then Some (Reduce production_num)
             else None)
    in
    (* If [S' -> S .] is in I_i, set ACTION[i, $] to `accept` *)
    let accept_action =
      if terminal_num = end_marker && Set.mem item_set (mk_item' 0 1)
      then Some Accept
      else None
    in
    (* We should always have exactly one action, otherwise it's a
     * shift-reduce(-accept) conflict.
     *)
    match shift_action, reduce_action, accept_action with
    | Some act, None, None | None, Some act, None | None, None, Some act -> act
    | Some _, Some _, None -> Error (Some ShiftReduce)
    | _, _, _ -> Error None
  ;;

  (* TODO: is this right? *)
  let states : state array = Array.init (Map.length lr0_items) ~f:Fn.id
  let terminals : terminal_num array = Array.init number_of_terminals ~f:Fn.id

  let nonterminal_nums_arr : nonterminal_num array =
    Array.init (Map.length nonterminal_nums) ~f:Fn.id
  ;;

  let full_lr0_action_table : unit -> action array array =
   fun () ->
    states
    |> Array.map ~f:(fun state -> terminals |> Array.map ~f:(lr0_action_table state))
 ;;

  let full_lr0_goto_table : unit -> (symbol * state option) array array =
   fun () ->
    states
    |> Array.map ~f:(fun state ->
           nonterminal_nums_arr
           |> Array.map ~f:(fun nt ->
                  let sym = Nonterminal nt in
                  sym, lr0_goto_table state sym))
 ;;

  let token_to_terminal : Lex.token -> terminal_num =
   fun { name; _ } ->
    Map.find terminal_nums name
    |> get_option' (fun () ->
           Printf.sprintf
             "Lr0 token_to_terminal: unable to find name %s in terminal_nums"
             name)
 ;;

  let token_to_symbol : Lex.token -> symbol =
   fun { name; _ } ->
    let t_match = Map.find terminal_nums name in
    let nt_match = Map.find nonterminal_nums name in
    match t_match, nt_match with
    | Some t_num, None -> Terminal t_num
    | None, Some nt_num -> Nonterminal nt_num
    | None, None -> failwith ("Failed to find a terminal or nonterminal named " ^ name)
    | Some _, Some _ ->
      failwith
        ("Found both a terminal *and* nonterminal with name "
        ^ name
        ^ " (this should never happen)")
 ;;

  let string_of_symbols : parse_result array -> string =
   fun parse_results ->
    parse_results
    |> Array.map ~f:(fun { production; _ } ->
           match production with
           | First terminal_num ->
             Map.find terminal_names terminal_num
             |> get_option' (fun () ->
                    Printf.sprintf
                      "string_of_symbols: failed to get terminal %n"
                      terminal_num)
           | Second production_num ->
             let nt_num =
               Hashtbl.find production_nonterminal_map production_num
               |> get_option' (fun () ->
                      Printf.sprintf
                        "Lr0 string_of_symbols: unable to find production %n in \
                         production_nonterminal_map"
                        production_num)
             in
             let nt_name =
               Map.find nonterminal_names nt_num
               |> get_option' (fun () ->
                      Printf.sprintf
                        "Lr0 string_of_symbols: unable to find nonterminal %n in \
                         nonterminal_names"
                        production_num)
             in
             nt_name)
    |> String.concat_array ~sep:" "
 ;;

  let string_of_trace_line { action; stack; results; input } =
    Printf.sprintf
      "action: %s\nstack: %s\nresults: %s\ninput: %s\n\n"
      (string_of_action action)
      (stack |> Array.map ~f:string_of_int |> String.concat_array ~sep:" ")
      (string_of_symbols results)
      (input
      |> Array.map ~f:(fun (tok : Lex.token) -> tok.name)
      |> String.concat_array ~sep:" ")
  ;;

  (* This is the main parsing function: CPTT Algorithm 4.44 / Figure 4.36. *)
  let parse_trace_tables
      :  lr0_action_table -> lr0_goto_table -> do_trace (* trace or not *)
      -> Lex.token Queue.t -> (parse_result, parse_error) Result.t * trace_line array
    =
   fun lr0_action_table lr0_goto_table do_trace toks ->
    (* Re stack / results:
     * These are called `stack` and `symbols` in CPTT. Their structure
     * mirrors one another: there is a 1-1 correspondence between states in
     * `stack` and symbols in `results`, expept that `stack` always has
     * `augmented_state`, at the bottom of its stack.
     *)
    let stack : state Stack.t = Stack.create () in
    Stack.push stack augmented_state;
    let results : parse_result Stack.t = Stack.create () in
    let trace = Queue.create () in
    try
      let a = ref @@ pop_front_exn 0 toks in
      while true do
        (* let x be the state on top of the stack *)
        let s =
          match Stack.top stack with
          | Some s' -> s'
          | None -> failwith "invariant violation: empty stack"
        in
        let tok = !a in
        let terminal_num = token_to_terminal tok in
        let action = lr0_action_table s terminal_num in
        if Caml.(do_trace = DoTrace)
        then
          Queue.enqueue
            trace
            { action
            ; stack = Stack.to_array stack
            ; results = Stack.to_array results
            ; input = Queue.to_array toks
            };
        match action with
        | Shift t ->
          Stack.push stack t;
          Stack.push
            results
            { production = Either.First terminal_num
            ; children = []
            ; start_pos = tok.start
            ; end_pos = tok.finish
            };
          a := pop_front_exn tok.start toks
        | Reduce production_num ->
          let pop_count =
            Hashtbl.find production_map production_num
            |> get_option' (fun () ->
                   Printf.sprintf
                     "Lr0 parse_trace: unable to find production %n in production_map"
                     production_num)
            |> List.length
          in
          (* pop symbols off the stack *)
          let children : parse_result list ref = ref [] in
          let start_pos : int ref = ref 0 in
          let end_pos : int ref = ref 0 in
          for i = 1 to pop_count do
            ignore (Stack.pop stack : state option);
            match Stack.pop results with
            | Some child ->
              children := child :: !children;
              (* Note: children appear in reverse *)
              if i = pop_count then start_pos := child.start_pos;
              if i = 1 then end_pos := child.end_pos
            | None -> failwith "invariant violation: popping from empty stack"
          done;
          let nt_num =
            Hashtbl.find production_nonterminal_map production_num
            |> get_option' (fun () ->
                   Printf.sprintf
                     "Lr0 parse_trace: unable to find production %n in \
                      production_nonterminal_map"
                     production_num)
          in
          (match Stack.top stack with
          | Some t ->
            (match lr0_goto_table t (Nonterminal nt_num) with
            | None -> failwith "invariant violation: invalid GOTO transition"
            | Some state -> Stack.push stack state)
          | None -> failwith "invariant violation: peeking empty stack");
          Stack.push
            results
            { production = Either.Second production_num
            ; children = !children
            ; start_pos = !start_pos
            ; end_pos = !end_pos
            }
        | Accept -> raise ParseFinished
        | Error (Some ReduceReduce) ->
          raise
            (ParseFailed
               ( tok.start
               , Printf.sprintf
                   "parse failed -- reduce/reduce conflict on this token (%s) from state \
                    %n"
                   tok.name
                   s ))
        | Error (Some ShiftReduce) ->
          raise
            (ParseFailed
               ( tok.start
               , Printf.sprintf
                   "parse failed -- shift/reduce conflict on this token (%s) from state \
                    %n"
                   tok.name
                   s ))
        | Error None ->
          let valid_token_msg = Array.get (full_lr0_action_table ()) s
            |> Array.filter ~f:(function
              | Error _ -> false
              | _ -> true
            )
            |> Array.to_list
            |> List.mapi ~f:(fun t_num action ->
                Printf.sprintf "%s: %s" (string_of_terminal t_num) (string_of_action
                action))
            |> String.concat ~sep:"\n"
          in
          raise
            (ParseFailed
               ( tok.start (* TODO: give a decent error message *)
               , Printf.sprintf
                   "parse failed -- no valid transition on this token (%s) from state \
                   %n.\n\
                   valid tokens at this point:\n\
                   %s"
                   tok.name
                   s
                   valid_token_msg))
      done;
      failwith "invariant violation: can't make it here"
    with
    | ParseFinished ->
      (match Stack.length results with
      | 1 ->
        (match Stack.top results with
        | Some result -> Ok result, Queue.to_array trace
        | None -> failwith "invariant violation: no result")
      | 0 -> failwith "invariant violation: no result"
      | n -> failwith (Printf.sprintf "invariant violation: multiple results (%n)" n))
    | ParseFailed parse_error -> Error parse_error, Queue.to_array trace
    | PopFailed pos ->
      Error (pos, "parsing invariant violation -- pop failed"), Queue.to_array trace
 ;;

  let parse_trace
      :  do_trace (* trace or not *) -> Lex.token Queue.t
      -> (parse_result, parse_error) Result.t * trace_line array
    =
    parse_trace_tables lr0_action_table lr0_goto_table
  ;;

  let parse : Lex.token Queue.t -> (parse_result, parse_error) Result.t =
   fun toks -> match parse_trace DontTrace toks with result, _ -> result
 ;;

  let lex_and_parse
      :  Lex.lexer -> string
      -> (parse_result, (LexerUtil.lex_error, parse_error) Either.t) Result.t
    =
   fun lexer input ->
    match Lex.lex lexer input with
    | Error error -> Error (First error)
    | Ok tokens ->
      let len = String.length input in
      let tokens' =
        tokens
        |> Array.filter ~f:(fun (token : Lex.token) -> String.(token.name <> "SPACE"))
        |> Queue.of_array
      in
      Queue.enqueue tokens' { name = "$"; start = len; finish = len };
      Result.map_error (parse tokens') ~f:(fun err -> Either.Second err)
 ;;
end