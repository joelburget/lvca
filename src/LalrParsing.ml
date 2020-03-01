open Core_kernel
open LrParsing
module Lex = Placemat.Lex

module LookaheadItem = struct
  module T = struct
    type t =
      { item : item
      ; (* the set of terminals that can follow this item *)
        lookahead_set : Int.Set.t
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module LookaheadClosureItem = struct
  module T = struct
    type t = nonterminal_num * terminal_num [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module LookaheadItemSet = struct
  module T = struct
    type t = (LookaheadItem.t, LookaheadItem.comparator_witness) Set.t

    let sexp_of_t = Set.sexp_of_m__t (module LookaheadItem)
    let t_of_sexp = Set.m__t_of_sexp (module LookaheadItem)
    let compare = Set.compare_m__t (module LookaheadItem)
  end

  include T
  include Comparable.Make (T)
end

let lookahead_item_set_from_array : LookaheadItem.t array -> LookaheadItemSet.t =
  Set.of_array (module LookaheadItem)
;;

type lookahead_configuration_set =
  { kernel_items : LookaheadItemSet.t
  ; (* set of items *)
    nonkernel_items : LookaheadItemSet.t (* set of nonterminals *)
  }

let simplify_lookahead_config_set : lookahead_configuration_set -> LookaheadItemSet.t =
 fun { kernel_items; nonkernel_items } -> Set.union kernel_items nonkernel_items
;;

type lookahead_propagation =
  { spontaneous_generation : (state * LookaheadItem.t) array
        (** A set of items with lookahead that were generated *)
  ; propagation : (state * item) array
        (** A set of kernel items where the lookahead propagates *)
  }

(* A mutable set of lookahead item sets. This is used to represent the set of
 * LR(1) items. Each set represents a set of encoded items with lookaheads.
 *)
type mutable_lookahead_item_sets = LookaheadItemSet.t Hash_set.t

(* Set of items with mutable lookahead (map from item number to its mutable
 * lookahead set) *)
module MutableLookaheadItemSet = struct
  module T = struct
    type t = Int.Hash_set.t Int.Map.t [@@deriving sexp]
  end

  include T

  (* include Comparable.Make(T) *)
end

(* Same as the corresponding lr0 types *)
type lalr1_action_table = state -> terminal_num -> action
type lalr1_goto_table = state -> symbol -> state option

let lookahead_item_set_to_item_set : LookaheadItemSet.t -> item_set =
 fun x -> x |> Set.to_array |> Array.map ~f:(fun { item; _ } -> item) |> Int.Set.of_array
;;

let mutable_lookahead_item_set_to_item_set : MutableLookaheadItemSet.t -> item_set =
 fun x -> x |> Int.Map.to_alist |> List.map ~f:(fun (k, _) -> k) |> Int.Set.of_list
;;

module type LALR = sig
  include LR0

  val state_to_lookahead_item_set : state -> LookaheadItemSet.t
  val lr1_closure' : LookaheadItemSet.t -> lookahead_configuration_set
  val string_of_lookahead_item_set : LookaheadItemSet.t -> string
  val full_lalr1_action_table : unit -> action array array
  val full_lalr1_goto_table : unit -> (symbol * state option) array array
end

module Lalr1 (G : GRAMMAR) = struct
  module Lr0' = Lr0 (G)

  let ( augmented_grammar
      , string_of_item
      , string_of_nonterminal
      , string_of_production
      , string_of_terminal
      , production_map
      , first_set
      , nonterminal_production_map
      , number_of_terminals
      , string_of_production_num
      , item_set_to_state
      , state_to_item_set
      , lr0_goto_kernel
      , lr0_items
      , terminal_names
      , token_to_terminal
      , augmented_state
      , production_nonterminal_map
      , parse_trace_tables
      , end_marker
      , string_of_trace_line
      , string_of_symbols
      , string_of_symbol
      , string_of_action
      , states
      , terminals
      , nonterminal_nums_arr
      , nonterminal_map )
    =
    Lr0'.(
      ( augmented_grammar
      , string_of_item
      , string_of_nonterminal
      , string_of_production
      , string_of_terminal
      , production_map
      , first_set
      , nonterminal_production_map
      , number_of_terminals
      , string_of_production_num
      , item_set_to_state
      , state_to_item_set
      , lr0_goto_kernel
      , lr0_items
      , terminal_names
      , token_to_terminal
      , augmented_state
      , production_nonterminal_map
      , parse_trace_tables
      , end_marker
      , string_of_trace_line
      , string_of_symbols
      , string_of_symbol
      , string_of_action
      , states
      , terminals
      , nonterminal_nums_arr
      , nonterminal_map ))
  ;;

  let string_of_lookahead_set lookahead_set =
    lookahead_set
    |> Int.Set.to_array
    |> Array.map ~f:(fun t_num ->
           Int.Map.find terminal_names t_num |> Option.value ~default:"#")
    |> String.concat_array ~sep:"/"
  ;;

  (* TODO: move to module *)
  let string_of_lookahead_item : LookaheadItem.t -> string =
   fun { item; lookahead_set; _ } ->
    Printf.sprintf
      "[%s, %s]"
      (string_of_item item)
      (string_of_lookahead_set lookahead_set)
 ;;

  let string_of_lookahead_item_set lookahead_item_set =
    lookahead_item_set
    |> Set.to_array
    |> Array.map ~f:string_of_lookahead_item
    |> String.concat_array ~sep:"\n"
  ;;

  let add_to mutable_lookahead_item_set nonterminal_num lookahead =
    match MMI.find mutable_lookahead_item_set nonterminal_num with
    | None ->
      MMI.set
        mutable_lookahead_item_set
        ~key:nonterminal_num
        ~data:(MSI.of_list [ lookahead ])
    | Some old_set -> MSI.add old_set lookahead
  ;;

  let add_all_to mutable_lookahead_item_set nonterminal_num lookaheads =
    let lookaheads' = Int.Set.to_list lookaheads in
    match MMI.find mutable_lookahead_item_set nonterminal_num with
    | None ->
      MMI.set
        mutable_lookahead_item_set
        ~key:nonterminal_num
        ~data:(MSI.of_list lookaheads')
    | Some old_set -> MSI.merge_many old_set lookaheads'
  ;;

  (* Convert a mutable set of items with mutable lookahead to an immutable set
   * of lookahead items.
   *)
  let convert : MSI.t MMI.t -> LookaheadItemSet.t =
   fun items ->
    items
    |> MMI.to_alist
    |> List.map ~f:(fun (nonterminal_num, mut_lookahead_set) ->
           let production_set =
             MMI.find nonterminal_production_map nonterminal_num
             |> get_option' (fun () ->
                    Printf.sprintf
                      "lr1_closure' convert: unable to find nonterminal %n in \
                       nonterminal_production_map (keys: %s)"
                      nonterminal_num
                      (nonterminal_production_map
                      |> MMI.keys
                      |> List.to_array
                      |> Array.map ~f:string_of_int
                      |> String.concat_array ~sep:", "))
           in
           production_set
           |> MSI.to_array
           |> Array.map ~f:(fun production_num ->
                  let item = mk_item { production_num; position = 0 } in
                  let lookahead_set =
                    mut_lookahead_set |> MSI.to_array |> Int.Set.of_array
                  in
                  ({ item; lookahead_set } : LookaheadItem.t)))
    |> Array.concat
    |> lookahead_item_set_from_array
 ;;

  let lr1_closure' : LookaheadItemSet.t -> lookahead_configuration_set =
   fun initial_items ->
    (* Map from nonterminal number to a (mutable) set of tokens in the
     * lookahead set
     *)
    let nonkernel_items = MMI.create () in
    let kernel_items = MutableSet.create (module LookaheadItem) in
    (* Set of (nonterminal, terminal that might be in its lookahead set) to
     * consider.
     *)
    let stack = MStack.create () in
    (* For each initial item, add an entry to the stack containing a
     * nonterminal and lookahead set to consider.
     *
     * Also add it to either kernel_items or nonkernel_items.
     * TODO: should this be a set of lookahead tokens instead of a single one?
     *)
    Set.iter initial_items ~f:(fun lookahead_item ->
        let ({ item; lookahead_set } : LookaheadItem.t) = lookahead_item in
        let { production_num; position } = view_item item in
        let nonterminal_num = MMI.find_exn production_nonterminal_map production_num in
        if production_num = 0 || position > 0
        then MutableSet.add kernel_items lookahead_item
        else add_all_to nonkernel_items nonterminal_num lookahead_set;
        let production =
          MMI.find production_map production_num
          |> get_option' (fun () ->
                 Printf.sprintf "lr1_closure': couldn't find production %n" production_num)
        in
        Int.Set.iter lookahead_set ~f:(fun lookahead_terminal_num ->
            (* first symbol right of the dot *)
            match List.nth production position with
            | Some (Nonterminal nt) ->
              (* Look at first symbol right of nonterminal right of the dot, b.
               * Compute FIRST(ba) where a is the lookahead.
               *)
              let first_set' =
                match List.nth production (position + 1) with
                | None -> first_set [ Terminal lookahead_terminal_num ]
                | Some symbol -> first_set [ symbol; Terminal lookahead_terminal_num ]
              in
              Int.Set.iter first_set' ~f:(fun new_lookahead ->
                  MStack.push stack (nt, new_lookahead))
            | _ -> ()));
    (* Consider every (nonterminal, lookahead terminal pair), adding it to
     * the nonkernel_items set if appropriate, and adding new entries to the
     * stack if appropriate
     *)
    while not (MStack.is_empty stack) do
      let nonterminal_num, lookahead =
        MStack.pop stack |> get_option' (fun () -> "the set is not empty!")
      in
      let is_added =
        MMI.mem nonkernel_items nonterminal_num
        && nonkernel_items
           |> Fn.flip MMI.find_exn nonterminal_num
           |> Fn.flip MSI.mem lookahead
      in
      if not is_added
      then (
        add_to nonkernel_items nonterminal_num lookahead;
        let { productions } =
          Int.Map.find nonterminal_map nonterminal_num
          |> get_option' (fun () ->
                 Printf.sprintf
                   "lr1_closure': unable to find nonterminal %n in nonterminal_map"
                   nonterminal_num)
        in
        List.iter productions ~f:(function
            | Terminal _ :: _ -> ()
            | Nonterminal new_nt :: rest ->
              let first_set' =
                first_set
                  (match rest with
                  | [] -> [ Terminal lookahead ]
                  | symbol :: _ -> [ symbol; Terminal lookahead ])
              in
              Int.Set.iter first_set' ~f:(fun new_lookahead ->
                  MStack.push stack (new_nt, new_lookahead))
            | _ -> failwith "Empty production"))
    done;
    { kernel_items = kernel_items |> MutableSet.to_array |> lookahead_item_set_from_array
    ; nonkernel_items = convert nonkernel_items
    }
 ;;

  let lr1_closure : LookaheadItemSet.t -> LookaheadItemSet.t =
   fun items -> simplify_lookahead_config_set @@ lr1_closure' items
 ;;

  (** Given the kernel (K) of a set of LR(0) items (I) and a grammar symbol * (X), return
      the set of lookaheads in GOTO(I, X) (1) generated * spontaneously and (2) propagated
      from I. * * CPTT Algorithm 4.62. * * raises: [NoItemSet] *)
  let generate_lookaheads : item_set -> item -> lookahead_propagation =
   fun kernel item ->
    let create, enqueue, to_array = Queue.(create, enqueue, to_array) in
    let propagation = create () in
    let generated = create () in
    let hash_terminal = number_of_terminals + 1 in
    let modified_item : LookaheadItem.t =
      { item; lookahead_set = Int.Set.of_list [ hash_terminal ] }
    in
    let j = lr1_closure @@ lookahead_item_set_from_array [| modified_item |] in
    Set.iter j ~f:(fun { lookahead_set; item = pre_item } ->
        let { production_num; position } = view_item pre_item in
        let production =
          MMI.find production_map production_num
          |> get_option' (fun () ->
                 "generate_lookaheads: failed to get production "
                 ^ string_of_int production_num)
        in
        if position = List.length production
        then () (* Already at the end of the production *)
        else (
          let item = mk_item { production_num; position = position + 1 } in
          let x =
            List.nth production position
            |> get_option' (fun () ->
                   Printf.sprintf
                     "failed to get position %n in production %s"
                     position
                     (string_of_production_num production_num))
          in
          let goto_kernel = lr0_goto_kernel kernel x in
          if Int.Set.length goto_kernel > 0
          then (
            let state = item_set_to_state @@ goto_kernel in
            (* Another terminal has been spontaneously generated *)
            let lookahead_set' = Int.Set.remove lookahead_set hash_terminal in
            if not (Int.Set.is_empty lookahead_set')
            then
              enqueue
                generated
                (state, ({ item; lookahead_set = lookahead_set' } : LookaheadItem.t));
            if Int.Set.mem lookahead_set hash_terminal
            then enqueue propagation (state, item))));
    { propagation = to_array propagation; spontaneous_generation = to_array generated }
 ;;

  (* CPTT Algorithm 4.63 step 1.
   * Convert each item set into a set of items with (empty) mutable lookahead.
   *)
  let mutable_lalr1_items : MutableLookaheadItemSet.t Int.Map.t =
    lr0_items
    |> Int.Map.map ~f:(fun items ->
           items
           |> Int.Set.to_list
           |> List.map ~f:(fun item -> item, Int.Hash_set.create ())
           |> Int.Map.of_alist_exn)
  ;;

  (* CPTT Algorithm 4.63 steps 2 & 3 *)
  (* For each state: for each item: what items (in what state) does its lookahead
     propagate to?

     While we're finding propagation, also fill in spontaneous generation.

     raises: [NoItemSet] *)
  let lookahead_propagation : (state * item) array Int.Map.t Int.Map.t =
    mutable_lalr1_items
    |> Int.Map.map ~f:(fun mutable_lookahead_item_set ->
           let kernel : item_set =
             mutable_lookahead_item_set_to_item_set mutable_lookahead_item_set
           in
           Map.mapi mutable_lookahead_item_set ~f:(fun ~key:item ~data:_ ->
               let { spontaneous_generation; propagation } =
                 generate_lookaheads kernel item
               in
               Array.iter
                 spontaneous_generation
                 ~f:(fun (state, { item; lookahead_set }) ->
                   let mutable_generation =
                     Int.Map.find mutable_lalr1_items state
                     |> get_option' (fun () ->
                            "lookahead_propagation: state not present in \
                             mutable_lalr1_items")
                     |> Fn.flip Int.Map.find item
                     |> get_option' (fun () ->
                            "lookahead_propagation: item not present in \
                             mutable_lalr1_items")
                   in
                   Int.Set.iter lookahead_set ~f:(Hash_set.add mutable_generation));
               propagation))
  ;;

  (* Special-case augmented item `S' -> . S` with lookahead $. *)
  let () =
    Int.Map.find mutable_lalr1_items 0
    |> get_option' (fun () -> "Lalr1: augmented state not present in mutable_lalr1_items")
    |> Fn.flip Int.Map.find (mk_item' 0 0)
    |> get_option' (fun () -> "Lalr1: augmented item not present in mutable_lalr1_items")
    |> Fn.flip Hash_set.add 0
  ;;

  (* Fill in LALR(1) item lookaheads. CPTT Algorithm 4.63 step 4
   *
   * raises: [NoItemSet]
   *)
  let () =
    (* Set lookaheads that were generated spontaneously *)

    (* Propagate lookaheads *)
    let made_update = ref true in
    while !made_update do
      made_update := false;
      Int.Map.iteri
        mutable_lalr1_items
        ~f:(fun ~key:source_state ~data:mutable_lookahead_item_set ->
          Int.Map.iteri
            mutable_lookahead_item_set
            ~f:(fun ~key:source_item ~data:source_lookahead ->
              (* lookaheads that propagate from the item we're currently looking at *)
              let propagation : (state * item) array =
                Int.Map.find lookahead_propagation source_state
                |> get_option' (fun () ->
                       Printf.sprintf
                         "step 4 lookahead_propagation: couldn't find state %n"
                         source_state)
                |> Fn.flip Int.Map.find source_item
                |> get_option' (fun () ->
                       Printf.sprintf
                         "step 4 lookahead_propagation: couldn't find item %s in state %n"
                         (string_of_item source_item)
                         source_state)
              in
              (* See if we can propagate any lookaheads (and do it) *)
              Array.iter propagation ~f:(fun (target_state, target_item) ->
                  let target_lookahead : Int.Hash_set.t =
                    Int.Map.find mutable_lalr1_items target_state
                    |> get_option' (fun () ->
                           Printf.sprintf
                             "step 4 mutable_lalr1_items: couldn't find state %n"
                             target_state)
                    |> Fn.flip Int.Map.find target_item
                    |> get_option' (fun () ->
                           Printf.sprintf
                             "step 4 mutable_lalr1_items: couldn't find item %s in state \
                              %n"
                             (string_of_item target_item)
                             target_state)
                  in
                  if not (Util.Hash_set.is_subset source_lookahead ~of_:target_lookahead)
                  then (
                    made_update := true;
                    Hash_set.iter source_lookahead ~f:(Hash_set.add target_lookahead)))))
    done
  ;;

  let lalr1_items : LookaheadItemSet.t Int.Map.t =
    mutable_lalr1_items
    |> Map.map ~f:(fun mutable_lookahead_item_set ->
           mutable_lookahead_item_set
           |> Map.to_alist
           |> Array.of_list
           |> Array.map ~f:(fun (item, mutable_lookahead) ->
                  ({ item
                   ; lookahead_set =
                       mutable_lookahead |> Hash_set.to_array |> Int.Set.of_array
                   }
                    : LookaheadItem.t))
           |> lookahead_item_set_from_array)
  ;;

  let lalr1_goto_table : lalr1_goto_table =
   fun state nt ->
    try Some (item_set_to_state @@ lr0_goto_kernel (state_to_item_set state) nt) with
    | NoItemSet _ -> None
 ;;

  let state_to_lookahead_item_set : state -> LookaheadItemSet.t =
   fun state ->
    Int.Map.find lalr1_items state
    |> get_option' (fun () -> "state_to_lookahead_item_set: state not found")
 ;;

  let lalr1_action_table : lalr1_action_table =
   fun state terminal_num ->
    let item_set_l : LookaheadItem.t list =
      state |> state_to_lookahead_item_set |> lr1_closure |> Set.to_array |> Array.to_list
    in
    (* If [A -> xs . a ys, b] is in I_i and GOTO(I_i, a) = I_j, set
     * ACTION[i, a] to `shift j` *)
    let shift_action =
      List.find_map item_set_l ~f:(fun l_item ->
          let { production_num; position } = view_item l_item.item in
          let symbols =
            MMI.find production_map production_num
            |> get_option' (fun () ->
                   Printf.sprintf
                     "Lalr1 shift_action: unable to find production %n in production_map"
                     production_num)
          in
          match List.nth symbols position with
          | Some (Terminal t_num as next_symbol) ->
            if t_num = terminal_num
            then lalr1_goto_table state next_symbol |> Option.map ~f:(fun x -> Shift x)
            else None
          | _ -> None)
    in
    (* If [A -> xs ., a] is in I_i, set ACTION[i, a] to `Reduce A -> xs` *)
    let reduce_action =
      List.find_map item_set_l ~f:(fun l_item ->
          let ({ item; lookahead_set } : LookaheadItem.t) = l_item in
          let { production_num; position } = view_item item in
          let nt_num =
            MMI.find production_nonterminal_map production_num
            |> get_option' (fun () ->
                   Printf.sprintf
                     "Lalr1 shift_action: unable to find production %n in \
                      production_nonterminal_map"
                     production_num)
          in
          let production =
            MMI.find production_map production_num
            |> get_option' (fun () ->
                   Printf.sprintf
                     "Lalr1 shift_action: unable to find production %n in production_map"
                     production_num)
          in
          if position = List.length production
             && Int.Set.mem lookahead_set terminal_num
             && (* Accept in this case (end marker on the augmented nonterminal) -- don't
                   reduce. *)
             nt_num <> 0
          then Some (Reduce production_num)
          else None)
    in
    (* If [S' -> S .] is in I_i, set ACTION[i, $] to `accept` *)
    let accept_action =
      List.find_map item_set_l ~f:(fun l_item ->
          if l_item.item = mk_item' 0 1 && terminal_num = end_marker
          then Some Accept
          else None)
    in
    (* We should always have exactly one action, otherwise it's a
     * shift-reduce(-accept) conflict.
     *)
    match shift_action, reduce_action, accept_action with
    | Some act, None, None | None, Some act, None | None, None, Some act -> act
    | Some _, Some _, None -> Error (Some ShiftReduce)
    | _, _, _ -> Error None
 ;;

  let full_lalr1_action_table : unit -> action array array =
   fun () ->
    states
    |> Array.map ~f:(fun state -> terminals |> Array.map ~f:(lalr1_action_table state))
 ;;

  let full_lalr1_goto_table : unit -> (symbol * state option) array array =
   fun () ->
    states
    |> Array.map ~f:(fun state ->
           nonterminal_nums_arr
           |> Array.map ~f:(fun nt ->
                  let sym = Nonterminal nt in
                  sym, lalr1_goto_table state sym))
 ;;

  (* This is the main parsing function: CPTT Algorithm 4.44 / Figure 4.36. *)
  let parse_trace
      :  do_trace (* trace or not *) -> Lex.token MQueue.t
      -> (parse_result, parse_error) Result.t * trace_line array
    =
    parse_trace_tables lalr1_action_table lalr1_goto_table
  ;;

  let parse : Lex.token MQueue.t -> (parse_result, parse_error) Result.t =
   fun toks ->
    let result, _ = parse_trace DoTrace toks in
    result
 ;;

  let lex_and_parse
      :  Lex.lexer -> string
      -> (parse_result, (Lex.lex_error, parse_error) Either.t) Result.t
    =
   fun lexer input ->
    match Lex.lex lexer input with
    | Error error -> Error (First error)
    | Ok tokens ->
      let len = String.length input in
      let tokens' =
        tokens
        |> Array.filter ~f:(fun (token : Lex.token) -> String.(token.name <> "SPACE"))
        |> MQueue.of_array
      in
      (* TODO: name might not always be "$" *)
      MQueue.enqueue tokens' { name = "$"; start = len; finish = len };
      parse tokens' |> Result.map_error ~f:(fun err -> Either.Second err)
 ;;
end
