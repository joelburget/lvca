open Tablecloth
open LrParsing

type lookahead_item =
  { item : item;
    (* the set of terminals that can follow this item *)
    lookahead_set : IntSet.t;
  }

module LookaheadItemCmp = Placemat.Id.MakeComparable(struct
    type t = lookahead_item
    let cmp { item = i1; lookahead_set = s1 } { item = i2; lookahead_set = s2 } =
      match Pervasives.compare i1 i2 with
      | 0 -> Placemat.IntSet.cmp s1 s2
      | c -> c
  end)

module LookaheadClosureItemCmp = Placemat.Id.MakeComparable(struct
    type t = nonterminal_num * terminal_num
    let cmp = Pervasives.compare
  end)

type lookahead_item_set =
  (lookahead_item, LookaheadItemCmp.identity) Placemat.Set.t

let lookahead_item_set_from_array : lookahead_item array -> lookahead_item_set
  = Placemat.Set.from_array ~id:(module LookaheadItemCmp)

type lookahead_configuration_set =
  { kernel_items : lookahead_item_set;    (* set of items *)
    nonkernel_items : lookahead_item_set; (* set of nonterminals *)
  }

let simplify_lookahead_config_set
  : lookahead_configuration_set -> lookahead_item_set
  = fun { kernel_items; nonkernel_items } ->
    Placemat.Set.union kernel_items nonkernel_items

type lookahead_propagation =
  { (** A set of items with lookahead that were generated *)
    spontaneous_generation : (state * lookahead_item) array;
    (** A set of kernel items where the lookahead propagates *)
    propagation : (state * item) array;
  }

module LookaheadItemSetCmp = Placemat.Id.MakeComparable(struct
    type t = lookahead_item_set
    let cmp = Placemat.Set.cmp
  end)

(* A mutable set of lookahead item sets. This is used to represent the set of
 * LR(1) items. Each set represents a set of encoded items with lookaheads.
*)
type mutable_lookahead_item_sets =
  (lookahead_item_set, LookaheadItemSetCmp.identity) MSet.t

(* Set of items with mutable lookahead *)
type mutable_lookahead_item_set = Placemat.MutableSet.Int.t IntDict.t

(* Same as the corresponding lr0 types *)
type lalr1_action_table = state -> terminal_num -> action
type lalr1_goto_table = state -> symbol -> state option

let lookahead_item_set_to_item_set
  : lookahead_item_set -> item_set
  = fun x -> x
             |> Placemat.Set.to_array
             |> Array.map ~f:(fun { item } -> item)
             |> Placemat.IntSet.from_array

let mutable_lookahead_item_set_to_item_set
  : mutable_lookahead_item_set -> item_set
  = fun x -> x
             |> Placemat.IntDict.to_array
             |> Array.map ~f:(fun (k, _) -> k)
             |> Placemat.IntSet.from_array

module type LALR = sig
  include LR0
  val state_to_lookahead_item_set : state -> lookahead_item_set
  val lr1_closure' : lookahead_item_set -> lookahead_configuration_set
  val string_of_lookahead_item_set : lookahead_item_set -> string
  val full_lalr1_action_table : unit -> action array array
  val full_lalr1_goto_table : unit -> (symbol * state option) array array
end

module Lalr1 (G : GRAMMAR) = struct

  module Lr0' = Lr0(G)

  let (
    string_of_item,
    string_of_nonterminal,
    string_of_production,
    string_of_terminal,
    production_map,
    first_set,
    nonterminal_production_map,
    number_of_terminals,
    string_of_production_num,
    item_set_to_state,
    state_to_item_set,
    lr0_goto_kernel,
    lr0_items,
    terminal_names,
    token_to_terminal,
    augmented_state,
    production_nonterminal_map,
    parse_trace_tables,
    end_marker,
    string_of_trace_line,
    string_of_symbols,
    string_of_symbol,
    string_of_action,
    states,
    terminals,
    nonterminals
  ) = Lr0'.(
    string_of_item,
    string_of_nonterminal,
    string_of_production,
    string_of_terminal,
    production_map,
    first_set,
    nonterminal_production_map,
    number_of_terminals,
    string_of_production_num,
    item_set_to_state,
    state_to_item_set,
    lr0_goto_kernel,
    lr0_items,
    terminal_names,
    token_to_terminal,
    augmented_state,
    production_nonterminal_map,
    parse_trace_tables,
    end_marker,
    string_of_trace_line,
    string_of_symbols,
    string_of_symbol,
    string_of_action,
    states,
    terminals,
    nonterminals
  )

  let string_of_lookahead_set = fun lookahead_set -> lookahead_set
    |> Placemat.IntSet.to_array
    |> Array.map
      ~f:(fun t_num -> terminal_names
        |> IntDict.get ~key:t_num
        |> Option.with_default ~default:"#"
    )
    |> Placemat.String.concat_array ~sep:"/"

  (* TODO: move to module *)
  let string_of_lookahead_item = fun { item; lookahead_set } ->
    Printf.sprintf "[%s, %s]"
      (string_of_item item)
      (string_of_lookahead_set lookahead_set)

  let string_of_lookahead_item_set = fun lookahead_item_set ->
    lookahead_item_set
    |> Placemat.Set.to_array
    |> Array.map ~f:string_of_lookahead_item
    |> Placemat.String.concat_array ~sep:"\n"

  let add_to mutable_lookahead_item_set nonterminal_num lookahead =
    match MMI.get mutable_lookahead_item_set nonterminal_num with
    | None -> MMI.set mutable_lookahead_item_set nonterminal_num
                (MSI.from_array [| lookahead |])
    | Some old_set -> MSI.add old_set lookahead

  let add_all_to mutable_lookahead_item_set nonterminal_num lookaheads =
    let lookaheads' = Placemat.IntSet.to_array lookaheads in
    match MMI.get mutable_lookahead_item_set nonterminal_num with
    | None -> MMI.set mutable_lookahead_item_set nonterminal_num
                (MSI.from_array lookaheads')
    | Some old_set -> MSI.merge_many old_set lookaheads'

  (* Convert a mutable set of items with mutable lookahead to an immutable set
   * of lookahead items.
  *)
  let convert : MSI.t MMI.t -> lookahead_item_set
    = fun items -> items
                   |> MMI.to_array
                   |> Array.map ~f:(fun (nonterminal_num, mut_lookahead_set) ->
                     let production_set = nonterminal_production_map
                       |. MMI.get nonterminal_num
                       |> get_option' (fun () -> Printf.sprintf
                         "lr1_closure' convert: unable to find nonterminal %n in nonterminal_production_map (keys: %s)"
                         nonterminal_num
                         (nonterminal_production_map
                           |> MMI.keys_to_array
                           |> Array.map ~f:string_of_int
                           |> Placemat.String.concat_array ~sep:", ")
                                      )
                     in

                     production_set
                     |> MSI.to_array
                     |> Array.map ~f:(fun production_num ->
                       let item = mk_item { production_num; position = 0 } in
                       let lookahead_set = mut_lookahead_set
                                           |> MSI.to_array
                                           |> Placemat.IntSet.from_array
                       in
                       { item; lookahead_set }
                     )
                   )
                   |> Array.concatenate
                   |> lookahead_item_set_from_array

  let lr1_closure' : lookahead_item_set -> lookahead_configuration_set
    = fun initial_items ->

      (* Map from nonterminal number to a (mutable) set of tokens in the
       * lookahead set
      *)
      let nonkernel_items = MMI.make () in
      let kernel_items = Placemat.MutableSet.make ~id:(module LookaheadItemCmp)
      in

      (* Set of (nonterminal, terminal that might be in its lookahead set) to
       * consider.
      *)
      let stack = MStack.make () in

      (* For each initial item, add an entry to the stack containing a
       * nonterminal and lookahead set to consider.
       *
       * Also add it to either kernel_items or nonkernel_items.
       * TODO: should this be a set of lookahead tokens instead of a single one?
      *)
      Placemat.Set.for_each initial_items ~f:(fun lookahead_item ->
        let { item; lookahead_set } = lookahead_item in
        let { production_num; position } = view_item item in

        let nonterminal_num = production_nonterminal_map
                              |. MMI.get_exn production_num
        in

        if production_num = 0 || position > 0
        then Placemat.MutableSet.add kernel_items ~value:lookahead_item
        else add_all_to nonkernel_items nonterminal_num lookahead_set;

        let production = production_map
                         |. MMI.get production_num
                         |> get_option' (fun () -> Printf.sprintf
                                           "lr1_closure': couldn't find production %n"
                                           production_num
                                        )
        in

        Placemat.IntSet.for_each lookahead_set ~f:(fun lookahead_terminal_num ->
          (* first symbol right of the dot *)
          match List.get_at production ~index:position with
          | Some (Nonterminal nt) -> (
              (* Look at first symbol right of nonterminal right of the dot, b.
               * Compute FIRST(ba) where a is the lookahead.
              *)
              let first_set' = match List.get_at production ~index:(position + 1) with
                | None -> first_set [Terminal lookahead_terminal_num]
                | Some symbol
                  -> first_set [symbol; Terminal lookahead_terminal_num]
              in

              Placemat.IntSet.for_each
                first_set'
                ~f:(fun new_lookahead -> MStack.push stack (nt, new_lookahead))
            )
          | _ -> ()
        )
      );

      (* Consider every (nonterminal, lookahead terminal pair), adding it to
       * the nonkernel_items set if appropriate, and adding new entries to the
       * stack if appropriate
      *)
      while not (MStack.is_empty stack) do
        let nonterminal_num, lookahead =
          MStack.pop stack |> get_option' (fun () -> "the set is not empty!")
        in

        let is_added = MMI.has nonkernel_items nonterminal_num &&
                       nonkernel_items |. MMI.get_exn nonterminal_num |. MSI.has lookahead
        in

        if not is_added then (
          add_to nonkernel_items nonterminal_num lookahead;

          let { productions } = IntDict.get G.grammar.nonterminals ~key:nonterminal_num
                                |> get_option' (fun () -> Printf.sprintf
                                                  "lr1_closure': unable to find nonterminal %n in G.grammar.nonterminals"
                                                  nonterminal_num
                                               )
          in


          Placemat.List.for_each productions ~f:(function
            | Terminal _         :: _ -> ()
            | Nonterminal new_nt :: rest ->
              let first_set' = first_set (match rest with
                | [] -> [Terminal lookahead]
                | symbol :: _ -> [symbol; Terminal lookahead]
              ) in
              Placemat.IntSet.for_each first_set' ~f:(fun new_lookahead ->
                MStack.push stack (new_nt, new_lookahead)
              );
            | _ -> failwith "Empty production"
          )
        );
      done;

      { kernel_items = kernel_items
                       |> Placemat.MutableSet.to_array
                       |> lookahead_item_set_from_array;
        nonkernel_items = convert nonkernel_items;
      }

  let lr1_closure : lookahead_item_set -> lookahead_item_set
    = fun items -> simplify_lookahead_config_set @@ lr1_closure' items

  (** Given the kernel (K) of a set of LR(0) items (I) and a grammar symbol
   * (X), return the set of lookaheads in GOTO(I, X) (1) generated
   * spontaneously and (2) propagated from I.
   *
   * CPTT Algorithm 4.62.
   *
   * raises: [NoItemSet]
  *)
  let generate_lookaheads : item_set -> item -> lookahead_propagation
    = fun kernel item ->
      let make, enqueue, to_array =
        Placemat.MutableQueue.(make, enqueue, to_array) in
      let propagation = make () in
      let generated = make () in
      let hash_terminal = number_of_terminals + 1 in
      let modified_item =
        { item; lookahead_set = IntSet.from_list [ hash_terminal ] }
      in
      let j = lr1_closure @@
        lookahead_item_set_from_array [| modified_item |]
      in

      Placemat.Set.for_each j ~f:(fun { lookahead_set; item = pre_item } ->
        let { production_num; position } = view_item pre_item in
        let production = production_map
                         |. MMI.get production_num
                         |> get_option'
                              (fun () ->
                               "generate_lookaheads: failed to get production " ^
                               string_of_int production_num)
        in

        if position = List.length production
        then () (* Already at the end of the production *)
        else (
          let item = mk_item { production_num; position = position + 1 } in

          let x = production
                  |> List.get_at ~index:position
                  |> get_option' (fun () -> Printf.sprintf
                                    "failed to get position %n in production %s"
                                    position
                                    (string_of_production_num production_num)
                                 )
          in

          let goto_kernel = lr0_goto_kernel kernel x in

          if Placemat.IntSet.size goto_kernel > 0 then (
            let state = item_set_to_state @@ goto_kernel in

            (* Another terminal has been spontaneously generated *)
            let lookahead_set' = IntSet.remove lookahead_set ~value:hash_terminal in
            if not (IntSet.isEmpty lookahead_set')
            then enqueue generated
              (state, { item; lookahead_set = lookahead_set' });

            if IntSet.has lookahead_set ~value:hash_terminal
            then enqueue propagation (state, item);
          )
        )
      );

      { propagation = to_array propagation
      ; spontaneous_generation = to_array generated
      }

  (* CPTT Algorithm 4.63 step 1.
   * Convert each item set into a set of items with (empty) mutable lookahead.
  *)
  let mutable_lalr1_items : mutable_lookahead_item_set IntDict.t
    = lr0_items
      |> IntDict.map ~f:(fun items -> items
                             |> Placemat.IntSet.to_array
                             |> Array.map ~f:(fun item -> item, Placemat.MutableSet.Int.make ())
                             |> Placemat.IntDict.from_array
               )

  (* CPTT Algorithm 4.63 steps 2 & 3 *)
  (* For each state:
     for each item:
     what items (in what state) does its lookahead propagate to?

     While we're finding propagation, also fill in spontaneous generation.

     raises: [NoItemSet]
  *)
  let lookahead_propagation : (state * item) array IntDict.t IntDict.t
    = mutable_lalr1_items |> IntDict.map ~f:(fun mutable_lookahead_item_set ->
      let kernel : item_set =
        mutable_lookahead_item_set_to_item_set mutable_lookahead_item_set
      in

      Placemat.IntDict.map_with_key mutable_lookahead_item_set (fun item _ ->
        let { spontaneous_generation; propagation } =
          generate_lookaheads kernel item
        in

        Array.for_each spontaneous_generation
          ~f:(fun (state, { item; lookahead_set }) ->
             mutable_lalr1_items
             |> IntDict.get ~key:state
             |> get_option' (fun () ->
               "lookahead_propagation: state not present in mutable_lalr1_items")
             |> IntDict.get ~key:item
             |> get_option' (fun () ->
               "lookahead_propagation: item not present in mutable_lalr1_items")
             |. Placemat.MutableSet.Int.merge_many (Placemat.IntSet.to_array lookahead_set);
          );

        propagation
      )
    )

  (* Special-case augmented item `S' -> . S` with lookahead $. *)
  let () = mutable_lalr1_items
           |> IntDict.get ~key:0
           |> get_option' (fun () ->
             "Lalr1: augmented state not present in mutable_lalr1_items")
           |> IntDict.get ~key:(mk_item' 0 0)
           |> get_option' (fun () ->
             "Lalr1: augmented item not present in mutable_lalr1_items")
           |. Placemat.MutableSet.Int.add 0

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

      Placemat.IntDict.for_each mutable_lalr1_items
        ~f:(fun source_state mutable_lookahead_item_set ->
        Placemat.IntDict.for_each mutable_lookahead_item_set
          ~f:(fun source_item source_lookahead ->

          (* lookaheads that propagate from the item we're currently looking at
          *)
          let propagation : (state * item) array = lookahead_propagation
                                                   |> IntDict.get ~key:source_state
                                                   |> get_option' (fun () -> Printf.sprintf
                                                                     "step 4 lookahead_propagation: couldn't find state %n" source_state
                                                                  )
                                                   |> IntDict.get ~key:source_item
                                                   |> get_option' (fun () -> Printf.sprintf
                                                                     "step 4 lookahead_propagation: couldn't find item %s in state %n"
                                                                     (string_of_item source_item)
                                                                     source_state
                                                                  )
          in

          (* See if we can propagate any lookaheads (and do it) *)
          Array.for_each propagation ~f:(fun (target_state, target_item) ->
            let target_lookahead : MSI.t = mutable_lalr1_items
                                           |> IntDict.get ~key:target_state
                                           |> get_option' (fun () -> Printf.sprintf
                                                             "step 4 mutable_lalr1_items: couldn't find state %n" target_state
                                                          )
                                           |> IntDict.get ~key:target_item
                                           |> get_option' (fun () -> Printf.sprintf
                                                             "step 4 mutable_lalr1_items: couldn't find item %s in state %n"
                                                             (string_of_item target_item)
                                                             target_state
                                                          )
            in

            if not (MSI.subset source_lookahead target_lookahead)
            then (
              made_update := true;
              MSI.merge_many target_lookahead (MSI.to_array source_lookahead);
            )
          );
        );
      );
    done

  let lalr1_items : lookahead_item_set IntDict.t
    = mutable_lalr1_items
      |> IntDict.map ~f:(fun mutable_lookahead_item_set -> mutable_lookahead_item_set
        |> Placemat.IntDict.to_array
        |> Array.map ~f:(fun (item, mutable_lookahead) ->
          { item; lookahead_set = mutable_lookahead
            |> Placemat.MutableSet.Int.to_array
            |> Placemat.IntSet.from_array
          }
        )
        |> lookahead_item_set_from_array
      )

  let lalr1_goto_table : lalr1_goto_table = fun state nt ->
    try
      Some (item_set_to_state @@
            lr0_goto_kernel (state_to_item_set state) nt)
    with
      NoItemSet _ -> None

  let state_to_lookahead_item_set
    : state -> lookahead_item_set
    = fun state -> lalr1_items
      |> IntDict.get ~key:state
      |> get_option' (fun () -> "state_to_lookahead_item_set: state not found")

  let lalr1_action_table : lalr1_action_table
    = fun state terminal_num ->

      let item_set_l : lookahead_item list
        = state
          |> state_to_lookahead_item_set
          |> lr1_closure
          |> Placemat.Set.to_array
          |> Array.to_list
      in

      (* If [A -> xs . a ys, b] is in I_i and GOTO(I_i, a) = I_j, set
       * ACTION[i, a] to `shift j` *)
      let shift_action = Util.find_by item_set_l @@ fun l_item ->
        let { production_num; position } = view_item l_item.item in
        let symbols = production_map
                      |. MMI.get production_num
                      |> get_option' (fun () -> Printf.sprintf
                                        "Lalr1 shift_action: unable to find production %n in production_map"
                                        production_num
                                     )
        in

        match List.get_at symbols ~index:position with
        | Some (Terminal t_num as next_symbol) ->
          if t_num = terminal_num
          then lalr1_goto_table state next_symbol
            |> Option.map ~f:(fun x -> Shift x)
          else None
        | _ -> None
      in

      (* If [A -> xs ., a] is in I_i, set ACTION[i, a] to `Reduce A -> xs` *)
      let reduce_action = Util.find_by item_set_l @@ fun l_item ->
        let { item; lookahead_set } = l_item in
        let { production_num; position } = view_item item in
        let nt_num = production_nonterminal_map
                     |. MMI.get production_num
                     |> get_option' (fun () -> Printf.sprintf
                                       "Lalr1 shift_action: unable to find production %n in production_nonterminal_map"
                                       production_num
                                    )
        in
        let production = production_map
                         |. MMI.get production_num
                         |> get_option' (fun () -> Printf.sprintf
                                           "Lalr1 shift_action: unable to find production %n in production_map"
                                           production_num
                                        )
        in
        if position = List.length production &&
           IntSet.has lookahead_set ~value:terminal_num &&
           (* Accept in this case (end marker on the augmented nonterminal) --
              don't reduce. *)
           nt_num != 0
        then Some (Reduce production_num)
        else None
      in

      (* If [S' -> S .] is in I_i, set ACTION[i, $] to `accept` *)
      let accept_action = Util.find_by item_set_l @@ fun l_item ->
        if l_item.item = mk_item' 0 1 && terminal_num = end_marker
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

  let full_lalr1_action_table : unit -> action array array
    = fun () -> states |> Array.map ~f:(fun state ->
      terminals |> Array.map ~f:(lalr1_action_table state)
    )

  let full_lalr1_goto_table : unit -> (symbol * state option) array array
    = fun () -> states
                |> Array.map ~f:(fun state ->
                  nonterminals
                  |> Array.map ~f:(fun nt ->
                    let sym = Nonterminal nt in
                    sym, lalr1_goto_table state sym
                  )
                )

  (* This is the main parsing function: CPTT Algorithm 4.44 / Figure 4.36. *)
  let parse_trace
    : do_trace (* trace or not *)
      -> Lex.token MQueue.t
      -> (parse_error, parse_result) Result.t * trace_line array

    = parse_trace_tables lalr1_action_table lalr1_goto_table

  let parse : Lex.token MQueue.t -> (parse_error, parse_result) Result.t
    = fun toks ->
      let result, _ = parse_trace DoTrace toks in
      result

  let lex_and_parse : Lex.lexer -> string
    -> ((Lex.lex_error, parse_error) Either.t, parse_result) Result.t
    = fun lexer input -> match Lex.lex lexer input with
      | Error error -> Error (Left error)
      | Ok tokens ->
        let len = String.length input in
        let tokens' = tokens
          |> Array.filter ~f:(fun (token: Lex.token) -> token.name != "SPACE")
          |> MQueue.from_array
        in
        (* TODO: name might not always be "$" *)
        MQueue.enqueue tokens' { name = "$"; start = len; finish = len };
        parse tokens' |. Util.map_error ~f:(fun err -> Either.Right err)

end
