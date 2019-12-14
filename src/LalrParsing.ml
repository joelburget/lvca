open LrParsing

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

type lookahead_item_set = (lookahead_item, LookaheadItemCmp.identity) S.t

let lookahead_item_set_from_array : lookahead_item array -> lookahead_item_set
  = S.fromArray ~id:(module LookaheadItemCmp)

type lookahead_configuration_set =
  { kernel_items : lookahead_item_set;    (* set of items *)
    nonkernel_items : lookahead_item_set; (* set of nonterminals *)
  }

let simplify_lookahead_config_set
  : lookahead_configuration_set -> lookahead_item_set
  = fun { kernel_items; nonkernel_items } ->
    S.union kernel_items nonkernel_items

type lookahead_propagation =
  { (** A set of items with lookahead that were generated *)
    spontaneous_generation : (state * lookahead_item) array;
    (** A set of kernel items where the lookahead propagates *)
    propagation : (state * item) array;
  }

module LookaheadItemSetCmp = Belt.Id.MakeComparable(struct
  type t = lookahead_item_set
  let cmp = S.cmp
end)

(* A mutable set of lookahead item sets. This is used to represent the set of
 * LR(1) items. Each set represents a set of encoded items with lookaheads.
 *)
type mutable_lookahead_item_sets =
  (lookahead_item_set, LookaheadItemSetCmp.identity) MSet.t

(* Set of items with mutable lookahead *)
type mutable_lookahead_item_set = Belt.MutableSet.Int.t M.t

(* Same as the corresponding lr0 types *)
type lalr1_action_table = state -> terminal_num -> action
type lalr1_goto_table = state -> symbol -> state option

let lookahead_item_set_to_item_set
  : lookahead_item_set -> item_set
  = fun x -> x
  |. S.toArray
  |. Belt.Array.map (fun { item } -> item)
  |. SI.fromArray

let mutable_lookahead_item_set_to_item_set
  : mutable_lookahead_item_set -> item_set
  = fun x -> x
  |. M.keysToArray
  |. SI.fromArray

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
    string_of_action,
    states,
    terminals,
    nonterminals
  )

  let string_of_lookahead_set = fun lookahead_set -> lookahead_set
    |. SI.toArray
    |. Belt.Array.map
      (fun t_num -> Belt.Map.Int.getWithDefault terminal_names t_num "#")
    |. Js.Array2.joinWith "/"

  (* TODO: move to module *)
  let string_of_lookahead_item = fun { item; lookahead_set } ->
    Printf.sprintf "[%s, %s]"
      (string_of_item item)
      (string_of_lookahead_set lookahead_set)

  let string_of_lookahead_item_set = fun lookahead_item_set ->
    lookahead_item_set
      |. S.toArray
      |. Belt.Array.map string_of_lookahead_item
      |. Js.Array2.joinWith "\n"

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
              "lr1_closure': couldn't find production %n"
              production_num
            )
        in
        lookahead_set |. SI.forEach (fun lookahead_terminal_num ->
          (* first symbol right of the dot *)
          match Belt.List.get production position with
            | Some (Nonterminal nt) -> (
              let first_set' =
                match Belt.List.get production (position + 1) with
                | None -> first_set [Terminal lookahead_terminal_num]
                | Some (Nonterminal nt')
                -> first_set [Nonterminal nt'; Terminal lookahead_terminal_num]
                | Some (Terminal t) -> SI.empty
              in

              SI.forEach
                first_set'
                (fun new_lookahead -> MStack.push stack (nt, new_lookahead))
            )
            | _ -> ()
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

        let is_added = MMI.has nonkernel_items nonterminal_num &&
          nonkernel_items |. MMI.getExn nonterminal_num |. MSI.has lookahead
        in

        if not is_added then (
          (match MMI.get nonkernel_items nonterminal_num with
            | None -> MMI.set nonkernel_items nonterminal_num
              (MSI.fromArray [| lookahead |])
            | Some old_set -> MSI.add old_set lookahead);

          let { productions } = M.get G.grammar.nonterminals nonterminal_num
            |> get_option' (Printf.sprintf
            "lr1_closure': unable to find nonterminal %n in G.grammar.nonterminals"
            nonterminal_num
            )
          in

          productions
            |. Belt.List.toArray
            |. Belt.Array.forEach (function
            | Terminal _         :: _ -> ()
            | Nonterminal new_nt :: rest ->
              let first_set' = first_set (match rest with
                | [] -> [Terminal lookahead]
                | symbol :: _ -> [symbol; Terminal lookahead]
              ) in
              SI.forEach first_set' (fun new_lookahead ->
                MStack.push stack (new_nt, new_lookahead)
              );
            | _ -> failwith "Empty production"
          )
        );
      done;

      { kernel_items = initial_items;
        nonkernel_items = nonkernel_items
          |. MMI.toArray
          |. A.map (fun (nonterminal_num, mut_lookahead_set) ->
            let production_set =
              MMI.get nonterminal_production_map nonterminal_num
                |> get_option' (Printf.sprintf
                "lr1_closure': unable to find nonterminal %n nonterminal_production_map"
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

  let lr1_closure : lookahead_item_set -> lookahead_item_set
    = fun items -> simplify_lookahead_config_set @@ lr1_closure' items

  (** Given the kernel (K) of a set of LR(0) items (I) and a grammar symbol
   * (X), return the set of lookaheads in GOTO(I, X) (1) generated
   * spontaneously and (2) propagated from I *)
  let generate_lookaheads : item_set -> item -> lookahead_propagation
    = fun kernel item ->
      let propagation = [||] in
      let generated = [||] in
      let hash_terminal = number_of_terminals + 1 in
      let modified_item =
        { item; lookahead_set = SI.fromArray [| hash_terminal |] }
      in
      let j = lr1_closure @@
        lookahead_item_set_from_array [| modified_item |]
      in

      Belt.Set.forEach j (fun { lookahead_set; item = pre_item } ->
        let { production_num; position } = view_item pre_item in
        let production = production_map
          |. MMI.get production_num
          |> get_option' ("failed to get production " ^ string_of_int production_num)
        in

        if position = Belt.List.length production
        then () (* Already at the end of the production *)
        else (
          let item = mk_item { production_num; position = position + 1 } in

          let x = production
            |. Belt.List.get position
            |> get_option' (Printf.sprintf
              "failed to get position %n in production %s"
              position
              (string_of_production_num production_num)
            )
          in

          let state = item_set_to_state @@ lr0_goto_kernel kernel x in

          (* Another terminal has been spontaneously generated *)
          let lookahead_set' = SI.remove lookahead_set hash_terminal in
          if not (SI.isEmpty lookahead_set')
          then (
            let _ = Js.Array2.push generated (state, { item; lookahead_set = lookahead_set' }) in
            ()
          );

          if SI.has lookahead_set hash_terminal
          then let _ = Js.Array2.push propagation (state, item) in ();
        )
      );

      { propagation; spontaneous_generation = generated }

  (* CPTT Algorithm 4.63 step 1 *)
  let mutable_lalr1_items : mutable_lookahead_item_set M.t
    = lr0_items
      |. M.map (fun items -> items
        |. SI.toArray
        |. Belt.Array.map (fun item -> item, Belt.MutableSet.Int.make ())
        |. M.fromArray
      )

  (* CPTT Algorithm 4.63 steps 2 & 3 *)
  (* For each state:
      for each item:
        what items (in what state) does its lookahead propagate to?

     While we're finding propagation, also fill in spontaneous generation.
  *)
  let lookahead_propagation : (state * item) array M.t M.t
    = mutable_lalr1_items
      |. M.mapWithKey (fun state_num mutable_lookahead_item_set ->
        let kernel : item_set =
          mutable_lookahead_item_set_to_item_set mutable_lookahead_item_set
        in

        M.mapWithKey mutable_lookahead_item_set (fun item lookahead_set ->
          let { spontaneous_generation; propagation } =
            generate_lookaheads kernel item
          in

          let spontaneous_generation_arr : (state * item * SI.t) array
            = spontaneous_generation
            |. Belt.Array.map (fun (state, { item; lookahead_set }) ->
              state, item, lookahead_set
            )
          in

          Belt.Array.forEach spontaneous_generation_arr
            (fun (state, item, lookahead_set) ->
            mutable_lalr1_items
              |. M.getExn state
              |. M.getExn item
              |. Belt.MutableSet.Int.mergeMany (SI.toArray lookahead_set);
          );

          propagation
        )
      )

  (* Special-case augmented item `S' -> . S` with lookahead $. *)
  let () = mutable_lalr1_items
    |. M.getExn 0
    |. M.getExn (mk_item' 0 0)
    |. Belt.MutableSet.Int.mergeMany [| 0 |]

  (* Fill in LALR(1) item lookaheads. CPTT Algorithm 4.63 step 4 *)
  let () =
    (* Set lookaheads that were generated spontaneously *)

    (* Propagate lookaheads *)
    let made_update = ref true in
    while !made_update do
      made_update := false;

      M.forEach mutable_lalr1_items (fun state_num mutable_lookahead_item_set ->
        M.forEach mutable_lookahead_item_set (fun item current_lookahead ->

          (* lookaheads that propagate from the item we're currently looking at
           *)
          let propagation : (state * item) array = lookahead_propagation
            |. M.get state_num
            |> get_option' (Printf.sprintf
              "step 4 lookahead_propagation: couldn't find state %n" state_num
            )
            |. M.get item
            |> get_option' (Printf.sprintf
              "step 4 lookahead_propagation: couldn't find item %s in state %n"
              (string_of_item item)
              state_num
            )
          in

          (* See if we can propagate any lookaheads (and do it) *)
          Belt.Array.forEach propagation (fun (state_num', item') ->
            let current_lookahead' : MSI.t = mutable_lalr1_items
              |. M.get state_num'
              |> get_option' (Printf.sprintf
                "step 4 mutable_lalr1_items: couldn't find state %n" state_num'
              )
              |. M.get item'
              |> get_option' (Printf.sprintf
                "step 4 mutable_lalr1_items: couldn't find item %s in state %n"
                (string_of_item item')
                state_num'
              )
            in

            if not (MSI.subset current_lookahead current_lookahead')
            then (
              made_update := true;
              MSI.mergeMany current_lookahead' (MSI.toArray current_lookahead);
            )
          );
        );
      );
    done

  let lalr1_items : lookahead_item_set M.t
    = mutable_lalr1_items
    |. M.map (fun mutable_lookahead_item_set -> mutable_lookahead_item_set
      |. M.toArray
      |. Belt.Array.map (fun (item, mutable_lookahead) ->
        { item; lookahead_set = mutable_lookahead
          |. Belt.MutableSet.Int.toArray
          |. Belt.Set.Int.fromArray
        }
      )
      |. lookahead_item_set_from_array
    )

  let lalr1_goto_table : lalr1_goto_table = fun state nt ->
    try
      Some (item_set_to_state @@
        lr0_goto_kernel (state_to_item_set state) nt)
    with
      (* TODO: this shouldn't catch all invariant violations *)
      Util.InvariantViolation _ -> None

  let state_to_lookahead_item_set
    : state -> lookahead_item_set
    = fun state -> lalr1_items
      |. Belt.Map.Int.getExn state
      |> lr1_closure

  let lalr1_action_table : lalr1_action_table
    = fun state terminal_num ->

    let item_set_l : lookahead_item list
      = state
      |. state_to_lookahead_item_set
      |. Belt.Set.toArray
      |. Belt.List.fromArray
    in

    (* If [A -> xs . a ys, b] is in I_i and GOTO(I_i, a) = I_j, set
     * ACTION[i, a] to `shift j` *)
    let shift_action = Util.find_by item_set_l @@ fun l_item ->
      let { item; lookahead_set } = l_item in
      let { production_num; position } = view_item item in
      let symbols = production_map
        |. MMI.get production_num
        |> get_option' (Printf.sprintf
        "Lalr1 shift_action: unable to find production %n in production_map"
        production_num
        )
      in

      match L.get symbols position with
        | Some (Terminal t_num as next_symbol) ->
          if t_num = terminal_num
          then (lalr1_goto_table state next_symbol
            |. Belt.Option.map (fun x -> Shift x)
          )
          else None
        | _ -> None
    in

    (* If [A -> xs ., a] is in I_i, set ACTION[i, a] to `Reduce A -> xs` *)
    let reduce_action = Util.find_by item_set_l @@ fun l_item ->
      let { item; lookahead_set } = l_item in
      let { production_num; position } = view_item item in
      let nt_num = production_nonterminal_map
        |. MMI.get production_num
        |> get_option' (Printf.sprintf
        "Lalr1 shift_action: unable to find production %n in production_nonterminal_map"
        production_num
        )
      in
      let production = production_map
        |. MMI.get production_num
        |> get_option' (Printf.sprintf
        "Lalr1 shift_action: unable to find production %n in production_map"
        production_num
        )
      in
      if position = L.length production &&
         SI.has lookahead_set terminal_num &&
         (* Accept in this case (end marker on the augmented nonterminal) --
            don't reduce. *)
         nt_num != 0
        then Some (Reduce production_num)
        else None
    in

    (* If [S' -> S .] is in I_i, set ACTION[i, $] to `accept` *)
    let accept_action = Util.find_by item_set_l @@ fun l_item ->
      let { item; lookahead_set } = l_item in
      if item = mk_item' 0 1 && terminal_num = end_marker
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
    = fun () -> states |. Belt.Array.map (fun state ->
      terminals |. Belt.Array.map (lalr1_action_table state)
    )

  let full_lalr1_goto_table : unit -> (symbol * state option) array array
    = fun () -> states
      |. Belt.Array.map (fun state ->
      nonterminals
        |. Belt.Array.map (fun nt ->
          let sym = Nonterminal nt in
          sym, lalr1_goto_table state sym
        )
    )

  (* This is the main parsing function: CPTT Algorithm 4.44 / Figure 4.36. *)
  let parse_trace
    : do_trace (* trace or not *)
    -> Lex.token MQueue.t
    -> (parse_result, parse_error) Result.t * trace_line array

    = parse_trace_tables lalr1_action_table lalr1_goto_table

  let parse : Lex.token MQueue.t -> (parse_result, parse_error) Result.t
    = fun toks ->
      let result, trace = parse_trace DoTrace toks in
      Belt.Array.forEach trace string_of_trace_line;
      result

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