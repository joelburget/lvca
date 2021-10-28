open Base
open Lvca_syntax
open Lvca_provenance

module Base_language = struct
  include
    [%lvca.abstract_syntax_module
    {|
string : *
list : * -> *

value :=
  | True()
  | False()
  | Fun(value. computation)
  | Handler_val(handler)

handler_clause :=
  | Return_clause(value. computation)
  | Op_clause(string; value. value. computation)

handler := Handler(list handler_clause)

computation :=
  | Return(value)
  | Op(string; value; value. computation)
  | Do(computation; value. computation)
  | If(value; computation; computation)
  | App(value; value)
  | With_handle(value; computation)

v_type :=
  | Bool()
  | Fun_ty(v_type; c_type)
  | Handler_ty(c_type; c_type)

c_type := Computation(v_type; list string)
|}
    , { string = "Primitive.String"; list = "Lvca_core.List_model.List" }]

  let subst ~body ~var:Single_var.{ name; _ } ~value =
    let value = Value.to_nominal value in
    let result =
      body
      |> Computation.to_nominal
      |> Nominal.Term.subst_all (Lvca_util.String.Map.singleton name value)
      |> Computation.of_nominal
    in
    match result with
    | Ok v -> v
    | Error err ->
      Lvca_util.invariant_violation
        ~here:[%here]
        (Fmt.to_to_string Nominal.Conversion_error.pp err)
  ;;

  let return_clause (Handler.Handler (_, clauses)) =
    let clauses = Lvca_core.List_model.to_list clauses in
    List.find_map_exn clauses ~f:(function
        | Handler_clause.Return_clause (_, contents) -> Some contents
        | _ -> None)
  ;;

  let find_handler (Handler.Handler (_, clauses)) op_name =
    let clauses = Lvca_core.List_model.to_list clauses in
    List.find clauses ~f:(function
        | Handler_clause.Op_clause (_, (_, op_name'), _) -> String.(op_name' = op_name)
        | _ -> false)
  ;;

  type step_action =
    | Evaluate_first of Computation.t
    | Stepped of Computation.t

  let step =
    let open Value in
    let open Handler_clause in
    let open Computation in
    let here = Provenance.of_here [%here] in
    function
    (* Q: Shouldn't If hold a computation? How can you avoid hardcoding true / false?
     * Similarly for With_handle / Handler_val. I guess by only allowing
     * variables? But this is not how examples in the paper are written. *)
    | If (_, True _, c1, _c2) -> Stepped c1
    | If (_, False _, _c1, c2) -> Stepped c2
    | If _ -> failwith "TODO"
    | Do (_, Return (_, value), (var, body)) -> Stepped (subst ~body ~var ~value)
    | Do (_, Op (_, op_name, v, (y, c1)), (x, c2)) ->
      Stepped (Op (here, op_name, v, (y, Do (here, c1, (x, c2)))))
    | Do (_, c1, _) -> Evaluate_first c1
    | App (_, Fun (_, (var, body)), value) -> Stepped (subst ~body ~var ~value)
    | App _ -> failwith "TODO"
    | With_handle (_, Handler_val (_, handler), Return (_, value)) ->
      let var, body = return_clause handler in
      Stepped (subst ~body ~value ~var)
    | With_handle (_, Handler_val (_, h), Op (_, op_name, v, (y, c))) ->
      let handler' = With_handle (here, Handler_val (here, h), c) in
      (match find_handler h (snd op_name) with
      | Some (Op_clause (_, _name, (x, k, c))) ->
        let body = subst ~body:c ~var:x ~value:v in
        let value = Fun (here, (y, handler')) in
        Stepped (subst ~body ~var:k ~value)
      | Some _ -> failwith "invariant violation"
      | None -> Stepped (Op (Provenance.of_here [%here], op_name, v, (y, handler'))))
    | With_handle (_, Handler_val _, c) -> Evaluate_first c
    | With_handle _ | Return _ | Op _ -> failwith "TODO"
  ;;
end

module Extended_language = struct
  (* As in the paper:
   * - extend with ~integers,~ primitive arithmetic functions, ~strings,~ recursive
   *   functions, ~unit, and pairs~.
   * - allow patterns in binding constructs (functions, handler clauses,
   *   operation calls, sequencing)
   *)
  include
    [%lvca.abstract_syntax_module
    {|
list : * -> *
nonbinding : *
string : *

value :=
  | Adt(nonbinding)
  | Fun(value. computation)
  | Handler_val(handler)

handler_clause :=
  | Return_clause(value. computation)
  | Op_clause(string; value. value. computation)

handler := Handler(list handler_clause)

computation :=
  | Return(value)
  | Op(string; value; value. computation)
  | Do(computation; value. computation)
  | If(value; computation; computation)
  | App(value; value)
  | With_handle(value; computation)

v_type :=
  | Bool()
  | Fun_ty(v_type; c_type)
  | Handler_ty(c_type; c_type)

c_type := Computation(v_type; list string)
|}
    , { string = "Primitive.String"
      ; list = "Lvca_core.List_model.List"
      ; nonbinding = "Lvca_syntax.Nonbinding"
      }]

  (* Lower from Extended_language to Base_language *)
  module Lower = struct
    let info i = Provenance.calculated_here [%here] [ i ]

    exception Lower_failure of Value.t

    let rec value v =
      match v with
      | Value.Adt (i, Operator (_, "True", [])) -> Base_language.Value.True (info i)
      | Adt (i, Operator (_, "False", [])) -> False (info i)
      | Adt _ -> raise (Lower_failure v)
      | Fun (i, (x, c)) -> Fun (info i, (x, computation c))
      | Handler_val (i, h) -> Handler_val (info i, handler h)
      | Value_var (i, x) -> Value_var (info i, x)

    and handler_clause = function
      | Handler_clause.Return_clause (i, (x, c)) ->
        Base_language.Handler_clause.Return_clause (info i, (x, computation c))
      | Op_clause (i, name, (x, y, c)) -> Op_clause (info i, name, (x, y, computation c))

    and handler (Handler.Handler (i, clauses)) =
      let clauses = Lvca_core.List_model.map ~f:handler_clause clauses in
      Base_language.Handler.Handler (info i, clauses)

    and computation = function
      | Computation.Return (i, v) -> Base_language.Computation.Return (info i, value v)
      | Op (i, name, v, (x, c)) -> Op (info i, name, value v, (x, computation c))
      | Do (i, c1, (x, c2)) -> Do (info i, computation c1, (x, computation c2))
      | If (i, v, c1, c2) -> If (info i, value v, computation c1, computation c2)
      | App (i, v1, v2) -> App (info i, value v1, value v2)
      | With_handle (i, v, c) -> With_handle (info i, value v, computation c)
    ;;
  end

  (* Lift from Base_language to Extended_language *)
  module Lift = struct
    let info i = Provenance.calculated_here [%here] [ i ]

    let rec value = function
      | Base_language.Value.True i -> Value.Adt (info i, Operator (info i, "True", []))
      | False i -> Adt (info i, Operator (info i, "False", []))
      | Fun (i, (x, c)) -> Fun (info i, (x, computation c))
      | Handler_val (i, h) -> Handler_val (info i, handler h)
      | Value_var (i, x) -> Value_var (info i, x)

    and handler_clause = function
      | Base_language.Handler_clause.Return_clause (i, (x, c)) ->
        Handler_clause.Return_clause (info i, (x, computation c))
      | Op_clause (i, name, (x, y, c)) -> Op_clause (info i, name, (x, y, computation c))

    and handler (Base_language.Handler.Handler (i, clauses)) =
      let clauses = Lvca_core.List_model.map ~f:handler_clause clauses in
      Handler.Handler (info i, clauses)

    and computation = function
      | Base_language.Computation.Return (i, v) -> Computation.Return (info i, value v)
      | Op (i, name, v, (x, c)) -> Op (info i, name, value v, (x, computation c))
      | Do (i, c1, (x, c2)) -> Do (info i, computation c1, (x, computation c2))
      | If (i, v, c1, c2) -> If (info i, value v, computation c1, computation c2)
      | App (i, v1, v2) -> App (info i, value v1, value v2)
      | With_handle (i, v, c) -> With_handle (info i, value v, computation c)
    ;;
  end

  module Type_prec = struct
    type t =
      | Atomic (** Only atomic types shown without parens *)
      | Arrow (** Arrows shown without parens *)

    let ( <= ) x y = match x, y with Arrow, Atomic -> false | _ -> true
  end

  module Pp = struct
    let op_name ppf name = Fmt.pf ppf "#%s" name

    let rec value ppf = function
      | Value.Adt (_, nonbinding) -> Nonbinding.pp ppf nonbinding
      | Fun (_, (Single_var.{ name; _ }, c)) ->
        Fmt.pf ppf "fun %s -> %a" name computation c
      | Handler_val (_, h) -> handler ppf h
      | Value_var (_, name) -> Fmt.string ppf name

    and handler_clause ppf = function
      | Handler_clause.Return_clause (_, (Single_var.{ name; _ }, c)) ->
        Fmt.pf ppf "return %s -> %a" name computation c
      | Op_clause
          (_, (_, op_name'), (Single_var.{ name = x; _ }, Single_var.{ name = k; _ }, c))
        ->
        Fmt.pf ppf "%a(%s; %s) -> %a" op_name op_name' x k computation c

    and handler ppf (Handler.Handler (_, clauses)) =
      let clauses = Lvca_core.List_model.to_list clauses in
      Fmt.pf
        ppf
        "handler @[<hv>{ %a }@]"
        Fmt.(list handler_clause ~sep:(any " | "))
        clauses

    and computation ppf = function
      | Computation.Return (_, v) -> Fmt.pf ppf "return %a" value v
      | Op (_, (_, op_name'), v, (Single_var.{ name = x; _ }, c)) ->
        Fmt.pf ppf "%a(%a; %s. %a)" op_name op_name' value v x computation c
      | Do (_, c1, (Single_var.{ name; _ }, c2)) ->
        Fmt.pf ppf "do %s <- %a in %a" name computation c1 computation c2
      | If (_, v, c1, c2) ->
        Fmt.pf ppf "if %a then %a else %a" value v computation c1 computation c2
      | App (_, v1, v2) -> Fmt.pf ppf "%a %a" value v1 value v2
      | With_handle (_, v, c) -> Fmt.pf ppf "with %a handle %a" value v computation c
    ;;

    let rec v_type ~prec ppf = function
      | V_type.Bool _ -> Fmt.pf ppf "bool"
      | Fun_ty (_, v, c) ->
        Fmt.pf
          ppf
          (if Type_prec.(prec <= Atomic) then "(%a -> %a)" else "%a -> %a")
          (v_type ~prec:Type_prec.Atomic)
          v
          c_type
          c
      | Handler_ty (_, c, d) ->
        Fmt.pf
          ppf
          (if Type_prec.(prec <= Atomic) then "(%a => %a)" else "%a => %a")
          c_type
          c
          c_type
          d

    and c_type ppf (C_type.Computation (_, v, ops)) =
      let ops = ops |> Lvca_core.List_model.to_list |> List.map ~f:snd in
      Fmt.pf
        ppf
        "%a!@[<hv>{%a}@]"
        (v_type ~prec:Atomic)
        v
        Fmt.(list op_name ~sep:comma)
        ops
    ;;
  end

  module Parse = struct
    open Lvca_parsing
    module Ws = C_comment_parser

    let make0, make1, make2, make4, make6 = Provenance.(make0, make1, make2, make4, make6)
    let ident = make1 Single_var.mk Ws.identifier

    let op_name =
      No_junk.char '#' *> attach_pos' Ws.identifier
      >>| fun (range, str) -> Provenance.of_range range, str
    ;;

    let of_list xs = Lvca_core.List_model.of_list xs

    let mk_handler_clause computation =
      let open Handler_clause in
      choice
        [ make4
            (fun ~info _ name _ c -> mk_Return_clause ~info (name, c))
            (Ws.string "return")
            ident
            (Ws.string "->")
            computation
        ; make4
            (fun ~info op_name (x, k) _ c -> mk_Op_clause ~info op_name (x, k, c))
            op_name
            (Ws.parens (lift3 (fun x _semi k -> x, k) ident (Ws.char ';') ident))
            (Ws.string "->")
            computation
        ]
      <?> "Handler_clause"
    ;;

    let mk_handler handler_clause =
      let open Handler in
      let go =
        make1
          mk_Handler
          (Ws.string "handler" *> Ws.braces (sep_by (Ws.char '|') handler_clause)
          >>| of_list)
      in
      Ws.parens go <|> go <?> "handler"
    ;;

    let mk_value handler computation =
      let open Value in
      let go =
        choice
          [ make4
              (fun ~info _ ident _ c -> mk_Fun ~info (ident, c))
              (Ws.string "fun")
              ident
              (Ws.string "->")
              computation
            <?> "Fun"
          ; make1 mk_Handler_val handler
          ; make1 mk_Adt Nonbinding.parse
          ; make1 mk_Value_var Ws.identifier
          ]
      in
      Ws.parens go <|> go <?> "value"
    ;;

    let mk_computation value computation =
      let open Computation in
      let go =
        choice
          [ make2 (fun ~info _ v -> mk_Return ~info v) (Ws.string "return") value
            <?> "Return"
          ; make2
              (fun ~info op_name (v, body) -> mk_Op ~info op_name v body)
              op_name
              (Ws.parens
                 (lift3
                    (fun v y body -> v, (y, body))
                    (value <* Ws.char ';')
                    (ident <* Ws.char '.')
                    computation))
          ; make6
              (fun ~info _ ident _ c1 _ c2 -> mk_Do ~info c1 (ident, c2))
              (Ws.string "do")
              ident
              (Ws.string "<-")
              computation
              (Ws.string "in")
              computation
            <?> "Do"
          ; make6
              (fun ~info _ v _ c1 _ c2 -> mk_If ~info v c1 c2)
              (Ws.string "if")
              value
              (Ws.string "then")
              computation
              (Ws.string "else")
              computation
            <?> "If"
          ; make4
              (fun ~info _ v _ c -> mk_With_handle ~info v c)
              (Ws.string "with")
              value
              (Ws.string "handle")
              computation
            <?> "With_handle"
          ; make2 mk_App value value <?> "App"
          ]
      in
      Ws.parens go <|> go <?> "computation"
    ;;

    let computation =
      fix (fun computation ->
          let handler_clause = mk_handler_clause computation in
          let handler = mk_handler handler_clause in
          let value = mk_value handler computation in
          mk_computation value computation)
    ;;

    let handler_clause = mk_handler_clause computation
    let handler = mk_handler handler_clause
    let value = mk_value handler computation

    let mk_c_type atomic_v_type =
      let go =
        make2
          C_type.mk_Computation
          atomic_v_type
          (Ws.char '!' *> Ws.braces (sep_by (Ws.char ',') op_name) >>| of_list)
      in
      choice [ Ws.parens go; go ] <?> "computation type"
    ;;

    let mk_atomic_v_type v_type =
      choice [ Ws.parens v_type; make0 V_type.mk_Bool (Ws.string "bool") ]
      <?> "atomic value type"
    ;;

    let v_type =
      let open V_type in
      fix (fun v_type ->
          let atomic_v_type = mk_atomic_v_type v_type in
          let c_type = mk_c_type atomic_v_type in
          choice
            [ make2 mk_Handler_ty (c_type <* Ws.string "=>") c_type
            ; (atomic_v_type
              >>== fun { value; range } ->
              choice
                [ Lvca_parsing.make1
                    (fun ~info c_type ->
                      let info = Opt_range.union range info |> Provenance.of_range in
                      mk_Fun_ty ~info value c_type)
                    (Ws.string "->" *> c_type)
                ; return ~range value
                ])
            ])
      <?> "value type"
    ;;

    let atomic_v_type = mk_atomic_v_type v_type
    let c_type = mk_c_type atomic_v_type
  end

  let%test_module "Parsing / Printing" =
    (module struct
      module Format = Stdlib.Format

      let margin = Format.(pp_get_margin std_formatter ())
      let () = Format.(pp_set_margin std_formatter 80)

      let parse_print : 'a Lvca_parsing.t -> 'a Fmt.t -> ?width:int -> string -> unit =
       fun parse pp ?width parser_str ->
        match Lvca_parsing.(parse_string (whitespace *> parse) parser_str) with
        | Error msg -> Stdio.print_string ("failed to parse: " ^ msg)
        | Ok tm ->
          let pre_geom =
            Option.map width ~f:(fun n ->
                let geom = Format.get_geometry () in
                Format.set_margin n;
                geom)
          in
          Fmt.pr "%a\n" pp tm;
          (match pre_geom with
          | None -> ()
          | Some { max_indent; margin } -> Format.set_geometry ~max_indent ~margin)
     ;;

      let parse_print_computation = parse_print Parse.computation Pp.computation
      let parse_print_value = parse_print Parse.value Pp.value
      let parse_print_handler = parse_print Parse.handler Pp.handler
      let parse_print_handler_clause = parse_print Parse.handler_clause Pp.handler_clause
      let parse_print_v_type = parse_print Parse.v_type (Pp.v_type ~prec:Type_prec.Arrow)
      let parse_print_c_type = parse_print Parse.c_type Pp.c_type

      let%expect_test _ =
        parse_print_value "true()";
        [%expect {| true() |}]
      ;;

      let%expect_test _ =
        parse_print_value "false()";
        [%expect {| false() |}]
      ;;

      let%expect_test _ =
        parse_print_value "fun x -> return true";
        [%expect {| fun x -> return true |}]
      ;;

      let%expect_test _ =
        parse_print_value "fun x -> return x";
        [%expect {| fun x -> return x |}]
      ;;

      (* TODO
      let%expect_test _ =
        parse_print_value "fun s -> return (handler { #read(_; k) -> k s })";
        [%expect {| fun s -> return (handler { #read(_; k) -> k s }) |}]
      ;;
      *)

      let%expect_test _ =
        parse_print_computation "return x";
        [%expect {| return x |}]
      ;;

      let%expect_test _ =
        parse_print_computation "return true";
        [%expect {| return true |}]
      ;;

      let%expect_test _ =
        parse_print_computation "#foo(x; y. return y)";
        [%expect {| #foo(x; y. return y) |}]
      ;;

      let%expect_test _ =
        parse_print_computation "do x <- return true in return x";
        [%expect {| do x <- return true in return x |}]
      ;;

      let%expect_test _ =
        parse_print_computation "if true then return true else return false";
        [%expect {| if true then return true else return false |}]
      ;;

      let%expect_test _ =
        parse_print_computation "with true handle return x";
        [%expect {| with true handle return x |}]
      ;;

      let%expect_test _ =
        parse_print_computation "with x handle return true";
        [%expect {| with x handle return true |}]
      ;;

      (* TODO
      let%expect_test _ =
        parse_print_computation {|
        do x <- print (return 1) in  f x
        |};
        [%expect]
      ;;
      *)

      (* TODO
      let%expect_test _ =
        parse_print_computation
          {|
        do _ <- print "What is your forename?" in
        do forename <- read () in
        do _ <- print "What is your surname?" in
        do surname <- read () in
        print (join forename surname)
        |};
        [%expect]
      ;;
      *)

      (* TODO
      let%expect_test _ =
        parse_print_computation
          {|
        print "What is your forename?";
        do forename <- read () in
        print "What is your surname?";
        do surname <- read () in
        print (join forename surname)
        |};
        [%expect]
      ;;
      *)

      let%expect_test _ =
        parse_print_handler_clause "return x -> return x";
        [%expect {| return x -> return x |}]
      ;;

      let%expect_test _ =
        parse_print_handler_clause "#foo(x; k) -> return x";
        [%expect {| #foo(x; k) -> return x |}]
      ;;

      let%expect_test _ =
        parse_print_handler {|handler { #read(_; k) -> k "bob" }|};
        [%expect {| handler { #read(_; k) -> k "bob" } |}]
      ;;

      let%expect_test _ =
        parse_print_handler "handler { #foo(x; k) -> return x }";
        [%expect {| handler { #foo(x; k) -> return x } |}]
      ;;

      let%expect_test _ =
        parse_print_handler "handler { #foo(x; k) -> return x | return x -> return x }";
        [%expect {| handler { #foo(x; k) -> return x | return x -> return x } |}]
      ;;

      (* TODO
      let%expect_test _ =
        parse_print_handler
          {|
          handler {
            | return x -> return (x, "")
            | print(x; k) ->
              do (x, acc) <- k () in
              return (x, join s acc)
          }
        |};
        [%expect]
      ;;
      *)

      (* TODO
      let%expect_test _ =
        parse_print_handler
          {|
          handler {
            | get(_; k) -> fun s -> (k s) s
            | set(s; k) -> fun _ -> (k unit()) s
            | return x -> fun _ -> return x
          }
        |};
        [%expect]
      ;;
      *)

      let%expect_test _ =
        parse_print_v_type "bool";
        [%expect {| bool |}]
      ;;

      let%expect_test _ =
        parse_print_v_type "(bool)";
        [%expect {| bool |}]
      ;;

      let%expect_test _ =
        parse_print_c_type "bool!{}";
        [%expect {| bool!{} |}]
      ;;

      let%expect_test _ =
        parse_print_c_type "bool!{#bar, #baz}";
        [%expect {| bool!{#bar, #baz} |}]
      ;;

      let%expect_test _ =
        parse_print_v_type "bool!{} => bool!{}";
        [%expect {| bool!{} => bool!{} |}]
      ;;

      let%expect_test _ =
        parse_print_v_type "bool!{#bar, #baz} => bool!{}";
        [%expect {| bool!{#bar, #baz} => bool!{} |}]
      ;;

      let%expect_test _ =
        parse_print_c_type "(bool!{} => bool!{})!{#bar, #baz}";
        [%expect {| (bool!{} => bool!{})!{#bar, #baz} |}]
      ;;

      let%expect_test _ =
        parse_print_v_type "(bool!{} => bool!{#bar, #baz}) -> bool!{}";
        [%expect {| (bool!{} => bool!{#bar, #baz}) -> bool!{} |}]
      ;;

      (* We don't currently parse arrows associatively *)
      let%expect_test _ =
        parse_print_v_type "bool!{} => bool!{#bar, #baz} -> bool!{}";
        [%expect {| failed to parse: : end_of_input |}]
      ;;

      let%expect_test _ =
        parse_print_v_type "bool -> bool!{}";
        [%expect {| bool -> bool!{} |}]
      ;;

      let%expect_test _ =
        parse_print_v_type "(bool -> bool!{}) -> bool!{}";
        [%expect {| (bool -> bool!{}) -> bool!{} |}]
      ;;

      (* We don't currently parse arrows associatively *)
      let%expect_test _ =
        parse_print_v_type "bool -> bool!{} -> bool!{}";
        [%expect {| failed to parse: : end_of_input |}]
      ;;

      let () = Format.(pp_set_margin std_formatter margin)
    end)
  ;;
end
