open Base
open Lvca_syntax

module Lang =
[%lvca.abstract_syntax_module
{|
string : *  // module Primitive.String
list : * -> *  // module Lvca_core.List_model.List

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
|}]

module Pp = struct
  open Lang

  let rec value ppf = function
    | Value.True _ -> Fmt.pf ppf "true"
    | False _ -> Fmt.pf ppf "false"
    | Fun (_, (Single_var.{ name; _ }, c)) -> Fmt.pf ppf "fun %s -> %a" name computation c
    | Handler_val (_, h) -> handler ppf h
    | Value_var (_, name) -> Fmt.string ppf name

  and handler_clause ppf = function
    | Handler_clause.Return_clause (_, (Single_var.{ name; _ }, c)) ->
      Fmt.pf ppf "return %s -> %a" name computation c
    | Op_clause
        (_, (_, op_name), (Single_var.{ name = x; _ }, Single_var.{ name = k; _ }, c)) ->
      Fmt.pf ppf "#%s(%s; %s) -> %a" op_name x k computation c

  and handler ppf (Handler.Handler (_, clauses)) =
    let clauses = Lvca_core.List_model.to_list clauses in
    Fmt.pf ppf "handler { %a }" Fmt.(list handler_clause ~sep:comma) clauses

  and computation ppf = function
    | Computation.Return (_, v) -> Fmt.pf ppf "return %a" value v
    | Op (_, (_, op_name), v, (Single_var.{ name = x; _ }, c)) ->
      Fmt.pf ppf "#%s(%a; %s. %a)" op_name value v x computation c
    | Do (_, c1, (Single_var.{ name; _ }, c2)) ->
      Fmt.pf ppf "do %s <- %a in %a" name computation c1 computation c2
    | If (_, v, c1, c2) ->
      Fmt.pf ppf "if %a then %a else %a" value v computation c1 computation c2
    | App (_, v1, v2) -> Fmt.pf ppf "%a %a" value v1 value v2
    | With_handle (_, v, c) -> Fmt.pf ppf "with %a handle %a" value v computation c
  ;;

  let rec v_type ppf = function
    | V_type.Bool _ -> Fmt.pf ppf "bool"
    | Fun_ty (_, v, c) -> Fmt.pf ppf "%a -> %a" v_type v c_type c
    | Handler_ty (_, c, d) -> Fmt.pf ppf "%a => %a" c_type c c_type d

  and c_type ppf (C_type.Computation (_, v, ops)) =
    let ops = ops |> Lvca_core.List_model.to_list |> List.map ~f:snd in
    Fmt.pf ppf "%a!{%a}" v_type v Fmt.(list string) ops
  ;;
end

module Parse = struct
  open Lvca_parsing
  open Lang.Value
  open Lang.Computation
  open Lang.Handler_clause
  open Lang.Handler

  let ident = make1 Single_var.mk Ws.identifier

  let mk_handler_clause computation =
    choice
      [ make4
          (fun ~info _ name _ c -> mk_Return_clause ~info (name, c))
          (Ws.string "return")
          ident
          (Ws.string "->")
          computation
      ; make4
          (fun ~info op_name (x, k) _ c -> mk_Op_clause ~info op_name (x, k, c))
          (No_ws.char '#' *> attach_pos' Ws.identifier)
          (Ws.parens (lift3 (fun x _semi k -> x, k) ident (Ws.char ';') ident))
          (Ws.string "->")
          computation
      ]
    <?> "Handler_clause"
  ;;

  let mk_handler handler_clause =
    make2
      (fun ~info _ clauses ->
        let clauses = Lvca_core.List_model.of_list ~empty_info:None clauses in
        mk_Handler ~info clauses)
      (Ws.string "handler")
      (Ws.braces (sep_by (Ws.char ',') handler_clause))
  ;;

  let mk_value handler computation =
    choice
      [ make0 mk_True (Ws.string "true") <?> "True"
      ; make0 mk_False (Ws.string "false") <?> "False"
      ; make4
          (fun ~info _ ident _ c -> mk_Fun ~info (ident, c))
          (Ws.string "fun")
          ident
          (Ws.string "->")
          computation
        <?> "Fun"
      ; make1 mk_Handler_val handler
      ; make1 mk_Value_var Ws.identifier
      ]
    <?> "value"
  ;;

  let mk_computation value computation =
    choice
      [ make2 (fun ~info _ v -> mk_Return ~info v) (Ws.string "return") value <?> "Return"
      ; make2
          (fun ~info op_name (v, body) -> mk_Op ~info op_name v body)
          (No_ws.char '#' *> attach_pos' Ws.identifier)
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
    <?> "computation"
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

    let%expect_test _ =
      parse_print_value "true";
      [%expect {| true |}]
    ;;

    let%expect_test _ =
      parse_print_value "false";
      [%expect {| false |}]
    ;;

    let%expect_test _ =
      parse_print_value "fun x -> return true";
      [%expect {| fun x -> return true |}]
    ;;

    let%expect_test _ =
      parse_print_value "fun x -> return x";
      [%expect {| fun x -> return x |}]
    ;;

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

    let%expect_test _ =
      parse_print_handler_clause "return x -> return x";
      [%expect {| return x -> return x |}]
    ;;

    let%expect_test _ =
      parse_print_handler_clause "#foo(x; k) -> return x";
      [%expect {| #foo(x; k) -> return x |}]
    ;;

    let%expect_test _ =
      parse_print_handler "handler { #foo(x; k) -> return x }";
      [%expect {| handler { #foo(x; k) -> return x } |}]
    ;;

    let%expect_test _ =
      parse_print_handler "handler { #foo(x; k) -> return x, return x -> return x }";
      [%expect
        {|
        handler { #foo(x; k) -> return x,
        return x -> return x } |}]
    ;;

    let () = Format.(pp_set_margin std_formatter margin)
  end)
;;
