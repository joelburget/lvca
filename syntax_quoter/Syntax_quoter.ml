open Base
open Lvca_provenance
open Lvca_syntax
open Abstract_syntax
open Ppxlib

let extract_string ~loc expr =
  (* payload and location of the string contents, inside "" or {||} *)
  let adjust shift loc =
    let adjust shift p = { p with Lexing.pos_cnum = p.pos_cnum + shift } in
    { loc with
      Location.loc_start = adjust shift loc.loc_start
    ; Location.loc_end = adjust (-shift) loc.loc_end
    }
  in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (str, _loc, None)) -> str, adjust 1 expr.pexp_loc
  | Pexp_constant (Pconst_string (str, _loc, Some x)) ->
    str, adjust (String.length x + 2) expr.pexp_loc
  | _ -> Location.raise_errorf ~loc "Expecting string payload"
;;

module Pat = struct
  let rec list ~loc = function
    | [] -> [%pat? []]
    | expr :: exprs -> [%pat? [%p expr] :: [%p list ~loc exprs]]
  ;;
end

module Exp = struct
  let rec list ~loc = function
    | [] -> [%expr []]
    | expr :: exprs -> [%expr [%e expr] :: [%e list ~loc exprs]]
  ;;

  let option ~loc maker = function
    | None -> [%expr None]
    | Some x -> [%expr Some [%e maker ~loc x]]
  ;;

  let string ~loc str = Ast_builder.Default.estring ~loc str
  let int ~loc i = Ast_builder.Default.eint ~loc i
  let int32 ~loc i = Ast_builder.Default.eint32 ~loc i
  let float ~loc f = Ast_builder.Default.efloat ~loc (Float.to_string f)
  let char ~loc c = Ast_builder.Default.echar ~loc c
  let bigint ~loc i = [%expr Z.of_string [%e string ~loc (Z.to_string i)]]

  let opt_range ~loc = function
    | None -> [%expr None]
    | Some Range.{ start; finish } ->
      let start = int ~loc start in
      let finish = int ~loc finish in
      [%expr Some Lvca_provenance.Range.{ start = [%e start]; finish = [%e finish] }]
  ;;

  let source_code_position ~loc { pos_fname; pos_lnum; pos_bol; pos_cnum } =
    let pos_fname = string ~loc pos_fname in
    let pos_lnum = int ~loc pos_lnum in
    let pos_bol = int ~loc pos_bol in
    let pos_cnum = int ~loc pos_cnum in
    [%expr
      { pos_fname = [%e pos_fname]
      ; pos_lnum = [%e pos_lnum]
      ; pos_bol = [%e pos_bol]
      ; pos_cnum = [%e pos_cnum]
      }]
  ;;

  let parse_input ~loc = function
    | Provenance.Parse_input.Input_unknown ->
      [%expr Lvca_syntax.Provenance.Parse_input.Input_unknown]
    | Buffer_name str ->
      [%expr Lvca_syntax.Provenance.Parse_input.Buffer_name [%e string ~loc str]]
    | String str -> [%expr Lvca_syntax.Provenance.Parse_input.String [%e string ~loc str]]
  ;;

  let parse_located ~loc Provenance.Parse_located.{ input; range } =
    [%expr
      Lvca_syntax.Provenance.Parse_located.
        { input = [%e parse_input ~loc input]; range = [%e opt_range ~loc range] }]
  ;;

  let located ~loc = function
    | Provenance.Located.Source_located p ->
      [%expr
        Lvca_syntax.Provenance.Located.Source_located [%e source_code_position ~loc p]]
    | Parse_located p ->
      [%expr Lvca_syntax.Provenance.Located.Parse_located [%e parse_located ~loc p]]
  ;;

  let rec provenance ~loc : Provenance.t -> expression = function
    | Located x -> [%expr Lvca_syntax.Provenance.Located [%e located ~loc x]]
    | Calculated (x, ts) ->
      let ts = List.map ts ~f:(provenance ~loc) in
      [%expr Lvca_syntax.Provenance.Calculated ([%e located ~loc x], [%e list ~loc ts])]
    | Indexed i -> [%expr Lvca_syntax.Provenance.Indexed [%e int ~loc i]]
  ;;

  module Primitive = struct
    let all ~loc (pos, prim) =
      let pos = provenance ~loc pos in
      match prim with
      | Primitive_impl.All_plain.Integer i ->
        [%expr [%e pos], Lvca_syntax.Primitive_impl.All_plain.Integer [%e bigint ~loc i]]
      | Primitive_impl.All_plain.Int32 i ->
        [%expr [%e pos], Lvca_syntax.Primitive_impl.All_plain.Int32 [%e int32 ~loc i]]
      | String s ->
        [%expr [%e pos], Lvca_syntax.Primitive_impl.All_plain.String [%e string ~loc s]]
      | Float f ->
        [%expr [%e pos], Lvca_syntax.Primitive_impl.All_plain.Float [%e float ~loc f]]
      | Char c ->
        [%expr [%e pos], Lvca_syntax.Primitive_impl.All_plain.Char [%e char ~loc c]]
    ;;

    let integer ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e bigint ~loc x]]
    let int32 ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e int32 ~loc x]]
    let float ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e float ~loc x]]
    let char ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e char ~loc x]]
    let string ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e string ~loc x]]
  end

  let rec pattern ~loc = function
    | Pattern.Operator (pos, name, pats) ->
      let name_exp = string ~loc name in
      let pats = pats |> List.map ~f:(pattern ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Pattern.Operator ([%e provenance ~loc pos], [%e name_exp], [%e pats])]
    | Var (pos, str) ->
      [%expr Lvca_syntax.Pattern.Var ([%e provenance ~loc pos], [%e string ~loc str])]
    | Primitive p -> [%expr Lvca_syntax.Pattern.Primitive [%e Primitive.all ~loc p]]
  ;;

  let rec nominal ~loc = function
    | Nominal.Term.Operator (pos, name, scopes) ->
      let name_exp = string ~loc name in
      let scopes = scopes |> List.map ~f:(scope ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Nominal.Term.Operator
          ([%e provenance ~loc pos], [%e name_exp], [%e scopes])]
    | Var (pos, str) ->
      [%expr
        Lvca_syntax.Nominal.Term.Var ([%e provenance ~loc pos], [%e string ~loc str])]
    | Primitive p -> [%expr Lvca_syntax.Nominal.Term.Primitive [%e Primitive.all ~loc p]]

  and scope ~loc (Nominal.Scope.Scope (pats, tm)) =
    let tm = nominal ~loc tm in
    let pats = pats |> List.map ~f:(pattern ~loc) |> list ~loc in
    [%expr Lvca_syntax.Nominal.Scope.Scope ([%e pats], [%e tm])]
  ;;

  let rec nonbinding ~loc = function
    | Nonbinding.Operator (pos, name, tms) ->
      let name_exp = string ~loc name in
      let tms = tms |> List.map ~f:(nonbinding ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Nonbinding.Operator ([%e provenance ~loc pos], [%e name_exp], [%e tms])]
    | Primitive p -> [%expr Lvca_syntax.Nonbinding.Primitive [%e Primitive.all ~loc p]]
  ;;

  let rec sort ~loc = function
    | Sort.Ap (pos, name, sorts) ->
      let sorts = sorts |> List.map ~f:(sort ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Sort.Ap ([%e provenance ~loc pos], [%e string ~loc name], [%e sorts])]
    | Sort.Name (pos, name) ->
      [%expr Lvca_syntax.Sort.Name ([%e provenance ~loc pos], [%e string ~loc name])]
  ;;

  let sort_slot ~loc = function
    | Sort_slot.Sort_binding s ->
      [%expr Lvca_syntax.Sort_slot.Sort_binding [%e sort ~loc s]]
    | Sort_pattern { pattern_sort; var_sort } ->
      [%expr
        Lvca_syntax.Sort_slot.Sort_pattern
          { pattern_sort = [%e sort ~loc pattern_sort]
          ; var_sort = [%e sort ~loc var_sort]
          }]
  ;;

  let valence ~loc (Valence.Valence (sort_slots, body_sort)) =
    let sort_slots = sort_slots |> List.map ~f:(sort_slot ~loc) |> list ~loc in
    let body_sort = sort ~loc body_sort in
    [%expr Lvca_syntax.Valence.Valence ([%e sort_slots], [%e body_sort])]
  ;;

  let arity ~loc (Arity.Arity (info, valences)) =
    let valences = valences |> List.map ~f:(valence ~loc) |> list ~loc in
    [%expr Lvca_syntax.Arity.Arity ([%e provenance ~loc info], [%e valences])]
  ;;

  let operator_def ~loc (Operator_def.Operator_def (info, name, arity')) =
    [%expr
      Lvca_syntax.Operator_def.Operator_def
        ([%e provenance ~loc info], [%e string ~loc name], [%e arity ~loc arity'])]
  ;;

  let kind ~loc (Kind.Kind (pos, n)) =
    [%expr Lvca_syntax.Kind.Kind ([%e provenance ~loc pos], [%e int ~loc n])]
  ;;

  let sort_def ~loc (Sort_def.Sort_def (vars, op_defs, var_names)) =
    let f (name, kind_opt) =
      [%expr [%e string ~loc name], [%e option ~loc kind kind_opt]]
    in
    let vars = vars |> List.map ~f |> list ~loc in
    let op_defs = op_defs |> List.map ~f:(operator_def ~loc) |> list ~loc in
    let var_names = var_names |> List.map ~f:(string ~loc) |> list ~loc in
    [%expr Lvca_syntax.Sort_def.Sort_def ([%e vars], [%e op_defs], [%e var_names])]
  ;;

  let language ~loc { externals; sort_defs } =
    let externals =
      externals
      |> List.map ~f:(fun (name, kind') ->
             [%expr [%e string ~loc name], [%e kind ~loc kind']])
      |> list ~loc
    in
    let sort_defs =
      sort_defs
      |> List.map ~f:(fun (name, sort_def') ->
             [%expr [%e string ~loc name], [%e sort_def ~loc sort_def']])
      |> list ~loc
    in
    [%expr
      Lvca_syntax.Abstract_syntax.
        { externals = [%e externals]; sort_defs = [%e sort_defs] }]
  ;;

  let single_var ~loc Single_var.{ name; info } =
    [%expr
      Lvca_syntax.Single_var.
        { name = [%e string ~loc name]; info = [%e provenance ~loc info] }]
  ;;

  module Regex = struct
    open Regex

    let class_base ~loc = function
      | Class_base.Word -> [%expr Lvca_syntax.Regex.Class_base.Word]
      | Whitespace -> [%expr Lvca_syntax.Regex.Class_base.Whitespace]
      | Digit -> [%expr Lvca_syntax.Regex.Class_base.Digit]
    ;;

    let class' ~loc = function
      | Class.Pos base -> [%expr Lvca_syntax.Regex.Class.Pos [%e class_base ~loc base]]
      | Neg base -> [%expr Lvca_syntax.Regex.Class.Neg [%e class_base ~loc base]]
    ;;

    let set_member ~loc = function
      | Set_member.Single_char c ->
        [%expr Lvca_syntax.Regex.Set_member.Single_char [%e char ~loc c]]
      | Range (c1, c2) ->
        [%expr Lvca_syntax.Regex.Set_member.Range ([%e char ~loc c1], [%e char ~loc c2])]
    ;;

    let set ~loc set_members = set_members |> List.map ~f:(set_member ~loc) |> list ~loc

    let rec t ~loc = function
      | Char c -> [%expr Lvca_syntax.Regex.Char [%e char ~loc c]]
      | Class cls -> [%expr Lvca_syntax.Regex.Class [%e class' ~loc cls]]
      | Set s -> [%expr Lvca_syntax.Regex.Set [%e set ~loc s]]
      | Star t' -> [%expr Lvca_syntax.Regex.Star [%e t ~loc t']]
      | Plus t' -> [%expr Lvca_syntax.Regex.Plus [%e t ~loc t']]
      | Count (t', n) -> [%expr Lvca_syntax.Regex.Count ([%e t ~loc t'], [%e int ~loc n])]
      | Option t' -> [%expr Lvca_syntax.Regex.Option [%e t ~loc t']]
      | Choice ts ->
        let ts = ts |> List.map ~f:(t ~loc) |> list ~loc in
        [%expr Lvca_syntax.Regex.Choice [%e ts]]
      | Any -> [%expr Lvca_syntax.Regex.Any]
      | Concat ts ->
        let ts = ts |> List.map ~f:(t ~loc) |> list ~loc in
        [%expr Lvca_syntax.Regex.Concat [%e ts]]
    ;;
  end

  module Parse_pretty = struct
    open Parse_pretty

    let fixity ~loc = function
      | Fixity.Left -> [%expr Lvca_syntax.Parse_pretty.Fixity.Left]
      | None -> [%expr Lvca_syntax.Parse_pretty.Fixity.None]
      | Right -> [%expr Lvca_syntax.Parse_pretty.Fixity.Right]
    ;;

    let operator_fixity ~loc (info, fixity', name) =
      [%expr [%e provenance ~loc info], [%e fixity ~loc fixity'], [%e string ~loc name]]
    ;;

    let operator_ranking ~loc (info, levels) =
      let levels =
        levels
        |> List.map ~f:Lvca_util.(List.map ~f:(operator_fixity ~loc) >> list ~loc)
        |> list ~loc
      in
      [%expr [%e provenance ~loc info], [%e levels]]
    ;;

    let sequence_item ~loc = function
      | Sequence_item.Var (info, str) ->
        [%expr
          Lvca_syntax.Parse_pretty.Sequence_item.Var
            ([%e provenance ~loc info], [%e string ~loc str])]
      | Literal (info, str) ->
        [%expr
          Lvca_syntax.Parse_pretty.Sequence_item.Literal
            ([%e provenance ~loc info], [%e string ~loc str])]
      | Space info ->
        [%expr Lvca_syntax.Parse_pretty.Sequence_item.Space [%e provenance ~loc info]]
    ;;

    let operator_concrete_syntax_row ~loc (info, sequence_items) =
      let sequence_items =
        sequence_items |> List.map ~f:(sequence_item ~loc) |> list ~loc
      in
      [%expr [%e provenance ~loc info], [%e sequence_items]]
    ;;

    let variable_syntax_row ~loc Variable_syntax_row.{ info; var_name; re } =
      [%expr
        Lvca_syntax.Parse_pretty.Variable_syntax_row.
          { info = [%e provenance ~loc info]
          ; var_name = [%e string ~loc var_name]
          ; re = [%e Regex.t ~loc re]
          }]
    ;;

    let operator_pattern_slot
        ~loc
        Operator_pattern_slot.{ info; variable_names; body_name }
      =
      [%expr
        Lvca_syntax.Parse_pretty.Operator_pattern_slot.
          { info = [%e provenance ~loc info]
          ; variable_names = [%e variable_names |> List.map ~f:(string ~loc) |> list ~loc]
          ; body_name = [%e string ~loc body_name]
          }]
    ;;

    let operator_pattern ~loc Operator_pattern.{ info; name; slots } =
      [%expr
        Lvca_syntax.Parse_pretty.Operator_pattern.
          { info = [%e provenance ~loc info]
          ; name = [%e string ~loc name]
          ; slots = [%e slots |> List.map ~f:(operator_pattern_slot ~loc) |> list ~loc]
          }]
    ;;

    let operator_syntax_row ~loc Operator_syntax_row.{ info; pattern; concrete_syntax } =
      [%expr
        Lvca_syntax.Parse_pretty.Operator_syntax_row.
          { info = [%e provenance ~loc info]
          ; pattern = [%e operator_pattern ~loc pattern]
          ; concrete_syntax = [%e operator_concrete_syntax_row ~loc concrete_syntax]
          }]
    ;;

    let sort_syntax
        ~loc
        Sort_syntax.
          { info
          ; name = name_info, name
          ; operators
          ; variables
          ; operator_ranking = ranking
          }
      =
      let operators = operators |> List.map ~f:(operator_syntax_row ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Parse_pretty.Sort_syntax.
          { info = [%e provenance ~loc info]
          ; name = [%e provenance ~loc name_info], [%e string ~loc name]
          ; operators = [%e operators]
          ; variables = [%e option ~loc variable_syntax_row variables]
          ; operator_ranking = [%e option ~loc operator_ranking ranking]
          }]
    ;;

    let t ~loc sort_syntaxes = List.map sort_syntaxes ~f:(sort_syntax ~loc) |> list ~loc
  end
end
