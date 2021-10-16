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

  let str ~loc str = Ast_builder.Default.estring ~loc str
  let int ~loc i = Ast_builder.Default.eint ~loc i
  let int32 ~loc i = Ast_builder.Default.eint32 ~loc i
  let float ~loc f = Ast_builder.Default.efloat ~loc (Float.to_string f)
  let char ~loc c = Ast_builder.Default.echar ~loc c
  let bigint ~loc i = [%expr Z.of_string [%e str ~loc (Z.to_string i)]]

  let opt_range ~loc = function
    | None -> [%expr None]
    | Some Range.{ start; finish } ->
      let start = int ~loc start in
      let finish = int ~loc finish in
      [%expr Some Lvca_provenance.Range.{ start = [%e start]; finish = [%e finish] }]
  ;;

  let source_code_position ~loc { pos_fname; pos_lnum; pos_bol; pos_cnum } =
    let pos_fname = str ~loc pos_fname in
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
    | Buffer_name x ->
      [%expr Lvca_syntax.Provenance.Parse_input.Buffer_name [%e str ~loc x]]
    | String x -> [%expr Lvca_syntax.Provenance.Parse_input.String [%e str ~loc x]]
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
        [%expr [%e pos], Lvca_syntax.Primitive_impl.All_plain.String [%e str ~loc s]]
      | Float f ->
        [%expr [%e pos], Lvca_syntax.Primitive_impl.All_plain.Float [%e float ~loc f]]
      | Char c ->
        [%expr [%e pos], Lvca_syntax.Primitive_impl.All_plain.Char [%e char ~loc c]]
    ;;

    let integer ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e bigint ~loc x]]
    let int32 ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e int32 ~loc x]]
    let float ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e float ~loc x]]
    let char ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e char ~loc x]]
    let string ~loc (pos, x) = [%expr [%e provenance ~loc pos], [%e str ~loc x]]
  end

  let rec pattern ~loc = function
    | Pattern.Operator (pos, name, pats) ->
      let name_exp = str ~loc name in
      let pats = pats |> List.map ~f:(pattern ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Pattern.Operator ([%e provenance ~loc pos], [%e name_exp], [%e pats])]
    | Var (pos, s) ->
      [%expr Lvca_syntax.Pattern.Var ([%e provenance ~loc pos], [%e str ~loc s])]
    | Primitive p -> [%expr Lvca_syntax.Pattern.Primitive [%e Primitive.all ~loc p]]
  ;;

  let rec nominal ~loc = function
    | Nominal.Term.Operator (pos, name, scopes) ->
      let name_exp = str ~loc name in
      let scopes = scopes |> List.map ~f:(scope ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Nominal.Term.Operator
          ([%e provenance ~loc pos], [%e name_exp], [%e scopes])]
    | Var (pos, s) ->
      [%expr Lvca_syntax.Nominal.Term.Var ([%e provenance ~loc pos], [%e str ~loc s])]
    | Primitive p -> [%expr Lvca_syntax.Nominal.Term.Primitive [%e Primitive.all ~loc p]]

  and scope ~loc (Nominal.Scope.Scope (pats, tm)) =
    let tm = nominal ~loc tm in
    let pats = pats |> List.map ~f:(pattern ~loc) |> list ~loc in
    [%expr Lvca_syntax.Nominal.Scope.Scope ([%e pats], [%e tm])]
  ;;

  let rec nonbinding ~loc = function
    | Nonbinding.Operator (pos, name, tms) ->
      let name_exp = str ~loc name in
      let tms = tms |> List.map ~f:(nonbinding ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Nonbinding.Operator ([%e provenance ~loc pos], [%e name_exp], [%e tms])]
    | Primitive p -> [%expr Lvca_syntax.Nonbinding.Primitive [%e Primitive.all ~loc p]]
  ;;

  let rec sort ~loc = function
    | Sort.Ap (pos, name, sorts) ->
      [%expr
        Lvca_syntax.Sort.Ap
          ([%e provenance ~loc pos], [%e str ~loc name], [%e ap_list ~loc sorts])]
    | Sort.Name (pos, name) ->
      [%expr Lvca_syntax.Sort.Name ([%e provenance ~loc pos], [%e str ~loc name])]

  and ap_list ~loc = function
    | Sort.Nil info -> [%expr Lvca_syntax.Sort.Nil [%e provenance ~loc info]]
    | Cons (info, x, xs) ->
      [%expr
        Lvca_syntax.Sort.Cons
          ([%e provenance ~loc info], [%e sort ~loc x], [%e ap_list ~loc xs])]
  ;;

  let sort_slot ~loc = function
    | Sort_slot.Sort_binding s ->
      [%expr Lvca_syntax.Abstract_syntax.Sort_slot.Sort_binding [%e sort ~loc s]]
    | Sort_pattern { pattern_sort; var_sort } ->
      [%expr
        Lvca_syntax.Abstract_syntax.Sort_slot.Sort_pattern
          { pattern_sort = [%e sort ~loc pattern_sort]
          ; var_sort = [%e sort ~loc var_sort]
          }]
  ;;

  let valence ~loc (Valence.Valence (sort_slots, body_sort)) =
    let sort_slots = sort_slots |> List.map ~f:(sort_slot ~loc) |> list ~loc in
    let body_sort = sort ~loc body_sort in
    [%expr Lvca_syntax.Abstract_syntax.Valence.Valence ([%e sort_slots], [%e body_sort])]
  ;;

  let arity ~loc (Arity.Arity (info, valences)) =
    let valences = valences |> List.map ~f:(valence ~loc) |> list ~loc in
    [%expr
      Lvca_syntax.Abstract_syntax.Arity.Arity ([%e provenance ~loc info], [%e valences])]
  ;;

  let operator_def ~loc (Operator_def.Operator_def (info, name, arity')) =
    [%expr
      Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
        ([%e provenance ~loc info], [%e str ~loc name], [%e arity ~loc arity'])]
  ;;

  let kind ~loc (Kind.Kind (pos, n)) =
    [%expr
      Lvca_syntax.Abstract_syntax.Kind.Kind ([%e provenance ~loc pos], [%e int ~loc n])]
  ;;

  let sort_def ~loc (Sort_def.Sort_def (vars, op_defs)) =
    let f (name, kind_opt) = [%expr [%e str ~loc name], [%e option ~loc kind kind_opt]] in
    let vars = vars |> List.map ~f |> list ~loc in
    let op_defs = op_defs |> List.map ~f:(operator_def ~loc) |> list ~loc in
    [%expr Lvca_syntax.Abstract_syntax.Sort_def.Sort_def ([%e vars], [%e op_defs])]
  ;;

  let language ~loc { externals; sort_defs } =
    let externals =
      externals
      |> List.map ~f:(fun (name, kind') ->
             [%expr [%e str ~loc name], [%e kind ~loc kind']])
      |> list ~loc
    in
    let sort_defs =
      sort_defs
      |> List.map ~f:(fun (name, sort_def') ->
             [%expr [%e str ~loc name], [%e sort_def ~loc sort_def']])
      |> list ~loc
    in
    [%expr
      Lvca_syntax.Abstract_syntax.
        { externals = [%e externals]; sort_defs = [%e sort_defs] }]
  ;;

  let single_var ~loc Single_var.{ name; info } =
    [%expr
      Lvca_syntax.Single_var.
        { name = [%e str ~loc name]; info = [%e provenance ~loc info] }]
  ;;
end
