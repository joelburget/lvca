(** Types for representing languages *)
open Belt
open Util

(** Sorts divide ASTs into syntactic categories. *)
type sort =
  | SortAp   of sort * sort
  (** A higher-kinded sort can be applied *)
  | SortName of string

(** A valence represents the sort of an argument (to an operator), as well as
 * the number and sorts of the variables bound within it *)
type valence =
  | FixedValence    of sort list * sort
  (** A fixed valence is known a priori *)
  | VariableValence of string * sort
  (** A variable valence binds a number of variables not known a priori. All
   must be of the same sort. *)

(** An arity specifies the arguments to an operator *)
type arity =
  | Arity of string list * valence list
  (** An arity is defined its arity indices and valences *)

type operatorDef =
  | OperatorDef of string * arity
  (** An operator is defined by its tag and arity *)

type sortDef =
  | SortDef of string list * operatorDef list
  (** A sort is defined by a set of variables and a set of operators *)

type language =
  | Language of sortDef Belt.Map.String.t
  (** A language is defined by its sorts *)

type primitive =
  | PrimInteger of Bigint.t
  | PrimString  of string
  | PrimBool    of bool

let prim_eq p1 p2 = match (p1, p2) with
  | (PrimInteger i1, PrimInteger i2) -> Bigint.(i1 = i2)
  | (PrimString  s1, PrimString  s2) -> s1 = s2
  | (PrimBool    b1, PrimBool    b2) -> b1 = b2
  | _                                -> false

module Sjcl = struct
  type sjcl

  external sjcl : sjcl = "sjcl" [@@bs.module]
end

module ArrayBuffer = struct
  type t

  let to_hex (buf : t) : string = [%raw {|
    function to_hex(buffer) {
      return Array.prototype.map.call(
        new Uint8Array(buffer),
        x => ('00' + x.toString(16)).slice(-2)
      ).join('');
    }
  |}] buf;
end

(* JavaScript built-in Uint8Array *)
module rec Uint8Array : sig
  type t

  val from_b_array      : BitArray.t    -> t
  val from_array_buffer : ArrayBuffer.t -> t
  val to_array_buffer   : t -> ArrayBuffer.t
end = struct
  type t

  (* from https://stackoverflow.com/q/26734033/383958 *)
  let from_b_array (arr : BitArray.t) = [%raw {|
    function fromBitArrayCodec(sjcl, arr) {
        var out = [], bl = sjcl.bitArray.bitLength(arr), i, tmp;
        for (i=0; i<bl/8; i++) {
            if ((i&3) === 0) {
                tmp = arr[i/4];
            }
            out.push(tmp >>> 24);
            tmp <<= 8;
        }
        return out;
    }
  |}] Sjcl.sjcl arr

  let from_array_buffer buf
    = [%raw "function(buf) { return new Uint8Array(buf); }"] buf

  let to_array_buffer buf
    = [%raw "function(arr) { return arr.buffer; }"] buf
end

(* SJCL bitArray *)
and BitArray : sig
  type t

  val from_u8_array : Uint8Array.t -> t
end = struct
  type t

  (* from https://stackoverflow.com/q/26734033/383958 *)
  let from_u8_array (arr : Uint8Array.t) = [%raw {|
    function toBitArrayCodec(sjcl, bytes) {
        var out = [], i, tmp=0;
        for (i=0; i<bytes.length; i++) {
            tmp = tmp << 8 | bytes[i];
            if ((i&3) === 3) {
                out.push(tmp);
                tmp = 0;
            }
        }
        if (i&3) {
            out.push(sjcl.bitArray.partial(8*(i&3), tmp));
        }
        return out;
    }
  |}] Sjcl.sjcl arr
end

module Sha256 = struct
  type t

  let hash_str (str : string) : string = [%raw {|
    function(sjcl, str) {
      var bitArray = sjcl.hash.sha256.hash(str);
      return sjcl.codec.hex.fromBits(bitArray);
    }
  |}] Sjcl.sjcl str

  let hash_ba (ba : BitArray.t) : string = [%raw {|
    function(sjcl, ba) {
      var bitArray = sjcl.hash.sha256.hash(ba);
      return sjcl.codec.hex.fromBits(bitArray);
    }
  |}] Sjcl.sjcl ba
end

module Cbor = struct
  type t

  (* TODO: how to set this path properly? *)
  external cbor : t = "../../../src/cbor" [@@bs.module]

  let encode_ab (it : Js.Json.t) : ArrayBuffer.t
    = [%raw "function(cbor, it) { return cbor.encode(it); }"] cbor it

  (* TODO how do these fail? *)
  let decode_ab (it : ArrayBuffer.t) : Js.Json.t
    = [%raw "function(cbor, it) { return cbor.decode(it); }"] cbor it
end

module rec Abt : sig
  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of int
    | Sequence  of term list
    | Primitive of primitive

  val to_ast : term -> Ast.term option

  val from_ast
    : language
    -> string
    -> Ast.term
    -> (term, string) Result.t

  val from_ast_with_bindings
    :  language
    -> string
    -> int Belt.Map.String.t
    -> Ast.term
    -> (term, string) Result.t

end = struct

  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of int
    | Sequence  of term list
    | Primitive of primitive

  let rec find_operator (operators : operatorDef list) (tag : string)
    : operatorDef option
    = match operators with
      | [] -> None
      | (OperatorDef (hd, _) as od) :: tl ->
        if hd = tag
        then Some od
        else find_operator tl tag

  let rec to_ast' ctx = function
    | Var ix
    -> Option.map (List.get ctx ix) (fun name -> Ast.Var name)
    | Term (tag, subtms)
    -> Option.map
      (sequence_list_option (List.map subtms (scope_to_ast ctx)))
      (fun subtms' -> Ast.Term (tag, subtms'))
    | Sequence tms
    -> Option.map
      (sequence_list_option (List.map tms (to_ast' ctx)))
      (fun tms' -> Ast.Sequence tms')
    | Primitive prim
    -> Some (Ast.Primitive prim)

  and scope_to_ast ctx (Scope (binders, body)) = Option.map
    (to_ast' (List.concat binders ctx) body)
    (fun body' -> Ast.Scope (binders, body'))

  let to_ast = to_ast' []

  let (get, empty) = Belt.Map.String.(get, empty)
  let rec from_ast_with_bindings (Language sorts as lang) current_sort env
    = function
      | Ast.Term(tag, subtms) as tm -> (match get sorts current_sort with
        | None -> Result.Error
          ("from_ast_with_bindings: couldn't find sort " ^ current_sort)
        | Some (SortDef (_vars, operators)) -> (match find_operator operators tag with
          | None -> Error
            ("from_ast_with_bindings: couldn't find operator " ^ tag ^
            " (in sort " ^ current_sort ^ ")")
          | Some (OperatorDef (_tag, Arity (_binds, valences))) ->
            if List.(length valences != length subtms)
            then Error (
              "Unexpected number of subterms (does not match the valence of " ^
              tag ^ ")"
            )
            else Result.map
              (sequence_list_result
                (List.zipBy valences subtms
                (fun valence subtm -> match valence with
                  | FixedValence (_binds, SortName result_sort)
                    -> scope_from_ast lang result_sort env subtm
                  | _ -> Result.Error "TODO 2")))
              (fun subtms' -> Term (tag, subtms'))))
      | Ast.Var name -> (match get env name with
        | None    -> Error ("couldn't find variable " ^ name)
        | Some ix -> Ok (Var ix))
      | Ast.Sequence tms -> Result.map
        (sequence_list_result
          (List.map tms (from_ast_with_bindings lang current_sort env)))
        (fun x' -> Sequence x')
      | Primitive prim -> Ok (Abt.Primitive prim)

  and scope_from_ast lang (current_sort : string) env (Ast.Scope (names, body))
    = Result.map
      (from_ast_with_bindings lang current_sort env body)
      (fun body' -> (Scope (names, body')))

  let from_ast lang current_sort = from_ast_with_bindings lang current_sort empty

end

and Ast : sig
  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of string
    | Sequence  of term list
    | Primitive of primitive

  val pp_term  : Format.formatter -> Ast.term -> unit
  val pp_term' : Ast.term -> string

  val jsonify   : Ast.term -> Js.Json.t
  val serialize : Ast.term -> Uint8Array.t
  val hash      : Ast.term -> string

end = struct
  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of string
    | Sequence  of term list
    | Primitive of primitive

  open Format

  let rec pp_term ppf = function
    | Term (tag, subtms) -> fprintf ppf "@[%s(%a)@]"
      tag
      pp_scope_list subtms
    | Var v        -> fprintf ppf "%s" v
    | Sequence tms -> fprintf ppf "@[[%a]@]" pp_inner_list tms
    | Primitive p  -> pp_prim ppf p

  and pp_inner_list ppf = function
    | []      -> ()
    | [x]     -> fprintf ppf "%a"     pp_term x
    | x :: xs -> fprintf ppf "%a, %a" pp_term x pp_inner_list xs

  and pp_scope_list ppf = function
    | []      -> ()
    | [x]     -> fprintf ppf "%a"     pp_scope x
    | x :: xs -> fprintf ppf "%a, %a" pp_scope x pp_scope_list xs

  and pp_scope ppf (Scope (bindings, body)) = match bindings with
    | [] -> pp_term ppf body
    | _  -> fprintf ppf "%a %a" pp_bindings bindings pp_term body

  and pp_bindings ppf = function
    | []      -> ()
    | [x]     -> fprintf ppf "%s." x
    | x :: xs -> fprintf ppf "%s. %a" x pp_bindings xs

  and pp_prim ppf = function
    | PrimInteger i -> fprintf ppf "%s" (Bigint.to_string i)
    | PrimString  s -> fprintf ppf "\"%s\"" s
    | PrimBool    b -> fprintf ppf "%b" b

  let pp_term' = asprintf "%a" pp_term

  let array_map f args = Js.Json.array (List.toArray (List.map args f))

  let jsonify_prim = Js.Json.(function
    | PrimInteger i -> array [| string "i"; string (Bigint.to_string i) |]
    | PrimString  s -> array [| string "s"; string s                    |]
    | PrimBool    b -> array [| string "b"; boolean b                   |]
  )

  let rec jsonify (tm : term) : Js.Json.t = Js.Json.(match tm with
    | Term (tag, tms)
    -> array [|
      string "t";
      string tag;
      array_map jsonify_scope tms
    |]
    | Var name     -> array [| string "v"; string name           |]
    | Sequence tms -> array [| string "s"; array_map jsonify tms |]
    | Primitive p  -> array [| string "p"; jsonify_prim p        |]
  )

  and jsonify_scope (Scope (args, body)) : Js.Json.t
    = Js.Json.(array [| array_map string args; jsonify body |])

  (* serialize by converting to JSON then cboring *)
  let serialize (tm : term) : Uint8Array.t
    = Uint8Array.from_array_buffer (Cbor.encode_ab (jsonify tm))

    (*
  let deserialize (buf : Uint8Array.t) : term option
    = dejsonify (Cbor.decode_ab (Uint8Array.to_array_buffer buf))
    *)

  let hash tm = Sha256.hash_ba (BitArray.from_u8_array (serialize tm))
end

module Core = struct

  type scope_pat =
    | DenotationScopePat of string list * denotation_pat

  and denotation_pat =
    | DPatternTm of string * scope_pat list
    | DVar       of string option

  type ty = | Ty (* TODO *)

  type core_pat =
    | PatternTerm of string * core_pat list
    | PatternVar  of string option
    | PatternLit  of primitive
    | PatternDefault

  type core_val =
    | ValTm     of string * core_val list
    | ValPrim   of primitive
    | ValLam    of string list * core

  and core =
    | CoreVar of string
    | CoreVal of core_val
    | CoreApp of core * core list
    | Case    of core * ty * (core_pat * core) list
    | Meaning of string

  type denotation_chart =
    | DenotationChart of (denotation_pat * core) list

  module M = Belt.Map.String
  module O = Belt.Option

  let rec val_to_ast (core_val : core_val) : Ast.term
    = match core_val with
    | ValTm (name, vals)
    -> Ast.Term (name, failwith "TODO")
    | ValPrim prim
    -> Ast.Primitive prim
    | ValLam (args, body)
    -> Ast.Term ("lam", [ Ast.Scope (args, to_ast body) ])

  and pat_to_ast (pat : core_pat) : Ast.term
    = failwith "TODO"

  and to_ast (core : core) : Ast.term = match core with
    | CoreVar name
    -> Ast.Term ("CoreVar", [Ast.Scope ([], Ast.Primitive (PrimString name))])
    | CoreVal core_val
    -> Ast.Term ("CoreVal", [Ast.Scope ([], val_to_ast core_val)])
    (* TODO *)

  let rec match_branch (v : core_val) (pat : core_pat)
    : core_val M.t option = match (v, pat) with

    | (ValTm (tag1, vals), PatternTerm (tag2, pats)) -> List.(
      let sub_results = zipBy vals pats match_branch in
      if tag1 = tag2 &&
         length vals = length pats && every sub_results O.isSome
      then Some (reduce (map sub_results O.getExn) M.empty union)
      else None
    )
    | (ValPrim l1, PatternLit l2)
    -> if prim_eq l1 l2 then Some M.empty else None
    | (tm, PatternVar (Some v)) -> Some (M.fromArray [|v,tm|])
    | (tm, PatternVar None)     -> Some M.empty
    | (_val, PatternDefault)    -> Some M.empty
    | _ -> None

  let rec find_core_match (v : core_val) (pats : (core_pat * core) list)
    : (core * core_val M.t) option = match pats with
      | []                 -> None
      | (pat, rhs) :: pats -> (match match_branch v pat with
        | None          -> find_core_match v pats
        | Some bindings -> Some (rhs, bindings))

  let rec matches (tm : Abt.term) (pat : denotation_pat)
    : ((string * string) list * Abt.term M.t) option
    = match (tm, pat) with
      | (Term(tag1, subtms), DPatternTm(tag2, subpats))
      -> if tag1 == tag2 && List.(length subtms == length subpats)
         then fold_right
           (fun ((scope, subpat), b_opt) ->
             match (matches_scope scope subpat, b_opt) with
             | (Some (assocs, bindings), Some (assocs', bindings'))
             -> Some (assocs @ assocs', union bindings bindings')
             | _ -> None)
           (List.zip subtms subpats)
           (Some ([], M.empty))
         else None
      | (_, DPatternTm _)   -> None
      | (_, DVar None)      -> Some ([], M.empty)
      | (tm, DVar (Some v)) -> Some ([], M.fromArray [|v,tm|])

  and matches_scope
    (Scope (binders, tm) : Abt.scope)
    (DenotationScopePat (patBinders, pat) : scope_pat)
    : ((string * string) list * Abt.term M.t) option
    = if List.(length patBinders == length binders)
      then O.map
        (matches tm pat)
        (fun (assocs, tmMatches)
          -> (List.zip patBinders binders @ assocs, tmMatches))
      else None

  let find_match
    (DenotationChart denotations : denotation_chart)
    (term : Abt.term)
    : ((string * string) list * Abt.term M.t * core) option
    = get_first
        (fun (pat, core) -> O.map
          (matches term pat)
          (fun (assocs, bindings) -> (assocs, bindings, core)))
        denotations

  type located_err = (string * Abt.term option)
  type 'a translation_result = ('a, located_err) Result.t

  let rec fill_in_core
    (dynamics : denotation_chart)
    ((assocs, assignments) as mr : (string * string) list * Abt.term M.t)
    (c : core)
    : core translation_result
    = match c with
      | Meaning name -> (match M.get assignments name with
        | Some tm -> term_to_core dynamics tm
        | None    -> Error ("TODO 3", None)
        )
      (* XXX same as Meaning *)
      | CoreVar name -> (match M.get assignments name with
        | Some tm -> Result.map (term_is_core_val tm) (fun cv -> CoreVal cv)
        | None    -> Ok c
        )
      | CoreVal v -> Result.map
        (fill_in_val dynamics mr v)
        (fun v' -> CoreVal v')
      | CoreApp(f, args) -> (match
        ( fill_in_core dynamics mr f
        , sequence_list_result (List.map args (fill_in_core dynamics mr))
        ) with
        | (Ok f', Ok args')               -> Ok (CoreApp (f', args'))
        | (Error msg, _) | (_, Error msg) -> Error msg
        )
      (* | Lam (binders, core) -> Result.map
        (fill_in_core dynamics mr core)
        (fun core' -> Lam (binders, core')) *)
      | Case (scrutinee, ty, branches) ->
          let mBranches : ((core_pat * core) list) translation_result
                = sequence_list_result (List.map branches
                (fun (pat, core) -> Result.map
                  (fill_in_core dynamics mr core)
                  (fun core' -> (pat, core'))
                )) in
          match (fill_in_core dynamics mr scrutinee, mBranches) with
            | (Ok scrutinee', Ok branches')
            -> Ok (Case (scrutinee', ty, branches'))
            | (Error msg, _) | (_, Error msg)
            -> Error msg

  and fill_in_val
      (dynamics : denotation_chart)
      (mr : (string * string) list * Abt.term M.t)
      (v : core_val)
    : core_val translation_result
    = match v with
      | ValTm (tag, vals) -> Result.map
        (traverse_list_result (fill_in_val dynamics mr) vals)
        (fun vals' -> ValTm (tag, vals'))
      | ValPrim _   -> Ok v
      | ValLam (binders, core) -> Result.map
        (fill_in_core dynamics mr core)
        (fun core' -> ValLam (binders, core'))

  and term_is_core_val (tm : Abt.term) : core_val translation_result
    = match tm with
    | Term (tag, subtms) -> Result.map
      (sequence_list_result (List.map subtms scope_is_core_val))
      (fun subtms' -> ValTm (tag, subtms'))
    | Abt.Primitive prim -> Ok (ValPrim prim)
    | Abt.Var _          -> Error ("TODO 4", Some tm)
    | _                  -> Error ("TODO 5", Some tm)

  and scope_is_core_val (scope : Abt.scope) : core_val translation_result
    = failwith "TODO 6"

  and term_to_core (dynamics : denotation_chart) (tm : Abt.term)
    : core translation_result
    = match find_match dynamics tm with
      | None
      -> Error ("no match found", Some tm)
      | Some (assocs, bindings, protoCore)
      -> fill_in_core dynamics (assocs, bindings) protoCore

  let eval (core : core) : (core_val, string) Result.t =

    let open Belt.Result in

    let rec go ctx = function
          | CoreVar v -> (match M.get ctx v with
            | Some result -> Ok result
            | None        -> Error ("Unbound variable " ^ v)
          )
          | CoreVal v -> Ok v
          | CoreApp (CoreVal (ValLam (argNames, body)), args) ->
              if List.(length argNames != length args)
              then Error "mismatched application lengths"
              else Result.flatMap
                (sequence_list_result (List.map args (go ctx)))
                (fun arg_vals ->
                   let new_args = M.fromArray
                     (List.(toArray (zip argNames arg_vals))) in
                   go (union ctx new_args) body)
          | Case (tm, _ty, branches) -> Result.flatMap (go ctx tm)
            (fun v -> (match find_core_match v branches with
              | None                    -> Error "no match found in case"
              | Some (branch, bindings) -> go (union ctx bindings) branch)
            )
          | Meaning _v -> Error "Found a metavar!"

          | _ -> Error "TODO 7"

    in go M.empty core

end

module Statics = struct

  module M = Belt.Map.String

  type scope = Scope of string list * term

  and term =
    | Term      of string * scope list
    | Bound     of int
    | Free      of string
    | Sequence  of term list
    | Primitive of primitive

  type inferenceRule = InferenceRule of term * term
  type checkingRule  = CheckingRule  of term * term

  type typingClause =
    | InferenceRule of inferenceRule
    | CheckingRule  of checkingRule

  type hypothesis = term M.t * typingClause

  (* TODO: the conclusion type differs from LVCA *)
  type rule = Rule of hypothesis list * string option * hypothesis

end
