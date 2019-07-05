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
  | (PrimInteger i1, PrimInteger i2) -> Bigint.(i1 = i2) [@warning "-44"]
  | (PrimString  s1, PrimString  s2) -> s1 = s2
  | (PrimBool    b1, PrimBool    b2) -> b1 = b2
  | _                                -> false

module Sjcl = struct
  type sjcl

  external sjcl : sjcl = "sjcl" [@@bs.module]
end

module ArrayBuffer = struct
  type t

  let to_hex (buf : t) : string = ([%raw {|
    function to_hex(buffer) {
      return Array.prototype.map.call(
        new Uint8Array(buffer),
        x => ('00' + x.toString(16)).slice(-2)
      ).join('');
    }
  |}] : t -> string) buf;
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
  let from_b_array (arr : BitArray.t) = ([%raw {|
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
  |}]: Sjcl.sjcl -> BitArray.t -> t) Sjcl.sjcl arr

  let from_array_buffer buf
    = ([%raw "function(buf) { return new Uint8Array(buf); }"]
      : ArrayBuffer.t -> t)
      buf

  let to_array_buffer buf
    = ([%raw "function(arr) { return arr.buffer; }"] : t -> ArrayBuffer.t) buf
end

(* SJCL bitArray *)
and BitArray : sig
  type t

  val from_u8_array : Uint8Array.t -> t
end = struct
  type t

  (* from https://stackoverflow.com/q/26734033/383958 *)
  let from_u8_array (arr : Uint8Array.t) = ([%raw {|
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
  |}] : Sjcl.sjcl -> Uint8Array.t -> t) Sjcl.sjcl arr
end

module Sha256 = struct
  type t

  let hash_str (str : string) : string = ([%raw {|
    function(sjcl, str) {
      var bitArray = sjcl.hash.sha256.hash(str);
      return sjcl.codec.hex.fromBits(bitArray);
    }
  |}] : Sjcl.sjcl -> string -> string) Sjcl.sjcl str

  let hash_ba (ba : BitArray.t) : string = ([%raw {|
    function(sjcl, ba) {
      var bitArray = sjcl.hash.sha256.hash(ba);
      return sjcl.codec.hex.fromBits(bitArray);
    }
  |}] : Sjcl.sjcl -> BitArray.t -> string) Sjcl.sjcl ba
end

module Cbor = struct
  type t

  (* TODO: how to set this path properly? *)
  external cbor : t = "../../../src/cbor" [@@bs.module]

  let encode_ab (it : Js.Json.t) : ArrayBuffer.t
    = ([%raw "function(cbor, it) { return cbor.encode(it); }"]
      : t -> Js.Json.t -> ArrayBuffer.t)
      cbor it

  let decode_ab (it : ArrayBuffer.t) : Js.Json.t option
    = let raw_decode : t -> ArrayBuffer.t -> Js.Json.t
            = [%raw "function(cbor, it) { return cbor.decode(it); }"]
      in try
           Some (raw_decode cbor it)
         with
           _ -> None
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

  module M = Belt.Map.String
  let (get, empty) = Belt.Map.String.(get, empty)
  let rec from_ast_with_bindings (Language sorts as lang) current_sort env
    = function
      | Ast.Term(tag, subtms) -> (match get sorts current_sort with
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

  and scope_from_ast
    lang
    (current_sort : string)
    env
    (Ast.Scope (names, body))
    =
      let n = List.length names in
      let argNums = List.zip names (List.makeBy n (fun i -> i)) in
      let env' = union
            (M.map env (fun i -> i + n))
            (M.fromArray (List.(toArray argNums)))
      in Result.map
           (from_ast_with_bindings lang current_sort env' body)
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

(** A description of the concrete syntax for a language *)
module ConcreteSyntax = struct

  module M = Belt.Map.String

  type fixity         = Infixl | Infixr | Infix
  type capture_number = int
  type terminal_id    = string
  type regex          = string
  type terminal_rule  = TerminalRule of terminal_id * regex

  type terminal_rules = regex M.t

  type nonterminal_token =
    | TerminalName    of string
    | NonterminalName of string

  type term_scope = TermScope of capture_number list * capture_number

  type operator_match' =
    { tokens       : nonterminal_token list;
      term_pattern : string * term_scope list;
    }
  type operator_match = OperatorMatch of operator_match'

  type variable_rule = { tokens: nonterminal_token list; }

  exception DuplicateVarRules

  let partition_nonterminal_matches
    (matches: operator_match list)
    : (operator_match list * variable_rule option)
    = fold_right
      (fun (match_, (matches, v_rule)) -> match match_ with
        | OperatorMatch { tokens; term_pattern = ("var", _) }
        -> (match v_rule with
          | Some _ -> raise DuplicateVarRules
          | None   -> (matches, Some { tokens })
        )
        | _
        -> (match_ :: matches, v_rule)
      )
      matches ([], None)

  type sort_rule' =
    { sort_name : string;
      operator_rules : operator_match list;
      variable  : variable_rule option;
    }
  (** A sort rule shows how to parse / pretty-print a sort *)
  type sort_rule = SortRule of sort_rule'

  (** Mapping from sort names to sort rules *)
  type sort_rules = sort_rule M.t

  type t = {
    terminal_rules: terminal_rules;
    sort_rules:     sort_rules;
  }

end
