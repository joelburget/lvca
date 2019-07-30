(** Types for representing languages *)
open Util

(** Sorts divide ASTs into syntactic categories. *)
type sort =
  (** A higher-kinded sort can be applied *)
  | SortAp   of string * sort array

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

let prim_eq p1 p2 = match (p1, p2) with
  | (PrimInteger i1, PrimInteger i2) -> Bigint.(i1 = i2) [@warning "-44"]
  | (PrimString  s1, PrimString  s2) -> s1 = s2
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

  external cbor : t = "./cbor" [@@bs.module]

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

module Statics = struct

  module M = Belt.Map.String

  type scope = Scope of string list * term

  and term =
    | Term      of string * scope list
    | Bound     of int
    | Free      of string
    | Sequence  of term list
    | Primitive of primitive

  type inference_rule = {
    tm : term;
    ty : term;
  }

  type checking_rule = inference_rule

  type typing_clause =
    | InferenceRule of inference_rule
    | CheckingRule  of checking_rule

  type hypothesis = term M.t * typing_clause

  (* TODO: the conclusion type differs from LVCA *)
  type rule = Rule of hypothesis list * string option * hypothesis

end

(** A description of the concrete syntax for a language *)
module ConcreteSyntaxDescription = struct

  module M = Belt.Map.String

  type capture_number = int
  type terminal_id    = string

  type regex_piece =
    | ReString of string
    | ReSet    of string
    | ReStar   of regex_piece
    | RePlus   of regex_piece
    | ReOption of regex_piece

  (* A regular expression used for lexical analysis. Currently, this uses the
   * jison format. *)
  type regex          = regex_piece list
  type terminal_rule  = TerminalRule of terminal_id * regex

  type terminal_rules = regex M.t

  type nonterminal_token =
    | TerminalName    of string
    | NonterminalName of string
    | Underscore

  let token_name = function
    | TerminalName    name -> Some name
    | NonterminalName name -> Some name
    | Underscore           -> None

  (* A term pattern with numbered holes for binder names and subterms *)
  type numbered_scope_pattern =
    NumberedScopePattern of capture_number list * capture_number

  type term_pattern =
    | TermPattern           of string * numbered_scope_pattern list
    | ParenthesizingPattern of capture_number

  type fixity =
    | Infixl
    | Infixr
    | Nofix

  let fixity_str = function
    | Infixl -> "left"
    | Infixr -> "right"
    | Nofix  -> "nonassoc"

  type operator_match' =
    { tokens       : nonterminal_token list;
      term_pattern : term_pattern;
      fixity       : fixity
    }
  type operator_match = OperatorMatch of operator_match'

  type variable_rule =
    { tokens      : nonterminal_token list;
      var_capture : capture_number;
    }

  exception DuplicateVarRules

  (* Extract a variable rule, if present. Currently we only recognize it on its
   * own precedence level, which seems like what you usually want, but still
   * arbitrary
   *)
  let partition_nonterminal_matches
    (matches: operator_match list list)
    : operator_match list list * variable_rule option
    = fold_right
      (fun (match_, (matches, v_rule)) -> match match_ with
        | [ OperatorMatch
            { tokens;
              term_pattern = TermPattern
                ("var", [NumberedScopePattern ([], var_capture)]);
            }
          ]
        -> (match v_rule with
          | Some _ -> raise DuplicateVarRules
          | None   -> matches, Some { tokens; var_capture }
        )
        | _
        -> match_ :: matches, v_rule
      )
      matches ([], None)

  type sort_rule' =
    { sort_name      : string;
      operator_rules : operator_match list list;
      variable       : variable_rule option;
    }
  (** A sort rule shows how to parse / pretty-print a sort *)
  type sort_rule = SortRule of sort_rule'

  (** Mapping from sort names to sort rules *)
  type sort_rules = sort_rule M.t

  type t = {
    terminal_rules : terminal_rules;
    sort_rules     : sort_rules;
  }

  let make (terminal_rules: terminal_rule list) (sort_rules : sort_rule list) =
    { terminal_rules = terminal_rules
      |> List.map (fun (TerminalRule (name, rule)) -> name, rule)
      |> Belt.List.toArray
      |> M.fromArray;
    sort_rules = sort_rules
      |> List.map (fun ((SortRule { sort_name }) as rule) -> sort_name, rule)
      |> Belt.List.toArray
      |> M.fromArray;
    }

end
