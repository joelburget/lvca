(** Types for representing languages *)

type sort_name = string

(** Sorts divide ASTs into syntactic categories. *)
type sort =
  (** A higher-kinded sort can be applied *)
  | SortAp of sort_name * sort array

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
  (** An arity is defined its arity indices and valences. Arity indices are
      variables bound in an arity rule specifying the length of variable-length
      slots. *)

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

let string_of_primitive = function
  | PrimInteger i  -> Bigint.to_string i
  | PrimString str -> "\"" ^ String.escaped str ^ "\""

let prim_eq p1 p2 = match (p1, p2) with
  | (PrimInteger i1, PrimInteger i2) -> Bigint.(i1 = i2) [@warning "-44"]
  | (PrimString  s1, PrimString  s2) -> s1 = s2
  | _                                -> false

type import =
  { imported_symbols: (string * string) list;
    location: string;
  }

type abstract_syntax =
  { imports: import list;
    language: language;
  }

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

  external cbor : t = "cbor" [@@bs.module]

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
