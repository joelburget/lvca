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
module rec Bytes : sig
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

  val from_u8_array : Bytes.t -> t
end = struct
  type t

  (* from https://stackoverflow.com/q/26734033/383958 *)
  let from_u8_array (arr : Bytes.t) = ([%raw {|
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
  |}] : Sjcl.sjcl -> Bytes.t -> t) Sjcl.sjcl arr
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
