type bitstring = Bitstring of int * bytes

(* internal *)
let index : int -> int * int
  = fun i -> if i < 0
    then raise (Invalid_argument "Bitstring.index: negative index")
    else i / 8, i mod 8

let getExn : bitstring -> int -> bool
  = fun (Bitstring (bs_len, bs)) i ->
    if i >= bs_len then
      raise (Invalid_argument "Bitstring.getExn: index too large");
    let (bytes_ix, char_ix) = index i in
    let c = Bytes.get bs bytes_ix in
    let c_code = Char.code c in
    let mask = 1 lsl char_ix in
    c_code land mask = mask

let get : bitstring -> int -> bool option
  = fun bs i ->
    try
      Some (getExn bs i)
    with
      _ -> None

let setExn : bitstring -> int -> bool -> unit
  = fun (Bitstring (bs_len, bs)) i b ->
    if i >= bs_len then
      raise (Invalid_argument "Bitstring.setExn: index too large");
    let (bytes_ix, char_ix) = index i in
    let c = Bytes.get bs bytes_ix in
    let c_code = Char.code c in
    let mask = 1 lsl char_ix in
    let new_char = if b then c_code lor mask else c_code land (lnot mask) in
    Bytes.set bs bytes_ix @@ Char.chr @@ new_char

let set : bitstring -> int -> bool -> unit option
  = fun bs i b ->
    try
      Some (setExn bs i b)
    with
      _ -> None

let alloc : int -> bool -> bitstring
  = fun len b ->
    let one = if len mod 8 = 0 then 0 else 1 in
    Bitstring
      ( len,
        Bytes.make (len / 8 + one) @@ Char.chr @@ if b then 0xff else 0x00
      )

let length : bitstring -> int
  = fun (Bitstring (bs_len, _)) -> bs_len
