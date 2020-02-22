type t = Bitstring of int * bytes
type bitstring = t

(* internal *)
let index : int -> int * int =
  fun i ->
  if i < 0
  then raise (Invalid_argument "Bitstring.index: negative index")
  else i / 8, i mod 8
;;

let get_exn : bitstring -> int -> bool =
  fun (Bitstring (bs_len, bs)) i ->
  if i >= bs_len then raise (Invalid_argument "Bitstring.get_exn: index too large");
  let bytes_ix, char_ix = index i in
  let c = Bytes.get bs bytes_ix in
  let c_code = Char.code c in
  let mask = 1 lsl char_ix in
  c_code land mask = mask
;;

let get : bitstring -> int -> bool option =
  fun bs i ->
  try
    Some (get_exn bs i)
  with
    _ -> None
;;

let set_exn : bitstring -> int -> bool -> unit =
  fun (Bitstring (bs_len, bs)) i b ->
  if i >= bs_len then raise (Invalid_argument "Bitstring.set_exn: index too large");
  let bytes_ix, char_ix = index i in
  let c = Bytes.get bs bytes_ix in
  let c_code = Char.code c in
  let mask = 1 lsl char_ix in
  let new_char = if b then c_code lor mask else c_code land lnot mask in
  Bytes.set bs bytes_ix @@ Char.chr @@ new_char
;;

let set : bitstring -> int -> bool -> unit option =
  fun bs i b ->
  try
    Some (set_exn bs i b)
  with
    _ -> None
;;

let alloc : int -> bool -> bitstring =
  fun len b ->
  if len < 0 then raise (Invalid_argument "Bitstring.alloc: length must be non-negative");
  let one = if len mod 8 = 0 then 0 else 1 in
  Bitstring (len, Bytes.make ((len / 8) + one) @@ Char.chr @@ if b then 0xff else 0x00)
;;

let length : bitstring -> int = fun (Bitstring (bs_len, _)) -> bs_len


let%test_module "bitstring tests" = (module struct
  let bs = alloc 10 false

  let%test "index" = index 0 = (0, 0)
  let%test "index" = index 7 = (0, 7)
  let%test "index" = index 8 = (1, 0)
  let%test "index" = index 9 = (1, 1)
  let%test "index" = index 15 = (1, 7)

  let%test "get_exn" = get_exn bs 0 = false
  let%test "set_exn" = set_exn bs 0 true = ()
  let%test "get" = get bs 0 = Some true
  let%test "set" = set bs 0 false = Some ()

  let%test "get" = get bs 9 = Some false
  let%test "get" = get bs 10 = None

end)
