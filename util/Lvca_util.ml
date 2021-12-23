open Base

let ( << ) f g x = f (g x)
let ( >> ) f g = g << f

(* Used by bidirectional *)
module Map = struct
  include Base.Map

  let remove_many : ('k, 'v, 'cmp) t -> string array -> ('k, 'v, 'cmp) t =
   fun map keys -> Array.fold keys ~init:map ~f:Map.remove
 ;;

  let union_left_biased m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_k -> function
      | `Both (v, _) -> Some v | `Left v | `Right v -> Some v)
  ;;

  let union_right_biased m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_k -> function
      | `Both (_, v) -> Some v | `Left v | `Right v -> Some v)
  ;;
end

module Int = struct
  include Base.Int

  module Map = struct
    type 'a t = (int, 'a, Base.Int.comparator_witness) Map.t
    type key = int

    let empty = Map.empty (module Base.Int)
    let of_alist_exn lst = Map.of_alist_exn (module Base.Int) lst
  end

  module Set = struct
    type t = (int, Base.Int.comparator_witness) Base.Set.t

    let empty = Base.Set.empty (module Base.Int)
    let of_list = Base.Set.of_list (module Base.Int)
    let singleton = Base.Set.singleton (module Base.Int)
    let union_list = Base.Set.union_list (module Base.Int)
  end
end

module String = struct
  include Base.String

  module Map = struct
    type 'a t = (string, 'a, Base.String.comparator_witness) Base.Map.t
    type key = string

    let empty = Base.Map.empty (module Base.String)
    let singleton k v = Base.Map.singleton (module Base.String) k v
    let of_alist lst = Base.Map.of_alist (module Base.String) lst
    let of_alist_exn lst = Base.Map.of_alist_exn (module Base.String) lst

    (* Used by Core.Types, Bidirectional *)
    let unions_left_biased : 'a t list -> 'a t =
      List.fold_left ~init:empty ~f:(Map.merge_skewed ~combine:(fun ~key:_k v1 _v2 -> v1))
    ;;

    let unions_right_biased : 'a t list -> 'a t =
      List.fold_left ~init:empty ~f:(Map.merge_skewed ~combine:(fun ~key:_k _v1 v2 -> v2))
    ;;

    exception DuplicateKey of string

    let strict_union : 'a t -> 'a t -> [ `Ok of 'a t | `Duplicate_key of string ] =
     fun m1 m2 ->
      try
        `Ok
          (Map.merge m1 m2 ~f:(fun ~key -> function
             | `Both _ -> raise (DuplicateKey key) | `Left v | `Right v -> Some v))
      with
      | DuplicateKey k -> `Duplicate_key k
   ;;

    let strict_unions : 'a t list -> [ `Ok of 'a t | `Duplicate_key of string ] =
     fun lst ->
      try
        `Ok
          (List.fold_left lst ~init:empty ~f:(fun accum m ->
               match strict_union accum m with
               | `Ok m' -> m'
               | `Duplicate_key key -> raise (DuplicateKey key)))
      with
      | DuplicateKey k -> `Duplicate_key k
   ;;

    let join_helper
        : 'a t option list -> [ `Ok of 'a t option | `Duplicate_key of string ]
      =
     fun lst ->
      try
        lst
        |> Option.all
        |> Option.map ~f:(fun map_list ->
               match strict_unions map_list with
               | `Ok result -> result
               | `Duplicate_key k -> raise (DuplicateKey k))
        |> fun m -> `Ok m
      with
      | DuplicateKey k -> `Duplicate_key k
   ;;

    let intersect : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t =
     fun a_map b_map ~f ->
      Map.merge a_map b_map ~f:(fun ~key:_ -> function
        | `Left _ | `Right _ -> None | `Both (a, b) -> Some (f a b))
   ;;
  end

  module Set = struct
    type t = (string, Base.String.comparator_witness) Base.Set.t

    let empty = Base.Set.empty (module Base.String)
    let of_list = Base.Set.of_list (module Base.String)
    let singleton = Base.Set.singleton (module Base.String)
    let union_list = Base.Set.union_list (module Base.String)
  end

  let slice : string -> int -> int -> string =
   fun t start stop ->
    let normalize t' i = if Int.(i < 0) then i + length t' else i in
    let stop = if Int.(stop = 0) then length t else stop in
    let pos = normalize t start in
    let len = normalize t stop - pos in
    sub t ~pos ~len
 ;;

  let is_ignore : string -> bool = fun name -> Char.(name.[0] = '_')
end

exception InvariantViolation of Lexing.position * string

let invariant_violation here str = raise (InvariantViolation (here, str))

module List = struct
  include Base.List

  let rec snoc xs x = match xs with [] -> [ x ] | x' :: xs -> x' :: snoc xs x

  let rec unsnoc lst =
    match lst with
    | [] -> invariant_violation [%here] "unsnoc empty list"
    | [ x ] -> [], x
    | x :: lst' ->
      let front, last = unsnoc lst' in
      x :: front, last
  ;;

  let rec remove_nth list i =
    match list with
    | [] -> list
    | x :: xs -> if i = 0 then xs else x :: remove_nth xs (i - 1)
  ;;

  let rec update_nth list ~i ~f =
    match list, i with
    | [], _ -> []
    | x :: xs, 0 -> f x :: xs
    | x :: xs, _ -> x :: update_nth xs ~i:(i - 1) ~f
  ;;

  let set_nth list ~i ~data = update_nth list ~i ~f:(Fn.const data)

  (* i, j < len list *)
  let swap list ~i ~j =
    let rec swap list n m =
      match n, list with
      | 0, x :: xs ->
        let mth = List.nth_exn xs (m - 1) in
        mth :: set_nth xs ~i:(m - 1) ~data:x
      | _ -> swap list (n - 1) (m - 1)
    in
    if Int.(i = j) then list else swap list (min i j) (max i j)
  ;;

  let split_exn = function
    | [] -> invariant_violation [%here] "called with empty sorts"
    | x :: xs -> x, xs
  ;;
end

module Option = struct
  include Base.Option

  let get_or_raise : 'b -> 'a option -> 'a =
   fun err -> function None -> raise err | Some a -> a
 ;;

  let get_invariant : Lexing.position -> (unit -> string) -> 'a option -> 'a =
   fun here msg -> function
    | None -> raise (InvariantViolation (here, "invariant violation: " ^ msg ()))
    | Some a -> a
 ;;
end

module Json = struct
  type t =
    | String of string
    | Array of t array
    | Float of float
    | Int of int

  type -'a serializer = 'a -> t
  type +'a deserializer = t -> 'a option

  let array : t array -> t = fun arr -> Array arr
  let string : string -> t = fun str -> String str
  let float : float -> t = fun f -> Float f
  let int : int -> t = fun f -> Int f

  let array_map : ('a -> t) -> 'a list -> t =
   fun f -> Base.List.map ~f >> Array.of_list >> array
 ;;

  let rec ( = ) : t -> t -> bool =
   fun t1 t2 ->
    match t1, t2 with
    | String s1, String s2 -> String.(s1 = s2)
    | Array arr1, Array arr2 ->
      (match Array.zip arr1 arr2 with
      | None -> false
      | Some arr -> Array.for_all arr ~f:(fun (t1, t2) -> t1 = t2))
    | Float f1, Float f2 -> Float.(f1 = f2)
    | Int i1, Int i2 -> Int.(i1 = i2)
    | _, _ -> false
 ;;
end

module Sha256 = struct
  let hash_str : string -> Digestif.SHA256.t = Digestif.SHA256.digest_string
  let to_hex : Digestif.SHA256.t -> string = Digestif.SHA256.to_hex

  let hash : Bytes.t -> string =
   fun bytes -> bytes |> Bytes.to_string |> hash_str |> to_hex
 ;;
end

module Cbor = struct
  let rec of_json : Json.t -> CBOR.Simple.t = function
    | String str -> `Text str
    | Array arr -> `Array (arr |> Array.map ~f:of_json |> Array.to_list)
    | Float f -> `Float f
    | Int i -> `Int i
  ;;

  let rec to_json : CBOR.Simple.t -> Json.t option = function
    | `Text str -> Some (String str)
    | `Array arr ->
      arr
      |> Base.List.map ~f:to_json
      |> Base.Option.all
      |> Base.Option.map ~f:(fun lst -> Json.Array (Array.of_list lst))
    | `Float f -> Some (Float f)
    | _ -> None
  ;;

  let encode : Json.t -> Bytes.t =
   fun json -> json |> of_json |> CBOR.Simple.encode |> Bytes.of_string
 ;;

  let decode : Bytes.t -> Json.t option =
   fun buf -> buf |> Bytes.to_string |> CBOR.Simple.decode |> to_json
 ;;
end

module type Tuple_elem = sig
  type t

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val ( = ) : t -> t -> bool
end

module Tuple2 = struct
  type ('a, 'b) t = 'a * 'b

  let sexp_of_t f1 f2 (x, y) = Sexplib0.Sexp.List [ f1 x; f2 y ]

  let compare ~cmp1 ~cmp2 (x1, y1) (x2, y2) =
    let c1 = cmp1 x1 x2 in
    if c1 <> 0 then c1 else cmp2 y1 y2
  ;;

  let equal eq1 eq2 (x1, y1) (x2, y2) = eq1 x1 x2 && eq2 y1 y2
  let get1 (x, _) = x
  let get2 (_, y) = y
  let map ~f (x, y) = f x, f y
  let map1 ~f (x, y) = f x, y
  let map2 ~f (x, y) = x, f y
  let curry f x y = f (x, y)
  let uncurry f (x, y) = f x y

  module Make (X : Tuple_elem) (Y : Tuple_elem) = struct
    let compare = compare ~cmp1:X.compare ~cmp2:Y.compare
    let sexp_of_t = sexp_of_t X.sexp_of_t Y.sexp_of_t
    let ( = ) = equal X.( = ) Y.( = )
  end

  module Int = Make (Int) (Int)
end

module Tuple3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let sexp_of_t f1 f2 f3 (x, y, z) = Sexplib0.Sexp.List [ f1 x; f2 y; f3 z ]

  let compare ~cmp1 ~cmp2 ~cmp3 (x1, y1, z1) (x2, y2, z2) =
    let c1 = cmp1 x1 x2 in
    if c1 <> 0
    then c1
    else (
      let c2 = cmp2 y1 y2 in
      if c2 <> 0 then c2 else cmp3 z1 z2)
  ;;

  let equal eq1 eq2 eq3 (x1, y1, z1) (x2, y2, z2) = eq1 x1 x2 && eq2 y1 y2 && eq3 z1 z2
  let get1 (x, _, _) = x
  let get2 (_, y, _) = y
  let get3 (_, _, z) = z
  let map ~f (x, y, z) = f x, f y, f z
  let map1 ~f (x, y, z) = f x, y, z
  let map2 ~f (x, y, z) = x, f y, z
  let map3 ~f (x, y, z) = x, y, f z
  let curry f x y z = f (x, y, z)
  let uncurry f (x, y, z) = f x y z

  module Make (X : Tuple_elem) (Y : Tuple_elem) (Z : Tuple_elem) = struct
    let compare = compare ~cmp1:X.compare ~cmp2:Y.compare ~cmp3:Z.compare
    let sexp_of_t = sexp_of_t X.sexp_of_t Y.sexp_of_t Z.sexp_of_t
    let ( = ) = equal X.( = ) Y.( = ) Z.( = )
  end

  module Int = Make (Int) (Int) (Int)
end

module Tuple4 = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  let sexp_of_t f1 f2 f3 f4 (w, x, y, z) = Sexplib0.Sexp.List [ f1 w; f2 x; f3 y; f4 z ]

  let compare ~cmp1 ~cmp2 ~cmp3 ~cmp4 (w1, x1, y1, z1) (w2, x2, y2, z2) =
    let c1 = cmp1 w1 w2 in
    if c1 <> 0
    then c1
    else (
      let c2 = cmp2 x1 x2 in
      if c2 <> 0
      then c2
      else (
        let c3 = cmp3 y1 y2 in
        if c3 <> 0 then c3 else cmp4 z1 z2))
  ;;

  let equal eq1 eq2 eq3 eq4 (w1, x1, y1, z1) (w2, x2, y2, z2) =
    eq1 w1 w2 && eq2 x1 x2 && eq3 y1 y2 && eq4 z1 z2
  ;;

  let get1 (w, _, _, _) = w
  let get2 (_, x, _, _) = x
  let get3 (_, _, y, _) = y
  let get4 (_, _, _, z) = z
  let map ~f (w, x, y, z) = f w, f x, f y, f z
  let map1 ~f (w, x, y, z) = f w, x, y, z
  let map2 ~f (w, x, y, z) = w, f x, y, z
  let map3 ~f (w, x, y, z) = w, x, f y, z
  let map4 ~f (w, x, y, z) = w, x, y, f z
  let curry f w x y z = f (w, x, y, z)
  let uncurry f (w, x, y, z) = f w x y z

  module Make (W : Tuple_elem) (X : Tuple_elem) (Y : Tuple_elem) (Z : Tuple_elem) = struct
    let compare = compare ~cmp1:W.compare ~cmp2:X.compare ~cmp3:Y.compare ~cmp4:Z.compare
    let sexp_of_t = sexp_of_t W.sexp_of_t X.sexp_of_t Y.sexp_of_t Z.sexp_of_t
    let ( = ) = equal W.( = ) X.( = ) Y.( = ) Z.( = )
  end

  module Int = Make (Int) (Int) (Int) (Int)
  module Bool = Make (Bool) (Bool) (Bool) (Bool)
end

module Property_result = struct
  type t =
    | Ok
    | Uninteresting
    | Failed of string

  let check b msg = if b then Ok else Failed msg
end

module Unique = struct
  let alphabet =
    "abcdefghijklmnopqrstuvwxyz" |> String.to_list |> List.map ~f:String.of_char
  ;;

  (** The sequence "a"; "b"; ...; "z"; "aa"; ... "az"; ... "zz"; "aaa"; ... "aaaa"; ... *)
  let name_sequence =
    let letter_seq = Sequence.of_list alphabet in
    let init = Sequence.of_list [ "" ] in
    Sequence.unfold ~init ~f:(fun (prev : string Sequence.t) ->
        let next =
          Sequence.cartesian_product prev letter_seq
          |> Sequence.map ~f:(fun (x, y) -> x ^ y)
        in
        Some (next, next))
    |> Sequence.concat
  ;;

  let empty_name_sequence = Sequence.append (Sequence.singleton "") name_sequence

  let%test_module "name_sequence" =
    (module struct
      let%expect_test _ =
        Stdio.print_string (Sequence.nth_exn name_sequence 0);
        [%expect "a"]
      ;;

      let%expect_test _ =
        Stdio.print_string (Sequence.nth_exn name_sequence 25);
        [%expect "z"]
      ;;

      let%expect_test _ =
        Stdio.print_string (Sequence.nth_exn name_sequence 26);
        [%expect "aa"]
      ;;

      let%expect_test _ =
        Stdio.print_string (Sequence.nth_exn name_sequence 51);
        [%expect "az"]
      ;;

      let%expect_test _ =
        Stdio.print_string (Sequence.nth_exn name_sequence 52);
        [%expect "ba"]
      ;;

      let%expect_test _ =
        Stdio.print_string (Sequence.nth_exn name_sequence 77);
        [%expect "bz"]
      ;;
    end)
  ;;

  let generate_name ?(base = "") taken =
    let name =
      if String.(base = "")
      then Sequence.find_exn name_sequence ~f:(fun name -> not (Set.mem taken name))
      else (
        let sequence = empty_name_sequence |> Sequence.map ~f:(fun name -> base ^ name) in
        Sequence.find_exn sequence ~f:(fun name -> not (Set.mem taken name)))
    in
    Set.add taken name, name
  ;;

  let generate_names ?(base = "") taken =
    if String.(base = "")
    then Sequence.filter name_sequence ~f:(fun name -> not (Set.mem taken name))
    else (
      let sequence = empty_name_sequence |> Sequence.map ~f:(fun name -> base ^ name) in
      Sequence.filter sequence ~f:(fun name -> not (Set.mem taken name)))
  ;;

  let%test_module "generate_name" =
    (module struct
      let%expect_test _ =
        let taken = String.Set.empty in
        Stdio.printf "%s\n" (generate_name taken |> snd);
        Stdio.printf "%s\n" (generate_name ~base:"b" taken |> snd);
        Sequence.take (generate_names ~base:"b" taken) 5
        |> Sequence.iter ~f:(Stdio.printf "%s\n");
        [%expect {|
      a
      b
      b
      ba
      bb
      bc
      bd |}]
      ;;

      let%expect_test _ =
        let taken = String.Set.of_list [ "a"; "b" ] in
        Stdio.printf "%s\n" (generate_name taken |> snd);
        Stdio.printf "%s\n" (generate_name ~base:"a" taken |> snd);
        Sequence.take (generate_names taken) 5 |> Sequence.iter ~f:(Stdio.printf "%s\n");
        [%expect {|
      c
      aa
      c
      d
      e
      f
      g |}]
      ;;

      let%expect_test _ =
        let taken = String.Set.of_list alphabet in
        Stdio.printf "%s\n" (generate_name taken |> snd);
        Stdio.printf "%s\n" (generate_name ~base:"c" taken |> snd);
        Sequence.take (generate_names taken) 5 |> Sequence.iter ~f:(Stdio.printf "%s\n");
        [%expect {|
      aa
      ca
      aa
      ab
      ac
      ad
      ae |}]
      ;;

      let%expect_test _ =
        let taken = String.Set.empty in
        let base = "the quick brown fox jumps over the lazy dog" in
        Stdio.printf "%s\n" (generate_name ~base taken |> snd);
        Sequence.take (generate_names ~base taken) 5
        |> Sequence.iter ~f:(Stdio.printf "%s\n");
        [%expect
          {|
        the quick brown fox jumps over the lazy dog
        the quick brown fox jumps over the lazy dog
        the quick brown fox jumps over the lazy doga
        the quick brown fox jumps over the lazy dogb
        the quick brown fox jumps over the lazy dogc
        the quick brown fox jumps over the lazy dogd |}]
      ;;
    end)
  ;;
end

module Format = struct
  let fmt_stag stag formatter ppf a =
    Stdlib.Format.pp_open_stag ppf stag;
    formatter ppf a;
    Stdlib.Format.pp_close_stag ppf ()
  ;;
end
