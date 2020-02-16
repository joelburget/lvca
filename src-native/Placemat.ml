(** Sits on top of the tablecloth, but still below everything else *)

module List = struct
  let unzip : ('a * 'b) list -> 'a list * 'b list
    = Base.List.unzip

  let for_each : f:('a -> 'b) -> 'a list -> unit
    = fun ~f lst ->
      let _ = Base.List.for_all lst ~f:(fun a -> f a; true)
      in ()

  let for_each_with_index : f:(int -> 'a -> 'b) -> 'a list -> unit
    = fun ~f lst ->
      let _ = Base.List.for_alli lst ~f:(fun i a -> f i a; true)
      in ()

  let initialize : length:int -> f:(int -> 'a) -> 'a list
    = fun ~length ~f -> Core_kernel.List.init length ~f

  let map_with_index : f:(int -> 'a -> 'b) -> 'a list -> 'b list
    = fun ~f lst -> Base.List.mapi lst ~f

  let zip_by : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    = fun ~f lst_a lst_b ->
      let rec go xs ys = match xs, ys with
        | x :: xs, y :: ys -> f x y :: go xs ys
        | _, _ -> []
      in go lst_a lst_b

  let fold_right : f:('a -> 'b -> 'b) -> initial:'b -> 'a list -> 'b
    = fun ~f ~initial ->
    let rec go = function
      | [] -> initial
      | x :: xs -> f x (go xs)
    in go

  let fold_left : f:('b -> 'a -> 'b) -> initial:'b -> 'a list -> 'b
    = fun ~f ~initial ->
    let rec go accum = function
      | [] -> accum
      | x :: xs -> go (f accum x) xs
    in go initial
end

module Array = struct
  type 'a t = 'a array

  let unzip : ('a * 'b) t -> 'a t * 'b t
    = Base.Array.unzip

  let map_with_index : f:(int -> 'a -> 'b) -> 'a t -> 'b t
    = fun ~f arr -> Base.Array.mapi arr ~f

  let filter_map : 'a t -> f:('a -> 'b option) -> 'b t
    = Base.Array.filter_map
end

module Id = struct

  type ('a, 'id) cmp = 'a -> 'a -> int

  module type Comparable = sig
    type identity
    type t
    val cmp: (t, identity) cmp
  end

  type ('key, 'id) comparable =
    (module Comparable with type identity = 'id and type t = 'key)

  module MakeComparable (M : sig
    type t
    val cmp: t -> t -> int
  end) =
  struct
    type identity
    type t = M.t
    let cmp = M.cmp
  end

  let comparable
    (type key)
    ~cmp
    =
    let module N = MakeComparable(struct
        type t = key
        let cmp = cmp
      end) in
    (module N : Comparable with type t = key)
end

module MutableMap = struct

  module Impl = struct
    module Avltree = Base.Avltree

    type ('k, 'cmp) comparator =
      (module Base.Comparator.S with
         type comparator_witness = 'cmp and
         type t = 'k
      )

    type ('key, 'value, 'id) t = {
      cmp: ('key, 'id) Base.Comparator.t;
      mutable size: int;
      mutable data: ('key, 'value) Avltree.t
    }

    let make (type key) (type identity)
      : id:(key, identity) comparator -> (key, 'value, identity) t
      = fun ~id ->
        let module M = (val id) in
        { cmp = M.comparator
        ; size = 0
        ; data = Avltree.empty
        }

    let from_array (type key) (type identity)
      : (key * 'value) array
      -> id:(key, identity) comparator
      -> (key, 'value, identity) t
      = fun arr ~id ->
        let module M = (val id) in
        let tree = ref Avltree.empty in
        let size = ref 0 in
        Base.Array.iter arr ~f:(fun (key, data) ->
          let added = ref false in
          tree := Avltree.add !tree
            ~compare:M.comparator.compare ~added ~replace:true ~key ~data;
          if !added then incr size;
        );
        { cmp = M.comparator
        ; size = Base.Array.length arr
        ; data = !tree
        }

    let to_array : ('key, 'value, 'id) t -> ('key * 'value) array
      = fun { data; size; _ } ->
        let q = Base.Queue.create ~capacity:size () in
        Avltree.iter data
          ~f:(fun ~key ~data -> Base.Queue.enqueue q (key, data));
        Base.Queue.to_array q

    let keys_to_array : ('key, 'value, 'id) t -> 'key array
      = fun t -> t
        |> to_array
        |> Base.Array.map ~f:(fun (k, _) -> k)

    let to_list : ('key, 'value, 'id) t -> ('key * 'value) list
      = fun map -> map |> to_array |> Base.Array.to_list

    let copy : ('key, 'value, 'id) t -> ('key, 'value, 'id) t
      = fun { cmp; size; data } ->
        let new_table = ref Avltree.empty in
        let added = ref false in
        Avltree.iter data ~f:(fun ~key ~data ->
          new_table := Avltree.add !new_table
            ~compare:cmp.compare ~added ~replace:false ~key ~data
        );
        { cmp; size; data = !new_table }

    let is_empty : ('key, 'value, 'id) t -> bool
      = fun { data; _ } -> Avltree.is_empty data

    let for_each : f:('key -> 'value -> unit) -> ('key, 'value, 'id) t -> unit
      = fun ~f { data; _ }
      -> Avltree.iter data ~f:(fun ~key ~data -> f key data)

    let has : ('key, 'value, 'a) t -> key:'key -> bool
      = fun { cmp; data; _ } ~key -> Avltree.mem data ~compare:cmp.compare key

    let add : ('key, 'value, 'id) t -> key:'key -> value:'value -> unit
      = fun ({ cmp; data; size } as map) ~key ~value ->
        let added = ref false in
        let new_table =
          Avltree.add data ~compare:cmp.compare ~added ~replace:true ~key ~data:value
        in
        map.data <- new_table;
        if !added then map.size <- size + 1

    let get : ('key, 'value, 'a) t -> key:'key -> 'value option
      = fun { data; cmp; _ } ~key -> Avltree.find data ~compare:cmp.compare key

    let remove (type key) (type identity)
      : (key, 'value, identity) t
      -> key:key
      -> unit
      = fun ({ data; cmp; size } as map) ~key ->
        let removed = ref false in
        let new_tree = Avltree.remove data ~removed ~compare:cmp.compare key in
        map.data <- new_tree;
        if !removed then map.size <- size - 1

    let size : ('key, 'value, 'a) t -> int
      = fun { size; _ } -> size

    let minimum : ('key, 'value, 'a) t -> ('key * 'value) option
      = fun { data; _ } -> Avltree.first data

    let maximum : ('key, 'value, 'a) t -> ('key * 'value) option
      = fun { data; _ } -> Avltree.last data

  end

  module Int = struct
    type 'a t = (int, 'a, Base.Int.comparator_witness) Impl.t

    let make : unit -> 'a t
      = fun () -> Impl.make ~id:(module Base.Int)

    let from_array : (int * 'a) array -> 'a t
      = fun arr -> Impl.from_array arr ~id:(module Base.Int)

    let get : 'a t -> key:int -> 'a option
      = fun map ~key -> Impl.get map ~key

    let get_exn : 'a t -> key:int -> 'a
      = fun map ~key -> match Impl.get map ~key with
        | None -> failwith "TODO"
        | Some value -> value

    let has : 'a t -> key:int -> bool
      = Impl.has

    (* XXX signature differs from other Placemat *)
    let set : 'a t -> key:int -> value:'a -> unit
      = Impl.add

    let remove : 'a t -> key:int -> unit
      = Impl.remove

    let to_list : 'a t -> (int * 'a) list
      = Impl.to_list

    let to_array : 'a t -> (int * 'a) array
      = Impl.to_array

    let keys_to_array : 'a t -> int array
      = Impl.keys_to_array

    let minimum : 'a t -> (int * 'a) option
      = Impl.minimum

    let maximum : 'a t -> (int * 'a) option
      = Impl.maximum

    let%test _ = ([| 0, 1; 1, 2; 0, 3 |]
      |> from_array
      |> get ~key:0) = Some 3

  end

  include Impl
end

(** Note: Belt's MutableSet is implemented as an AVL tree. This is why we use
 * MutableMap (of unit) rather than Base's HashSet. *)
module MutableSet = struct

  module Impl = struct
    type ('value, 'id) t = ('value, unit, 'id) MutableMap.t

    type ('k, 'cmp) comparator =
      (module Base.Comparator.S with
         type comparator_witness = 'cmp and
         type t = 'k
      )

    let make : id:('value, 'identity) comparator -> ('value, 'identity) t
      = MutableMap.make

    let from_array
      : 'value array -> id:('value, 'identity) comparator -> ('value, 'id) t
      = fun arr ~id -> arr
        |> Tablecloth.Array.map ~f:(fun k -> k, ())
        |> MutableMap.from_array ~id

    let to_array : ('value, 'id) t -> 'value array
      = fun t -> t
        |> MutableMap.to_array
        |> Tablecloth.Array.map ~f:(fun (k, ()) -> k)

    let to_list : ('value, 'id) t -> 'value list
      = fun set -> set
        |> MutableMap.to_array
        |> Tablecloth.Array.map ~f:(fun (k, ()) -> k)
        |> Base.Array.to_list

    let copy : ('value, 'id) t -> ('value, 'id) t
      = MutableMap.copy

    let is_empty : ('value, 'id) t -> bool
      = MutableMap.is_empty

    let remove : ('value, 'id) t -> 'value -> unit
      = fun set value -> MutableMap.remove set ~key:value

    let for_each : ('value, 'id) t -> f:('value -> unit) -> unit
      = fun set ~f -> set
        |> MutableMap.for_each ~f:(fun k _ -> f k)

    let has : ('value, 'a) t -> value:'value -> bool
      = fun t ~value -> MutableMap.has t ~key:value

    let add : ('value, 'id) t -> value:'value -> unit
      = fun set ~value -> MutableMap.add set ~key:value ~value:()

    let size : ('value, 'id) t -> int
      = MutableMap.size

    let minimum : ('value, 'id) t -> 'value option
      = fun set -> set
        |> MutableMap.minimum
        |> Tablecloth.Option.map ~f:(fun (k, ()) -> k)

    let maximum : ('value, 'id) t -> 'value option
      = fun set -> set
        |> MutableMap.maximum
        |> Tablecloth.Option.map ~f:(fun (k, ()) -> k)
  end

  module Int = struct
    type t = (int, Base.Int.comparator_witness) Impl.t

    let copy : t -> t
      = Impl.copy

    let is_empty : t -> bool
      = Impl.is_empty

    let minimum : t -> int option
      = Impl.minimum

    let maximum : t -> int option
      = Impl.maximum

    let remove : t -> int -> unit
      = Impl.remove

    let add : t -> value:int -> unit
      = Impl.add

    let make : unit -> t
      = fun () -> Impl.make ~id:(module Base.Int)

    let to_list : t -> int list
      = Impl.to_list

    let for_each : t -> f:(int -> unit) -> unit
      = Impl.for_each

    let from_array : int array -> t
      = Impl.from_array ~id:(module Base.Int)

        (*
    let merge_many : t -> int array -> unit
      = fun set ints ->
        let _ = Base.Array.for_all ints
          ~f:(fun i -> Impl.add set i; true)
        in ()
        *)

    let to_array : t -> int array
      = Impl.to_array

    let has : t -> value:int -> bool
      = Impl.has

    (*
    let subset : t -> t -> bool
      = Impl.subset
      *)

    let size : t -> int
      = Impl.size

    (*
    let cmp : t -> t -> int
      = Impl.cmp
      *)
      (*
      = fun s1 s2 ->
        let len1, len2 = size s1, size s2 in
        if len1 = len2 then
          let arr1, arr2 = to_array s1, to_array s2 in
          Base.Array.compare (failwith "TODO") arr1 arr2
        else if len1 < len2 then -1 else 1
        *)
  end

  include Impl

end

module MutableStack = struct
  type 'a t = 'a Core_kernel.Stack.t

  let for_each : 'a t -> ('a -> unit) -> unit
    = fun stack f ->
      let _ = Core_kernel.Stack.for_all stack ~f:(fun a -> f a; true) in ()

  let make : unit -> 'a t
    = fun () -> Core_kernel.Stack.of_list []

  let push : 'a t -> 'a -> unit
    = Core_kernel.Stack.push

  let top : 'a t -> 'a option
    = Core_kernel.Stack.top

  let pop : 'a t -> 'a option
    = Core_kernel.Stack.pop

  let size : 'a t -> int
    = Core_kernel.Stack.length

  let is_empty : 'a t -> bool
    = Core_kernel.Stack.is_empty

  let to_array : 'a t -> 'a array
    = Core_kernel.Stack.to_array

  let%test _ = (
    let stack = make () in
    push stack 1;
    push stack 2;
    push stack 3;
    to_array stack
  ) = [| 3; 2; 1 |]
end

module MutableQueue = struct
  type 'a t = 'a Core_kernel.Queue.t

  let dequeue : 'a t -> 'a option
    = Core_kernel.Queue.dequeue

  let make : unit -> 'a t
    = fun () -> Core_kernel.Queue.of_list []

  let enqueue : 'a t -> 'a -> unit
    = Core_kernel.Queue.enqueue

  let to_array : 'a t -> 'a array
    = Core_kernel.Queue.to_array

  let from_array : 'a array -> 'a t
    = Core_kernel.Queue.of_array
end

module IntDict = struct
  type 'a t = 'a Core_kernel.Int.Map.t

  let remove : 'v t -> int -> 'v t
    = Core_kernel.Int.Map.remove

  let has : 'v t -> int -> bool
    = fun map k -> match Core_kernel.Int.Map.find map k with
      | None -> false
      | Some _ -> true

  let size : 'v t -> int
    = Core_kernel.Int.Map.length

  let for_each
    : f:(int -> 'a -> unit) -> 'a t -> unit
    = fun ~f int_map ->
      let _ = Core_kernel.Int.Map.for_alli int_map
        ~f:(fun ~key ~data -> f key data; true)
      in ()

  let find_first_by
    : f:(int -> 'v -> bool) -> 'v t -> (int * 'v) option
    = fun ~f int_map -> int_map
      |> Core_kernel.Int.Map.to_sequence
      |> Base.Sequence.find ~f:(fun (k, v) -> f k v)

  let from_array : (int * 'v) array -> 'v t
    = fun arr -> arr
      |> Base.Array.to_list
      |> Core_kernel.Int.Map.of_alist_reduce
        ~f:Base.Fn.const

  let to_array : 'v t -> (int * 'v) array
    = fun map -> map
      |> Core_kernel.Int.Map.to_alist
      |> Base.Array.of_list

  let map_with_key : 'v t -> (int -> 'v -> 'v2) -> 'v2 t
    = fun map f -> map
      |> Core_kernel.Int.Map.mapi ~f:(fun ~key ~data -> f key data)

  let cmp : 'v t -> 'v t -> ('v -> 'v -> int) -> int
    = fun m1 m2 f -> Core_kernel.Int.Map.compare f m1 m2

  let values_to_array dict = dict
    |> to_array
    |> Tablecloth.Array.map ~f:(fun (_, v) -> v)
end

module StrDict = struct
  type 'a t = 'a Core_kernel.String.Map.t

  let remove : 'v t -> string -> 'v t
    = Core_kernel.String.Map.remove

  let remove_many : 'v t -> string array -> 'v t
    = fun set keys -> Base.Array.fold_right
      keys
      ~init:set
      ~f:(fun key set' -> Core_kernel.String.Map.remove set' key)

  let has : 'v t -> string -> bool
    = fun map k -> match Core_kernel.String.Map.find map k with
      | None -> false
      | Some _ -> true

  let size : 'v t -> int
    = Core_kernel.String.Map.length

  let for_each : f:(string -> 'v -> unit) -> 'v t -> unit
    = fun ~f str_map ->
      let _ = Core_kernel.String.Map.for_alli str_map
        ~f:(fun ~key ~data -> f key data; true)
      in ()

  let from_array : (string * 'v) array -> 'v t
    = fun arr -> arr
      |> Base.Array.to_list
      |> Core_kernel.String.Map.of_alist_reduce
        ~f:Base.Fn.const

  let to_array : 'v t -> (string * 'v) array
    = fun map -> map
      |> Core_kernel.String.Map.to_alist
      |> Base.Array.of_list
end

module IntSet = struct
  type t = Core_kernel.Int.Set.t

  let eq : t -> t -> bool
    = Core_kernel.Int.Set.equal

  let size : t -> int
    = Core_kernel.Int.Set.length

  let cmp : t -> t -> int
    = Core_kernel.Int.Set.compare

  let intersect : t -> t -> t
    = Core_kernel.Int.Set.inter

  let from_array : int array -> t
    = Core_kernel.Int.Set.of_array

  let to_array : t -> int array
    = Core_kernel.Int.Set.to_array

  let for_each
    : f:(int -> unit) -> t -> unit
    = fun ~f set ->
      let _ = Core_kernel.Int.Set.for_all set ~f:(fun str -> f str; true) in ()
end

module Set = struct

  type ('value, 'id) id = ('value, 'id) Core_kernel.Set.comparator

  type ('value, 'id) t =
    { cmp: ('value, 'id) Core_kernel.Comparator.t
    ; data: ('value, 'id) Core_kernel.Set.t
    }

  let to_array : ('value, 'id) t -> 'value array
    = fun { data; _ } -> Core_kernel.Set.to_array data

  let from_array (type value) (type identity)
    : value array -> id:(value, identity) id -> (value, identity) t
    = fun arr ~id ->
      let module M = (val id) in
      { cmp = M.comparator
      ; data = Core_kernel.Set.of_array id arr
      }

  let union : ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
    = fun { data = data1; cmp } { data = data2; _ } ->
      { cmp
      ; data = Core_kernel.Set.union data1 data2
      }

  let cmp : ('value, 'id) t -> ('value, 'id) t -> int
    = fun { data = s1; cmp } { data = s2; _ } ->
      let len1, len2 = Core_kernel.Set.(length s1, length s2) in
      if len1 = len2 then
        let arr1, arr2 = Core_kernel.Set.(to_array s1, to_array s2) in
        Base.Array.compare cmp.compare arr1 arr2
      else if len1 < len2 then -1 else 1

  let eq : ('value, 'id) t -> ('value, 'id) t -> bool
    = fun { data = s1; _ } { data = s2; _ } -> Core_kernel.Set.equal s1 s2

  let for_each
    : f:('value -> unit) -> ('value, 'id) t -> unit
    = fun ~f { data = set; _ } ->
      let _ = Core_kernel.Set.for_all set ~f:(fun v -> f v; true) in ()
end

module Result = struct
  type ('err, 'ok) t = ('ok, 'err) Base.Result.t
end

module Bytes = struct
 type t = Core_kernel.Bytes.t
end

module Json = struct
  type t =
    | String of string
    | Array of t array

  let array : t array -> t
    = fun arr -> Array arr

  let string : string -> t
    = fun str -> String str
end

module Sha256 = struct
  let hash_str : string -> Sha256.t
    = Sha256.string

  let to_hex : Sha256.t -> string
    = Sha256.to_hex

  let hash : Bytes.t -> string
    = fun bytes -> bytes
      |> Core_kernel.Bytes.to_string
      |> hash_str
      |> to_hex
end

module Cbor = struct
  let rec of_json : Json.t -> CBOR.Simple.t
    = function
      | String str -> `Text str
      | Array arr -> `Array (arr
        |> Base.Array.map ~f:of_json
        |> Base.Array.to_list
      )

  let rec to_json : CBOR.Simple.t -> Json.t option
    = function
      | `Text str -> Some (String str)
      | `Array arr -> arr
        |> Base.List.map ~f:to_json
        |> Base.Option.all
        |> Base.Option.map ~f:(fun lst -> Json.Array (Base.Array.of_list lst))
      | _ -> None

  let encode : Json.t -> Bytes.t
    = fun json -> json
      |> of_json
      |> CBOR.Simple.encode
      |> Core_kernel.Bytes.of_string

  let decode : Bytes.t -> Json.t option
    = fun buf -> buf
      |> Core_kernel.Bytes.to_string
      |> CBOR.Simple.decode
      |> to_json
end

module String = struct
  let concat_array : ?sep:string -> string array -> string
    = Core_kernel.String.concat_array

  (** raises [Invalid_argument] *)
  let get : string -> int -> char
    = Core_kernel.String.get
end

module Re = struct
  type t = Re2.t

  let of_string : string -> t
    = Re2.of_string

  (* XXX error behavior *)
  let replace : re:t -> replacement:string -> string -> string
    = fun ~re ~replacement str -> Re2.rewrite_exn re ~template:replacement str
end

module Lex : sig
  type token =
    { name : string
    ; start : int (* inclusive *)
    ; finish : int (* exclusive *)
    }

  type token_name = string
  type regex = string
  type lexer = (regex * token_name) list

  type position = int

  type lex_error =
    { start_pos : position
    ; end_pos : position
    ; message : string
    }

  type lexbuf =
    { buf : string
    ; mutable pos : position
    }

  exception LexError of lex_error

  val string_of_tokens : token array -> string
  val lex : lexer -> string -> (lex_error, token array) Tablecloth.Result.t
end = struct
  type token =
    { name : string
    ; start : int (* inclusive *)
    ; finish : int (* exclusive *)
    }

  type token_name = string
  type regex = string
  type lexer = (regex * token_name) list

  type position = int

  type lex_error =
    { start_pos : position
    ; end_pos : position
    ; message : string
    }

  type lexbuf =
    { buf : string
    ; mutable pos : position
    }

  exception LexError of lex_error

  let string_of_tokens : token array -> string
    = fun toks -> toks
                  |> Tablecloth.Array.map ~f:(fun { name; _ } -> name)
                  |> Base.String.concat_array ~sep:" "

  (** raises: [LexError] *)
  let get_next_tok_exn : string IntDict.t -> Str.regexp -> lexbuf -> token
    = fun tok_names re { buf; pos } ->

      let input = Tablecloth.String.slice buf
        ~from:pos
        ~to_:(Tablecloth.String.length buf)
      in
      let matches = Str.string_match re input pos in

      if matches
      then
        let match_end = Str.match_end () in

        let possible_match = IntDict.find_first_by tok_names
          ~f:(fun i _re ->
            try
              let _ = Str.matched_group i input in true
            with
              Not_found -> false
          )
        in

        match possible_match with
          | Some (_match_num, name)
          -> { name; start = pos; finish = match_end }
          | None -> raise (LexError
            { start_pos = pos
            ; end_pos = match_end
            ; message =
              "Failed to find matching group even though a match was reported"
            })
      else
        raise
          (LexError
             { start_pos = pos
             ; end_pos = pos
             ; message = "Failed lex, \nlexbuf: " ^ buf
             })
  ;;

  (** raises: [LexError] *)
  let tokenize : lexer -> string -> token array
    = fun lexer input ->

      let result = MutableQueue.make () in
      let lexbuf = { buf = input; pos = 0 } in
      let mut_tok_names = MutableMap.Int.make () in

      let re_str = lexer
        |> List.map_with_index ~f:(fun i (re_str, tok_name) ->
          MutableMap.Int.set mut_tok_names ~key:i ~value:tok_name;
          "(" ^ re_str ^ ")")
        |> Tablecloth.String.join ~sep:"|"
      in

      let tok_names = mut_tok_names
        |> MutableMap.Int.to_array
        |> IntDict.from_array
      in
      let re = Str.regexp re_str in

      while lexbuf.pos < Tablecloth.String.length lexbuf.buf do
        let tok = get_next_tok_exn tok_names re lexbuf in
        let { start; finish; _ } = tok in
        assert (start = lexbuf.pos);
        lexbuf.pos <- finish;
        ignore (MutableQueue.enqueue result tok)
      done;

      MutableQueue.to_array result

  let lex : lexer -> string -> (lex_error, token array) Tablecloth.Result.t
    = fun lexer input ->
      try
        Ok (tokenize lexer input)
      with
        LexError err -> Error err

end
