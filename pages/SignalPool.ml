module Map = Base.Map

module IntMap = struct
  type 'a t = (int, 'a, Base.Int.comparator_witness) Map.t
  let empty = Map.empty (module Base.Int)
end

type 'value signal = 'value React.S.t * (?step:React.step -> 'value -> unit)
type key = int
type 'value t =
  { mutable pool: 'value signal IntMap.t
  ; mutable pool_key: key
  }

let create () = { pool = IntMap.empty; pool_key = 0 }

let add t signal =
  let ret = t.pool_key in
  t.pool <- Map.set t.pool ~key:t.pool_key ~data:signal;
  t.pool_key <- t.pool_key + 1;
  ret

let remove t i = t.pool <- Map.remove t.pool i; ()
let find t i = Map.find t.pool i
let find_exn t i = Map.find_exn t.pool i
