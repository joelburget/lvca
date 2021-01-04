module Map = Base.Map

(* TODO: factor out commonalities, etc *)

module Signal = struct
  type 'value signal = 'value Note.S.t * (?step:Note.Step.t -> 'value -> unit)
  type key = int
  type 'value t =
    { mutable pool: 'value signal Lvca_util.Int.Map.t
    ; mutable pool_key: key
    }

  let create () = { pool = Lvca_util.Int.Map.empty; pool_key = 0 }

  let add t signal =
    let ret = t.pool_key in
    t.pool <- Map.set t.pool ~key:t.pool_key ~data:signal;
    t.pool_key <- t.pool_key + 1;
    ret

  let remove t i = t.pool <- Map.remove t.pool i; ()
  let find t i = Map.find t.pool i
  let find_exn t i = Map.find_exn t.pool i
end

(*
module RList = struct
  type 'value entry = 'value ReactiveData.RList.t * 'value ReactiveData.RList.handle
  type key = int
  type 'value t =
    { mutable pool: 'value entry Lvca_util.Int.Map.t
    ; mutable pool_key: key
    }

  let create () = { pool = Lvca_util.Int.Map.empty; pool_key = 0 }

  let add t entry =
    let ret = t.pool_key in
    t.pool <- Map.set t.pool ~key:t.pool_key ~data:entry;
    t.pool_key <- t.pool_key + 1;
    ret

  let remove t i = t.pool <- Map.remove t.pool i; ()
  let find t i = Map.find t.pool i
  let find_exn t i = Map.find_exn t.pool i
end
*)
