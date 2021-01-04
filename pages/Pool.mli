module Signal : sig
  type 'value signal = 'value Note.S.t * (?step:Note.Step.t -> 'value -> unit)
  type 'value t
  type key

  val create : unit -> 'value t
  val add : 'value t -> 'value signal -> key
  val remove : _ t -> key -> unit
  val find : 'value t -> key -> 'value signal option
  val find_exn : 'value t -> key -> 'value signal
end

(*
module RList : sig
  type 'value entry = 'value ReactiveData.RList.t * 'value ReactiveData.RList.handle
  type 'value t
  type key

  val create : unit -> 'value t
  val add : 'value t -> 'value entry -> key
  val remove : _ t -> key -> unit
  val find : 'value t -> key -> 'value entry option
  val find_exn : 'value t -> key -> 'value entry
end
*)
