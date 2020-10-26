type 'value signal = 'value React.S.t * (?step:React.step -> 'value -> unit)
type 'value t
type key

val create : unit -> 'value t
val add : 'value t -> 'value signal -> key
val remove : _ t -> key -> unit
val find : 'value t -> key -> 'value signal option
val find_exn : 'value t -> key -> 'value signal
