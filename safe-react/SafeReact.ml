type 'a event = 'a React.event
type 'a signal = 'a React.signal
type step = React.step

module E = React.E

module S : sig
  type 'a t = 'a signal
  val const : 'a -> 'a signal
  val create : eq:('a -> 'a -> bool) -> 'a -> 'a signal * (?step:step -> 'a -> unit)
  val value : 'a signal -> 'a
  val equal : eq:('a -> 'a -> bool) -> 'a signal -> 'a signal -> bool
  val hold : eq:('a -> 'a -> bool) -> 'a -> 'a event -> 'a signal
  val app : eq:('b -> 'b -> bool) -> ('a -> 'b) signal -> 'a signal -> 'b signal
  val map : eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal
  val filter : eq:('a -> 'a -> bool) -> ('a -> bool) -> 'a -> 'a signal -> 'a signal
  val fmap : eq:('b -> 'b -> bool) -> ('a -> 'b option) -> 'b -> 'a signal -> 'b signal
  val diff : ('a -> 'a -> 'b) -> 'a signal -> 'b event
  val changes : 'a signal -> 'a event
  val merge : eq:('a -> 'a -> bool) -> ('a -> 'b -> 'a) -> 'a -> 'b signal list -> 'a signal
  val switch : eq:('a -> 'a -> bool) -> 'a signal signal -> 'a signal
  val l1 : eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal
  val l2 : eq:('c -> 'c -> bool) -> ('a -> 'b -> 'c) -> 'a signal -> 'b signal -> 'c signal
  module Pair : sig
    val pair : eq:(('a * 'b) -> ('a * 'b) -> bool) -> 'a signal -> 'b signal -> ('a * 'b) signal
    val fst : eq:('a -> 'a -> bool) -> ('a * 'b) signal -> 'a signal
    val snd : eq:('a -> 'a -> bool) -> ('b * 'a) signal -> 'a signal
  end
end = struct
  type 'a t = 'a signal
  let const = React.S.const
  let create ~eq i = React.S.create ~eq i
  let value = React.S.value
  let equal ~eq s1 s2 = React.S.equal ~eq s1 s2
  let hold ~eq i e = React.S.hold ~eq i e
  let app ~eq sf s = React.S.app ~eq sf s
  let map ~eq f s = React.S.map ~eq f s
  let filter ~eq f i s = React.S.filter ~eq f i s
  let fmap ~eq fm i s = React.S.fmap ~eq fm i s
  let diff = React.S.diff
  let changes = React.S.changes
  let merge ~eq f a sl = React.S.merge ~eq f a sl
  let switch ~eq s = React.S.switch ~eq s
  let l1 ~eq f a1 = React.S.l1 ~eq f a1
  let l2 ~eq f a1 a2 = React.S.l2 ~eq f a1 a2

  module Pair = struct
    let pair ~eq sa sb = React.S.Pair.pair ~eq sa sb
    let fst ~eq s = React.S.Pair.fst ~eq s
    let snd ~eq s = React.S.Pair.snd ~eq s
  end
end
