type hash = string
type tag = string
type content = Lvca_syntax.Nominal.Term.t
type return_code = Sqlite3.Rc.t

module Db : sig
  type t

  val open_ : string -> t
  val create_tables : t -> return_code
  val create_tables' : t -> return_code option
end

module Hash : sig
  type t = hash

  val lookup : Db.t -> t -> (content option, return_code) Result.t
  val insert : Db.t -> content -> return_code
end

module Tag : sig
  type t = tag

  val dereference_tag : Db.t -> t -> (hash option, return_code) Result.t
  val lookup_tag : Db.t -> t -> (content option, return_code) Result.t
  val set : Db.t -> hash -> t -> return_code

  (* TODO: delete? *)
end
