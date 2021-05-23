open ParseUtil_intf

type +'a t = latest_pos:int -> 'a ParseResult.t Angstrom.t

val whitespace : unit Angstrom.t
val whitespace1 : unit Angstrom.t
val parse_string_pos : 'a t -> string -> ('a ParseResult.t, string) Base.Result.t
val parse_string : 'a t -> string -> ('a, string) Base.Result.t

module ParseResult : ParseResult_s
module Junkless : Junkless_s
module Parsers : Parsers_s
