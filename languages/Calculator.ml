open Base
open Constructive_real
open Lvca_syntax

type term = (OptRange.t, Primitive.t) NonBinding.term

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (ParseUtil.CComment)
  open Parsers

  (* type t = ConstructiveReal.t *)

  let lit =
    integer_lit >>|| fun ~pos str ->
    let tm = NonBinding.Operator
      ( pos
      , "lit"
      , [ [ Primitive (pos, Primitive.PrimInteger (Z.of_string str)) ] ]
      )
    in
    tm, pos

  let t : term Parsers.t =
    fix (fun t ->
        let atom = attach_pos (lit <|> parens t) in
        let plus = char '+' in
        let f (l, rng1) (r, rng2) =
          let rng = OptRange.union rng1 rng2 in
          NonBinding.Operator (rng, "add", [ [ l ]; [ r ] ]), rng
        in
        atom
        >>= fun init -> many (plus *> atom) >>| fun lst -> List.fold lst ~init ~f |> fst)
    <?> "parser"
end

let interpret : term -> (ConstructiveReal.t, term * string) Result.t
  = fun tm -> match tm with
    | NonBinding.Operator (_, "lit", [[ Primitive (_, Primitive.PrimInteger i) ]])
    -> Ok (ConstructiveReal.of_bigint i)
    | _ -> failwith "TODO"
