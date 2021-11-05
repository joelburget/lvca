include [%lvca.abstract_syntax_module "empty :="]

type t = Empty.t

let pp _ppf = function (_ : t) -> .
let parse = Lvca_parsing.fail "(empty type)"
