module Lexer = AbstractSyntax_Lexer
module Parser = AbstractSyntax_Parser
module ParseErrors = AbstractSyntax_ParseErrors
include AbstractSyntax_Types

let%test_module "AbstractSyntax_Parser" = (module struct
  open Core_kernel

  let expect_parse parser str lang =
    assert (eq (parser Lexer.read (Lexing.from_string str)) lang)
  ;;


  let%test_unit _ =
    let tm = SortAp ("tm", [||]) in
    let tm_v = FixedValence ([], tm) in
    let integer = SortAp ("integer", [||]) in
    let integer_v = FixedValence ([], integer) in
    expect_parse
      Parser.language_def
      {|
import {integer} from "builtins"

tm :=
  | add(tm(); tm())
  | lit(integer())
      |}
      { imports =
        [ { imported_symbols = [ "integer", "integer" ]
          ; location = "builtins"
          }
        ]
      ; sort_defs = SortDefs (String.Map.of_alist_exn
        [ "tm", SortDef
          ( []
          , [ OperatorDef ("add", Arity ([], [tm_v; tm_v]))
            ; OperatorDef ("lit", Arity ([], [integer_v]))
            ]
          )
        ])
      }
end);;
