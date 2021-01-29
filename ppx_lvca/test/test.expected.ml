open Lvca_syntax
let test_term =
  Nominal.Operator
    ((Some (let open Range in { start = 0; finish = 9 })), "foo",
      [Nominal.Scope
         ([Pattern.Var
             ((Some ((let open Range in { start = 4; finish = 5 }))), "x")],
           (Nominal.Var
              ((Some ((let open Range in { start = 7; finish = 8 }))), "x")))])
let test_pattern =
  Pattern.Operator
    ((Some (let open Range in { start = 0; finish = 6 })), "foo",
      [Pattern.Var
         ((Some ((let open Range in { start = 4; finish = 5 }))), "x")])
let test_language =
  [("foo",
     (AbstractSyntax.SortDef
        ([],
          [OperatorDef
             ("foo",
               [AbstractSyntax.Valence
                  ([],
                    (Sort.Name
                       ((Some
                           ((let open Range in { start = 12; finish = 19 }))),
                         "integer")))])])))]
