open Lvca_syntax
let test_nominal =
  Nominal.Term.Operator
    ((Some (let open Range in { start = 0; finish = 9 })), "foo",
      [Nominal.Scope.Scope
         ([Pattern.Var
             ((Some ((let open Range in { start = 4; finish = 5 }))), "x")],
           (Nominal.Term.Var
              ((Some ((let open Range in { start = 7; finish = 8 }))), "x")))])
let test_nonbinding =
  NonBinding.Operator
    ((Some (let open Range in { start = 0; finish = 11 })), "foo",
      [NonBinding.Operator
         ((Some ((let open Range in { start = 4; finish = 10 }))), "bar",
           [NonBinding.Primitive
              ((Some ((let open Range in { start = 8; finish = 9 }))),
                (Lvca_syntax.Primitive.PrimInteger (Z.of_string "1")))])])
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
