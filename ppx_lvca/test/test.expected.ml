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
    {
      externals = [];
      sort_defs =
        [("foo",
           (AbstractSyntax.SortDef.SortDef
              ([],
                [AbstractSyntax.OperatorDef.OperatorDef
                   ("foo",
                     [AbstractSyntax.Valence.Valence
                        ([],
                          (Sort.Name
                             ((Some
                                 ((let open Range in
                                     { start = 12; finish = 19 }))),
                               "integer")))])])))]
    }

module Lang (Integer : LanguageObject.AllTermS) =
  struct
    module Foo =
      struct
        type 'info t =
          | Foo of 'info * 'info Integer.t 
          | Bar of 'info * (('info, Lvca_util.Void.t) Pattern.t * string *
          'info t) 
        module Plain =
          struct
            type t =
              | Foo of Integer.Plain.t 
              | Bar of ((unit, Lvca_util.Void.t) Pattern.t * string * t) 
          end
        let rec to_plain =
          function
          | Foo (_, x1) -> Plain.Foo (Integer.to_plain x1)
          | Bar (_, (x1, x2, x3)) ->
              Plain.Bar ((Pattern.to_plain x1), x2, (to_plain x3))
        let rec of_plain =
          function
          | Plain.Foo x1 -> Foo ((), (Integer.of_plain x1))
          | Plain.Bar (x1, x2, x3) ->
              Bar ((), ((Pattern.of_plain x1), x2, (of_plain x3)))
        let rec equal ~info_eq  t1 t2 =
          match (t1, t2) with
          | (Foo (x0, x1), Foo (y0, y1)) ->
              (info_eq x0 y0) && (Integer.equal ~info_eq x1 y1)
          | (Bar (x0, (x1, x2, x3)), Bar (y0, (y1, y2, y3))) ->
              (info_eq x0 y0) &&
                ((Pattern.equal info_eq Lvca_util.Void.(=) x1 y1) &&
                   ((String.(=) x2 y2) && (equal ~info_eq x3 y3)))
          | (_, _) -> false
        let info = function | Foo (x0, _) -> x0 | Bar (x0, (_, _, _)) -> x0
        let rec map_info ~f  =
          function
          | Foo (x0, x1) -> Foo ((f x0), (Integer.map_info ~f x1))
          | Bar (x0, (x1, x2, x3)) ->
              Bar ((f x0), ((Pattern.map_info ~f x1), x2, (map_info ~f x3)))
      end
  end
