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
  let open AbstractSyntax in
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
module Lang(Integer:LanguageObject.AllTermS) =
  struct
    let language =
      let open AbstractSyntax in
        {
          externals =
            [("integer",
               (AbstractSyntax.Kind.Kind
                  ((Some ((let open Range in { start = 11; finish = 121 }))),
                    1)))];
          sort_defs =
            [("foo",
               (AbstractSyntax.SortDef.SortDef
                  ([],
                    [AbstractSyntax.OperatorDef.OperatorDef
                       ("Foo",
                         [AbstractSyntax.Valence.Valence
                            ([],
                              (Sort.Name
                                 ((Some
                                     ((let open Range in
                                         { start = 136; finish = 143 }))),
                                   "integer")))]);
                    AbstractSyntax.OperatorDef.OperatorDef
                      ("Bar",
                        [AbstractSyntax.Valence.Valence
                           ([AbstractSyntax.SortSlot.SortPattern
                               {
                                 pattern_sort =
                                   (Sort.Name
                                      ((Some
                                          ((let open Range in
                                              { start = 153; finish = 156 }))),
                                        "foo"));
                                 var_sort =
                                   (Sort.Name
                                      ((Some
                                          ((let open Range in
                                              { start = 157; finish = 160 }))),
                                        "foo"))
                               };
                            AbstractSyntax.SortSlot.SortBinding
                              (Sort.Name
                                 ((Some
                                     ((let open Range in
                                         { start = 163; finish = 166 }))),
                                   "foo"))],
                             (Sort.Name
                                ((Some
                                    ((let open Range in
                                        { start = 168; finish = 171 }))),
                                  "foo")))])])));
            ("nat",
              (AbstractSyntax.SortDef.SortDef
                 ([],
                   [AbstractSyntax.OperatorDef.OperatorDef ("Z", []);
                   AbstractSyntax.OperatorDef.OperatorDef
                     ("S",
                       [AbstractSyntax.Valence.Valence
                          ([],
                            (Sort.Name
                               ((Some
                                   ((let open Range in
                                       { start = 189; finish = 192 }))),
                                 "nat")))])])));
            ("list",
              (AbstractSyntax.SortDef.SortDef
                 ([("a", None)],
                   [AbstractSyntax.OperatorDef.OperatorDef ("Nil", []);
                   AbstractSyntax.OperatorDef.OperatorDef
                     ("Cons",
                       [AbstractSyntax.Valence.Valence
                          ([],
                            (Sort.Name
                               ((Some
                                   ((let open Range in
                                       { start = 218; finish = 219 }))), "a")));
                       AbstractSyntax.Valence.Valence
                         ([],
                           (Sort.Ap
                              ((Some
                                  ((let open Range in
                                      { start = 221; finish = 228 }))),
                                "list",
                                [Sort.Name
                                   ((Some
                                       ((let open Range in
                                           { start = 226; finish = 227 }))),
                                     "a")])))])])))]
        }
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
              | Bar of (Lvca_util.Void.t Pattern.Plain.t * string * t) 
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
                ((Pattern.equal ~info_eq ~prim_eq:Lvca_util.Void.(=) x1 y1)
                   && ((Base.String.(=) x2 y2) && (equal ~info_eq x3 y3)))
          | (_, _) -> false
        let info = function | Foo (x0, _) -> x0 | Bar (x0, (_, _, _)) -> x0
        let rec map_info ~f  =
          function
          | Foo (x0, x1) -> Foo ((f x0), (Integer.map_info ~f x1))
          | Bar (x0, (x1, x2, x3)) ->
              Bar ((f x0), ((Pattern.map_info ~f x1), x2, (map_info ~f x3)))
      end
    module Nat =
      struct
        type 'info t =
          | Z of 'info 
          | S of 'info * 'info t 
        module Plain = struct type t =
                                | Z 
                                | S of t  end
        let rec to_plain =
          function | Z _ -> Plain.Z | S (_, x1) -> Plain.S (to_plain x1)
        let rec of_plain =
          function | Plain.Z -> Z () | Plain.S x1 -> S ((), (of_plain x1))
        let rec equal ~info_eq  t1 t2 =
          match (t1, t2) with
          | (Z x0, Z y0) -> info_eq x0 y0
          | (S (x0, x1), S (y0, y1)) ->
              (info_eq x0 y0) && (equal ~info_eq x1 y1)
          | (_, _) -> false
        let info = function | Z x0 -> x0 | S (x0, _) -> x0
        let rec map_info ~f  =
          function
          | Z x0 -> Z (f x0)
          | S (x0, x1) -> S ((f x0), (map_info ~f x1))
      end
    module List(A:LanguageObject.AllTermS) =
      struct
        type 'info t =
          | Nil of 'info 
          | Cons of 'info * 'info A.t * 'info t 
        module Plain = struct type t =
                                | Nil 
                                | Cons of A.Plain.t * t  end
        let rec to_plain =
          function
          | Nil _ -> Plain.Nil
          | Cons (_, x1, x2) -> Plain.Cons ((A.to_plain x1), (to_plain x2))
        let rec of_plain =
          function
          | Plain.Nil -> Nil ()
          | Plain.Cons (x1, x2) -> Cons ((), (A.of_plain x1), (of_plain x2))
        let rec equal ~info_eq  t1 t2 =
          match (t1, t2) with
          | (Nil x0, Nil y0) -> info_eq x0 y0
          | (Cons (x0, x1, x2), Cons (y0, y1, y2)) ->
              (info_eq x0 y0) &&
                ((A.equal ~info_eq x1 y1) && (equal ~info_eq x2 y2))
          | (_, _) -> false
        let info = function | Nil x0 -> x0 | Cons (x0, _, _) -> x0
        let rec map_info ~f  =
          function
          | Nil x0 -> Nil (f x0)
          | Cons (x0, x1, x2) ->
              Cons ((f x0), (A.map_info ~f x1), (map_info ~f x2))
      end
  end
