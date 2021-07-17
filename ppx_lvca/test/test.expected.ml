open Lvca_syntax
let test_nominal =
  Lvca_syntax.Nominal.Term.Operator
    ((let open Lvca_provenance.Commented in
        {
          range =
            (Some
               (let open Lvca_provenance.Range in { start = 0; finish = 9 }));
          comment = None
        }), "foo",
      [Lvca_syntax.Nominal.Scope.Scope
         ([Lvca_syntax.Pattern.Var
             (((let open Lvca_provenance.Commented in
                  {
                    range =
                      (Some
                         (let open Lvca_provenance.Range in
                            { start = 4; finish = 5 }));
                    comment = None
                  })), "x")],
           (Lvca_syntax.Nominal.Term.Var
              (((let open Lvca_provenance.Commented in
                   {
                     range =
                       (Some
                          (let open Lvca_provenance.Range in
                             { start = 7; finish = 8 }));
                     comment = None
                   })), "x")))])
let test_nonbinding =
  Lvca_syntax.Nonbinding.Operator
    ((let open Lvca_provenance.Commented in
        {
          range =
            (Some
               (let open Lvca_provenance.Range in { start = 0; finish = 11 }));
          comment = None
        }), "foo",
      [Lvca_syntax.Nonbinding.Operator
         (((let open Lvca_provenance.Commented in
              {
                range =
                  (Some
                     (let open Lvca_provenance.Range in
                        { start = 4; finish = 10 }));
                comment = None
              })), "bar",
           [Lvca_syntax.Nonbinding.Primitive
              (((let open Lvca_provenance.Commented in
                   {
                     range =
                       (Some
                          (let open Lvca_provenance.Range in
                             { start = 8; finish = 9 }));
                     comment = None
                   })),
                (Lvca_syntax.Primitive_impl.All_plain.Integer
                   (Z.of_string "1")))])])
let test_pattern =
  Lvca_syntax.Pattern.Operator
    ((let open Lvca_provenance.Commented in
        {
          range =
            (Some
               (let open Lvca_provenance.Range in { start = 0; finish = 6 }));
          comment = None
        }), "foo",
      [Lvca_syntax.Pattern.Var
         (((let open Lvca_provenance.Commented in
              {
                range =
                  (Some
                     (let open Lvca_provenance.Range in
                        { start = 4; finish = 5 }));
                comment = None
              })), "x")])
let test_language =
  let open Lvca_syntax.Abstract_syntax in
    {
      externals = [];
      sort_defs =
        [("foo",
           (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
              ([],
                [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                   ("foo",
                     [Lvca_syntax.Abstract_syntax.Valence.Valence
                        ([],
                          (Lvca_syntax.Sort.Name
                             (((let open Lvca_provenance.Commented in
                                  {
                                    range =
                                      (Some
                                         (let open Lvca_provenance.Range in
                                            { start = 12; finish = 19 }));
                                    comment = None
                                  })), "integer")))])])))]
    }
module Lang =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type 'info mut_a =
              | Mut_a of 'info * 'info mut_b 
            and 'info mut_b =
              | Mut_b of 'info * 'info mut_a 
            and 'info ifz =
              | Ifz of 'info * 'info ifz * ('info Lvca_syntax.Single_var.t *
              'info ifz) * 'info ifz 
              | Ifz_var of 'info * string 
            and 'info term =
              | Operator of 'info * 'info Lvca_syntax.Nominal.Term.t 
            and ('info, 'a, 'b) pair_plus =
              | PairPlus of 'info * 'a * 'b * 'info foo 
            and 'info foo =
              | Foo of 'info * 'info Lvca_syntax.Nominal.Term.t 
              | Bar of 'info * ('info Pattern.t * 'info
              Lvca_syntax.Single_var.t * 'info foo) 
              | Foo_var of 'info * string 
            and ('info, 'a, 'b) pair =
              | Pair of 'info * 'a * 'b 
            and 'info nonempty =
              | Nonempty of 'info * 'info Primitive.String.t * 'info
              Lvca_syntax.Nominal.Term.t 
            and 'info nat =
              | Z of 'info 
              | S of 'info * 'info nat 
          end
        module Plain =
          struct
            type mut_a =
              | Mut_a of mut_b 
            and mut_b =
              | Mut_b of mut_a 
            and ifz =
              | Ifz of ifz * (Lvca_syntax.Single_var.Plain.t * ifz) * ifz 
              | Ifz_var of string 
            and term =
              | Operator of Lvca_syntax.Nominal.Term.Plain.t 
            and ('a, 'b) pair_plus =
              | PairPlus of 'a * 'b * foo 
            and foo =
              | Foo of Lvca_syntax.Nominal.Term.Plain.t 
              | Bar of (Pattern.Plain.t * Lvca_syntax.Single_var.Plain.t *
              foo) 
              | Foo_var of string 
            and ('a, 'b) pair =
              | Pair of 'a * 'b 
            and nonempty =
              | Nonempty of Primitive.String.Plain.t *
              Lvca_syntax.Nominal.Term.Plain.t 
            and nat =
              | Z 
              | S of nat 
          end
        module Info =
          struct
            let nat = function | Types.Z x0 -> x0 | Types.S (x0, _) -> x0
            let nonempty = function | Types.Nonempty (x0, _, _) -> x0
            let pair _a _b = function | Types.Pair (x0, _, _) -> x0
            let foo =
              function
              | Types.Foo (x0, _) -> x0
              | Types.Bar (x0, (_, _, _)) -> x0
              | Types.Foo_var (info, _) -> info
            let pair_plus _a _b =
              function | Types.PairPlus (x0, _, _, _) -> x0
            let term = function | Types.Operator (x0, _) -> x0
            let ifz =
              function
              | Types.Ifz (x0, _, (_, _), _) -> x0
              | Types.Ifz_var (info, _) -> info
            let mut_a = function | Types.Mut_a (x0, _) -> x0
            and mut_b = function | Types.Mut_b (x0, _) -> x0
          end
        module To_plain =
          struct
            let rec nat =
              function
              | Types.Z _ -> Plain.Z
              | Types.S (_, x1) -> Plain.S (nat x1)
            let nonempty =
              function
              | Types.Nonempty (_, x1, x2) ->
                  Plain.Nonempty
                    ((Primitive.String.to_plain x1),
                      (Lvca_syntax.Nominal.Term.to_plain x2))
            let pair a b =
              function
              | Types.Pair (_, x1, x2) -> Plain.Pair ((a x1), (b x2))
            let rec foo =
              function
              | Types.Foo (_, x1) ->
                  Plain.Foo (Lvca_syntax.Nominal.Term.to_plain x1)
              | Types.Bar (_, (x1, x2, x3)) ->
                  Plain.Bar
                    ((Lvca_syntax.Pattern.to_plain x1),
                      (let open Lvca_syntax.Single_var.Plain in
                         { name = (x2.name) }), (foo x3))
              | Types.Foo_var (_, name) -> Plain.Foo_var name
            let pair_plus a b =
              function
              | Types.PairPlus (_, x1, x2, x3) ->
                  Plain.PairPlus ((a x1), (b x2), (foo x3))
            let term =
              function
              | Types.Operator (_, x1) ->
                  Plain.Operator (Lvca_syntax.Nominal.Term.to_plain x1)
            let rec ifz =
              function
              | Types.Ifz (_, x1, (x2, x3), x4) ->
                  Plain.Ifz
                    ((ifz x1),
                      ((let open Lvca_syntax.Single_var.Plain in
                          { name = (x2.name) }), (ifz x3)), (ifz x4))
              | Types.Ifz_var (_, name) -> Plain.Ifz_var name
            let rec mut_a =
              function | Types.Mut_a (_, x1) -> Plain.Mut_a (mut_b x1)
            and mut_b =
              function | Types.Mut_b (_, x1) -> Plain.Mut_b (mut_a x1)
          end
        module Of_plain =
          struct
            let rec nat =
              function
              | Plain.Z -> Types.Z ()
              | Plain.S x1 -> Types.S ((), (nat x1))
            let nonempty =
              function
              | Plain.Nonempty (x1, x2) ->
                  Types.Nonempty
                    ((), (Primitive.String.of_plain x1),
                      (Lvca_syntax.Nominal.Term.of_plain x2))
            let pair a b =
              function
              | Plain.Pair (x1, x2) -> Types.Pair ((), (a x1), (b x2))
            let rec foo =
              function
              | Plain.Foo x1 ->
                  Types.Foo ((), (Lvca_syntax.Nominal.Term.of_plain x1))
              | Plain.Bar (x1, x2, x3) ->
                  Types.Bar
                    ((),
                      ((Lvca_syntax.Pattern.of_plain x1),
                        (let open Lvca_syntax.Single_var in
                           { info = (); name = (x2.name) }), (foo x3)))
              | Plain.Foo_var name -> Types.Foo_var ((), name)
            let pair_plus a b =
              function
              | Plain.PairPlus (x1, x2, x3) ->
                  Types.PairPlus ((), (a x1), (b x2), (foo x3))
            let term =
              function
              | Plain.Operator x1 ->
                  Types.Operator ((), (Lvca_syntax.Nominal.Term.of_plain x1))
            let rec ifz =
              function
              | Plain.Ifz (x1, (x2, x3), x4) ->
                  Types.Ifz
                    ((), (ifz x1),
                      ((let open Lvca_syntax.Single_var in
                          { info = (); name = (x2.name) }), (ifz x3)),
                      (ifz x4))
              | Plain.Ifz_var name -> Types.Ifz_var ((), name)
            let rec mut_a =
              function | Plain.Mut_a x1 -> Types.Mut_a ((), (mut_b x1))
            and mut_b =
              function | Plain.Mut_b x1 -> Types.Mut_b ((), (mut_a x1))
          end
        module Map_info =
          struct
            let rec nat ~f  =
              function
              | Types.Z x0 -> Types.Z (f x0)
              | Types.S (x0, x1) -> Types.S ((f x0), (nat ~f x1))
            let nonempty ~f  =
              function
              | Types.Nonempty (x0, x1, x2) ->
                  Types.Nonempty
                    ((f x0), (Primitive.String.map_info ~f x1),
                      (Lvca_syntax.Nominal.Term.map_info ~f x2))
            let pair a b ~f  =
              function
              | Types.Pair (x0, x1, x2) ->
                  Types.Pair ((f x0), (a ~f x1), (b ~f x2))
            let rec foo ~f  =
              function
              | Types.Foo (x0, x1) ->
                  Types.Foo
                    ((f x0), (Lvca_syntax.Nominal.Term.map_info ~f x1))
              | Types.Bar (x0, (x1, x2, x3)) ->
                  Types.Bar
                    ((f x0),
                      ((Lvca_syntax.Pattern.map_info ~f x1),
                        (let open Lvca_syntax.Single_var in
                           { info = (f x2.info); name = (x2.name) }),
                        (foo ~f x3)))
              | Types.Foo_var (info, name) -> Types.Foo_var ((f info), name)
            let pair_plus a b ~f  =
              function
              | Types.PairPlus (x0, x1, x2, x3) ->
                  Types.PairPlus ((f x0), (a ~f x1), (b ~f x2), (foo ~f x3))
            let term ~f  =
              function
              | Types.Operator (x0, x1) ->
                  Types.Operator
                    ((f x0), (Lvca_syntax.Nominal.Term.map_info ~f x1))
            let rec ifz ~f  =
              function
              | Types.Ifz (x0, x1, (x2, x3), x4) ->
                  Types.Ifz
                    ((f x0), (ifz ~f x1),
                      ((let open Lvca_syntax.Single_var in
                          { info = (f x2.info); name = (x2.name) }),
                        (ifz ~f x3)), (ifz ~f x4))
              | Types.Ifz_var (info, name) -> Types.Ifz_var ((f info), name)
            let rec mut_a ~f  =
              function
              | Types.Mut_a (x0, x1) -> Types.Mut_a ((f x0), (mut_b ~f x1))
            and mut_b ~f  =
              function
              | Types.Mut_b (x0, x1) -> Types.Mut_b ((f x0), (mut_a ~f x1))
          end
        module To_nominal =
          struct
            let rec nat =
              function
              | Types.Z x0 -> Lvca_syntax.Nominal.Term.Operator (x0, "Z", [])
              | Types.S (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "S",
                      [Lvca_syntax.Nominal.Scope.Scope ([], (nat x1))])
            let nonempty =
              function
              | Types.Nonempty (x0, x1, x2) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Nonempty",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (Primitive.String.to_nominal x1));
                      Lvca_syntax.Nominal.Scope.Scope ([], x2)])
            let pair a b =
              function
              | Types.Pair (x0, x1, x2) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Pair",
                      [Lvca_syntax.Nominal.Scope.Scope ([], (a x1));
                      Lvca_syntax.Nominal.Scope.Scope ([], (b x2))])
            let rec foo =
              function
              | Types.Foo (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Foo", [Lvca_syntax.Nominal.Scope.Scope ([], x1)])
              | Types.Bar (x0, (x1, x2, x3)) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Bar",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([x1;
                          Lvca_syntax.Pattern.Var ((x2.info), (x2.name))],
                           (foo x3))])
              | Foo_var (info, name) ->
                  Lvca_syntax.Nominal.Term.Var (info, name)
            let pair_plus a b =
              function
              | Types.PairPlus (x0, x1, x2, x3) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "PairPlus",
                      [Lvca_syntax.Nominal.Scope.Scope ([], (a x1));
                      Lvca_syntax.Nominal.Scope.Scope ([], (b x2));
                      Lvca_syntax.Nominal.Scope.Scope ([], (foo x3))])
            let term =
              function
              | Types.Operator (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Operator",
                      [Lvca_syntax.Nominal.Scope.Scope ([], x1)])
            let rec ifz =
              function
              | Types.Ifz (x0, x1, (x2, x3), x4) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Ifz",
                      [Lvca_syntax.Nominal.Scope.Scope ([], (ifz x1));
                      Lvca_syntax.Nominal.Scope.Scope
                        ([Lvca_syntax.Pattern.Var ((x2.info), (x2.name))],
                          (ifz x3));
                      Lvca_syntax.Nominal.Scope.Scope ([], (ifz x4))])
              | Ifz_var (info, name) ->
                  Lvca_syntax.Nominal.Term.Var (info, name)
            let rec mut_a =
              function
              | Types.Mut_a (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Mut_a",
                      [Lvca_syntax.Nominal.Scope.Scope ([], (mut_b x1))])
            and mut_b =
              function
              | Types.Mut_b (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Mut_b",
                      [Lvca_syntax.Nominal.Scope.Scope ([], (mut_a x1))])
          end
        module Of_nominal =
          struct
            let rec nat =
              function
              | Lvca_syntax.Nominal.Term.Operator (x0, "Z", []) ->
                  Ok (Types.Z x0)
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "S", (Lvca_syntax.Nominal.Scope.Scope ([], x1))::[])
                  ->
                  (match nat x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.S (x0, x1)))
              | tm -> Error tm
            let nonempty =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Nonempty", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::(Lvca_syntax.Nominal.Scope.Scope ([], x2))::[])
                  ->
                  (match Primitive.String.of_nominal x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.Nonempty (x0, x1, x2)))
              | tm -> Error tm
            let pair a b =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Pair", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::(Lvca_syntax.Nominal.Scope.Scope ([], x2))::[])
                  ->
                  (match a x1 with
                   | Error msg -> Error msg
                   | Ok x1 ->
                       (match b x2 with
                        | Error msg -> Error msg
                        | Ok x2 -> Ok (Types.Pair (x0, x1, x2))))
              | tm -> Error tm
            let rec foo =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Foo", (Lvca_syntax.Nominal.Scope.Scope ([], x1))::[])
                  -> Ok (Types.Foo (x0, x1))
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Bar", (Lvca_syntax.Nominal.Scope.Scope
                   (x1::(Lvca_syntax.Pattern.Var (x2, x3))::[], x4))::[])
                  ->
                  (match foo x4 with
                   | Error msg -> Error msg
                   | Ok x4 ->
                       Ok
                         (Types.Bar
                            (x0,
                              (x1,
                                (let open Lvca_syntax.Single_var in
                                   { info = x2; name = x3 }), x4))))
              | Lvca_syntax.Nominal.Term.Var (info, name) ->
                  Ok (Foo_var (info, name))
              | tm -> Error tm
            let pair_plus a b =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "PairPlus", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::(Lvca_syntax.Nominal.Scope.Scope
                   ([], x2))::(Lvca_syntax.Nominal.Scope.Scope ([], x3))::[])
                  ->
                  (match a x1 with
                   | Error msg -> Error msg
                   | Ok x1 ->
                       (match b x2 with
                        | Error msg -> Error msg
                        | Ok x2 ->
                            (match foo x3 with
                             | Error msg -> Error msg
                             | Ok x3 -> Ok (Types.PairPlus (x0, x1, x2, x3)))))
              | tm -> Error tm
            let term =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Operator", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  -> Ok (Types.Operator (x0, x1))
              | tm -> Error tm
            let rec ifz =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Ifz", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::(Lvca_syntax.Nominal.Scope.Scope
                   ((Lvca_syntax.Pattern.Var (x2, x3))::[], x4))::(Lvca_syntax.Nominal.Scope.Scope
                   ([], x5))::[])
                  ->
                  (match ifz x1 with
                   | Error msg -> Error msg
                   | Ok x1 ->
                       (match ifz x4 with
                        | Error msg -> Error msg
                        | Ok x4 ->
                            (match ifz x5 with
                             | Error msg -> Error msg
                             | Ok x5 ->
                                 Ok
                                   (Types.Ifz
                                      (x0, x1,
                                        ((let open Lvca_syntax.Single_var in
                                            { info = x2; name = x3 }), x4),
                                        x5)))))
              | Lvca_syntax.Nominal.Term.Var (info, name) ->
                  Ok (Ifz_var (info, name))
              | tm -> Error tm
            let rec mut_a =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Mut_a", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match mut_b x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.Mut_a (x0, x1)))
              | tm -> Error tm
            and mut_b =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Mut_b", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match mut_a x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.Mut_b (x0, x1)))
              | tm -> Error tm
          end
      end
    module Types = Wrapper.Types
    module Plain = Wrapper.Plain
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals =
            [("integer",
               (Lvca_syntax.Abstract_syntax.Kind.Kind
                  (((let open Lvca_provenance.Commented in
                       {
                         range =
                           (Some
                              (let open Lvca_provenance.Range in
                                 { start = 11; finish = 12 }));
                         comment = None
                       })), 1)));
            ("string",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 (((let open Lvca_provenance.Commented in
                      {
                        range =
                          (Some
                             (let open Lvca_provenance.Range in
                                { start = 22; finish = 23 }));
                        comment = (Some " module Primitive.String")
                      })), 1)));
            ("maybe",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 (((let open Lvca_provenance.Commented in
                      {
                        range =
                          (Some
                             (let open Lvca_provenance.Range in
                                { start = 59; finish = 65 }));
                        comment = None
                      })), 2)));
            ("list",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 (((let open Lvca_provenance.Commented in
                      {
                        range =
                          (Some
                             (let open Lvca_provenance.Range in
                                { start = 73; finish = 79 }));
                        comment = None
                      })), 2)))];
          sort_defs =
            [("foo",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ("Foo",
                         [Lvca_syntax.Abstract_syntax.Valence.Valence
                            ([],
                              (Lvca_syntax.Sort.Name
                                 (((let open Lvca_provenance.Commented in
                                      {
                                        range =
                                          (Some
                                             (let open Lvca_provenance.Range in
                                                { start = 96; finish = 103 }));
                                        comment = None
                                      })), "integer")))]);
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("Bar",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([Lvca_syntax.Abstract_syntax.Sort_slot.Sort_pattern
                               {
                                 pattern_sort =
                                   (Lvca_syntax.Sort.Name
                                      (((let open Lvca_provenance.Commented in
                                           {
                                             range =
                                               (Some
                                                  (let open Lvca_provenance.Range in
                                                     {
                                                       start = 113;
                                                       finish = 116
                                                     }));
                                             comment = None
                                           })), "foo"));
                                 var_sort =
                                   (Lvca_syntax.Sort.Name
                                      (((let open Lvca_provenance.Commented in
                                           {
                                             range =
                                               (Some
                                                  (let open Lvca_provenance.Range in
                                                     {
                                                       start = 117;
                                                       finish = 120
                                                     }));
                                             comment = None
                                           })), "foo"))
                               };
                            Lvca_syntax.Abstract_syntax.Sort_slot.Sort_binding
                              (Lvca_syntax.Sort.Name
                                 (((let open Lvca_provenance.Commented in
                                      {
                                        range =
                                          (Some
                                             (let open Lvca_provenance.Range in
                                                { start = 123; finish = 126 }));
                                        comment = None
                                      })), "foo"))],
                             (Lvca_syntax.Sort.Name
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 128; finish = 131 }));
                                       comment = None
                                     })), "foo")))])])));
            ("nat",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("Z", []);
                   Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                     ("S",
                       [Lvca_syntax.Abstract_syntax.Valence.Valence
                          ([],
                            (Lvca_syntax.Sort.Name
                               (((let open Lvca_provenance.Commented in
                                    {
                                      range =
                                        (Some
                                           (let open Lvca_provenance.Range in
                                              { start = 149; finish = 152 }));
                                      comment = None
                                    })), "nat")))])])));
            ("pair",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None); ("b", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("Pair",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Name
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 172; finish = 173 }));
                                       comment = None
                                     })), "a")));
                        Lvca_syntax.Abstract_syntax.Valence.Valence
                          ([],
                            (Lvca_syntax.Sort.Name
                               (((let open Lvca_provenance.Commented in
                                    {
                                      range =
                                        (Some
                                           (let open Lvca_provenance.Range in
                                              { start = 175; finish = 176 }));
                                      comment = None
                                    })), "b")))])])));
            ("pair_plus",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None); ("b", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("PairPlus",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Name
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 204; finish = 205 }));
                                       comment = None
                                     })), "a")));
                        Lvca_syntax.Abstract_syntax.Valence.Valence
                          ([],
                            (Lvca_syntax.Sort.Name
                               (((let open Lvca_provenance.Commented in
                                    {
                                      range =
                                        (Some
                                           (let open Lvca_provenance.Range in
                                              { start = 207; finish = 208 }));
                                      comment = None
                                    })), "b")));
                        Lvca_syntax.Abstract_syntax.Valence.Valence
                          ([],
                            (Lvca_syntax.Sort.Name
                               (((let open Lvca_provenance.Commented in
                                    {
                                      range =
                                        (Some
                                           (let open Lvca_provenance.Range in
                                              { start = 210; finish = 213 }));
                                      comment = None
                                    })), "foo")))])])));
            ("nonempty",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("Nonempty",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Name
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 237; finish = 243 }));
                                       comment = None
                                     })), "string")));
                        Lvca_syntax.Abstract_syntax.Valence.Valence
                          ([],
                            (Lvca_syntax.Sort.Ap
                               (((let open Lvca_provenance.Commented in
                                    {
                                      range =
                                        (Some
                                           (let open Lvca_provenance.Range in
                                              { start = 245; finish = 249 }));
                                      comment = None
                                    })), "list",
                                 [Lvca_syntax.Sort.Name
                                    (((let open Lvca_provenance.Commented in
                                         {
                                           range =
                                             (Some
                                                (let open Lvca_provenance.Range in
                                                   {
                                                     start = 250;
                                                     finish = 256
                                                   }));
                                           comment = None
                                         })), "string")])))])])));
            ("term",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("Operator",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Ap
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 276; finish = 280 }));
                                       comment = None
                                     })), "list",
                                  [Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 281;
                                                      finish = 285
                                                    }));
                                            comment = None
                                          })), "term")])))])])));
            ("mut_a",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("Mut_a",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Name
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 303; finish = 308 }));
                                       comment = None
                                     })), "mut_b")))])])));
            ("mut_b",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("Mut_b",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Name
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 325; finish = 330 }));
                                       comment = None
                                     })), "mut_a")))])])));
            ("ifz",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("Ifz",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Name
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 343; finish = 346 }));
                                       comment = None
                                     })), "ifz")));
                        Lvca_syntax.Abstract_syntax.Valence.Valence
                          ([Lvca_syntax.Abstract_syntax.Sort_slot.Sort_binding
                              (Lvca_syntax.Sort.Name
                                 (((let open Lvca_provenance.Commented in
                                      {
                                        range =
                                          (Some
                                             (let open Lvca_provenance.Range in
                                                { start = 348; finish = 351 }));
                                        comment = None
                                      })), "ifz"))],
                            (Lvca_syntax.Sort.Name
                               (((let open Lvca_provenance.Commented in
                                    {
                                      range =
                                        (Some
                                           (let open Lvca_provenance.Range in
                                              { start = 353; finish = 356 }));
                                      comment = None
                                    })), "ifz")));
                        Lvca_syntax.Abstract_syntax.Valence.Valence
                          ([],
                            (Lvca_syntax.Sort.Name
                               (((let open Lvca_provenance.Commented in
                                    {
                                      range =
                                        (Some
                                           (let open Lvca_provenance.Range in
                                              { start = 358; finish = 361 }));
                                      comment = None
                                    })), "ifz")))])])))]
        }
    module Foo =
      struct
        type 'info t = 'info Wrapper.Types.foo =
          | Foo of 'info * 'info Lvca_syntax.Nominal.Term.t 
          | Bar of 'info * ('info Pattern.t * 'info Lvca_syntax.Single_var.t
          * 'info Wrapper.Types.foo) 
          | Foo_var of 'info * string 
        let info tm = Wrapper.Info.foo tm
        let to_plain tm = Wrapper.To_plain.foo tm
        let of_plain tm = Wrapper.Of_plain.foo tm
        let map_info ~f  tm = Wrapper.Map_info.foo ~f tm
        let to_nominal tm = Wrapper.To_nominal.foo tm
        let of_nominal tm = Wrapper.Of_nominal.foo tm
        module Plain =
          struct
            type t = Wrapper.Plain.foo =
              | Foo of Lvca_syntax.Nominal.Term.Plain.t 
              | Bar of (Pattern.Plain.t * Lvca_syntax.Single_var.Plain.t *
              Wrapper.Plain.foo) 
              | Foo_var of string 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module Nat =
      struct
        type 'info t = 'info Wrapper.Types.nat =
          | Z of 'info 
          | S of 'info * 'info Wrapper.Types.nat 
        let info tm = Wrapper.Info.nat tm
        let to_plain tm = Wrapper.To_plain.nat tm
        let of_plain tm = Wrapper.Of_plain.nat tm
        let map_info ~f  tm = Wrapper.Map_info.nat ~f tm
        let to_nominal tm = Wrapper.To_nominal.nat tm
        let of_nominal tm = Wrapper.Of_nominal.nat tm
        module Plain =
          struct
            type t = Wrapper.Plain.nat =
              | Z 
              | S of Wrapper.Plain.nat 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module Pair =
      struct
        type ('info, 'a, 'b) t = ('info, 'a, 'b) Wrapper.Types.pair =
          | Pair of 'info * 'a * 'b 
        let info tm =
          Wrapper.Info.pair Lvca_syntax.Nominal.Term.info
            Lvca_syntax.Nominal.Term.info tm
        let to_plain tm =
          Wrapper.To_plain.pair Lvca_syntax.Nominal.Term.to_plain
            Lvca_syntax.Nominal.Term.to_plain tm
        let of_plain tm =
          Wrapper.Of_plain.pair Lvca_syntax.Nominal.Term.of_plain
            Lvca_syntax.Nominal.Term.of_plain tm
        let map_info ~f  tm =
          Wrapper.Map_info.pair ~f Lvca_syntax.Nominal.Term.map_info
            Lvca_syntax.Nominal.Term.map_info tm
        let to_nominal tm = Wrapper.To_nominal.pair Base.Fn.id Base.Fn.id tm
        let of_nominal tm =
          Wrapper.Of_nominal.pair Base.Result.return Base.Result.return tm
        module Plain =
          struct
            type ('a, 'b) t = ('a, 'b) Wrapper.Plain.pair =
              | Pair of 'a * 'b 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module Pair_plus =
      struct
        type ('info, 'a, 'b) t = ('info, 'a, 'b) Wrapper.Types.pair_plus =
          | PairPlus of 'info * 'a * 'b * 'info Wrapper.Types.foo 
        let info tm =
          Wrapper.Info.pair_plus Lvca_syntax.Nominal.Term.info
            Lvca_syntax.Nominal.Term.info tm
        let to_plain tm =
          Wrapper.To_plain.pair_plus Lvca_syntax.Nominal.Term.to_plain
            Lvca_syntax.Nominal.Term.to_plain tm
        let of_plain tm =
          Wrapper.Of_plain.pair_plus Lvca_syntax.Nominal.Term.of_plain
            Lvca_syntax.Nominal.Term.of_plain tm
        let map_info ~f  tm =
          Wrapper.Map_info.pair_plus ~f Lvca_syntax.Nominal.Term.map_info
            Lvca_syntax.Nominal.Term.map_info tm
        let to_nominal tm =
          Wrapper.To_nominal.pair_plus Base.Fn.id Base.Fn.id tm
        let of_nominal tm =
          Wrapper.Of_nominal.pair_plus Base.Result.return Base.Result.return
            tm
        module Plain =
          struct
            type ('a, 'b) t = ('a, 'b) Wrapper.Plain.pair_plus =
              | PairPlus of 'a * 'b * Wrapper.Plain.foo 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module Nonempty =
      struct
        type 'info t = 'info Wrapper.Types.nonempty =
          | Nonempty of 'info * 'info Primitive.String.t * 'info
          Lvca_syntax.Nominal.Term.t 
        let info tm = Wrapper.Info.nonempty tm
        let to_plain tm = Wrapper.To_plain.nonempty tm
        let of_plain tm = Wrapper.Of_plain.nonempty tm
        let map_info ~f  tm = Wrapper.Map_info.nonempty ~f tm
        let to_nominal tm = Wrapper.To_nominal.nonempty tm
        let of_nominal tm = Wrapper.Of_nominal.nonempty tm
        module Plain =
          struct
            type t = Wrapper.Plain.nonempty =
              | Nonempty of Primitive.String.Plain.t *
              Lvca_syntax.Nominal.Term.Plain.t 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module Term =
      struct
        type 'info t = 'info Wrapper.Types.term =
          | Operator of 'info * 'info Lvca_syntax.Nominal.Term.t 
        let info tm = Wrapper.Info.term tm
        let to_plain tm = Wrapper.To_plain.term tm
        let of_plain tm = Wrapper.Of_plain.term tm
        let map_info ~f  tm = Wrapper.Map_info.term ~f tm
        let to_nominal tm = Wrapper.To_nominal.term tm
        let of_nominal tm = Wrapper.Of_nominal.term tm
        module Plain =
          struct
            type t = Wrapper.Plain.term =
              | Operator of Lvca_syntax.Nominal.Term.Plain.t 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module Mut_a =
      struct
        type 'info t = 'info Wrapper.Types.mut_a =
          | Mut_a of 'info * 'info Wrapper.Types.mut_b 
        let info tm = Wrapper.Info.mut_a tm
        let to_plain tm = Wrapper.To_plain.mut_a tm
        let of_plain tm = Wrapper.Of_plain.mut_a tm
        let map_info ~f  tm = Wrapper.Map_info.mut_a ~f tm
        let to_nominal tm = Wrapper.To_nominal.mut_a tm
        let of_nominal tm = Wrapper.Of_nominal.mut_a tm
        module Plain =
          struct
            type t = Wrapper.Plain.mut_a =
              | Mut_a of Wrapper.Plain.mut_b 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module Mut_b =
      struct
        type 'info t = 'info Wrapper.Types.mut_b =
          | Mut_b of 'info * 'info Wrapper.Types.mut_a 
        let info tm = Wrapper.Info.mut_b tm
        let to_plain tm = Wrapper.To_plain.mut_b tm
        let of_plain tm = Wrapper.Of_plain.mut_b tm
        let map_info ~f  tm = Wrapper.Map_info.mut_b ~f tm
        let to_nominal tm = Wrapper.To_nominal.mut_b tm
        let of_nominal tm = Wrapper.Of_nominal.mut_b tm
        module Plain =
          struct
            type t = Wrapper.Plain.mut_b =
              | Mut_b of Wrapper.Plain.mut_a 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module Ifz =
      struct
        type 'info t = 'info Wrapper.Types.ifz =
          | Ifz of 'info * 'info Wrapper.Types.ifz * ('info
          Lvca_syntax.Single_var.t * 'info Wrapper.Types.ifz) * 'info
          Wrapper.Types.ifz 
          | Ifz_var of 'info * string 
        let info tm = Wrapper.Info.ifz tm
        let to_plain tm = Wrapper.To_plain.ifz tm
        let of_plain tm = Wrapper.Of_plain.ifz tm
        let map_info ~f  tm = Wrapper.Map_info.ifz ~f tm
        let to_nominal tm = Wrapper.To_nominal.ifz tm
        let of_nominal tm = Wrapper.Of_nominal.ifz tm
        module Plain =
          struct
            type t = Wrapper.Plain.ifz =
              | Ifz of Wrapper.Plain.ifz * (Lvca_syntax.Single_var.Plain.t *
              Wrapper.Plain.ifz) * Wrapper.Plain.ifz 
              | Ifz_var of string 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
  end
module List_lang =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type 'info list_external =
              | List_external of 'info * ('info,
              'info Lvca_syntax.Nominal.Term.t) list 
            and 'info list_predefined =
              | List_predefined of 'info * ('info, 'info predefined) list 
            and 'info list_list_string_2 =
              | List_list_string_2 of 'info * ('info,
              'info Lvca_syntax.Nominal.Term.t) list_list_a 
            and 'info list_list_string_1 =
              | List_list_string_1 of 'info * ('info,
              ('info, 'info Lvca_syntax.Nominal.Term.t) list) list 
            and 'info list_list_predefined_2 =
              | List_list_predefined_2 of 'info * ('info, 'info predefined)
              list_list_a 
            and ('info, 'a) list_list_a =
              | List_list_a of 'info * ('info, ('info, 'a) list) list 
            and 'info list_list_predefined_1 =
              | List_list_predefined_1 of 'info * ('info,
              ('info, 'info predefined) list) list 
            and 'info predefined =
              | Predefined of 'info 
            and ('info, 'a) list =
              | Nil of 'info 
              | Cons of 'info * 'a * ('info, 'a) list 
          end
        module Plain =
          struct
            type list_external =
              | List_external of Lvca_syntax.Nominal.Term.Plain.t list 
            and list_predefined =
              | List_predefined of predefined list 
            and list_list_string_2 =
              | List_list_string_2 of Lvca_syntax.Nominal.Term.Plain.t
              list_list_a 
            and list_list_string_1 =
              | List_list_string_1 of Lvca_syntax.Nominal.Term.Plain.t list
              list 
            and list_list_predefined_2 =
              | List_list_predefined_2 of predefined list_list_a 
            and 'a list_list_a =
              | List_list_a of 'a list list 
            and list_list_predefined_1 =
              | List_list_predefined_1 of predefined list list 
            and predefined =
              | Predefined 
            and 'a list =
              | Nil 
              | Cons of 'a * 'a list 
          end
        module Info =
          struct
            let list _a =
              function | Types.Nil x0 -> x0 | Types.Cons (x0, _, _) -> x0
            let predefined = function | Types.Predefined x0 -> x0
            let list_list_predefined_1 =
              function | Types.List_list_predefined_1 (x0, _) -> x0
            let list_list_a _a = function | Types.List_list_a (x0, _) -> x0
            let list_list_predefined_2 =
              function | Types.List_list_predefined_2 (x0, _) -> x0
            let list_list_string_1 =
              function | Types.List_list_string_1 (x0, _) -> x0
            let list_list_string_2 =
              function | Types.List_list_string_2 (x0, _) -> x0
            let list_predefined =
              function | Types.List_predefined (x0, _) -> x0
            let list_external = function | Types.List_external (x0, _) -> x0
          end
        module To_plain =
          struct
            let rec list a =
              function
              | Types.Nil _ -> Plain.Nil
              | Types.Cons (_, x1, x2) -> Plain.Cons ((a x1), (list a x2))
            let predefined =
              function | Types.Predefined _ -> Plain.Predefined
            let list_list_predefined_1 =
              function
              | Types.List_list_predefined_1 (_, x1) ->
                  Plain.List_list_predefined_1 (list (list predefined) x1)
            let list_list_a a =
              function
              | Types.List_list_a (_, x1) ->
                  Plain.List_list_a (list (list a) x1)
            let list_list_predefined_2 =
              function
              | Types.List_list_predefined_2 (_, x1) ->
                  Plain.List_list_predefined_2 (list_list_a predefined x1)
            let list_list_string_1 =
              function
              | Types.List_list_string_1 (_, x1) ->
                  Plain.List_list_string_1
                    (list (list Lvca_syntax.Nominal.Term.to_plain) x1)
            let list_list_string_2 =
              function
              | Types.List_list_string_2 (_, x1) ->
                  Plain.List_list_string_2
                    (list_list_a Lvca_syntax.Nominal.Term.to_plain x1)
            let list_predefined =
              function
              | Types.List_predefined (_, x1) ->
                  Plain.List_predefined (list predefined x1)
            let list_external =
              function
              | Types.List_external (_, x1) ->
                  Plain.List_external
                    (list Lvca_syntax.Nominal.Term.to_plain x1)
          end
        module Of_plain =
          struct
            let rec list a =
              function
              | Plain.Nil -> Types.Nil ()
              | Plain.Cons (x1, x2) -> Types.Cons ((), (a x1), (list a x2))
            let predefined =
              function | Plain.Predefined -> Types.Predefined ()
            let list_list_predefined_1 =
              function
              | Plain.List_list_predefined_1 x1 ->
                  Types.List_list_predefined_1
                    ((), (list (list predefined) x1))
            let list_list_a a =
              function
              | Plain.List_list_a x1 ->
                  Types.List_list_a ((), (list (list a) x1))
            let list_list_predefined_2 =
              function
              | Plain.List_list_predefined_2 x1 ->
                  Types.List_list_predefined_2
                    ((), (list_list_a predefined x1))
            let list_list_string_1 =
              function
              | Plain.List_list_string_1 x1 ->
                  Types.List_list_string_1
                    ((), (list (list Lvca_syntax.Nominal.Term.of_plain) x1))
            let list_list_string_2 =
              function
              | Plain.List_list_string_2 x1 ->
                  Types.List_list_string_2
                    ((), (list_list_a Lvca_syntax.Nominal.Term.of_plain x1))
            let list_predefined =
              function
              | Plain.List_predefined x1 ->
                  Types.List_predefined ((), (list predefined x1))
            let list_external =
              function
              | Plain.List_external x1 ->
                  Types.List_external
                    ((), (list Lvca_syntax.Nominal.Term.of_plain x1))
          end
        module Map_info =
          struct
            let rec list a ~f  =
              function
              | Types.Nil x0 -> Types.Nil (f x0)
              | Types.Cons (x0, x1, x2) ->
                  Types.Cons ((f x0), (a ~f x1), (list a ~f x2))
            let predefined ~f  =
              function | Types.Predefined x0 -> Types.Predefined (f x0)
            let list_list_predefined_1 ~f  =
              function
              | Types.List_list_predefined_1 (x0, x1) ->
                  Types.List_list_predefined_1
                    ((f x0), (list (list predefined) ~f x1))
            let list_list_a a ~f  =
              function
              | Types.List_list_a (x0, x1) ->
                  Types.List_list_a ((f x0), (list (list a) ~f x1))
            let list_list_predefined_2 ~f  =
              function
              | Types.List_list_predefined_2 (x0, x1) ->
                  Types.List_list_predefined_2
                    ((f x0), (list_list_a predefined ~f x1))
            let list_list_string_1 ~f  =
              function
              | Types.List_list_string_1 (x0, x1) ->
                  Types.List_list_string_1
                    ((f x0),
                      (list (list Lvca_syntax.Nominal.Term.map_info) ~f x1))
            let list_list_string_2 ~f  =
              function
              | Types.List_list_string_2 (x0, x1) ->
                  Types.List_list_string_2
                    ((f x0),
                      (list_list_a Lvca_syntax.Nominal.Term.map_info ~f x1))
            let list_predefined ~f  =
              function
              | Types.List_predefined (x0, x1) ->
                  Types.List_predefined ((f x0), (list predefined ~f x1))
            let list_external ~f  =
              function
              | Types.List_external (x0, x1) ->
                  Types.List_external
                    ((f x0), (list Lvca_syntax.Nominal.Term.map_info ~f x1))
          end
        module To_nominal =
          struct
            let rec list a =
              function
              | Types.Nil x0 ->
                  Lvca_syntax.Nominal.Term.Operator (x0, "Nil", [])
              | Types.Cons (x0, x1, x2) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Cons",
                      [Lvca_syntax.Nominal.Scope.Scope ([], (a x1));
                      Lvca_syntax.Nominal.Scope.Scope ([], (list a x2))])
            let predefined =
              function
              | Types.Predefined x0 ->
                  Lvca_syntax.Nominal.Term.Operator (x0, "Predefined", [])
            let list_list_predefined_1 =
              function
              | Types.List_list_predefined_1 (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "List_list_predefined_1",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (list (list predefined) x1))])
            let list_list_a a =
              function
              | Types.List_list_a (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "List_list_a",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (list (list a) x1))])
            let list_list_predefined_2 =
              function
              | Types.List_list_predefined_2 (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "List_list_predefined_2",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (list_list_a predefined x1))])
            let list_list_string_1 =
              function
              | Types.List_list_string_1 (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "List_list_string_1",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (list (list (fun x -> x)) x1))])
            let list_list_string_2 =
              function
              | Types.List_list_string_2 (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "List_list_string_2",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (list_list_a (fun x -> x) x1))])
            let list_predefined =
              function
              | Types.List_predefined (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "List_predefined",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (list predefined x1))])
            let list_external =
              function
              | Types.List_external (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "List_external",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (list (fun x -> x) x1))])
          end
        module Of_nominal =
          struct
            let rec list a =
              function
              | Lvca_syntax.Nominal.Term.Operator (x0, "Nil", []) ->
                  Ok (Types.Nil x0)
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Cons", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::(Lvca_syntax.Nominal.Scope.Scope ([], x2))::[])
                  ->
                  (match a x1 with
                   | Error msg -> Error msg
                   | Ok x1 ->
                       (match list a x2 with
                        | Error msg -> Error msg
                        | Ok x2 -> Ok (Types.Cons (x0, x1, x2))))
              | tm -> Error tm
            let predefined =
              function
              | Lvca_syntax.Nominal.Term.Operator (x0, "Predefined", []) ->
                  Ok (Types.Predefined x0)
              | tm -> Error tm
            let list_list_predefined_1 =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_predefined_1",
                   (Lvca_syntax.Nominal.Scope.Scope ([], x1))::[])
                  ->
                  (match list (list predefined) x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.List_list_predefined_1 (x0, x1)))
              | tm -> Error tm
            let list_list_a a =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_a", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list (list a) x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.List_list_a (x0, x1)))
              | tm -> Error tm
            let list_list_predefined_2 =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_predefined_2",
                   (Lvca_syntax.Nominal.Scope.Scope ([], x1))::[])
                  ->
                  (match list_list_a predefined x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.List_list_predefined_2 (x0, x1)))
              | tm -> Error tm
            let list_list_string_1 =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_string_1", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list (list (fun x -> Ok x)) x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.List_list_string_1 (x0, x1)))
              | tm -> Error tm
            let list_list_string_2 =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_string_2", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list_list_a (fun x -> Ok x) x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.List_list_string_2 (x0, x1)))
              | tm -> Error tm
            let list_predefined =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_predefined", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list predefined x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.List_predefined (x0, x1)))
              | tm -> Error tm
            let list_external =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_external", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list (fun x -> Ok x) x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.List_external (x0, x1)))
              | tm -> Error tm
          end
      end
    module Types = Wrapper.Types
    module Plain = Wrapper.Plain
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals =
            [("string",
               (Lvca_syntax.Abstract_syntax.Kind.Kind
                  (((let open Lvca_provenance.Commented in
                       {
                         range =
                           (Some
                              (let open Lvca_provenance.Range in
                                 { start = 10; finish = 11 }));
                         comment = None
                       })), 1)))];
          sort_defs =
            [("predefined",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ("Predefined", [])])));
            ("list",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("Nil", []);
                   Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                     ("Cons",
                       [Lvca_syntax.Abstract_syntax.Valence.Valence
                          ([],
                            (Lvca_syntax.Sort.Name
                               (((let open Lvca_provenance.Commented in
                                    {
                                      range =
                                        (Some
                                           (let open Lvca_provenance.Range in
                                              { start = 63; finish = 64 }));
                                      comment = None
                                    })), "a")));
                       Lvca_syntax.Abstract_syntax.Valence.Valence
                         ([],
                           (Lvca_syntax.Sort.Ap
                              (((let open Lvca_provenance.Commented in
                                   {
                                     range =
                                       (Some
                                          (let open Lvca_provenance.Range in
                                             { start = 66; finish = 70 }));
                                     comment = None
                                   })), "list",
                                [Lvca_syntax.Sort.Name
                                   (((let open Lvca_provenance.Commented in
                                        {
                                          range =
                                            (Some
                                               (let open Lvca_provenance.Range in
                                                  { start = 71; finish = 72 }));
                                          comment = None
                                        })), "a")])))])])));
            ("list_external",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("List_external",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Ap
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 105; finish = 109 }));
                                       comment = None
                                     })), "list",
                                  [Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 110;
                                                      finish = 116
                                                    }));
                                            comment = None
                                          })), "string")])))])])));
            ("list_predefined",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("List_predefined",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Ap
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 153; finish = 157 }));
                                       comment = None
                                     })), "list",
                                  [Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 158;
                                                      finish = 168
                                                    }));
                                            comment = None
                                          })), "predefined")])))])])));
            ("list_list_a",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("List_list_a",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Ap
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 200; finish = 204 }));
                                       comment = None
                                     })), "list",
                                  [Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 206;
                                                      finish = 210
                                                    }));
                                            comment = None
                                          })), "list",
                                       [Lvca_syntax.Sort.Name
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 211;
                                                           finish = 212
                                                         }));
                                                 comment = None
                                               })), "a")])])))])])));
            ("list_list_string_1",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("List_list_string_1",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Ap
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 256; finish = 260 }));
                                       comment = None
                                     })), "list",
                                  [Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 262;
                                                      finish = 266
                                                    }));
                                            comment = None
                                          })), "list",
                                       [Lvca_syntax.Sort.Name
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 267;
                                                           finish = 273
                                                         }));
                                                 comment = None
                                               })), "string")])])))])])));
            ("list_list_string_2",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("List_list_string_2",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Ap
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 317; finish = 328 }));
                                       comment = None
                                     })), "list_list_a",
                                  [Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 329;
                                                      finish = 335
                                                    }));
                                            comment = None
                                          })), "string")])))])])));
            ("list_list_predefined_1",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("List_list_predefined_1",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Ap
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 386; finish = 390 }));
                                       comment = None
                                     })), "list",
                                  [Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 392;
                                                      finish = 396
                                                    }));
                                            comment = None
                                          })), "list",
                                       [Lvca_syntax.Sort.Name
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 397;
                                                           finish = 407
                                                         }));
                                                 comment = None
                                               })), "predefined")])])))])])));
            ("list_list_predefined_2",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ("List_list_predefined_2",
                        [Lvca_syntax.Abstract_syntax.Valence.Valence
                           ([],
                             (Lvca_syntax.Sort.Ap
                                (((let open Lvca_provenance.Commented in
                                     {
                                       range =
                                         (Some
                                            (let open Lvca_provenance.Range in
                                               { start = 459; finish = 470 }));
                                       comment = None
                                     })), "list_list_a",
                                  [Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 471;
                                                      finish = 481
                                                    }));
                                            comment = None
                                          })), "predefined")])))])])))]
        }
    module Predefined =
      struct
        type 'info t = 'info Wrapper.Types.predefined =
          | Predefined of 'info 
        let info tm = Wrapper.Info.predefined tm
        let to_plain tm = Wrapper.To_plain.predefined tm
        let of_plain tm = Wrapper.Of_plain.predefined tm
        let map_info ~f  tm = Wrapper.Map_info.predefined ~f tm
        let to_nominal tm = Wrapper.To_nominal.predefined tm
        let of_nominal tm = Wrapper.Of_nominal.predefined tm
        module Plain =
          struct
            type t = Wrapper.Plain.predefined =
              | Predefined 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module List =
      struct
        type ('info, 'a) t = ('info, 'a) Wrapper.Types.list =
          | Nil of 'info 
          | Cons of 'info * 'a * ('info, 'a) Wrapper.Types.list 
        let info tm = Wrapper.Info.list Lvca_syntax.Nominal.Term.info tm
        let to_plain tm =
          Wrapper.To_plain.list Lvca_syntax.Nominal.Term.to_plain tm
        let of_plain tm =
          Wrapper.Of_plain.list Lvca_syntax.Nominal.Term.of_plain tm
        let map_info ~f  tm =
          Wrapper.Map_info.list ~f Lvca_syntax.Nominal.Term.map_info tm
        let to_nominal tm = Wrapper.To_nominal.list Base.Fn.id tm
        let of_nominal tm = Wrapper.Of_nominal.list Base.Result.return tm
        module Plain =
          struct
            type 'a t = 'a Wrapper.Plain.list =
              | Nil 
              | Cons of 'a * 'a Wrapper.Plain.list 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module List_external =
      struct
        type 'info t = 'info Wrapper.Types.list_external =
          | List_external of 'info * ('info,
          'info Lvca_syntax.Nominal.Term.t) Wrapper.Types.list 
        let info tm = Wrapper.Info.list_external tm
        let to_plain tm = Wrapper.To_plain.list_external tm
        let of_plain tm = Wrapper.Of_plain.list_external tm
        let map_info ~f  tm = Wrapper.Map_info.list_external ~f tm
        let to_nominal tm = Wrapper.To_nominal.list_external tm
        let of_nominal tm = Wrapper.Of_nominal.list_external tm
        module Plain =
          struct
            type t = Wrapper.Plain.list_external =
              | List_external of Lvca_syntax.Nominal.Term.Plain.t
              Wrapper.Plain.list 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module List_predefined =
      struct
        type 'info t = 'info Wrapper.Types.list_predefined =
          | List_predefined of 'info * ('info,
          'info Wrapper.Types.predefined) Wrapper.Types.list 
        let info tm = Wrapper.Info.list_predefined tm
        let to_plain tm = Wrapper.To_plain.list_predefined tm
        let of_plain tm = Wrapper.Of_plain.list_predefined tm
        let map_info ~f  tm = Wrapper.Map_info.list_predefined ~f tm
        let to_nominal tm = Wrapper.To_nominal.list_predefined tm
        let of_nominal tm = Wrapper.Of_nominal.list_predefined tm
        module Plain =
          struct
            type t = Wrapper.Plain.list_predefined =
              | List_predefined of Wrapper.Plain.predefined
              Wrapper.Plain.list 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module List_list_a =
      struct
        type ('info, 'a) t = ('info, 'a) Wrapper.Types.list_list_a =
          | List_list_a of 'info * ('info, ('info, 'a) Wrapper.Types.list)
          Wrapper.Types.list 
        let info tm =
          Wrapper.Info.list_list_a Lvca_syntax.Nominal.Term.info tm
        let to_plain tm =
          Wrapper.To_plain.list_list_a Lvca_syntax.Nominal.Term.to_plain tm
        let of_plain tm =
          Wrapper.Of_plain.list_list_a Lvca_syntax.Nominal.Term.of_plain tm
        let map_info ~f  tm =
          Wrapper.Map_info.list_list_a ~f Lvca_syntax.Nominal.Term.map_info
            tm
        let to_nominal tm = Wrapper.To_nominal.list_list_a Base.Fn.id tm
        let of_nominal tm =
          Wrapper.Of_nominal.list_list_a Base.Result.return tm
        module Plain =
          struct
            type 'a t = 'a Wrapper.Plain.list_list_a =
              | List_list_a of 'a Wrapper.Plain.list Wrapper.Plain.list 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module List_list_string_1 =
      struct
        type 'info t = 'info Wrapper.Types.list_list_string_1 =
          | List_list_string_1 of 'info * ('info,
          ('info, 'info Lvca_syntax.Nominal.Term.t) Wrapper.Types.list)
          Wrapper.Types.list 
        let info tm = Wrapper.Info.list_list_string_1 tm
        let to_plain tm = Wrapper.To_plain.list_list_string_1 tm
        let of_plain tm = Wrapper.Of_plain.list_list_string_1 tm
        let map_info ~f  tm = Wrapper.Map_info.list_list_string_1 ~f tm
        let to_nominal tm = Wrapper.To_nominal.list_list_string_1 tm
        let of_nominal tm = Wrapper.Of_nominal.list_list_string_1 tm
        module Plain =
          struct
            type t = Wrapper.Plain.list_list_string_1 =
              | List_list_string_1 of Lvca_syntax.Nominal.Term.Plain.t
              Wrapper.Plain.list Wrapper.Plain.list 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module List_list_string_2 =
      struct
        type 'info t = 'info Wrapper.Types.list_list_string_2 =
          | List_list_string_2 of 'info * ('info,
          'info Lvca_syntax.Nominal.Term.t) Wrapper.Types.list_list_a 
        let info tm = Wrapper.Info.list_list_string_2 tm
        let to_plain tm = Wrapper.To_plain.list_list_string_2 tm
        let of_plain tm = Wrapper.Of_plain.list_list_string_2 tm
        let map_info ~f  tm = Wrapper.Map_info.list_list_string_2 ~f tm
        let to_nominal tm = Wrapper.To_nominal.list_list_string_2 tm
        let of_nominal tm = Wrapper.Of_nominal.list_list_string_2 tm
        module Plain =
          struct
            type t = Wrapper.Plain.list_list_string_2 =
              | List_list_string_2 of Lvca_syntax.Nominal.Term.Plain.t
              Wrapper.Plain.list_list_a 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module List_list_predefined_1 =
      struct
        type 'info t = 'info Wrapper.Types.list_list_predefined_1 =
          | List_list_predefined_1 of 'info * ('info,
          ('info, 'info Wrapper.Types.predefined) Wrapper.Types.list)
          Wrapper.Types.list 
        let info tm = Wrapper.Info.list_list_predefined_1 tm
        let to_plain tm = Wrapper.To_plain.list_list_predefined_1 tm
        let of_plain tm = Wrapper.Of_plain.list_list_predefined_1 tm
        let map_info ~f  tm = Wrapper.Map_info.list_list_predefined_1 ~f tm
        let to_nominal tm = Wrapper.To_nominal.list_list_predefined_1 tm
        let of_nominal tm = Wrapper.Of_nominal.list_list_predefined_1 tm
        module Plain =
          struct
            type t = Wrapper.Plain.list_list_predefined_1 =
              | List_list_predefined_1 of Wrapper.Plain.predefined
              Wrapper.Plain.list Wrapper.Plain.list 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
    module List_list_predefined_2 =
      struct
        type 'info t = 'info Wrapper.Types.list_list_predefined_2 =
          | List_list_predefined_2 of 'info * ('info,
          'info Wrapper.Types.predefined) Wrapper.Types.list_list_a 
        let info tm = Wrapper.Info.list_list_predefined_2 tm
        let to_plain tm = Wrapper.To_plain.list_list_predefined_2 tm
        let of_plain tm = Wrapper.Of_plain.list_list_predefined_2 tm
        let map_info ~f  tm = Wrapper.Map_info.list_list_predefined_2 ~f tm
        let to_nominal tm = Wrapper.To_nominal.list_list_predefined_2 tm
        let of_nominal tm = Wrapper.Of_nominal.list_list_predefined_2 tm
        module Plain =
          struct
            type t = Wrapper.Plain.list_list_predefined_2 =
              | List_list_predefined_2 of Wrapper.Plain.predefined
              Wrapper.Plain.list_list_a 
            let (=) x y =
              let x = (x |> of_plain) |> to_nominal in
              let y = (y |> of_plain) |> to_nominal in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify tm =
              ((tm |> of_plain) |> to_nominal) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match of_nominal tm with
                         | Ok tm -> Some (to_plain tm)
                         | Error _ -> None))
            let pp ppf tm =
              ((tm |> of_plain) |> to_nominal) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match of_nominal tm with
                     | Ok tm -> return (to_plain tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
  end
module type Is_rec_sig  =
  sig
    val language :
      string Lvca_provenance.Commented.t Lvca_syntax.Abstract_syntax.t
    module Is_rec :
    sig
      type 'info t =
        | Rec of 'info 
        | No_rec of 'info 
      module Plain :
      sig
        type t =
          | Rec 
          | No_rec 
        val pp : t Fmt.t
        val (=) : t -> t -> bool
        val parse : t Lvca_parsing.t
        val jsonify : t Lvca_util.Json.serializer
        val unjsonify : t Lvca_util.Json.deserializer
      end
    end
  end
