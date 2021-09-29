open Lvca_syntax
let test_nominal =
  Lvca_syntax.Nominal.Term.Operator
    ((`Source_located
        (let open Lvca_syntax.Provenance in
           {
             at =
               (`Implementation
                  {
                    pos_fname = "syntax/Nominal.ml";
                    pos_lnum = 14;
                    pos_bol = 252;
                    pos_cnum = 302
                  })
           })), "foo",
      [Lvca_syntax.Nominal.Scope.Scope
         ([Lvca_syntax.Pattern.Var
             ((`Source_located
                 ((let open Lvca_syntax.Provenance in
                     {
                       at =
                         (`Implementation
                            {
                              pos_fname = "syntax/Nominal.ml";
                              pos_lnum = 18;
                              pos_bol = 373;
                              pos_cnum = 418
                            })
                     }))), "x")],
           (Lvca_syntax.Nominal.Term.Var
              ((`Source_located
                  ((let open Lvca_syntax.Provenance in
                      {
                        at =
                          (`Implementation
                             {
                               pos_fname = "syntax/Nominal.ml";
                               pos_lnum = 18;
                               pos_bol = 373;
                               pos_cnum = 418
                             })
                      }))), "x")))])
let test_pattern =
  Lvca_syntax.Pattern.Operator
    (`Empty, "foo", [Lvca_syntax.Pattern.Var (`Empty, "x")])
let test_language =
  let open Lvca_syntax.Abstract_syntax in
    {
      externals = [];
      sort_defs =
        [("foo",
           (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
              ([],
                [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                   ((`Parse_located
                       (Some
                          ((let open Lvca_provenance.Range in
                              { start = 11; finish = 20 })))), "foo",
                     (Lvca_syntax.Abstract_syntax.Arity.Arity
                        ((`Source_located
                            ((let open Lvca_syntax.Provenance in
                                {
                                  at =
                                    (`Implementation
                                       {
                                         pos_fname =
                                           "syntax/Abstract_syntax.ml";
                                         pos_lnum = 252;
                                         pos_bol = 6468;
                                         pos_cnum = 6519
                                       })
                                }))),
                          [Lvca_syntax.Abstract_syntax.Valence.Valence
                             ([],
                               (Lvca_syntax.Sort.Name (`Empty, "integer")))])))])))]
    }
module List_model :
  sig
    val language : Lvca_syntax.Abstract_syntax.t
    module Wrapper :
    sig
      module Types :
      sig
        type 'a list =
          | Nil of Lvca_syntax.Provenance.t 
          | Cons of Lvca_syntax.Provenance.t * 'a * 'a list 
      end
    end
    module List :
    sig
      type 'a t = 'a Wrapper.Types.list =
        | Nil of Lvca_syntax.Provenance.t 
        | Cons of Lvca_syntax.Provenance.t * 'a * 'a Wrapper.Types.list 
      val to_nominal :
        ('a_ -> Lvca_syntax.Nominal.Term.t) ->
          'a_ t -> Lvca_syntax.Nominal.Term.t
      val of_nominal :
        (Lvca_syntax.Nominal.Term.t ->
           ('a_, Lvca_syntax.Nominal.Term.t) Result.t)
          ->
          Lvca_syntax.Nominal.Term.t ->
            ('a_ t, Lvca_syntax.Nominal.Term.t) Result.t
      val info : _ -> 'a__ t -> Lvca_syntax.Provenance.t
      val mk_Nil : info:Lvca_syntax.Provenance.t -> 'a t
      val mk_Cons :
        info:Lvca_syntax.Provenance.t -> 'a -> 'a Wrapper.Types.list -> 'a t
    end
  end =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type 'a list =
              | Nil of Lvca_syntax.Provenance.t 
              | Cons of Lvca_syntax.Provenance.t * 'a * 'a list 
          end
        module Info =
          struct
            let list _a =
              function | Types.Nil x0 -> x0 | Types.Cons (x0, _, _) -> x0
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
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals = [];
          sort_defs =
            [("list",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([("a", None)],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ((`Parse_located
                           (Some
                              ((let open Lvca_provenance.Range in
                                  { start = 13; finish = 15 })))), "Nil",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((`Source_located
                                ((let open Lvca_syntax.Provenance in
                                    {
                                      at =
                                        (`Implementation
                                           {
                                             pos_fname =
                                               "syntax/Abstract_syntax.ml";
                                             pos_lnum = 252;
                                             pos_bol = 6468;
                                             pos_cnum = 6519
                                           })
                                    }))), [])));
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 22; finish = 33 })))), "Cons",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([], (Lvca_syntax.Sort.Name (`Empty, "a")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Ap
                                    (`Empty, "list",
                                      (Lvca_syntax.Sort.Cons
                                         ((`Source_located
                                             ((let open Lvca_syntax.Provenance in
                                                 {
                                                   at =
                                                     (`Implementation
                                                        {
                                                          pos_fname =
                                                            "syntax/Sort.ml";
                                                          pos_lnum = 44;
                                                          pos_bol = 1397;
                                                          pos_cnum = 1439
                                                        })
                                                 }))),
                                           (Lvca_syntax.Sort.Name
                                              (`Empty, "a")),
                                           (Lvca_syntax.Sort.Nil
                                              (`Source_located
                                                 ((let open Lvca_syntax.Provenance in
                                                     {
                                                       at =
                                                         (`Implementation
                                                            {
                                                              pos_fname =
                                                                "syntax/Sort.ml";
                                                              pos_lnum = 43;
                                                              pos_bol = 1352;
                                                              pos_cnum = 1388
                                                            })
                                                     })))))))))])))])))]
        }
    module List =
      struct
        type 'a t = 'a Wrapper.Types.list =
          | Nil of Lvca_syntax.Provenance.t 
          | Cons of Lvca_syntax.Provenance.t * 'a * 'a Wrapper.Types.list 
        let info = Wrapper.Info.list
        let to_nominal = Wrapper.To_nominal.list
        let of_nominal = Wrapper.Of_nominal.list
        let mk_Nil ~info  = Nil info
        let mk_Cons ~info  x_0 x_1 = Cons (info, x_0, x_1)
      end
  end 
module Lang =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type nat =
              | Z of Lvca_syntax.Provenance.t 
              | S of Lvca_syntax.Provenance.t * nat 
            and mut_a =
              | Mut_a of Lvca_syntax.Provenance.t * mut_b 
            and mut_b =
              | Mut_b of Lvca_syntax.Provenance.t * mut_a 
            and term =
              | Operator of Lvca_syntax.Provenance.t *
              Lvca_syntax.Nominal.Term.t 
            and ('a, 'b) pair_plus =
              | PairPlus of Lvca_syntax.Provenance.t * 'a * 'b * foo 
            and foo =
              | Foo of Lvca_syntax.Provenance.t * Lvca_syntax.Nominal.Term.t
              
              | Bar of Lvca_syntax.Provenance.t * (Pattern.t *
              Lvca_syntax.Single_var.t * foo) 
              | Foo_var of Lvca_syntax.Provenance.t * string 
            and ('a, 'b) pair =
              | Pair of Lvca_syntax.Provenance.t * 'a * 'b 
            and nonempty =
              | Nonempty of Lvca_syntax.Provenance.t *
              Lvca_syntax.Nominal.Term.t * Lvca_syntax.Nominal.Term.t 
          end
        module Info =
          struct
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
            let mut_a = function | Types.Mut_a (x0, _) -> x0
            and mut_b = function | Types.Mut_b (x0, _) -> x0
            let nat = function | Types.Z x0 -> x0 | Types.S (x0, _) -> x0
          end
        module To_nominal =
          struct
            let nonempty =
              function
              | Types.Nonempty (x0, x1, x2) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Nonempty",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (Lvca_syntax.Nominal.Term.to_nominal x1));
                      Lvca_syntax.Nominal.Scope.Scope
                        ([], (Lvca_syntax.Nominal.Term.to_nominal x2))])
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
                    (x0, "Foo",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (Lvca_syntax.Nominal.Term.to_nominal x1))])
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
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (Lvca_syntax.Nominal.Term.to_nominal x1))])
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
            let rec nat =
              function
              | Types.Z x0 -> Lvca_syntax.Nominal.Term.Operator (x0, "Z", [])
              | Types.S (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "S",
                      [Lvca_syntax.Nominal.Scope.Scope ([], (nat x1))])
          end
        module Of_nominal =
          struct
            let nonempty =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Nonempty", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::(Lvca_syntax.Nominal.Scope.Scope ([], x2))::[])
                  -> Ok (Types.Nonempty (x0, x1, x2))
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
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals =
            [("integer",
               (Lvca_syntax.Abstract_syntax.Kind.Kind
                  ((`Parse_located
                      (Some
                         ((let open Lvca_provenance.Range in
                             { start = 11; finish = 12 })))), 1)));
            ("string",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 ((`Parse_located
                     (Some
                        ((let open Lvca_provenance.Range in
                            { start = 22; finish = 23 })))), 1)));
            ("maybe",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 ((`Parse_located
                     (Some
                        ((let open Lvca_provenance.Range in
                            { start = 32; finish = 38 })))), 2)));
            ("list",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 ((`Parse_located
                     (Some
                        ((let open Lvca_provenance.Range in
                            { start = 46; finish = 52 })))), 2)))];
          sort_defs =
            [("foo",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ((`Parse_located
                           (Some
                              ((let open Lvca_provenance.Range in
                                  { start = 68; finish = 77 })))), "Foo",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((`Source_located
                                ((let open Lvca_syntax.Provenance in
                                    {
                                      at =
                                        (`Implementation
                                           {
                                             pos_fname =
                                               "syntax/Abstract_syntax.ml";
                                             pos_lnum = 252;
                                             pos_bol = 6468;
                                             pos_cnum = 6519
                                           })
                                    }))),
                              [Lvca_syntax.Abstract_syntax.Valence.Valence
                                 ([],
                                   (Lvca_syntax.Sort.Name (`Empty, "integer")))])));
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 85; finish = 105 })))), "Bar",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([Lvca_syntax.Abstract_syntax.Sort_slot.Sort_pattern
                                    {
                                      pattern_sort =
                                        (Lvca_syntax.Sort.Name
                                           (`Empty, "foo"));
                                      var_sort =
                                        (Lvca_syntax.Sort.Name
                                           (`Empty, "foo"))
                                    };
                                 Lvca_syntax.Abstract_syntax.Sort_slot.Sort_binding
                                   (Lvca_syntax.Sort.Name (`Empty, "foo"))],
                                  (Lvca_syntax.Sort.Name (`Empty, "foo")))])))])));
            ("nat",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 115; finish = 117 })))), "Z",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))), [])));
                   Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                     ((`Parse_located
                         (Some
                            ((let open Lvca_provenance.Range in
                                { start = 121; finish = 126 })))), "S",
                       (Lvca_syntax.Abstract_syntax.Arity.Arity
                          ((`Source_located
                              ((let open Lvca_syntax.Provenance in
                                  {
                                    at =
                                      (`Implementation
                                         {
                                           pos_fname =
                                             "syntax/Abstract_syntax.ml";
                                           pos_lnum = 252;
                                           pos_bol = 6468;
                                           pos_cnum = 6519
                                         })
                                  }))),
                            [Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([], (Lvca_syntax.Sort.Name (`Empty, "nat")))])))])));
            ("pair",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None); ("b", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 144; finish = 150 })))), "Pair",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([], (Lvca_syntax.Sort.Name (`Empty, "a")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([], (Lvca_syntax.Sort.Name (`Empty, "b")))])))])));
            ("pair_plus",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None); ("b", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 176; finish = 187 })))),
                        "PairPlus",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([], (Lvca_syntax.Sort.Name (`Empty, "a")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([], (Lvca_syntax.Sort.Name (`Empty, "b")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([], (Lvca_syntax.Sort.Name (`Empty, "foo")))])))])));
            ("nonempty",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 209; finish = 230 })))),
                        "Nonempty",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name (`Empty, "string")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Ap
                                    (`Empty, "list",
                                      (Lvca_syntax.Sort.Cons
                                         ((`Source_located
                                             ((let open Lvca_syntax.Provenance in
                                                 {
                                                   at =
                                                     (`Implementation
                                                        {
                                                          pos_fname =
                                                            "syntax/Sort.ml";
                                                          pos_lnum = 44;
                                                          pos_bol = 1397;
                                                          pos_cnum = 1439
                                                        })
                                                 }))),
                                           (Lvca_syntax.Sort.Name
                                              (`Empty, "string")),
                                           (Lvca_syntax.Sort.Nil
                                              (`Source_located
                                                 ((let open Lvca_syntax.Provenance in
                                                     {
                                                       at =
                                                         (`Implementation
                                                            {
                                                              pos_fname =
                                                                "syntax/Sort.ml";
                                                              pos_lnum = 43;
                                                              pos_bol = 1352;
                                                              pos_cnum = 1388
                                                            })
                                                     })))))))))])))])));
            ("term",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 248; finish = 259 })))),
                        "Operator",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (`Empty, "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((`Source_located
                                              ((let open Lvca_syntax.Provenance in
                                                  {
                                                    at =
                                                      (`Implementation
                                                         {
                                                           pos_fname =
                                                             "syntax/Sort.ml";
                                                           pos_lnum = 44;
                                                           pos_bol = 1397;
                                                           pos_cnum = 1439
                                                         })
                                                  }))),
                                            (Lvca_syntax.Sort.Name
                                               (`Empty, "term")),
                                            (Lvca_syntax.Sort.Nil
                                               (`Source_located
                                                  ((let open Lvca_syntax.Provenance in
                                                      {
                                                        at =
                                                          (`Implementation
                                                             {
                                                               pos_fname =
                                                                 "syntax/Sort.ml";
                                                               pos_lnum = 43;
                                                               pos_bol = 1352;
                                                               pos_cnum =
                                                                 1388
                                                             })
                                                      })))))))))])))])));
            ("mut_a",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 275; finish = 282 })))), "Mut_a",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name (`Empty, "mut_b")))])))])));
            ("mut_b",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 297; finish = 304 })))), "Mut_b",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name (`Empty, "mut_a")))])))])))]
        }
    module Foo =
      struct
        type t = Wrapper.Types.foo =
          | Foo of Lvca_syntax.Provenance.t * Lvca_syntax.Nominal.Term.t 
          | Bar of Lvca_syntax.Provenance.t * (Pattern.t *
          Lvca_syntax.Single_var.t * Wrapper.Types.foo) 
          | Foo_var of Lvca_syntax.Provenance.t * string 
        let info = Wrapper.Info.foo
        let to_nominal = Wrapper.To_nominal.foo
        let of_nominal = Wrapper.Of_nominal.foo
        let mk_Foo ~info  x_0 = Foo (info, x_0)
        let mk_Bar ~info  x_0 = Bar (info, x_0)
        let mk_Foo_var ~info  name = Foo_var (info, name)
      end
    module Nat =
      struct
        type t = Wrapper.Types.nat =
          | Z of Lvca_syntax.Provenance.t 
          | S of Lvca_syntax.Provenance.t * Wrapper.Types.nat 
        let info = Wrapper.Info.nat
        let to_nominal = Wrapper.To_nominal.nat
        let of_nominal = Wrapper.Of_nominal.nat
        let mk_Z ~info  = Z info
        let mk_S ~info  x_0 = S (info, x_0)
      end
    module Pair =
      struct
        type ('a, 'b) t = ('a, 'b) Wrapper.Types.pair =
          | Pair of Lvca_syntax.Provenance.t * 'a * 'b 
        let info = Wrapper.Info.pair
        let to_nominal = Wrapper.To_nominal.pair
        let of_nominal = Wrapper.Of_nominal.pair
        let mk_Pair ~info  x_0 x_1 = Pair (info, x_0, x_1)
      end
    module Pair_plus =
      struct
        type ('a, 'b) t = ('a, 'b) Wrapper.Types.pair_plus =
          | PairPlus of Lvca_syntax.Provenance.t * 'a * 'b *
          Wrapper.Types.foo 
        let info = Wrapper.Info.pair_plus
        let to_nominal = Wrapper.To_nominal.pair_plus
        let of_nominal = Wrapper.Of_nominal.pair_plus
        let mk_PairPlus ~info  x_0 x_1 x_2 = PairPlus (info, x_0, x_1, x_2)
      end
    module Nonempty =
      struct
        type t = Wrapper.Types.nonempty =
          | Nonempty of Lvca_syntax.Provenance.t * Lvca_syntax.Nominal.Term.t
          * Lvca_syntax.Nominal.Term.t 
        let info = Wrapper.Info.nonempty
        let to_nominal = Wrapper.To_nominal.nonempty
        let of_nominal = Wrapper.Of_nominal.nonempty
        let mk_Nonempty ~info  x_0 x_1 = Nonempty (info, x_0, x_1)
      end
    module Term =
      struct
        type t = Wrapper.Types.term =
          | Operator of Lvca_syntax.Provenance.t * Lvca_syntax.Nominal.Term.t 
        let info = Wrapper.Info.term
        let to_nominal = Wrapper.To_nominal.term
        let of_nominal = Wrapper.Of_nominal.term
        let mk_Operator ~info  x_0 = Operator (info, x_0)
      end
    module Mut_a =
      struct
        type t = Wrapper.Types.mut_a =
          | Mut_a of Lvca_syntax.Provenance.t * Wrapper.Types.mut_b 
        let info = Wrapper.Info.mut_a
        let to_nominal = Wrapper.To_nominal.mut_a
        let of_nominal = Wrapper.Of_nominal.mut_a
        let mk_Mut_a ~info  x_0 = Mut_a (info, x_0)
      end
    module Mut_b =
      struct
        type t = Wrapper.Types.mut_b =
          | Mut_b of Lvca_syntax.Provenance.t * Wrapper.Types.mut_a 
        let info = Wrapper.Info.mut_b
        let to_nominal = Wrapper.To_nominal.mut_b
        let of_nominal = Wrapper.Of_nominal.mut_b
        let mk_Mut_b ~info  x_0 = Mut_b (info, x_0)
      end
  end
module Ifz_lang :
  sig
    val language : Lvca_syntax.Abstract_syntax.t
    module Wrapper :
    sig
      module Types :
      sig
        type ifz =
          | Ifz of Lvca_syntax.Provenance.t * ifz * (Lvca_syntax.Single_var.t
          * ifz) * ifz 
          | Ifz_var of Lvca_syntax.Provenance.t * string 
      end
    end
    module Ifz :
    sig
      type t = Wrapper.Types.ifz =
        | Ifz of Lvca_syntax.Provenance.t * Wrapper.Types.ifz *
        (Lvca_syntax.Single_var.t * Wrapper.Types.ifz) * Wrapper.Types.ifz 
        | Ifz_var of Lvca_syntax.Provenance.t * string 
      val to_nominal : t -> Lvca_syntax.Nominal.Term.t
      val of_nominal :
        Lvca_syntax.Nominal.Term.t ->
          (t, Lvca_syntax.Nominal.Term.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val mk_Ifz :
        info:Lvca_syntax.Provenance.t ->
          Wrapper.Types.ifz ->
            (Lvca_syntax.Single_var.t * Wrapper.Types.ifz) ->
              Wrapper.Types.ifz -> t
      val mk_Ifz_var : info:Lvca_syntax.Provenance.t -> string -> t
    end
  end =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type ifz =
              | Ifz of Lvca_syntax.Provenance.t * ifz *
              (Lvca_syntax.Single_var.t * ifz) * ifz 
              | Ifz_var of Lvca_syntax.Provenance.t * string 
          end
        module Info =
          struct
            let ifz =
              function
              | Types.Ifz (x0, _, (_, _), _) -> x0
              | Types.Ifz_var (info, _) -> info
          end
        module To_nominal =
          struct
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
          end
        module Of_nominal =
          struct
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
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals = [];
          sort_defs =
            [("ifz",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ((`Parse_located
                           (Some
                              ((let open Lvca_provenance.Range in
                                  { start = 10; finish = 30 })))), "Ifz",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((`Source_located
                                ((let open Lvca_syntax.Provenance in
                                    {
                                      at =
                                        (`Implementation
                                           {
                                             pos_fname =
                                               "syntax/Abstract_syntax.ml";
                                             pos_lnum = 252;
                                             pos_bol = 6468;
                                             pos_cnum = 6519
                                           })
                                    }))),
                              [Lvca_syntax.Abstract_syntax.Valence.Valence
                                 ([],
                                   (Lvca_syntax.Sort.Name (`Empty, "ifz")));
                              Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([Lvca_syntax.Abstract_syntax.Sort_slot.Sort_binding
                                    (Lvca_syntax.Sort.Name (`Empty, "ifz"))],
                                  (Lvca_syntax.Sort.Name (`Empty, "ifz")));
                              Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([], (Lvca_syntax.Sort.Name (`Empty, "ifz")))])))])))]
        }
    module Ifz =
      struct
        type t = Wrapper.Types.ifz =
          | Ifz of Lvca_syntax.Provenance.t * Wrapper.Types.ifz *
          (Lvca_syntax.Single_var.t * Wrapper.Types.ifz) * Wrapper.Types.ifz
          
          | Ifz_var of Lvca_syntax.Provenance.t * string 
        let info = Wrapper.Info.ifz
        let to_nominal = Wrapper.To_nominal.ifz
        let of_nominal = Wrapper.Of_nominal.ifz
        let mk_Ifz ~info  x_0 x_1 x_2 = Ifz (info, x_0, x_1, x_2)
        let mk_Ifz_var ~info  name = Ifz_var (info, name)
      end
  end 
module List_lang =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type list_external =
              | List_external of Lvca_syntax.Provenance.t *
              Lvca_syntax.Nominal.Term.t list 
            and list_predefined =
              | List_predefined of Lvca_syntax.Provenance.t * predefined list 
            and list_list_string_2 =
              | List_list_string_2 of Lvca_syntax.Provenance.t *
              Lvca_syntax.Nominal.Term.t list_list_a 
            and list_list_string_1 =
              | List_list_string_1 of Lvca_syntax.Provenance.t *
              Lvca_syntax.Nominal.Term.t list list 
            and list_list_predefined_2 =
              | List_list_predefined_2 of Lvca_syntax.Provenance.t *
              predefined list_list_a 
            and 'a list_list_a =
              | List_list_a of Lvca_syntax.Provenance.t * 'a list list 
            and list_list_predefined_1 =
              | List_list_predefined_1 of Lvca_syntax.Provenance.t *
              predefined list list 
            and predefined =
              | Predefined of Lvca_syntax.Provenance.t 
            and 'a list =
              | Nil of Lvca_syntax.Provenance.t 
              | Cons of Lvca_syntax.Provenance.t * 'a * 'a list 
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
                         ([],
                           (list (list Lvca_syntax.Nominal.Term.to_nominal)
                              x1))])
            let list_list_string_2 =
              function
              | Types.List_list_string_2 (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "List_list_string_2",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([],
                           (list_list_a Lvca_syntax.Nominal.Term.to_nominal
                              x1))])
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
                         ([], (list Lvca_syntax.Nominal.Term.to_nominal x1))])
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
                  (match list (list Lvca_syntax.Nominal.Term.of_nominal) x1
                   with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.List_list_string_1 (x0, x1)))
              | tm -> Error tm
            let list_list_string_2 =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_string_2", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list_list_a Lvca_syntax.Nominal.Term.of_nominal x1
                   with
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
                  (match list Lvca_syntax.Nominal.Term.of_nominal x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.List_external (x0, x1)))
              | tm -> Error tm
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals =
            [("string",
               (Lvca_syntax.Abstract_syntax.Kind.Kind
                  ((`Parse_located
                      (Some
                         ((let open Lvca_provenance.Range in
                             { start = 10; finish = 11 })))), 1)))];
          sort_defs =
            [("predefined",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ((`Parse_located
                           (Some
                              ((let open Lvca_provenance.Range in
                                  { start = 37; finish = 39 })))),
                         "Predefined",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((`Source_located
                                ((let open Lvca_syntax.Provenance in
                                    {
                                      at =
                                        (`Implementation
                                           {
                                             pos_fname =
                                               "syntax/Abstract_syntax.ml";
                                             pos_lnum = 252;
                                             pos_bol = 6468;
                                             pos_cnum = 6519
                                           })
                                    }))), [])))])));
            ("list",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 53; finish = 55 })))), "Nil",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))), [])));
                   Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                     ((`Parse_located
                         (Some
                            ((let open Lvca_provenance.Range in
                                { start = 62; finish = 73 })))), "Cons",
                       (Lvca_syntax.Abstract_syntax.Arity.Arity
                          ((`Source_located
                              ((let open Lvca_syntax.Provenance in
                                  {
                                    at =
                                      (`Implementation
                                         {
                                           pos_fname =
                                             "syntax/Abstract_syntax.ml";
                                           pos_lnum = 252;
                                           pos_bol = 6468;
                                           pos_cnum = 6519
                                         })
                                  }))),
                            [Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([], (Lvca_syntax.Sort.Name (`Empty, "a")));
                            Lvca_syntax.Abstract_syntax.Valence.Valence
                              ([],
                                (Lvca_syntax.Sort.Ap
                                   (`Empty, "list",
                                     (Lvca_syntax.Sort.Cons
                                        ((`Source_located
                                            ((let open Lvca_syntax.Provenance in
                                                {
                                                  at =
                                                    (`Implementation
                                                       {
                                                         pos_fname =
                                                           "syntax/Sort.ml";
                                                         pos_lnum = 44;
                                                         pos_bol = 1397;
                                                         pos_cnum = 1439
                                                       })
                                                }))),
                                          (Lvca_syntax.Sort.Name
                                             (`Empty, "a")),
                                          (Lvca_syntax.Sort.Nil
                                             (`Source_located
                                                ((let open Lvca_syntax.Provenance in
                                                    {
                                                      at =
                                                        (`Implementation
                                                           {
                                                             pos_fname =
                                                               "syntax/Sort.ml";
                                                             pos_lnum = 43;
                                                             pos_bol = 1352;
                                                             pos_cnum = 1388
                                                           })
                                                    })))))))))])))])));
            ("list_external",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 104; finish = 117 })))),
                        "List_external",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (`Empty, "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((`Source_located
                                              ((let open Lvca_syntax.Provenance in
                                                  {
                                                    at =
                                                      (`Implementation
                                                         {
                                                           pos_fname =
                                                             "syntax/Sort.ml";
                                                           pos_lnum = 44;
                                                           pos_bol = 1397;
                                                           pos_cnum = 1439
                                                         })
                                                  }))),
                                            (Lvca_syntax.Sort.Name
                                               (`Empty, "string")),
                                            (Lvca_syntax.Sort.Nil
                                               (`Source_located
                                                  ((let open Lvca_syntax.Provenance in
                                                      {
                                                        at =
                                                          (`Implementation
                                                             {
                                                               pos_fname =
                                                                 "syntax/Sort.ml";
                                                               pos_lnum = 43;
                                                               pos_bol = 1352;
                                                               pos_cnum =
                                                                 1388
                                                             })
                                                      })))))))))])))])));
            ("list_predefined",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 152; finish = 169 })))),
                        "List_predefined",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (`Empty, "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((`Source_located
                                              ((let open Lvca_syntax.Provenance in
                                                  {
                                                    at =
                                                      (`Implementation
                                                         {
                                                           pos_fname =
                                                             "syntax/Sort.ml";
                                                           pos_lnum = 44;
                                                           pos_bol = 1397;
                                                           pos_cnum = 1439
                                                         })
                                                  }))),
                                            (Lvca_syntax.Sort.Name
                                               (`Empty, "predefined")),
                                            (Lvca_syntax.Sort.Nil
                                               (`Source_located
                                                  ((let open Lvca_syntax.Provenance in
                                                      {
                                                        at =
                                                          (`Implementation
                                                             {
                                                               pos_fname =
                                                                 "syntax/Sort.ml";
                                                               pos_lnum = 43;
                                                               pos_bol = 1352;
                                                               pos_cnum =
                                                                 1388
                                                             })
                                                      })))))))))])))])));
            ("list_list_a",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 199; finish = 214 })))),
                        "List_list_a",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (`Empty, "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((`Source_located
                                              ((let open Lvca_syntax.Provenance in
                                                  {
                                                    at =
                                                      (`Implementation
                                                         {
                                                           pos_fname =
                                                             "syntax/Sort.ml";
                                                           pos_lnum = 44;
                                                           pos_bol = 1397;
                                                           pos_cnum = 1439
                                                         })
                                                  }))),
                                            (Lvca_syntax.Sort.Ap
                                               (`Empty, "list",
                                                 (Lvca_syntax.Sort.Cons
                                                    ((`Source_located
                                                        ((let open Lvca_syntax.Provenance in
                                                            {
                                                              at =
                                                                (`Implementation
                                                                   {
                                                                    pos_fname
                                                                    =
                                                                    "syntax/Sort.ml";
                                                                    pos_lnum
                                                                    = 44;
                                                                    pos_bol =
                                                                    1397;
                                                                    pos_cnum
                                                                    = 1439
                                                                   })
                                                            }))),
                                                      (Lvca_syntax.Sort.Name
                                                         (`Empty, "a")),
                                                      (Lvca_syntax.Sort.Nil
                                                         (`Source_located
                                                            ((let open Lvca_syntax.Provenance in
                                                                {
                                                                  at =
                                                                    (
                                                                    `Implementation
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "syntax/Sort.ml";
                                                                    pos_lnum
                                                                    = 43;
                                                                    pos_bol =
                                                                    1352;
                                                                    pos_cnum
                                                                    = 1388
                                                                    })
                                                                })))))))),
                                            (Lvca_syntax.Sort.Nil
                                               (`Source_located
                                                  ((let open Lvca_syntax.Provenance in
                                                      {
                                                        at =
                                                          (`Implementation
                                                             {
                                                               pos_fname =
                                                                 "syntax/Sort.ml";
                                                               pos_lnum = 43;
                                                               pos_bol = 1352;
                                                               pos_cnum =
                                                                 1388
                                                             })
                                                      })))))))))])))])));
            ("list_list_string_1",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 255; finish = 275 })))),
                        "List_list_string_1",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (`Empty, "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((`Source_located
                                              ((let open Lvca_syntax.Provenance in
                                                  {
                                                    at =
                                                      (`Implementation
                                                         {
                                                           pos_fname =
                                                             "syntax/Sort.ml";
                                                           pos_lnum = 44;
                                                           pos_bol = 1397;
                                                           pos_cnum = 1439
                                                         })
                                                  }))),
                                            (Lvca_syntax.Sort.Ap
                                               (`Empty, "list",
                                                 (Lvca_syntax.Sort.Cons
                                                    ((`Source_located
                                                        ((let open Lvca_syntax.Provenance in
                                                            {
                                                              at =
                                                                (`Implementation
                                                                   {
                                                                    pos_fname
                                                                    =
                                                                    "syntax/Sort.ml";
                                                                    pos_lnum
                                                                    = 44;
                                                                    pos_bol =
                                                                    1397;
                                                                    pos_cnum
                                                                    = 1439
                                                                   })
                                                            }))),
                                                      (Lvca_syntax.Sort.Name
                                                         (`Empty, "string")),
                                                      (Lvca_syntax.Sort.Nil
                                                         (`Source_located
                                                            ((let open Lvca_syntax.Provenance in
                                                                {
                                                                  at =
                                                                    (
                                                                    `Implementation
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "syntax/Sort.ml";
                                                                    pos_lnum
                                                                    = 43;
                                                                    pos_bol =
                                                                    1352;
                                                                    pos_cnum
                                                                    = 1388
                                                                    })
                                                                })))))))),
                                            (Lvca_syntax.Sort.Nil
                                               (`Source_located
                                                  ((let open Lvca_syntax.Provenance in
                                                      {
                                                        at =
                                                          (`Implementation
                                                             {
                                                               pos_fname =
                                                                 "syntax/Sort.ml";
                                                               pos_lnum = 43;
                                                               pos_bol = 1352;
                                                               pos_cnum =
                                                                 1388
                                                             })
                                                      })))))))))])))])));
            ("list_list_string_2",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 316; finish = 336 })))),
                        "List_list_string_2",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (`Empty, "list_list_a",
                                       (Lvca_syntax.Sort.Cons
                                          ((`Source_located
                                              ((let open Lvca_syntax.Provenance in
                                                  {
                                                    at =
                                                      (`Implementation
                                                         {
                                                           pos_fname =
                                                             "syntax/Sort.ml";
                                                           pos_lnum = 44;
                                                           pos_bol = 1397;
                                                           pos_cnum = 1439
                                                         })
                                                  }))),
                                            (Lvca_syntax.Sort.Name
                                               (`Empty, "string")),
                                            (Lvca_syntax.Sort.Nil
                                               (`Source_located
                                                  ((let open Lvca_syntax.Provenance in
                                                      {
                                                        at =
                                                          (`Implementation
                                                             {
                                                               pos_fname =
                                                                 "syntax/Sort.ml";
                                                               pos_lnum = 43;
                                                               pos_bol = 1352;
                                                               pos_cnum =
                                                                 1388
                                                             })
                                                      })))))))))])))])));
            ("list_list_predefined_1",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 385; finish = 409 })))),
                        "List_list_predefined_1",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (`Empty, "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((`Source_located
                                              ((let open Lvca_syntax.Provenance in
                                                  {
                                                    at =
                                                      (`Implementation
                                                         {
                                                           pos_fname =
                                                             "syntax/Sort.ml";
                                                           pos_lnum = 44;
                                                           pos_bol = 1397;
                                                           pos_cnum = 1439
                                                         })
                                                  }))),
                                            (Lvca_syntax.Sort.Ap
                                               (`Empty, "list",
                                                 (Lvca_syntax.Sort.Cons
                                                    ((`Source_located
                                                        ((let open Lvca_syntax.Provenance in
                                                            {
                                                              at =
                                                                (`Implementation
                                                                   {
                                                                    pos_fname
                                                                    =
                                                                    "syntax/Sort.ml";
                                                                    pos_lnum
                                                                    = 44;
                                                                    pos_bol =
                                                                    1397;
                                                                    pos_cnum
                                                                    = 1439
                                                                   })
                                                            }))),
                                                      (Lvca_syntax.Sort.Name
                                                         (`Empty,
                                                           "predefined")),
                                                      (Lvca_syntax.Sort.Nil
                                                         (`Source_located
                                                            ((let open Lvca_syntax.Provenance in
                                                                {
                                                                  at =
                                                                    (
                                                                    `Implementation
                                                                    {
                                                                    pos_fname
                                                                    =
                                                                    "syntax/Sort.ml";
                                                                    pos_lnum
                                                                    = 43;
                                                                    pos_bol =
                                                                    1352;
                                                                    pos_cnum
                                                                    = 1388
                                                                    })
                                                                })))))))),
                                            (Lvca_syntax.Sort.Nil
                                               (`Source_located
                                                  ((let open Lvca_syntax.Provenance in
                                                      {
                                                        at =
                                                          (`Implementation
                                                             {
                                                               pos_fname =
                                                                 "syntax/Sort.ml";
                                                               pos_lnum = 43;
                                                               pos_bol = 1352;
                                                               pos_cnum =
                                                                 1388
                                                             })
                                                      })))))))))])))])));
            ("list_list_predefined_2",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 458; finish = 482 })))),
                        "List_list_predefined_2",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (`Empty, "list_list_a",
                                       (Lvca_syntax.Sort.Cons
                                          ((`Source_located
                                              ((let open Lvca_syntax.Provenance in
                                                  {
                                                    at =
                                                      (`Implementation
                                                         {
                                                           pos_fname =
                                                             "syntax/Sort.ml";
                                                           pos_lnum = 44;
                                                           pos_bol = 1397;
                                                           pos_cnum = 1439
                                                         })
                                                  }))),
                                            (Lvca_syntax.Sort.Name
                                               (`Empty, "predefined")),
                                            (Lvca_syntax.Sort.Nil
                                               (`Source_located
                                                  ((let open Lvca_syntax.Provenance in
                                                      {
                                                        at =
                                                          (`Implementation
                                                             {
                                                               pos_fname =
                                                                 "syntax/Sort.ml";
                                                               pos_lnum = 43;
                                                               pos_bol = 1352;
                                                               pos_cnum =
                                                                 1388
                                                             })
                                                      })))))))))])))])))]
        }
    module Predefined =
      struct
        type t = Wrapper.Types.predefined =
          | Predefined of Lvca_syntax.Provenance.t 
        let info = Wrapper.Info.predefined
        let to_nominal = Wrapper.To_nominal.predefined
        let of_nominal = Wrapper.Of_nominal.predefined
        let mk_Predefined ~info  = Predefined info
      end
    module List =
      struct
        type 'a t = 'a Wrapper.Types.list =
          | Nil of Lvca_syntax.Provenance.t 
          | Cons of Lvca_syntax.Provenance.t * 'a * 'a Wrapper.Types.list 
        let info = Wrapper.Info.list
        let to_nominal = Wrapper.To_nominal.list
        let of_nominal = Wrapper.Of_nominal.list
        let mk_Nil ~info  = Nil info
        let mk_Cons ~info  x_0 x_1 = Cons (info, x_0, x_1)
      end
    module List_external =
      struct
        type t = Wrapper.Types.list_external =
          | List_external of Lvca_syntax.Provenance.t *
          Lvca_syntax.Nominal.Term.t Wrapper.Types.list 
        let info = Wrapper.Info.list_external
        let to_nominal = Wrapper.To_nominal.list_external
        let of_nominal = Wrapper.Of_nominal.list_external
        let mk_List_external ~info  x_0 = List_external (info, x_0)
      end
    module List_predefined =
      struct
        type t = Wrapper.Types.list_predefined =
          | List_predefined of Lvca_syntax.Provenance.t *
          Wrapper.Types.predefined Wrapper.Types.list 
        let info = Wrapper.Info.list_predefined
        let to_nominal = Wrapper.To_nominal.list_predefined
        let of_nominal = Wrapper.Of_nominal.list_predefined
        let mk_List_predefined ~info  x_0 = List_predefined (info, x_0)
      end
    module List_list_a =
      struct
        type 'a t = 'a Wrapper.Types.list_list_a =
          | List_list_a of Lvca_syntax.Provenance.t * 'a Wrapper.Types.list
          Wrapper.Types.list 
        let info = Wrapper.Info.list_list_a
        let to_nominal = Wrapper.To_nominal.list_list_a
        let of_nominal = Wrapper.Of_nominal.list_list_a
        let mk_List_list_a ~info  x_0 = List_list_a (info, x_0)
      end
    module List_list_string_1 =
      struct
        type t = Wrapper.Types.list_list_string_1 =
          | List_list_string_1 of Lvca_syntax.Provenance.t *
          Lvca_syntax.Nominal.Term.t Wrapper.Types.list Wrapper.Types.list 
        let info = Wrapper.Info.list_list_string_1
        let to_nominal = Wrapper.To_nominal.list_list_string_1
        let of_nominal = Wrapper.Of_nominal.list_list_string_1
        let mk_List_list_string_1 ~info  x_0 = List_list_string_1 (info, x_0)
      end
    module List_list_string_2 =
      struct
        type t = Wrapper.Types.list_list_string_2 =
          | List_list_string_2 of Lvca_syntax.Provenance.t *
          Lvca_syntax.Nominal.Term.t Wrapper.Types.list_list_a 
        let info = Wrapper.Info.list_list_string_2
        let to_nominal = Wrapper.To_nominal.list_list_string_2
        let of_nominal = Wrapper.Of_nominal.list_list_string_2
        let mk_List_list_string_2 ~info  x_0 = List_list_string_2 (info, x_0)
      end
    module List_list_predefined_1 =
      struct
        type t = Wrapper.Types.list_list_predefined_1 =
          | List_list_predefined_1 of Lvca_syntax.Provenance.t *
          Wrapper.Types.predefined Wrapper.Types.list Wrapper.Types.list 
        let info = Wrapper.Info.list_list_predefined_1
        let to_nominal = Wrapper.To_nominal.list_list_predefined_1
        let of_nominal = Wrapper.Of_nominal.list_list_predefined_1
        let mk_List_list_predefined_1 ~info  x_0 =
          List_list_predefined_1 (info, x_0)
      end
    module List_list_predefined_2 =
      struct
        type t = Wrapper.Types.list_list_predefined_2 =
          | List_list_predefined_2 of Lvca_syntax.Provenance.t *
          Wrapper.Types.predefined Wrapper.Types.list_list_a 
        let info = Wrapper.Info.list_list_predefined_2
        let to_nominal = Wrapper.To_nominal.list_list_predefined_2
        let of_nominal = Wrapper.Of_nominal.list_list_predefined_2
        let mk_List_list_predefined_2 ~info  x_0 =
          List_list_predefined_2 (info, x_0)
      end
  end
module type Is_rec_sig  =
  sig
    val language : Lvca_syntax.Abstract_syntax.t
    module Wrapper :
    sig
      module Types :
      sig
        type ty =
          | Sort of Lvca_syntax.Provenance.t * Lvca_syntax.Nominal.Term.t 
          | Arrow of Lvca_syntax.Provenance.t * ty * ty 
        and mut_a =
          | Mut_a of Lvca_syntax.Provenance.t * mut_b 
        and mut_b =
          | Mut_b of Lvca_syntax.Provenance.t * mut_a 
        and is_rec =
          | Rec of Lvca_syntax.Provenance.t 
          | No_rec of Lvca_syntax.Provenance.t 
      end
    end
    module Is_rec :
    sig
      type t = Wrapper.Types.is_rec =
        | Rec of Lvca_syntax.Provenance.t 
        | No_rec of Lvca_syntax.Provenance.t 
      val to_nominal : t -> Lvca_syntax.Nominal.Term.t
      val of_nominal :
        Lvca_syntax.Nominal.Term.t ->
          (t, Lvca_syntax.Nominal.Term.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val mk_Rec : info:Lvca_syntax.Provenance.t -> t
      val mk_No_rec : info:Lvca_syntax.Provenance.t -> t
    end
    module Ty :
    sig
      type t = Wrapper.Types.ty =
        | Sort of Lvca_syntax.Provenance.t * Lvca_syntax.Nominal.Term.t 
        | Arrow of Lvca_syntax.Provenance.t * Wrapper.Types.ty *
        Wrapper.Types.ty 
      val to_nominal : t -> Lvca_syntax.Nominal.Term.t
      val of_nominal :
        Lvca_syntax.Nominal.Term.t ->
          (t, Lvca_syntax.Nominal.Term.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val mk_Sort :
        info:Lvca_syntax.Provenance.t -> Lvca_syntax.Nominal.Term.t -> t
      val mk_Arrow :
        info:Lvca_syntax.Provenance.t ->
          Wrapper.Types.ty -> Wrapper.Types.ty -> t
    end
    module Mut_a :
    sig
      type t = Wrapper.Types.mut_a =
        | Mut_a of Lvca_syntax.Provenance.t * Wrapper.Types.mut_b 
      val to_nominal : t -> Lvca_syntax.Nominal.Term.t
      val of_nominal :
        Lvca_syntax.Nominal.Term.t ->
          (t, Lvca_syntax.Nominal.Term.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val mk_Mut_a :
        info:Lvca_syntax.Provenance.t -> Wrapper.Types.mut_b -> t
    end
    module Mut_b :
    sig
      type t = Wrapper.Types.mut_b =
        | Mut_b of Lvca_syntax.Provenance.t * Wrapper.Types.mut_a 
      val to_nominal : t -> Lvca_syntax.Nominal.Term.t
      val of_nominal :
        Lvca_syntax.Nominal.Term.t ->
          (t, Lvca_syntax.Nominal.Term.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val mk_Mut_b :
        info:Lvca_syntax.Provenance.t -> Wrapper.Types.mut_a -> t
    end
  end
module Option_model :
  sig
    val language : Lvca_syntax.Abstract_syntax.t
    module Wrapper :
    sig
      module Types :
      sig
        type 'a option =
          | None of Lvca_syntax.Provenance.t 
          | Some of Lvca_syntax.Provenance.t * 'a 
      end
    end
    module Option :
    sig
      type 'a t = 'a Wrapper.Types.option =
        | None of Lvca_syntax.Provenance.t 
        | Some of Lvca_syntax.Provenance.t * 'a 
      val to_nominal :
        ('a_ -> Lvca_syntax.Nominal.Term.t) ->
          'a_ t -> Lvca_syntax.Nominal.Term.t
      val of_nominal :
        (Lvca_syntax.Nominal.Term.t ->
           ('a_, Lvca_syntax.Nominal.Term.t) Result.t)
          ->
          Lvca_syntax.Nominal.Term.t ->
            ('a_ t, Lvca_syntax.Nominal.Term.t) Result.t
      val info : _ -> 'a__ t -> Lvca_syntax.Provenance.t
      val mk_None : info:Lvca_syntax.Provenance.t -> 'a t
      val mk_Some : info:Lvca_syntax.Provenance.t -> 'a -> 'a t
    end
  end =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type 'a option =
              | None of Lvca_syntax.Provenance.t 
              | Some of Lvca_syntax.Provenance.t * 'a 
          end
        module Info =
          struct
            let option _a =
              function | Types.None x0 -> x0 | Types.Some (x0, _) -> x0
          end
        module To_nominal =
          struct
            let option a =
              function
              | Types.None x0 ->
                  Lvca_syntax.Nominal.Term.Operator (x0, "None", [])
              | Types.Some (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Some",
                      [Lvca_syntax.Nominal.Scope.Scope ([], (a x1))])
          end
        module Of_nominal =
          struct
            let option a =
              function
              | Lvca_syntax.Nominal.Term.Operator (x0, "None", []) ->
                  Ok (Types.None x0)
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Some", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match a x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.Some (x0, x1)))
              | tm -> Error tm
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals = [];
          sort_defs =
            [("option",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([("a", None)],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ((`Parse_located
                           (Some
                              ((let open Lvca_provenance.Range in
                                  { start = 16; finish = 18 })))), "None",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((`Source_located
                                ((let open Lvca_syntax.Provenance in
                                    {
                                      at =
                                        (`Implementation
                                           {
                                             pos_fname =
                                               "syntax/Abstract_syntax.ml";
                                             pos_lnum = 252;
                                             pos_bol = 6468;
                                             pos_cnum = 6519
                                           })
                                    }))), [])));
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((`Parse_located
                          (Some
                             ((let open Lvca_provenance.Range in
                                 { start = 25; finish = 28 })))), "Some",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((`Source_located
                               ((let open Lvca_syntax.Provenance in
                                   {
                                     at =
                                       (`Implementation
                                          {
                                            pos_fname =
                                              "syntax/Abstract_syntax.ml";
                                            pos_lnum = 252;
                                            pos_bol = 6468;
                                            pos_cnum = 6519
                                          })
                                   }))),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([], (Lvca_syntax.Sort.Name (`Empty, "a")))])))])))]
        }
    module Option =
      struct
        type 'a t = 'a Wrapper.Types.option =
          | None of Lvca_syntax.Provenance.t 
          | Some of Lvca_syntax.Provenance.t * 'a 
        let info = Wrapper.Info.option
        let to_nominal = Wrapper.To_nominal.option
        let of_nominal = Wrapper.Of_nominal.option
        let mk_None ~info  = None info
        let mk_Some ~info  x_0 = Some (info, x_0)
      end
  end 
