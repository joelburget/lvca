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
                   (((let open Lvca_provenance.Commented in
                        {
                          range =
                            (Some
                               (let open Lvca_provenance.Range in
                                  { start = 8; finish = 20 }));
                          comment = None
                        })), "foo",
                     (Lvca_syntax.Abstract_syntax.Arity.Arity
                        (((let open Lvca_provenance.Commented in
                             {
                               range =
                                 (Some
                                    (let open Lvca_provenance.Range in
                                       { start = 11; finish = 20 }));
                               comment = None
                             })),
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
                                       })), "integer")))])))])))]
    }
module List_model :
  sig
    val language :
      string Lvca_provenance.Commented.t Lvca_syntax.Abstract_syntax.t
    module Wrapper :
    sig
      module Types :
      sig
        type ('info, 'a) list =
          | Nil of 'info 
          | Cons of 'info * 'a * ('info, 'a) list 
      end
      module Plain : sig type 'a list =
                           | Nil 
                           | Cons of 'a * 'a list  end
    end
    module List :
    sig
      type ('info, 'a) t =
        | Nil of 'info 
        | Cons of 'info * 'a * ('info, 'a) Wrapper.Types.list 
      module Plain :
      sig type 'a t =
            | Nil 
            | Cons of 'a * 'a Wrapper.Plain.list  end
      val to_plain : ('a_ -> 'a__) -> (_, 'a_) t -> 'a__ Plain.t
      val of_plain : ('a_ -> 'a__) -> 'a_ Plain.t -> (unit, 'a__) t
      val to_nominal :
        ('a_ -> 'infoa Lvca_syntax.Nominal.Term.t) ->
          ('infoa, 'a_) t -> 'infoa Lvca_syntax.Nominal.Term.t
      val of_nominal :
        ('infoa Lvca_syntax.Nominal.Term.t ->
           ('a_, 'infoa Lvca_syntax.Nominal.Term.t) Result.t)
          ->
          'infoa Lvca_syntax.Nominal.Term.t ->
            (('infoa, 'a_) t, 'infoa Lvca_syntax.Nominal.Term.t) Result.t
      val info : _ -> ('info, 'a__) t -> 'info
      val map_info :
        (f:('infoa -> 'infob) -> 'a_ -> 'a__) ->
          f:('infoa -> 'infob) -> ('infoa, 'a_) t -> ('infob, 'a__) t
    end
  end =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type ('info, 'a) list =
              | Nil of 'info 
              | Cons of 'info * 'a * ('info, 'a) list 
          end
        module Plain = struct type 'a list =
                                | Nil 
                                | Cons of 'a * 'a list  end
        module Info =
          struct
            let list _a =
              function | Types.Nil x0 -> x0 | Types.Cons (x0, _, _) -> x0
          end
        module To_plain =
          struct
            let rec list a =
              function
              | Types.Nil _ -> Plain.Nil
              | Types.Cons (_, x1, x2) -> Plain.Cons ((a x1), (list a x2))
          end
        module Of_plain =
          struct
            let rec list a =
              function
              | Plain.Nil -> Types.Nil ()
              | Plain.Cons (x1, x2) -> Types.Cons ((), (a x1), (list a x2))
          end
        module Map_info =
          struct
            let rec list a ~f  =
              function
              | Types.Nil x0 -> Types.Nil (f x0)
              | Types.Cons (x0, x1, x2) ->
                  Types.Cons ((f x0), (a ~f x1), (list a ~f x2))
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
    module Plain = Wrapper.Plain
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals = [];
          sort_defs =
            [("list",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([("a", None)],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       (((let open Lvca_provenance.Commented in
                            {
                              range =
                                (Some
                                   (let open Lvca_provenance.Range in
                                      { start = 10; finish = 15 }));
                              comment = None
                            })), "Nil",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            (((let open Lvca_provenance.Commented in
                                 {
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 13; finish = 15 }));
                                   comment = None
                                 })), [])));
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 18; finish = 33 }));
                             comment = None
                           })), "Cons",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 22; finish = 33 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    { start = 23; finish = 24
                                                    }));
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
                                                   { start = 26; finish = 30
                                                   }));
                                           comment = None
                                         })), "list",
                                      (Lvca_syntax.Sort.Cons
                                         (((let open Lvca_provenance.Commented in
                                              {
                                                range =
                                                  (Some
                                                     (let open Lvca_provenance.Range in
                                                        {
                                                          start = 26;
                                                          finish = 30
                                                        }));
                                                comment = None
                                              })),
                                           (Lvca_syntax.Sort.Name
                                              (((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 31;
                                                               finish = 32
                                                             }));
                                                     comment = None
                                                   })), "a")),
                                           (Lvca_syntax.Sort.Nil
                                              ((let open Lvca_provenance.Commented in
                                                  {
                                                    range =
                                                      (Some
                                                         (let open Lvca_provenance.Range in
                                                            {
                                                              start = 26;
                                                              finish = 30
                                                            }));
                                                    comment = None
                                                  }))))))))])))])))]
        }
    module List =
      struct
        type ('info, 'a) t = ('info, 'a) Wrapper.Types.list =
          | Nil of 'info 
          | Cons of 'info * 'a * ('info, 'a) Wrapper.Types.list 
        let info = Wrapper.Info.list
        let to_plain = Wrapper.To_plain.list
        let of_plain = Wrapper.Of_plain.list
        let map_info = Wrapper.Map_info.list
        let to_nominal = Wrapper.To_nominal.list
        let of_nominal = Wrapper.Of_nominal.list
        module Plain =
          struct
            type 'a t = 'a Wrapper.Plain.list =
              | Nil 
              | Cons of 'a * 'a Wrapper.Plain.list 
          end
      end
  end 
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
              | Operator of 'info * ('info, 'info term) List_model.List.t 
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
              | Nonempty of 'info * 'info Primitive.String.t * ('info,
              'info Primitive.String.t) List_model.List.t 
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
              | Operator of term List_model.List.Plain.t 
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
              Primitive.String.Plain.t List_model.List.Plain.t 
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
                      (List_model.List.to_plain Primitive.String.to_plain x2))
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
            let rec term =
              function
              | Types.Operator (_, x1) ->
                  Plain.Operator (List_model.List.to_plain term x1)
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
                      (List_model.List.of_plain Primitive.String.of_plain x2))
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
            let rec term =
              function
              | Plain.Operator x1 ->
                  Types.Operator ((), (List_model.List.of_plain term x1))
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
                      (List_model.List.map_info Primitive.String.map_info ~f
                         x2))
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
            let rec term ~f  =
              function
              | Types.Operator (x0, x1) ->
                  Types.Operator
                    ((f x0), (List_model.List.map_info term ~f x1))
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
                      Lvca_syntax.Nominal.Scope.Scope
                        ([],
                          (List_model.List.to_nominal
                             Primitive.String.to_nominal x2))])
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
            let rec term =
              function
              | Types.Operator (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Operator",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (List_model.List.to_nominal term x1))])
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
                   | Ok x1 ->
                       (match List_model.List.of_nominal
                                Primitive.String.of_nominal x2
                        with
                        | Error msg -> Error msg
                        | Ok x2 -> Ok (Types.Nonempty (x0, x1, x2))))
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
            let rec term =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Operator", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match List_model.List.of_nominal term x1 with
                   | Error msg -> Error msg
                   | Ok x1 -> Ok (Types.Operator (x0, x1)))
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
                                { start = 60; finish = 66 }));
                        comment = None
                      })), 2)));
            ("list",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 (((let open Lvca_provenance.Commented in
                      {
                        range =
                          (Some
                             (let open Lvca_provenance.Range in
                                { start = 74; finish = 80 }));
                        comment = (Some " module List_model.List")
                      })), 2)))];
          sort_defs =
            [("foo",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       (((let open Lvca_provenance.Commented in
                            {
                              range =
                                (Some
                                   (let open Lvca_provenance.Range in
                                      { start = 120; finish = 132 }));
                              comment = None
                            })), "Foo",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            (((let open Lvca_provenance.Commented in
                                 {
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 123; finish = 132 }));
                                   comment = None
                                 })),
                              [Lvca_syntax.Abstract_syntax.Valence.Valence
                                 ([],
                                   (Lvca_syntax.Sort.Name
                                      (((let open Lvca_provenance.Commented in
                                           {
                                             range =
                                               (Some
                                                  (let open Lvca_provenance.Range in
                                                     {
                                                       start = 124;
                                                       finish = 131
                                                     }));
                                             comment = None
                                           })), "integer")))])));
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 137; finish = 160 }));
                             comment = None
                           })), "Bar",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 140; finish = 160 }));
                                  comment = None
                                })),
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
                                                            start = 141;
                                                            finish = 144
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
                                                            start = 145;
                                                            finish = 148
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
                                                     {
                                                       start = 151;
                                                       finish = 154
                                                     }));
                                             comment = None
                                           })), "foo"))],
                                  (Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 156;
                                                      finish = 159
                                                    }));
                                            comment = None
                                          })), "foo")))])))])));
            ("nat",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 169; finish = 172 }));
                             comment = None
                           })), "Z",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 170; finish = 172 }));
                                  comment = None
                                })), [])));
                   Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                     (((let open Lvca_provenance.Commented in
                          {
                            range =
                              (Some
                                 (let open Lvca_provenance.Range in
                                    { start = 175; finish = 181 }));
                            comment = None
                          })), "S",
                       (Lvca_syntax.Abstract_syntax.Arity.Arity
                          (((let open Lvca_provenance.Commented in
                               {
                                 range =
                                   (Some
                                      (let open Lvca_provenance.Range in
                                         { start = 176; finish = 181 }));
                                 comment = None
                               })),
                            [Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Name
                                    (((let open Lvca_provenance.Commented in
                                         {
                                           range =
                                             (Some
                                                (let open Lvca_provenance.Range in
                                                   {
                                                     start = 177;
                                                     finish = 180
                                                   }));
                                           comment = None
                                         })), "nat")))])))])));
            ("pair",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None); ("b", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 195; finish = 205 }));
                             comment = None
                           })), "Pair",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 199; finish = 205 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 200;
                                                      finish = 201
                                                    }));
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
                                                   {
                                                     start = 203;
                                                     finish = 204
                                                   }));
                                           comment = None
                                         })), "b")))])))])));
            ("pair_plus",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None); ("b", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 223; finish = 242 }));
                             comment = None
                           })), "PairPlus",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 231; finish = 242 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 232;
                                                      finish = 233
                                                    }));
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
                                                   {
                                                     start = 235;
                                                     finish = 236
                                                   }));
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
                                                   {
                                                     start = 238;
                                                     finish = 241
                                                   }));
                                           comment = None
                                         })), "foo")))])))])));
            ("nonempty",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 256; finish = 285 }));
                             comment = None
                           })), "Nonempty",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 264; finish = 285 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 265;
                                                      finish = 271
                                                    }));
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
                                                   {
                                                     start = 273;
                                                     finish = 277
                                                   }));
                                           comment = None
                                         })), "list",
                                      (Lvca_syntax.Sort.Cons
                                         (((let open Lvca_provenance.Commented in
                                              {
                                                range =
                                                  (Some
                                                     (let open Lvca_provenance.Range in
                                                        {
                                                          start = 273;
                                                          finish = 277
                                                        }));
                                                comment = None
                                              })),
                                           (Lvca_syntax.Sort.Name
                                              (((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 278;
                                                               finish = 284
                                                             }));
                                                     comment = None
                                                   })), "string")),
                                           (Lvca_syntax.Sort.Nil
                                              ((let open Lvca_provenance.Commented in
                                                  {
                                                    range =
                                                      (Some
                                                         (let open Lvca_provenance.Range in
                                                            {
                                                              start = 273;
                                                              finish = 277
                                                            }));
                                                    comment = None
                                                  }))))))))])))])));
            ("term",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 295; finish = 314 }));
                             comment = None
                           })), "Operator",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 303; finish = 314 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 304;
                                                      finish = 308
                                                    }));
                                            comment = None
                                          })), "list",
                                       (Lvca_syntax.Sort.Cons
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 304;
                                                           finish = 308
                                                         }));
                                                 comment = None
                                               })),
                                            (Lvca_syntax.Sort.Name
                                               (((let open Lvca_provenance.Commented in
                                                    {
                                                      range =
                                                        (Some
                                                           (let open Lvca_provenance.Range in
                                                              {
                                                                start = 309;
                                                                finish = 313
                                                              }));
                                                      comment = None
                                                    })), "term")),
                                            (Lvca_syntax.Sort.Nil
                                               ((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 304;
                                                               finish = 308
                                                             }));
                                                     comment = None
                                                   }))))))))])))])));
            ("mut_a",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 325; finish = 337 }));
                             comment = None
                           })), "Mut_a",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 330; finish = 337 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 331;
                                                      finish = 336
                                                    }));
                                            comment = None
                                          })), "mut_b")))])))])));
            ("mut_b",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 347; finish = 359 }));
                             comment = None
                           })), "Mut_b",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 352; finish = 359 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 353;
                                                      finish = 358
                                                    }));
                                            comment = None
                                          })), "mut_a")))])))])));
            ("ifz",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 367; finish = 390 }));
                             comment = None
                           })), "Ifz",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 370; finish = 390 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 371;
                                                      finish = 374
                                                    }));
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
                                                     {
                                                       start = 376;
                                                       finish = 379
                                                     }));
                                             comment = None
                                           })), "ifz"))],
                                 (Lvca_syntax.Sort.Name
                                    (((let open Lvca_provenance.Commented in
                                         {
                                           range =
                                             (Some
                                                (let open Lvca_provenance.Range in
                                                   {
                                                     start = 381;
                                                     finish = 384
                                                   }));
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
                                                   {
                                                     start = 386;
                                                     finish = 389
                                                   }));
                                           comment = None
                                         })), "ifz")))])))])))]
        }
    module Foo =
      struct
        type 'info t = 'info Wrapper.Types.foo =
          | Foo of 'info * 'info Lvca_syntax.Nominal.Term.t 
          | Bar of 'info * ('info Pattern.t * 'info Lvca_syntax.Single_var.t
          * 'info Wrapper.Types.foo) 
          | Foo_var of 'info * string 
        let info = Wrapper.Info.foo
        let to_plain = Wrapper.To_plain.foo
        let of_plain = Wrapper.Of_plain.foo
        let map_info = Wrapper.Map_info.foo
        let to_nominal = Wrapper.To_nominal.foo
        let of_nominal = Wrapper.Of_nominal.foo
        module Plain =
          struct
            type t = Wrapper.Plain.foo =
              | Foo of Lvca_syntax.Nominal.Term.Plain.t 
              | Bar of (Pattern.Plain.t * Lvca_syntax.Single_var.Plain.t *
              Wrapper.Plain.foo) 
              | Foo_var of string 
          end
      end
    module Nat =
      struct
        type 'info t = 'info Wrapper.Types.nat =
          | Z of 'info 
          | S of 'info * 'info Wrapper.Types.nat 
        let info = Wrapper.Info.nat
        let to_plain = Wrapper.To_plain.nat
        let of_plain = Wrapper.Of_plain.nat
        let map_info = Wrapper.Map_info.nat
        let to_nominal = Wrapper.To_nominal.nat
        let of_nominal = Wrapper.Of_nominal.nat
        module Plain =
          struct
            type t = Wrapper.Plain.nat =
              | Z 
              | S of Wrapper.Plain.nat 
          end
      end
    module Pair =
      struct
        type ('info, 'a, 'b) t = ('info, 'a, 'b) Wrapper.Types.pair =
          | Pair of 'info * 'a * 'b 
        let info = Wrapper.Info.pair
        let to_plain = Wrapper.To_plain.pair
        let of_plain = Wrapper.Of_plain.pair
        let map_info = Wrapper.Map_info.pair
        let to_nominal = Wrapper.To_nominal.pair
        let of_nominal = Wrapper.Of_nominal.pair
        module Plain =
          struct
            type ('a, 'b) t = ('a, 'b) Wrapper.Plain.pair =
              | Pair of 'a * 'b 
          end
      end
    module Pair_plus =
      struct
        type ('info, 'a, 'b) t = ('info, 'a, 'b) Wrapper.Types.pair_plus =
          | PairPlus of 'info * 'a * 'b * 'info Wrapper.Types.foo 
        let info = Wrapper.Info.pair_plus
        let to_plain = Wrapper.To_plain.pair_plus
        let of_plain = Wrapper.Of_plain.pair_plus
        let map_info = Wrapper.Map_info.pair_plus
        let to_nominal = Wrapper.To_nominal.pair_plus
        let of_nominal = Wrapper.Of_nominal.pair_plus
        module Plain =
          struct
            type ('a, 'b) t = ('a, 'b) Wrapper.Plain.pair_plus =
              | PairPlus of 'a * 'b * Wrapper.Plain.foo 
          end
      end
    module Nonempty =
      struct
        type 'info t = 'info Wrapper.Types.nonempty =
          | Nonempty of 'info * 'info Primitive.String.t * ('info,
          'info Primitive.String.t) List_model.List.t 
        let info = Wrapper.Info.nonempty
        let to_plain = Wrapper.To_plain.nonempty
        let of_plain = Wrapper.Of_plain.nonempty
        let map_info = Wrapper.Map_info.nonempty
        let to_nominal = Wrapper.To_nominal.nonempty
        let of_nominal = Wrapper.Of_nominal.nonempty
        module Plain =
          struct
            type t = Wrapper.Plain.nonempty =
              | Nonempty of Primitive.String.Plain.t *
              Primitive.String.Plain.t List_model.List.Plain.t 
          end
      end
    module Term =
      struct
        type 'info t = 'info Wrapper.Types.term =
          | Operator of 'info * ('info, 'info Wrapper.Types.term)
          List_model.List.t 
        let info = Wrapper.Info.term
        let to_plain = Wrapper.To_plain.term
        let of_plain = Wrapper.Of_plain.term
        let map_info = Wrapper.Map_info.term
        let to_nominal = Wrapper.To_nominal.term
        let of_nominal = Wrapper.Of_nominal.term
        module Plain =
          struct
            type t = Wrapper.Plain.term =
              | Operator of Wrapper.Plain.term List_model.List.Plain.t 
          end
      end
    module Mut_a =
      struct
        type 'info t = 'info Wrapper.Types.mut_a =
          | Mut_a of 'info * 'info Wrapper.Types.mut_b 
        let info = Wrapper.Info.mut_a
        let to_plain = Wrapper.To_plain.mut_a
        let of_plain = Wrapper.Of_plain.mut_a
        let map_info = Wrapper.Map_info.mut_a
        let to_nominal = Wrapper.To_nominal.mut_a
        let of_nominal = Wrapper.Of_nominal.mut_a
        module Plain =
          struct
            type t = Wrapper.Plain.mut_a =
              | Mut_a of Wrapper.Plain.mut_b 
          end
      end
    module Mut_b =
      struct
        type 'info t = 'info Wrapper.Types.mut_b =
          | Mut_b of 'info * 'info Wrapper.Types.mut_a 
        let info = Wrapper.Info.mut_b
        let to_plain = Wrapper.To_plain.mut_b
        let of_plain = Wrapper.Of_plain.mut_b
        let map_info = Wrapper.Map_info.mut_b
        let to_nominal = Wrapper.To_nominal.mut_b
        let of_nominal = Wrapper.Of_nominal.mut_b
        module Plain =
          struct
            type t = Wrapper.Plain.mut_b =
              | Mut_b of Wrapper.Plain.mut_a 
          end
      end
    module Ifz =
      struct
        type 'info t = 'info Wrapper.Types.ifz =
          | Ifz of 'info * 'info Wrapper.Types.ifz * ('info
          Lvca_syntax.Single_var.t * 'info Wrapper.Types.ifz) * 'info
          Wrapper.Types.ifz 
          | Ifz_var of 'info * string 
        let info = Wrapper.Info.ifz
        let to_plain = Wrapper.To_plain.ifz
        let of_plain = Wrapper.Of_plain.ifz
        let map_info = Wrapper.Map_info.ifz
        let to_nominal = Wrapper.To_nominal.ifz
        let of_nominal = Wrapper.Of_nominal.ifz
        module Plain =
          struct
            type t = Wrapper.Plain.ifz =
              | Ifz of Wrapper.Plain.ifz * (Lvca_syntax.Single_var.Plain.t *
              Wrapper.Plain.ifz) * Wrapper.Plain.ifz 
              | Ifz_var of string 
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
                       (((let open Lvca_provenance.Commented in
                            {
                              range =
                                (Some
                                   (let open Lvca_provenance.Range in
                                      { start = 27; finish = 39 }));
                              comment = None
                            })), "Predefined",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            (((let open Lvca_provenance.Commented in
                                 {
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 37; finish = 39 }));
                                   comment = None
                                 })), [])))])));
            ("list",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 50; finish = 55 }));
                             comment = None
                           })), "Nil",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 53; finish = 55 }));
                                  comment = None
                                })), [])));
                   Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                     (((let open Lvca_provenance.Commented in
                          {
                            range =
                              (Some
                                 (let open Lvca_provenance.Range in
                                    { start = 58; finish = 73 }));
                            comment = None
                          })), "Cons",
                       (Lvca_syntax.Abstract_syntax.Arity.Arity
                          (((let open Lvca_provenance.Commented in
                               {
                                 range =
                                   (Some
                                      (let open Lvca_provenance.Range in
                                         { start = 62; finish = 73 }));
                                 comment = None
                               })),
                            [Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Name
                                    (((let open Lvca_provenance.Commented in
                                         {
                                           range =
                                             (Some
                                                (let open Lvca_provenance.Range in
                                                   { start = 63; finish = 64
                                                   }));
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
                                     (Lvca_syntax.Sort.Cons
                                        (((let open Lvca_provenance.Commented in
                                             {
                                               range =
                                                 (Some
                                                    (let open Lvca_provenance.Range in
                                                       {
                                                         start = 66;
                                                         finish = 70
                                                       }));
                                               comment = None
                                             })),
                                          (Lvca_syntax.Sort.Name
                                             (((let open Lvca_provenance.Commented in
                                                  {
                                                    range =
                                                      (Some
                                                         (let open Lvca_provenance.Range in
                                                            {
                                                              start = 71;
                                                              finish = 72
                                                            }));
                                                    comment = None
                                                  })), "a")),
                                          (Lvca_syntax.Sort.Nil
                                             ((let open Lvca_provenance.Commented in
                                                 {
                                                   range =
                                                     (Some
                                                        (let open Lvca_provenance.Range in
                                                           {
                                                             start = 66;
                                                             finish = 70
                                                           }));
                                                   comment = None
                                                 }))))))))])))])));
            ("list_external",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 91; finish = 117 }));
                             comment = None
                           })), "List_external",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 104; finish = 117 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 105;
                                                      finish = 109
                                                    }));
                                            comment = None
                                          })), "list",
                                       (Lvca_syntax.Sort.Cons
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 105;
                                                           finish = 109
                                                         }));
                                                 comment = None
                                               })),
                                            (Lvca_syntax.Sort.Name
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
                                                    })), "string")),
                                            (Lvca_syntax.Sort.Nil
                                               ((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 105;
                                                               finish = 109
                                                             }));
                                                     comment = None
                                                   }))))))))])))])));
            ("list_predefined",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 137; finish = 169 }));
                             comment = None
                           })), "List_predefined",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 152; finish = 169 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 153;
                                                      finish = 157
                                                    }));
                                            comment = None
                                          })), "list",
                                       (Lvca_syntax.Sort.Cons
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 153;
                                                           finish = 157
                                                         }));
                                                 comment = None
                                               })),
                                            (Lvca_syntax.Sort.Name
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
                                                    })), "predefined")),
                                            (Lvca_syntax.Sort.Nil
                                               ((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 153;
                                                               finish = 157
                                                             }));
                                                     comment = None
                                                   }))))))))])))])));
            ("list_list_a",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 188; finish = 214 }));
                             comment = None
                           })), "List_list_a",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 199; finish = 214 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 200;
                                                      finish = 204
                                                    }));
                                            comment = None
                                          })), "list",
                                       (Lvca_syntax.Sort.Cons
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 200;
                                                           finish = 204
                                                         }));
                                                 comment = None
                                               })),
                                            (Lvca_syntax.Sort.Ap
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
                                                 (Lvca_syntax.Sort.Cons
                                                    (((let open Lvca_provenance.Commented in
                                                         {
                                                           range =
                                                             (Some
                                                                (let open Lvca_provenance.Range in
                                                                   {
                                                                    start =
                                                                    206;
                                                                    finish =
                                                                    210
                                                                   }));
                                                           comment = None
                                                         })),
                                                      (Lvca_syntax.Sort.Name
                                                         (((let open Lvca_provenance.Commented in
                                                              {
                                                                range =
                                                                  (Some
                                                                    (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    211;
                                                                    finish =
                                                                    212
                                                                    }));
                                                                comment =
                                                                  None
                                                              })), "a")),
                                                      (Lvca_syntax.Sort.Nil
                                                         ((let open Lvca_provenance.Commented in
                                                             {
                                                               range =
                                                                 (Some
                                                                    (
                                                                    let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    206;
                                                                    finish =
                                                                    210
                                                                    }));
                                                               comment = None
                                                             }))))))),
                                            (Lvca_syntax.Sort.Nil
                                               ((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 200;
                                                               finish = 204
                                                             }));
                                                     comment = None
                                                   }))))))))])))])));
            ("list_list_string_1",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 237; finish = 275 }));
                             comment = None
                           })), "List_list_string_1",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 255; finish = 275 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 256;
                                                      finish = 260
                                                    }));
                                            comment = None
                                          })), "list",
                                       (Lvca_syntax.Sort.Cons
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 256;
                                                           finish = 260
                                                         }));
                                                 comment = None
                                               })),
                                            (Lvca_syntax.Sort.Ap
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
                                                 (Lvca_syntax.Sort.Cons
                                                    (((let open Lvca_provenance.Commented in
                                                         {
                                                           range =
                                                             (Some
                                                                (let open Lvca_provenance.Range in
                                                                   {
                                                                    start =
                                                                    262;
                                                                    finish =
                                                                    266
                                                                   }));
                                                           comment = None
                                                         })),
                                                      (Lvca_syntax.Sort.Name
                                                         (((let open Lvca_provenance.Commented in
                                                              {
                                                                range =
                                                                  (Some
                                                                    (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    267;
                                                                    finish =
                                                                    273
                                                                    }));
                                                                comment =
                                                                  None
                                                              })), "string")),
                                                      (Lvca_syntax.Sort.Nil
                                                         ((let open Lvca_provenance.Commented in
                                                             {
                                                               range =
                                                                 (Some
                                                                    (
                                                                    let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    262;
                                                                    finish =
                                                                    266
                                                                    }));
                                                               comment = None
                                                             }))))))),
                                            (Lvca_syntax.Sort.Nil
                                               ((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 256;
                                                               finish = 260
                                                             }));
                                                     comment = None
                                                   }))))))))])))])));
            ("list_list_string_2",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 298; finish = 336 }));
                             comment = None
                           })), "List_list_string_2",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 316; finish = 336 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 317;
                                                      finish = 328
                                                    }));
                                            comment = None
                                          })), "list_list_a",
                                       (Lvca_syntax.Sort.Cons
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 317;
                                                           finish = 328
                                                         }));
                                                 comment = None
                                               })),
                                            (Lvca_syntax.Sort.Name
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
                                                    })), "string")),
                                            (Lvca_syntax.Sort.Nil
                                               ((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 317;
                                                               finish = 328
                                                             }));
                                                     comment = None
                                                   }))))))))])))])));
            ("list_list_predefined_1",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 363; finish = 409 }));
                             comment = None
                           })), "List_list_predefined_1",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 385; finish = 409 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 386;
                                                      finish = 390
                                                    }));
                                            comment = None
                                          })), "list",
                                       (Lvca_syntax.Sort.Cons
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 386;
                                                           finish = 390
                                                         }));
                                                 comment = None
                                               })),
                                            (Lvca_syntax.Sort.Ap
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
                                                 (Lvca_syntax.Sort.Cons
                                                    (((let open Lvca_provenance.Commented in
                                                         {
                                                           range =
                                                             (Some
                                                                (let open Lvca_provenance.Range in
                                                                   {
                                                                    start =
                                                                    392;
                                                                    finish =
                                                                    396
                                                                   }));
                                                           comment = None
                                                         })),
                                                      (Lvca_syntax.Sort.Name
                                                         (((let open Lvca_provenance.Commented in
                                                              {
                                                                range =
                                                                  (Some
                                                                    (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    397;
                                                                    finish =
                                                                    407
                                                                    }));
                                                                comment =
                                                                  None
                                                              })),
                                                           "predefined")),
                                                      (Lvca_syntax.Sort.Nil
                                                         ((let open Lvca_provenance.Commented in
                                                             {
                                                               range =
                                                                 (Some
                                                                    (
                                                                    let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    392;
                                                                    finish =
                                                                    396
                                                                    }));
                                                               comment = None
                                                             }))))))),
                                            (Lvca_syntax.Sort.Nil
                                               ((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 386;
                                                               finish = 390
                                                             }));
                                                     comment = None
                                                   }))))))))])))])));
            ("list_list_predefined_2",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      (((let open Lvca_provenance.Commented in
                           {
                             range =
                               (Some
                                  (let open Lvca_provenance.Range in
                                     { start = 436; finish = 482 }));
                             comment = None
                           })), "List_list_predefined_2",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           (((let open Lvca_provenance.Commented in
                                {
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 458; finish = 482 }));
                                  comment = None
                                })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     (((let open Lvca_provenance.Commented in
                                          {
                                            range =
                                              (Some
                                                 (let open Lvca_provenance.Range in
                                                    {
                                                      start = 459;
                                                      finish = 470
                                                    }));
                                            comment = None
                                          })), "list_list_a",
                                       (Lvca_syntax.Sort.Cons
                                          (((let open Lvca_provenance.Commented in
                                               {
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 459;
                                                           finish = 470
                                                         }));
                                                 comment = None
                                               })),
                                            (Lvca_syntax.Sort.Name
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
                                                    })), "predefined")),
                                            (Lvca_syntax.Sort.Nil
                                               ((let open Lvca_provenance.Commented in
                                                   {
                                                     range =
                                                       (Some
                                                          (let open Lvca_provenance.Range in
                                                             {
                                                               start = 459;
                                                               finish = 470
                                                             }));
                                                     comment = None
                                                   }))))))))])))])))]
        }
    module Predefined =
      struct
        type 'info t = 'info Wrapper.Types.predefined =
          | Predefined of 'info 
        let info = Wrapper.Info.predefined
        let to_plain = Wrapper.To_plain.predefined
        let of_plain = Wrapper.Of_plain.predefined
        let map_info = Wrapper.Map_info.predefined
        let to_nominal = Wrapper.To_nominal.predefined
        let of_nominal = Wrapper.Of_nominal.predefined
        module Plain =
          struct type t = Wrapper.Plain.predefined =
                   | Predefined  end
      end
    module List =
      struct
        type ('info, 'a) t = ('info, 'a) Wrapper.Types.list =
          | Nil of 'info 
          | Cons of 'info * 'a * ('info, 'a) Wrapper.Types.list 
        let info = Wrapper.Info.list
        let to_plain = Wrapper.To_plain.list
        let of_plain = Wrapper.Of_plain.list
        let map_info = Wrapper.Map_info.list
        let to_nominal = Wrapper.To_nominal.list
        let of_nominal = Wrapper.Of_nominal.list
        module Plain =
          struct
            type 'a t = 'a Wrapper.Plain.list =
              | Nil 
              | Cons of 'a * 'a Wrapper.Plain.list 
          end
      end
    module List_external =
      struct
        type 'info t = 'info Wrapper.Types.list_external =
          | List_external of 'info * ('info,
          'info Lvca_syntax.Nominal.Term.t) Wrapper.Types.list 
        let info = Wrapper.Info.list_external
        let to_plain = Wrapper.To_plain.list_external
        let of_plain = Wrapper.Of_plain.list_external
        let map_info = Wrapper.Map_info.list_external
        let to_nominal = Wrapper.To_nominal.list_external
        let of_nominal = Wrapper.Of_nominal.list_external
        module Plain =
          struct
            type t = Wrapper.Plain.list_external =
              | List_external of Lvca_syntax.Nominal.Term.Plain.t
              Wrapper.Plain.list 
          end
      end
    module List_predefined =
      struct
        type 'info t = 'info Wrapper.Types.list_predefined =
          | List_predefined of 'info * ('info,
          'info Wrapper.Types.predefined) Wrapper.Types.list 
        let info = Wrapper.Info.list_predefined
        let to_plain = Wrapper.To_plain.list_predefined
        let of_plain = Wrapper.Of_plain.list_predefined
        let map_info = Wrapper.Map_info.list_predefined
        let to_nominal = Wrapper.To_nominal.list_predefined
        let of_nominal = Wrapper.Of_nominal.list_predefined
        module Plain =
          struct
            type t = Wrapper.Plain.list_predefined =
              | List_predefined of Wrapper.Plain.predefined
              Wrapper.Plain.list 
          end
      end
    module List_list_a =
      struct
        type ('info, 'a) t = ('info, 'a) Wrapper.Types.list_list_a =
          | List_list_a of 'info * ('info, ('info, 'a) Wrapper.Types.list)
          Wrapper.Types.list 
        let info = Wrapper.Info.list_list_a
        let to_plain = Wrapper.To_plain.list_list_a
        let of_plain = Wrapper.Of_plain.list_list_a
        let map_info = Wrapper.Map_info.list_list_a
        let to_nominal = Wrapper.To_nominal.list_list_a
        let of_nominal = Wrapper.Of_nominal.list_list_a
        module Plain =
          struct
            type 'a t = 'a Wrapper.Plain.list_list_a =
              | List_list_a of 'a Wrapper.Plain.list Wrapper.Plain.list 
          end
      end
    module List_list_string_1 =
      struct
        type 'info t = 'info Wrapper.Types.list_list_string_1 =
          | List_list_string_1 of 'info * ('info,
          ('info, 'info Lvca_syntax.Nominal.Term.t) Wrapper.Types.list)
          Wrapper.Types.list 
        let info = Wrapper.Info.list_list_string_1
        let to_plain = Wrapper.To_plain.list_list_string_1
        let of_plain = Wrapper.Of_plain.list_list_string_1
        let map_info = Wrapper.Map_info.list_list_string_1
        let to_nominal = Wrapper.To_nominal.list_list_string_1
        let of_nominal = Wrapper.Of_nominal.list_list_string_1
        module Plain =
          struct
            type t = Wrapper.Plain.list_list_string_1 =
              | List_list_string_1 of Lvca_syntax.Nominal.Term.Plain.t
              Wrapper.Plain.list Wrapper.Plain.list 
          end
      end
    module List_list_string_2 =
      struct
        type 'info t = 'info Wrapper.Types.list_list_string_2 =
          | List_list_string_2 of 'info * ('info,
          'info Lvca_syntax.Nominal.Term.t) Wrapper.Types.list_list_a 
        let info = Wrapper.Info.list_list_string_2
        let to_plain = Wrapper.To_plain.list_list_string_2
        let of_plain = Wrapper.Of_plain.list_list_string_2
        let map_info = Wrapper.Map_info.list_list_string_2
        let to_nominal = Wrapper.To_nominal.list_list_string_2
        let of_nominal = Wrapper.Of_nominal.list_list_string_2
        module Plain =
          struct
            type t = Wrapper.Plain.list_list_string_2 =
              | List_list_string_2 of Lvca_syntax.Nominal.Term.Plain.t
              Wrapper.Plain.list_list_a 
          end
      end
    module List_list_predefined_1 =
      struct
        type 'info t = 'info Wrapper.Types.list_list_predefined_1 =
          | List_list_predefined_1 of 'info * ('info,
          ('info, 'info Wrapper.Types.predefined) Wrapper.Types.list)
          Wrapper.Types.list 
        let info = Wrapper.Info.list_list_predefined_1
        let to_plain = Wrapper.To_plain.list_list_predefined_1
        let of_plain = Wrapper.Of_plain.list_list_predefined_1
        let map_info = Wrapper.Map_info.list_list_predefined_1
        let to_nominal = Wrapper.To_nominal.list_list_predefined_1
        let of_nominal = Wrapper.Of_nominal.list_list_predefined_1
        module Plain =
          struct
            type t = Wrapper.Plain.list_list_predefined_1 =
              | List_list_predefined_1 of Wrapper.Plain.predefined
              Wrapper.Plain.list Wrapper.Plain.list 
          end
      end
    module List_list_predefined_2 =
      struct
        type 'info t = 'info Wrapper.Types.list_list_predefined_2 =
          | List_list_predefined_2 of 'info * ('info,
          'info Wrapper.Types.predefined) Wrapper.Types.list_list_a 
        let info = Wrapper.Info.list_list_predefined_2
        let to_plain = Wrapper.To_plain.list_list_predefined_2
        let of_plain = Wrapper.Of_plain.list_list_predefined_2
        let map_info = Wrapper.Map_info.list_list_predefined_2
        let to_nominal = Wrapper.To_nominal.list_list_predefined_2
        let of_nominal = Wrapper.Of_nominal.list_list_predefined_2
        module Plain =
          struct
            type t = Wrapper.Plain.list_list_predefined_2 =
              | List_list_predefined_2 of Wrapper.Plain.predefined
              Wrapper.Plain.list_list_a 
          end
      end
  end
module type Is_rec_sig  =
  sig
    val language :
      string Lvca_provenance.Commented.t Lvca_syntax.Abstract_syntax.t
    module Wrapper :
    sig
      module Types :
      sig
        type 'info ty =
          | Sort of 'info * 'info Lvca_syntax.Nominal.Term.t 
          | Arrow of 'info * 'info ty * 'info ty 
        and 'info mut_a =
          | Mut_a of 'info * 'info mut_b 
        and 'info mut_b =
          | Mut_b of 'info * 'info mut_a 
        and 'info is_rec =
          | Rec of 'info 
          | No_rec of 'info 
      end
      module Plain :
      sig
        type ty =
          | Sort of Lvca_syntax.Nominal.Term.Plain.t 
          | Arrow of ty * ty 
        and mut_a =
          | Mut_a of mut_b 
        and mut_b =
          | Mut_b of mut_a 
        and is_rec =
          | Rec 
          | No_rec 
      end
    end
    module Is_rec :
    sig
      type 'info t =
        | Rec of 'info 
        | No_rec of 'info 
      module Plain : sig type t =
                           | Rec 
                           | No_rec  end
      val to_plain : _ t -> Plain.t
      val of_plain : Plain.t -> unit t
      val to_nominal : 'infoa t -> 'infoa Lvca_syntax.Nominal.Term.t
      val of_nominal :
        'infoa Lvca_syntax.Nominal.Term.t ->
          ('infoa t, 'infoa Lvca_syntax.Nominal.Term.t) Result.t
      val info : 'info t -> 'info
      val map_info : f:('infoa -> 'infob) -> 'infoa t -> 'infob t
    end
    module Ty :
    sig
      type 'info t =
        | Sort of 'info * 'info Lvca_syntax.Nominal.Term.t 
        | Arrow of 'info * 'info Wrapper.Types.ty * 'info Wrapper.Types.ty 
      module Plain :
      sig
        type t =
          | Sort of Lvca_syntax.Nominal.Term.Plain.t 
          | Arrow of Wrapper.Plain.ty * Wrapper.Plain.ty 
      end
      val to_plain : _ t -> Plain.t
      val of_plain : Plain.t -> unit t
      val to_nominal : 'infoa t -> 'infoa Lvca_syntax.Nominal.Term.t
      val of_nominal :
        'infoa Lvca_syntax.Nominal.Term.t ->
          ('infoa t, 'infoa Lvca_syntax.Nominal.Term.t) Result.t
      val info : 'info t -> 'info
      val map_info : f:('infoa -> 'infob) -> 'infoa t -> 'infob t
    end
    module Mut_a :
    sig
      type 'info t =
        | Mut_a of 'info * 'info Wrapper.Types.mut_b 
      module Plain : sig type t =
                           | Mut_a of Wrapper.Plain.mut_b  end
      val to_plain : _ t -> Plain.t
      val of_plain : Plain.t -> unit t
      val to_nominal : 'infoa t -> 'infoa Lvca_syntax.Nominal.Term.t
      val of_nominal :
        'infoa Lvca_syntax.Nominal.Term.t ->
          ('infoa t, 'infoa Lvca_syntax.Nominal.Term.t) Result.t
      val info : 'info t -> 'info
      val map_info : f:('infoa -> 'infob) -> 'infoa t -> 'infob t
    end
    module Mut_b :
    sig
      type 'info t =
        | Mut_b of 'info * 'info Wrapper.Types.mut_a 
      module Plain : sig type t =
                           | Mut_b of Wrapper.Plain.mut_a  end
      val to_plain : _ t -> Plain.t
      val of_plain : Plain.t -> unit t
      val to_nominal : 'infoa t -> 'infoa Lvca_syntax.Nominal.Term.t
      val of_nominal :
        'infoa Lvca_syntax.Nominal.Term.t ->
          ('infoa t, 'infoa Lvca_syntax.Nominal.Term.t) Result.t
      val info : 'info t -> 'info
      val map_info : f:('infoa -> 'infob) -> 'infoa t -> 'infob t
    end
  end
