open Lvca_syntax
let test_nominal =
  Lvca_syntax.Nominal.Term.Operator
    ((Some (let open Lvca_provenance.Range in { start = 0; finish = 9 })),
      "foo",
      [Lvca_syntax.Nominal.Scope.Scope
         ([Lvca_syntax.Pattern.Var
             ((Some
                 ((let open Lvca_provenance.Range in
                     { start = 4; finish = 5 }))), "x")],
           (Lvca_syntax.Nominal.Term.Var
              ((Some
                  ((let open Lvca_provenance.Range in
                      { start = 7; finish = 8 }))), "x")))])
let test_nonbinding =
  Lvca_syntax.Nonbinding.Operator
    ((Some (let open Lvca_provenance.Range in { start = 0; finish = 11 })),
      "foo",
      [Lvca_syntax.Nonbinding.Operator
         ((Some
             ((let open Lvca_provenance.Range in { start = 4; finish = 10 }))),
           "bar",
           [Lvca_syntax.Nonbinding.Primitive
              ((Some
                  ((let open Lvca_provenance.Range in
                      { start = 8; finish = 9 }))),
                (Lvca_syntax.Primitive_impl.Plain.Integer (Z.of_string "1")))])])
let test_pattern =
  Lvca_syntax.Pattern.Operator
    ((Some (let open Lvca_provenance.Range in { start = 0; finish = 6 })),
      "foo",
      [Lvca_syntax.Pattern.Var
         ((Some
             ((let open Lvca_provenance.Range in { start = 4; finish = 5 }))),
           "x")])
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
                             ((Some
                                 ((let open Lvca_provenance.Range in
                                     { start = 12; finish = 19 }))),
                               "integer")))])])))]
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
              | Operator of 'info * 'info Lvca_syntax.Nominal.Types.term 
            and ('info, 'a, 'b) pair_plus =
              | PairPlus of 'info * 'a * 'b * 'info foo 
            and 'info foo =
              | Foo of 'info * 'info Lvca_syntax.Nominal.Types.term 
              | Bar of 'info * ('info Pattern.t * 'info
              Lvca_syntax.Single_var.t * 'info foo) 
              | Foo_var of 'info * string 
            and ('info, 'a, 'b) pair =
              | Pair of 'info * 'a * 'b 
            and 'info nonempty =
              | Nonempty of 'info * 'info Lvca_syntax.Nominal.Types.term *
              'info Lvca_syntax.Nominal.Types.term 
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
              | Operator of Lvca_syntax.Nominal.Plain.term 
            and ('a, 'b) pair_plus =
              | PairPlus of 'a * 'b * foo 
            and foo =
              | Foo of Lvca_syntax.Nominal.Plain.term 
              | Bar of (Pattern.Plain.t * Lvca_syntax.Single_var.Plain.t *
              foo) 
              | Foo_var of string 
            and ('a, 'b) pair =
              | Pair of 'a * 'b 
            and nonempty =
              | Nonempty of Lvca_syntax.Nominal.Plain.term *
              Lvca_syntax.Nominal.Plain.term 
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
                    ((Lvca_syntax.Nominal.Term.to_plain x1),
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
                    ((), (Lvca_syntax.Nominal.Term.of_plain x1),
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
                    ((f x0), (Lvca_syntax.Nominal.Term.map_info ~f x1),
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
                      [Lvca_syntax.Nominal.Scope.Scope ([], x1);
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
    module Foo =
      struct
        type 'info t = 'info Wrapper.Types.foo
        module Plain = struct type t = Wrapper.Plain.foo end
        let info tm = Wrapper.Info.foo tm
        let to_plain tm = Wrapper.To_plain.foo tm
        let of_plain tm = Wrapper.Of_plain.foo tm
        let map_info ~f  tm = Wrapper.Map_info.foo ~f tm
        let to_nominal tm = Wrapper.To_nominal.foo tm
        let of_nominal tm = Wrapper.Of_nominal.foo tm
      end
    module Nat =
      struct
        type 'info t = 'info Wrapper.Types.nat
        module Plain = struct type t = Wrapper.Plain.nat end
        let info tm = Wrapper.Info.nat tm
        let to_plain tm = Wrapper.To_plain.nat tm
        let of_plain tm = Wrapper.Of_plain.nat tm
        let map_info ~f  tm = Wrapper.Map_info.nat ~f tm
        let to_nominal tm = Wrapper.To_nominal.nat tm
        let of_nominal tm = Wrapper.Of_nominal.nat tm
      end
    module Pair =
      struct
        type ('info, 'a, 'b) t = ('info, 'a, 'b) Wrapper.Types.pair
        module Plain =
          struct type ('a, 'b) t = ('a, 'b) Wrapper.Plain.pair end
        let info tm = Wrapper.Info.pair tm
        let to_plain tm = Wrapper.To_plain.pair tm
        let of_plain tm = Wrapper.Of_plain.pair tm
        let map_info ~f  tm = Wrapper.Map_info.pair ~f tm
        let to_nominal tm = Wrapper.To_nominal.pair tm
        let of_nominal tm = Wrapper.Of_nominal.pair tm
      end
    module Pair_plus =
      struct
        type ('info, 'a, 'b) t = ('info, 'a, 'b) Wrapper.Types.pair_plus
        module Plain =
          struct type ('a, 'b) t = ('a, 'b) Wrapper.Plain.pair_plus end
        let info tm = Wrapper.Info.pair_plus tm
        let to_plain tm = Wrapper.To_plain.pair_plus tm
        let of_plain tm = Wrapper.Of_plain.pair_plus tm
        let map_info ~f  tm = Wrapper.Map_info.pair_plus ~f tm
        let to_nominal tm = Wrapper.To_nominal.pair_plus tm
        let of_nominal tm = Wrapper.Of_nominal.pair_plus tm
      end
    module Nonempty =
      struct
        type 'info t = 'info Wrapper.Types.nonempty
        module Plain = struct type t = Wrapper.Plain.nonempty end
        let info tm = Wrapper.Info.nonempty tm
        let to_plain tm = Wrapper.To_plain.nonempty tm
        let of_plain tm = Wrapper.Of_plain.nonempty tm
        let map_info ~f  tm = Wrapper.Map_info.nonempty ~f tm
        let to_nominal tm = Wrapper.To_nominal.nonempty tm
        let of_nominal tm = Wrapper.Of_nominal.nonempty tm
      end
    module Term =
      struct
        type 'info t = 'info Wrapper.Types.term
        module Plain = struct type t = Wrapper.Plain.term end
        let info tm = Wrapper.Info.term tm
        let to_plain tm = Wrapper.To_plain.term tm
        let of_plain tm = Wrapper.Of_plain.term tm
        let map_info ~f  tm = Wrapper.Map_info.term ~f tm
        let to_nominal tm = Wrapper.To_nominal.term tm
        let of_nominal tm = Wrapper.Of_nominal.term tm
      end
    module Mut_a =
      struct
        type 'info t = 'info Wrapper.Types.mut_a
        module Plain = struct type t = Wrapper.Plain.mut_a end
        let info tm = Wrapper.Info.mut_a tm
        let to_plain tm = Wrapper.To_plain.mut_a tm
        let of_plain tm = Wrapper.Of_plain.mut_a tm
        let map_info ~f  tm = Wrapper.Map_info.mut_a ~f tm
        let to_nominal tm = Wrapper.To_nominal.mut_a tm
        let of_nominal tm = Wrapper.Of_nominal.mut_a tm
      end
    module Mut_b =
      struct
        type 'info t = 'info Wrapper.Types.mut_b
        module Plain = struct type t = Wrapper.Plain.mut_b end
        let info tm = Wrapper.Info.mut_b tm
        let to_plain tm = Wrapper.To_plain.mut_b tm
        let of_plain tm = Wrapper.Of_plain.mut_b tm
        let map_info ~f  tm = Wrapper.Map_info.mut_b ~f tm
        let to_nominal tm = Wrapper.To_nominal.mut_b tm
        let of_nominal tm = Wrapper.Of_nominal.mut_b tm
      end
    module Ifz =
      struct
        type 'info t = 'info Wrapper.Types.ifz
        module Plain = struct type t = Wrapper.Plain.ifz end
        let info tm = Wrapper.Info.ifz tm
        let to_plain tm = Wrapper.To_plain.ifz tm
        let of_plain tm = Wrapper.Of_plain.ifz tm
        let map_info ~f  tm = Wrapper.Map_info.ifz ~f tm
        let to_nominal tm = Wrapper.To_nominal.ifz tm
        let of_nominal tm = Wrapper.Of_nominal.ifz tm
      end
  end
