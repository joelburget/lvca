open Lvca_syntax
let test_nominal =
  Nominal.Term.Operator
    ((Some (let open Lvca_provenance.Range in { start = 0; finish = 9 })),
      "foo",
      [Nominal.Scope.Scope
         ([Pattern.Var
             ((Some
                 ((let open Lvca_provenance.Range in
                     { start = 4; finish = 5 }))), "x")],
           (Nominal.Term.Var
              ((Some
                  ((let open Lvca_provenance.Range in
                      { start = 7; finish = 8 }))), "x")))])
let test_nonbinding =
  Nonbinding.Operator
    ((Some (let open Lvca_provenance.Range in { start = 0; finish = 11 })),
      "foo",
      [Nonbinding.Operator
         ((Some
             ((let open Lvca_provenance.Range in { start = 4; finish = 10 }))),
           "bar",
           [Nonbinding.Primitive
              ((Some
                  ((let open Lvca_provenance.Range in
                      { start = 8; finish = 9 }))),
                (Lvca_syntax.Primitive_impl.Plain.Integer (Z.of_string "1")))])])
let test_pattern =
  Pattern.Operator
    ((Some (let open Lvca_provenance.Range in { start = 0; finish = 6 })),
      "foo",
      [Pattern.Var
         ((Some
             ((let open Lvca_provenance.Range in { start = 4; finish = 5 }))),
           "x")])
let test_language =
  let open Abstract_syntax in
    {
      externals = [];
      sort_defs =
        [("foo",
           (Abstract_syntax.Sort_def.Sort_def
              ([],
                [Abstract_syntax.Operator_def.Operator_def
                   ("foo",
                     [Abstract_syntax.Valence.Valence
                        ([],
                          (Sort.Name
                             ((Some
                                 ((let open Lvca_provenance.Range in
                                     { start = 12; finish = 19 }))),
                               "integer")))])])))]
    }
module Lang =
  (functor (Integer : Language_object_intf.Extended_s) -> functor (String :
    Language_object_intf.Extended_s) -> functor (Maybe :
    Language_object_intf.Extended_s) -> functor (List :
    Language_object_intf.Extended_s) ->
    struct
      module Wrapper =
        struct
          module Types =
            struct
              type 'info foo =
                | Foo of 'info * 'info Integer.t 
                | Bar of 'info * ('info Pattern.t * string * 'info foo) 
              and 'info mut_a =
                | Mut_a of 'info * 'info mut_b 
              and 'info mut_b =
                | Mut_b of 'info * 'info mut_a 
              and 'info nat =
                | Z of 'info 
                | S of 'info * 'info nat 
              and 'info nonempty =
                | Nonempty of 'info * 'info String.t * 'info List.t 
              and ('info, 'a, 'b) pair =
                | Pair of 'info * 'a * 'b 
              and ('info, 'a, 'b) pair_plus =
                | PairPlus of 'info * 'a * 'b * 'info foo 
              and 'info term =
                | Operator of 'info * 'info List.t 
            end
          module Plain =
            struct
              type foo =
                | Foo of Integer.Plain.t 
                | Bar of (Pattern.Plain.t * string * foo) 
              and mut_a =
                | Mut_a of mut_b 
              and mut_b =
                | Mut_b of mut_a 
              and nat =
                | Z 
                | S of nat 
              and nonempty =
                | Nonempty of String.Plain.t * List.Plain.t 
              and ('a, 'b) pair =
                | Pair of 'a * 'b 
              and ('a, 'b) pair_plus =
                | PairPlus of 'a * 'b * foo 
              and term =
                | Operator of List.Plain.t 
            end
          module Info =
            struct
              let nonempty = function | Types.Nonempty (x0, _, _) -> x0
              let pair _f_a _f_b = function | Types.Pair (x0, _, _) -> x0
              let foo =
                function
                | Types.Foo (x0, _) -> x0
                | Types.Bar (x0, (_, _, _)) -> x0
              let pair_plus _f_a _f_b =
                function | Types.PairPlus (x0, _, _, _) -> x0
              let term = function | Types.Operator (x0, _) -> x0
              let mut_a = function | Types.Mut_a (x0, _) -> x0
              and mut_b = function | Types.Mut_b (x0, _) -> x0
              let nat = function | Types.Z x0 -> x0 | Types.S (x0, _) -> x0
            end
          module To_plain =
            struct
              let nonempty =
                function
                | Types.Nonempty (_, x1, x2) ->
                    Plain.Nonempty ((String.to_plain x1), (List.to_plain x2))
              let pair f_a f_b =
                function
                | Types.Pair (_, x1, x2) -> Plain.Pair ((f_a x1), (f_b x2))
              let rec foo =
                function
                | Types.Foo (_, x1) -> Plain.Foo (Integer.to_plain x1)
                | Types.Bar (_, (x1, x2, x3)) ->
                    Plain.Bar ((Pattern.to_plain x1), x2, (foo x3))
              let pair_plus f_a f_b =
                function
                | Types.PairPlus (_, x1, x2, x3) ->
                    Plain.PairPlus ((f_a x1), (f_b x2), (foo x3))
              let term =
                function
                | Types.Operator (_, x1) -> Plain.Operator (List.to_plain x1)
              let rec mut_a =
                function | Types.Mut_a (_, x1) -> Plain.Mut_a (mut_b x1)
              and mut_b =
                function | Types.Mut_b (_, x1) -> Plain.Mut_b (mut_a x1)
              let rec nat =
                function
                | Types.Z _ -> Plain.Z
                | Types.S (_, x1) -> Plain.S (nat x1)
            end
          module Of_plain =
            struct
              let nonempty =
                function
                | Plain.Nonempty (x1, x2) ->
                    Types.Nonempty
                      ((), (String.of_plain x1), (List.of_plain x2))
              let pair f_a f_b =
                function
                | Plain.Pair (x1, x2) -> Types.Pair ((), (f_a x1), (f_b x2))
              let rec foo =
                function
                | Plain.Foo x1 -> Types.Foo ((), (Integer.of_plain x1))
                | Plain.Bar (x1, x2, x3) ->
                    Types.Bar ((), ((Pattern.of_plain x1), x2, (foo x3)))
              let pair_plus f_a f_b =
                function
                | Plain.PairPlus (x1, x2, x3) ->
                    Types.PairPlus ((), (f_a x1), (f_b x2), (foo x3))
              let term =
                function
                | Plain.Operator x1 ->
                    Types.Operator ((), (List.of_plain x1))
              let rec mut_a =
                function | Plain.Mut_a x1 -> Types.Mut_a ((), (mut_b x1))
              and mut_b =
                function | Plain.Mut_b x1 -> Types.Mut_b ((), (mut_a x1))
              let rec nat =
                function
                | Plain.Z -> Types.Z ()
                | Plain.S x1 -> Types.S ((), (nat x1))
            end
          module Equal =
            struct
              let nonempty ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Nonempty (x0, x1, x2), Types.Nonempty (y0, y1, y2))
                    ->
                    (info_eq x0 y0) &&
                      ((String.equal ~info_eq x1 y1) &&
                         (List.equal ~info_eq x2 y2))
              let pair f_a f_b ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Pair (x0, x1, x2), Types.Pair (y0, y1, y2)) ->
                    (info_eq x0 y0) &&
                      ((f_a ~info_eq x1 y1) && (f_b ~info_eq x2 y2))
              let rec foo ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Foo (x0, x1), Types.Foo (y0, y1)) ->
                    (info_eq x0 y0) && (Integer.equal ~info_eq x1 y1)
                | (Types.Bar (x0, (x1, x2, x3)), Types.Bar
                   (y0, (y1, y2, y3))) ->
                    (info_eq x0 y0) &&
                      ((Pattern.equal ~info_eq x1 y1) &&
                         ((Base.String.(=) x2 y2) && (foo ~info_eq x3 y3)))
                | (_, _) -> false
              let pair_plus f_a f_b ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.PairPlus (x0, x1, x2, x3), Types.PairPlus
                   (y0, y1, y2, y3)) ->
                    (info_eq x0 y0) &&
                      ((f_a ~info_eq x1 y1) &&
                         ((f_b ~info_eq x2 y2) && (foo ~info_eq x3 y3)))
              let term ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Operator (x0, x1), Types.Operator (y0, y1)) ->
                    (info_eq x0 y0) && (List.equal ~info_eq x1 y1)
              let rec mut_a ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Mut_a (x0, x1), Types.Mut_a (y0, y1)) ->
                    (info_eq x0 y0) && (mut_b ~info_eq x1 y1)
              and mut_b ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Mut_b (x0, x1), Types.Mut_b (y0, y1)) ->
                    (info_eq x0 y0) && (mut_a ~info_eq x1 y1)
              let rec nat ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Z x0, Types.Z y0) -> info_eq x0 y0
                | (Types.S (x0, x1), Types.S (y0, y1)) ->
                    (info_eq x0 y0) && (nat ~info_eq x1 y1)
                | (_, _) -> false
            end
          module Map_info =
            struct
              let nonempty ~f  =
                function
                | Types.Nonempty (x0, x1, x2) ->
                    Types.Nonempty
                      ((f x0), (String.map_info ~f x1),
                        (List.map_info ~f x2))
              let pair f_a f_b ~f  =
                function
                | Types.Pair (x0, x1, x2) ->
                    Types.Pair ((f x0), (f_a ~f x1), (f_b ~f x2))
              let rec foo ~f  =
                function
                | Types.Foo (x0, x1) ->
                    Types.Foo ((f x0), (Integer.map_info ~f x1))
                | Types.Bar (x0, (x1, x2, x3)) ->
                    Types.Bar
                      ((f x0), ((Pattern.map_info ~f x1), x2, (foo ~f x3)))
              let pair_plus f_a f_b ~f  =
                function
                | Types.PairPlus (x0, x1, x2, x3) ->
                    Types.PairPlus
                      ((f x0), (f_a ~f x1), (f_b ~f x2), (foo ~f x3))
              let term ~f  =
                function
                | Types.Operator (x0, x1) ->
                    Types.Operator ((f x0), (List.map_info ~f x1))
              let rec mut_a ~f  =
                function
                | Types.Mut_a (x0, x1) -> Types.Mut_a ((f x0), (mut_b ~f x1))
              and mut_b ~f  =
                function
                | Types.Mut_b (x0, x1) -> Types.Mut_b ((f x0), (mut_a ~f x1))
              let rec nat ~f  =
                function
                | Types.Z x0 -> Types.Z (f x0)
                | Types.S (x0, x1) -> Types.S ((f x0), (nat ~f x1))
            end
          module To_nominal =
            struct
              let nonempty =
                function
                | Types.Nonempty (x0, x1, x2) ->
                    Nominal.Term.Operator
                      (x0, "Nonempty",
                        [Nominal.Scope.Scope ([], (String.to_nominal x1));
                        Nominal.Scope.Scope ([], (List.to_nominal x2))])
              let pair f_a f_b =
                function
                | Types.Pair (x0, x1, x2) ->
                    Nominal.Term.Operator
                      (x0, "Pair",
                        [Nominal.Scope.Scope ([], (f_a x1));
                        Nominal.Scope.Scope ([], (f_b x2))])
              let rec foo =
                function
                | Types.Foo (x0, x1) ->
                    Nominal.Term.Operator
                      (x0, "Foo",
                        [Nominal.Scope.Scope ([], (Integer.to_nominal x1))])
                | Types.Bar (x0, (x1, x2, x3)) ->
                    Nominal.Term.Operator
                      (x0, "Bar",
                        [Nominal.Scope.Scope
                           ([x1; Pattern.Var (x0, x2)], (foo x3))])
              let pair_plus f_a f_b =
                function
                | Types.PairPlus (x0, x1, x2, x3) ->
                    Nominal.Term.Operator
                      (x0, "PairPlus",
                        [Nominal.Scope.Scope ([], (f_a x1));
                        Nominal.Scope.Scope ([], (f_b x2));
                        Nominal.Scope.Scope ([], (foo x3))])
              let term =
                function
                | Types.Operator (x0, x1) ->
                    Nominal.Term.Operator
                      (x0, "Operator",
                        [Nominal.Scope.Scope ([], (List.to_nominal x1))])
              let rec mut_a =
                function
                | Types.Mut_a (x0, x1) ->
                    Nominal.Term.Operator
                      (x0, "Mut_a", [Nominal.Scope.Scope ([], (mut_b x1))])
              and mut_b =
                function
                | Types.Mut_b (x0, x1) ->
                    Nominal.Term.Operator
                      (x0, "Mut_b", [Nominal.Scope.Scope ([], (mut_a x1))])
              let rec nat =
                function
                | Types.Z x0 -> Nominal.Term.Operator (x0, "Z", [])
                | Types.S (x0, x1) ->
                    Nominal.Term.Operator
                      (x0, "S", [Nominal.Scope.Scope ([], (nat x1))])
            end
          module Of_nominal =
            struct
              let nonempty =
                function
                | Nominal.Term.Operator
                    (x0, "Nonempty", (Nominal.Scope.Scope
                     ([], x1))::(Nominal.Scope.Scope ([], x2))::[])
                    ->
                    (match String.of_nominal x1 with
                     | Error msg -> Error msg
                     | Ok x1 ->
                         (match List.of_nominal x2 with
                          | Error msg -> Error msg
                          | Ok x2 -> Ok (Types.Nonempty (x0, x1, x2))))
                | tm -> Error tm
              let pair f_a f_b =
                function
                | Nominal.Term.Operator
                    (x0, "Pair", (Nominal.Scope.Scope
                     ([], x1))::(Nominal.Scope.Scope ([], x2))::[])
                    ->
                    (match f_a x1 with
                     | Error msg -> Error msg
                     | Ok x1 ->
                         (match f_b x2 with
                          | Error msg -> Error msg
                          | Ok x2 -> Ok (Types.Pair (x0, x1, x2))))
                | tm -> Error tm
              let rec foo =
                function
                | Nominal.Term.Operator
                    (x0, "Foo", (Nominal.Scope.Scope ([], x1))::[]) ->
                    (match Integer.of_nominal x1 with
                     | Error msg -> Error msg
                     | Ok x1 -> Ok (Types.Foo (x0, x1)))
                | Nominal.Term.Operator
                    (x0, "Bar", (Nominal.Scope.Scope
                     (x1::(Pattern.Var (_, x2))::[], x3))::[])
                    ->
                    (match foo x3 with
                     | Error msg -> Error msg
                     | Ok x3 -> Ok (Types.Bar (x0, (x1, x2, x3))))
                | tm -> Error tm
              let pair_plus f_a f_b =
                function
                | Nominal.Term.Operator
                    (x0, "PairPlus", (Nominal.Scope.Scope
                     ([], x1))::(Nominal.Scope.Scope
                     ([], x2))::(Nominal.Scope.Scope ([], x3))::[])
                    ->
                    (match f_a x1 with
                     | Error msg -> Error msg
                     | Ok x1 ->
                         (match f_b x2 with
                          | Error msg -> Error msg
                          | Ok x2 ->
                              (match foo x3 with
                               | Error msg -> Error msg
                               | Ok x3 ->
                                   Ok (Types.PairPlus (x0, x1, x2, x3)))))
                | tm -> Error tm
              let term =
                function
                | Nominal.Term.Operator
                    (x0, "Operator", (Nominal.Scope.Scope ([], x1))::[]) ->
                    (match List.of_nominal x1 with
                     | Error msg -> Error msg
                     | Ok x1 -> Ok (Types.Operator (x0, x1)))
                | tm -> Error tm
              let rec mut_a =
                function
                | Nominal.Term.Operator
                    (x0, "Mut_a", (Nominal.Scope.Scope ([], x1))::[]) ->
                    (match mut_b x1 with
                     | Error msg -> Error msg
                     | Ok x1 -> Ok (Types.Mut_a (x0, x1)))
                | tm -> Error tm
              and mut_b =
                function
                | Nominal.Term.Operator
                    (x0, "Mut_b", (Nominal.Scope.Scope ([], x1))::[]) ->
                    (match mut_a x1 with
                     | Error msg -> Error msg
                     | Ok x1 -> Ok (Types.Mut_b (x0, x1)))
                | tm -> Error tm
              let rec nat =
                function
                | Nominal.Term.Operator (x0, "Z", []) -> Ok (Types.Z x0)
                | Nominal.Term.Operator
                    (x0, "S", (Nominal.Scope.Scope ([], x1))::[]) ->
                    (match nat x1 with
                     | Error msg -> Error msg
                     | Ok x1 -> Ok (Types.S (x0, x1)))
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
          let equal ~info_eq  tm = Wrapper.Equal.foo ~info_eq tm
          let map_info ~f  tm = Wrapper.Map_info.foo ~f tm
          let to_nominal tm = Wrapper.To_nominal.foo tm
          let of_nominal tm = Wrapper.Of_nominal.foo tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse = struct let t = Lvca_parsing.fail "TODO: parse" end
        end
      module Nat =
        struct
          type 'info t = 'info Wrapper.Types.nat
          module Plain = struct type t = Wrapper.Plain.nat end
          let info tm = Wrapper.Info.nat tm
          let to_plain tm = Wrapper.To_plain.nat tm
          let of_plain tm = Wrapper.Of_plain.nat tm
          let equal ~info_eq  tm = Wrapper.Equal.nat ~info_eq tm
          let map_info ~f  tm = Wrapper.Map_info.nat ~f tm
          let to_nominal tm = Wrapper.To_nominal.nat tm
          let of_nominal tm = Wrapper.Of_nominal.nat tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse = struct let t = Lvca_parsing.fail "TODO: parse" end
        end
      module Pair(A:Language_object_intf.Extended_s)(B:Language_object_intf.Extended_s) =
        struct
          type 'info t = ('info, 'info A.t, 'info B.t) Wrapper.Types.pair
          module Plain =
            struct type t = (A.Plain.t, B.Plain.t) Wrapper.Plain.pair end
          let info tm = Wrapper.Info.pair A.info B.info tm
          let to_plain tm = Wrapper.To_plain.pair A.to_plain B.to_plain tm
          let of_plain tm = Wrapper.Of_plain.pair A.of_plain B.of_plain tm
          let equal ~info_eq  tm =
            Wrapper.Equal.pair A.equal B.equal ~info_eq tm
          let map_info ~f  tm =
            Wrapper.Map_info.pair A.map_info B.map_info ~f tm
          let to_nominal tm =
            Wrapper.To_nominal.pair A.to_nominal B.to_nominal tm
          let of_nominal tm =
            Wrapper.Of_nominal.pair A.of_nominal B.of_nominal tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse = struct let t = Lvca_parsing.fail "TODO: parse" end
        end
      module Pair_plus(A:Language_object_intf.Extended_s)(B:Language_object_intf.Extended_s) =
        struct
          type 'info t =
            ('info, 'info A.t, 'info B.t) Wrapper.Types.pair_plus
          module Plain =
            struct
              type t = (A.Plain.t, B.Plain.t) Wrapper.Plain.pair_plus
            end
          let info tm = Wrapper.Info.pair_plus A.info B.info tm
          let to_plain tm =
            Wrapper.To_plain.pair_plus A.to_plain B.to_plain tm
          let of_plain tm =
            Wrapper.Of_plain.pair_plus A.of_plain B.of_plain tm
          let equal ~info_eq  tm =
            Wrapper.Equal.pair_plus A.equal B.equal ~info_eq tm
          let map_info ~f  tm =
            Wrapper.Map_info.pair_plus A.map_info B.map_info ~f tm
          let to_nominal tm =
            Wrapper.To_nominal.pair_plus A.to_nominal B.to_nominal tm
          let of_nominal tm =
            Wrapper.Of_nominal.pair_plus A.of_nominal B.of_nominal tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse = struct let t = Lvca_parsing.fail "TODO: parse" end
        end
      module Nonempty =
        struct
          type 'info t = 'info Wrapper.Types.nonempty
          module Plain = struct type t = Wrapper.Plain.nonempty end
          let info tm = Wrapper.Info.nonempty tm
          let to_plain tm = Wrapper.To_plain.nonempty tm
          let of_plain tm = Wrapper.Of_plain.nonempty tm
          let equal ~info_eq  tm = Wrapper.Equal.nonempty ~info_eq tm
          let map_info ~f  tm = Wrapper.Map_info.nonempty ~f tm
          let to_nominal tm = Wrapper.To_nominal.nonempty tm
          let of_nominal tm = Wrapper.Of_nominal.nonempty tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse = struct let t = Lvca_parsing.fail "TODO: parse" end
        end
      module Term =
        struct
          type 'info t = 'info Wrapper.Types.term
          module Plain = struct type t = Wrapper.Plain.term end
          let info tm = Wrapper.Info.term tm
          let to_plain tm = Wrapper.To_plain.term tm
          let of_plain tm = Wrapper.Of_plain.term tm
          let equal ~info_eq  tm = Wrapper.Equal.term ~info_eq tm
          let map_info ~f  tm = Wrapper.Map_info.term ~f tm
          let to_nominal tm = Wrapper.To_nominal.term tm
          let of_nominal tm = Wrapper.Of_nominal.term tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse = struct let t = Lvca_parsing.fail "TODO: parse" end
        end
      module Mut_a =
        struct
          type 'info t = 'info Wrapper.Types.mut_a
          module Plain = struct type t = Wrapper.Plain.mut_a end
          let info tm = Wrapper.Info.mut_a tm
          let to_plain tm = Wrapper.To_plain.mut_a tm
          let of_plain tm = Wrapper.Of_plain.mut_a tm
          let equal ~info_eq  tm = Wrapper.Equal.mut_a ~info_eq tm
          let map_info ~f  tm = Wrapper.Map_info.mut_a ~f tm
          let to_nominal tm = Wrapper.To_nominal.mut_a tm
          let of_nominal tm = Wrapper.Of_nominal.mut_a tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse = struct let t = Lvca_parsing.fail "TODO: parse" end
        end
      module Mut_b =
        struct
          type 'info t = 'info Wrapper.Types.mut_b
          module Plain = struct type t = Wrapper.Plain.mut_b end
          let info tm = Wrapper.Info.mut_b tm
          let to_plain tm = Wrapper.To_plain.mut_b tm
          let of_plain tm = Wrapper.Of_plain.mut_b tm
          let equal ~info_eq  tm = Wrapper.Equal.mut_b ~info_eq tm
          let map_info ~f  tm = Wrapper.Map_info.mut_b ~f tm
          let to_nominal tm = Wrapper.To_nominal.mut_b tm
          let of_nominal tm = Wrapper.Of_nominal.mut_b tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse = struct let t = Lvca_parsing.fail "TODO: parse" end
        end
    end :
    functor (Integer : Language_object_intf.Extended_s) ->
      functor (String : Language_object_intf.Extended_s) ->
        functor (Maybe : Language_object_intf.Extended_s) ->
          functor (List : Language_object_intf.Extended_s) ->
            sig
              module Types :
              sig
                type 'info foo =
                  | Foo of 'info * 'info Integer.t 
                  | Bar of 'info * ('info Pattern.t * string * 'info foo) 
                and 'info mut_a =
                  | Mut_a of 'info * 'info mut_b 
                and 'info mut_b =
                  | Mut_b of 'info * 'info mut_a 
                and 'info nat =
                  | Z of 'info 
                  | S of 'info * 'info nat 
                and 'info nonempty =
                  | Nonempty of 'info * 'info String.t * 'info List.t 
                and ('info, 'a, 'b) pair =
                  | Pair of 'info * 'a * 'b 
                and ('info, 'a, 'b) pair_plus =
                  | PairPlus of 'info * 'a * 'b * 'info foo 
                and 'info term =
                  | Operator of 'info * 'info List.t 
              end
              module Plain :
              sig
                type foo =
                  | Foo of Integer.Plain.t 
                  | Bar of (Pattern.Plain.t * string * foo) 
                and mut_a =
                  | Mut_a of mut_b 
                and mut_b =
                  | Mut_b of mut_a 
                and nat =
                  | Z 
                  | S of nat 
                and nonempty =
                  | Nonempty of String.Plain.t * List.Plain.t 
                and ('a, 'b) pair =
                  | Pair of 'a * 'b 
                and ('a, 'b) pair_plus =
                  | PairPlus of 'a * 'b * foo 
                and term =
                  | Operator of List.Plain.t 
              end
              module Foo :
              Language_object_intf.S with type 'info t =  'info Types.foo and
                type  Plain.t =  Plain.foo
              module Nat :
              Language_object_intf.S with type 'info t =  'info Types.nat and
                type  Plain.t =  Plain.nat
              module Pair :
              functor (A : Language_object_intf.Extended_s) ->
                functor (B : Language_object_intf.Extended_s) ->
                  Language_object_intf.S with type 'info t = 
                    ('info, 'info A.t, 'info B.t) Types.pair and type
                     Plain.t =  (A.Plain.t, B.Plain.t) Plain.pair
              module Pair_plus :
              functor (A : Language_object_intf.Extended_s) ->
                functor (B : Language_object_intf.Extended_s) ->
                  Language_object_intf.S with type 'info t = 
                    ('info, 'info A.t, 'info B.t) Types.pair_plus and type
                     Plain.t =  (A.Plain.t, B.Plain.t) Plain.pair_plus
              module Nonempty :
              Language_object_intf.S with type 'info t = 
                'info Types.nonempty and type  Plain.t =  Plain.nonempty
              module Term :
              Language_object_intf.S with type 'info t =  'info Types.term
                and type  Plain.t =  Plain.term
              module Mut_a :
              Language_object_intf.S with type 'info t =  'info Types.mut_a
                and type  Plain.t =  Plain.mut_a
              module Mut_b :
              Language_object_intf.S with type 'info t =  'info Types.mut_b
                and type  Plain.t =  Plain.mut_b
            end)
