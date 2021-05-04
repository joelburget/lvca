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
                (Lvca_syntax.Primitive.Plain.Integer (Z.of_string "1")))])])
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
module Lang =
  (functor (Integer : LanguageObject.AllTermS) ->
    struct
      module Wrapper =
        struct
          module Types =
            struct
              type 'info foo =
                | Foo of 'info * 'info Integer.t 
                | Bar of 'info * ('info Pattern.t * string * 'info foo) 
              and ('info, 'a) list =
                | Nil of 'info 
                | Cons of 'info * 'a * ('info, 'a) list 
              and 'info mut_a =
                | Mut_a of 'info * 'info mut_b 
              and 'info mut_b =
                | Mut_b of 'info * 'info mut_a 
              and 'info nat =
                | Z of 'info 
                | S of 'info * 'info nat 
              and ('info, 'a, 'b) pair =
                | Pair of 'info * 'a * 'b 
              and ('info, 'a, 'b) pair_plus =
                | PairPlus of 'info * 'a * 'b * 'info foo 
            end
          module Plain =
            struct
              type foo =
                | Foo of Integer.Plain.t 
                | Bar of (Pattern.Plain.t * string * foo) 
              and 'a list =
                | Nil 
                | Cons of 'a * 'a list 
              and mut_a =
                | Mut_a of mut_b 
              and mut_b =
                | Mut_b of mut_a 
              and nat =
                | Z 
                | S of nat 
              and ('a, 'b) pair =
                | Pair of 'a * 'b 
              and ('a, 'b) pair_plus =
                | PairPlus of 'a * 'b * foo 
            end
          module Info =
            struct
              let mut_a = function | Types.Mut_a (x0, _) -> x0
              and mut_b = function | Types.Mut_b (x0, _) -> x0
              let foo =
                function
                | Types.Foo (x0, _) -> x0
                | Types.Bar (x0, (_, _, _)) -> x0
              let pair_plus _f_a _f_b =
                function | Types.PairPlus (x0, _, _, _) -> x0
              let nat = function | Types.Z x0 -> x0 | Types.S (x0, _) -> x0
              let pair _f_a _f_b = function | Types.Pair (x0, _, _) -> x0
              let list _f_a =
                function | Types.Nil x0 -> x0 | Types.Cons (x0, _, _) -> x0
            end
          module ToPlain =
            struct
              let rec mut_a =
                function | Types.Mut_a (_, x1) -> Plain.Mut_a (mut_b x1)
              and mut_b =
                function | Types.Mut_b (_, x1) -> Plain.Mut_b (mut_a x1)
              let rec foo =
                function
                | Types.Foo (_, x1) -> Plain.Foo (Integer.to_plain x1)
                | Types.Bar (_, (x1, x2, x3)) ->
                    Plain.Bar ((Pattern.to_plain x1), x2, (foo x3))
              let pair_plus f_a f_b =
                function
                | Types.PairPlus (_, x1, x2, x3) ->
                    Plain.PairPlus ((f_a x1), (f_b x2), (foo x3))
              let rec nat =
                function
                | Types.Z _ -> Plain.Z
                | Types.S (_, x1) -> Plain.S (nat x1)
              let pair f_a f_b =
                function
                | Types.Pair (_, x1, x2) -> Plain.Pair ((f_a x1), (f_b x2))
              let rec list f_a =
                function
                | Types.Nil _ -> Plain.Nil
                | Types.Cons (_, x1, x2) ->
                    Plain.Cons ((f_a x1), (list f_a x2))
            end
          module OfPlain =
            struct
              let rec mut_a =
                function | Plain.Mut_a x1 -> Types.Mut_a ((), (mut_b x1))
              and mut_b =
                function | Plain.Mut_b x1 -> Types.Mut_b ((), (mut_a x1))
              let rec foo =
                function
                | Plain.Foo x1 -> Types.Foo ((), (Integer.of_plain x1))
                | Plain.Bar (x1, x2, x3) ->
                    Types.Bar ((), ((Pattern.of_plain x1), x2, (foo x3)))
              let pair_plus f_a f_b =
                function
                | Plain.PairPlus (x1, x2, x3) ->
                    Types.PairPlus ((), (f_a x1), (f_b x2), (foo x3))
              let rec nat =
                function
                | Plain.Z -> Types.Z ()
                | Plain.S x1 -> Types.S ((), (nat x1))
              let pair f_a f_b =
                function
                | Plain.Pair (x1, x2) -> Types.Pair ((), (f_a x1), (f_b x2))
              let rec list f_a =
                function
                | Plain.Nil -> Types.Nil ()
                | Plain.Cons (x1, x2) ->
                    Types.Cons ((), (f_a x1), (list f_a x2))
            end
          module Equal =
            struct
              let rec mut_a ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Mut_a (x0, x1), Types.Mut_a (y0, y1)) ->
                    (info_eq x0 y0) && (mut_b ~info_eq x1 y1)
              and mut_b ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Mut_b (x0, x1), Types.Mut_b (y0, y1)) ->
                    (info_eq x0 y0) && (mut_a ~info_eq x1 y1)
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
              let rec nat ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Z x0, Types.Z y0) -> info_eq x0 y0
                | (Types.S (x0, x1), Types.S (y0, y1)) ->
                    (info_eq x0 y0) && (nat ~info_eq x1 y1)
                | (_, _) -> false
              let pair f_a f_b ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Pair (x0, x1, x2), Types.Pair (y0, y1, y2)) ->
                    (info_eq x0 y0) &&
                      ((f_a ~info_eq x1 y1) && (f_b ~info_eq x2 y2))
              let rec list f_a ~info_eq  t1 t2 =
                match (t1, t2) with
                | (Types.Nil x0, Types.Nil y0) -> info_eq x0 y0
                | (Types.Cons (x0, x1, x2), Types.Cons (y0, y1, y2)) ->
                    (info_eq x0 y0) &&
                      ((f_a ~info_eq x1 y1) && (list f_a ~info_eq x2 y2))
                | (_, _) -> false
            end
          module MapInfo =
            struct
              let rec mut_a ~f  =
                function
                | Types.Mut_a (x0, x1) -> Types.Mut_a ((f x0), (mut_b ~f x1))
              and mut_b ~f  =
                function
                | Types.Mut_b (x0, x1) -> Types.Mut_b ((f x0), (mut_a ~f x1))
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
              let rec nat ~f  =
                function
                | Types.Z x0 -> Types.Z (f x0)
                | Types.S (x0, x1) -> Types.S ((f x0), (nat ~f x1))
              let pair f_a f_b ~f  =
                function
                | Types.Pair (x0, x1, x2) ->
                    Types.Pair ((f x0), (f_a ~f x1), (f_b ~f x2))
              let rec list f_a ~f  =
                function
                | Types.Nil x0 -> Types.Nil (f x0)
                | Types.Cons (x0, x1, x2) ->
                    Types.Cons ((f x0), (f_a ~f x1), (list ~f f_a x2))
            end
        end
      module Foo =
        struct
          type 'info t = 'info Wrapper.Types.foo
          module Plain = struct type t = Wrapper.Plain.foo end
          let info tm = Wrapper.Info.foo tm
          let to_plain tm = Wrapper.ToPlain.foo tm
          let of_plain tm = Wrapper.OfPlain.foo tm
          let equal ~info_eq  tm = Wrapper.Equal.foo ~info_eq tm
          let map_info ~f  tm = Wrapper.MapInfo.foo ~f tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse(Comment:ParseUtil.Comment_int) =
            struct let t = failwith "TODO" end
        end
      module Nat =
        struct
          type 'info t = 'info Wrapper.Types.nat
          module Plain = struct type t = Wrapper.Plain.nat end
          let info tm = Wrapper.Info.nat tm
          let to_plain tm = Wrapper.ToPlain.nat tm
          let of_plain tm = Wrapper.OfPlain.nat tm
          let equal ~info_eq  tm = Wrapper.Equal.nat ~info_eq tm
          let map_info ~f  tm = Wrapper.MapInfo.nat ~f tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse(Comment:ParseUtil.Comment_int) =
            struct let t = failwith "TODO" end
        end
      module List(A:LanguageObject.AllTermS) =
        struct
          type 'info t = ('info, 'info A.t) Wrapper.Types.list
          module Plain = struct type t = A.Plain.t Wrapper.Plain.list end
          let info tm = Wrapper.Info.list A.info tm
          let to_plain tm = Wrapper.ToPlain.list A.to_plain tm
          let of_plain tm = Wrapper.OfPlain.list A.of_plain tm
          let equal ~info_eq  tm = Wrapper.Equal.list A.equal ~info_eq tm
          let map_info ~f  tm = Wrapper.MapInfo.list A.map_info ~f tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse(Comment:ParseUtil.Comment_int) =
            struct let t = failwith "TODO" end
        end
      module Pair(A:LanguageObject.AllTermS)(B:LanguageObject.AllTermS) =
        struct
          type 'info t = ('info, 'info A.t, 'info B.t) Wrapper.Types.pair
          module Plain =
            struct type t = (A.Plain.t, B.Plain.t) Wrapper.Plain.pair end
          let info tm = Wrapper.Info.pair A.info B.info tm
          let to_plain tm = Wrapper.ToPlain.pair A.to_plain B.to_plain tm
          let of_plain tm = Wrapper.OfPlain.pair A.of_plain B.of_plain tm
          let equal ~info_eq  tm =
            Wrapper.Equal.pair A.equal B.equal ~info_eq tm
          let map_info ~f  tm =
            Wrapper.MapInfo.pair A.map_info B.map_info ~f tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse(Comment:ParseUtil.Comment_int) =
            struct let t = failwith "TODO" end
        end
      module Pair_plus(A:LanguageObject.AllTermS)(B:LanguageObject.AllTermS) =
        struct
          type 'info t =
            ('info, 'info A.t, 'info B.t) Wrapper.Types.pair_plus
          module Plain =
            struct
              type t = (A.Plain.t, B.Plain.t) Wrapper.Plain.pair_plus
            end
          let info tm = Wrapper.Info.pair_plus A.info B.info tm
          let to_plain tm =
            Wrapper.ToPlain.pair_plus A.to_plain B.to_plain tm
          let of_plain tm =
            Wrapper.OfPlain.pair_plus A.of_plain B.of_plain tm
          let equal ~info_eq  tm =
            Wrapper.Equal.pair_plus A.equal B.equal ~info_eq tm
          let map_info ~f  tm =
            Wrapper.MapInfo.pair_plus A.map_info B.map_info ~f tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse(Comment:ParseUtil.Comment_int) =
            struct let t = failwith "TODO" end
        end
      module Mut_a =
        struct
          type 'info t = 'info Wrapper.Types.mut_a
          module Plain = struct type t = Wrapper.Plain.mut_a end
          let info tm = Wrapper.Info.mut_a tm
          let to_plain tm = Wrapper.ToPlain.mut_a tm
          let of_plain tm = Wrapper.OfPlain.mut_a tm
          let equal ~info_eq  tm = Wrapper.Equal.mut_a ~info_eq tm
          let map_info ~f  tm = Wrapper.MapInfo.mut_a ~f tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse(Comment:ParseUtil.Comment_int) =
            struct let t = failwith "TODO" end
        end
      module Mut_b =
        struct
          type 'info t = 'info Wrapper.Types.mut_b
          module Plain = struct type t = Wrapper.Plain.mut_b end
          let info tm = Wrapper.Info.mut_b tm
          let to_plain tm = Wrapper.ToPlain.mut_b tm
          let of_plain tm = Wrapper.OfPlain.mut_b tm
          let equal ~info_eq  tm = Wrapper.Equal.mut_b ~info_eq tm
          let map_info ~f  tm = Wrapper.MapInfo.mut_b ~f tm
          let pp_generic ~open_loc:_  ~close_loc:_  ppf _tm =
            Fmt.pf ppf "TODO: pp_generic"
          module Parse(Comment:ParseUtil.Comment_int) =
            struct let t = failwith "TODO" end
        end
    end :
    functor (Integer : LanguageObject.AllTermS) ->
      sig
        module Foo : LanguageObject.AllTermS
        module Nat : LanguageObject.AllTermS
        module List :
        functor (A : LanguageObject.AllTermS) -> LanguageObject.AllTermS
        module Pair :
        functor (A : LanguageObject.AllTermS) ->
          functor (B : LanguageObject.AllTermS) -> LanguageObject.AllTermS
        module Pair_plus :
        functor (A : LanguageObject.AllTermS) ->
          functor (B : LanguageObject.AllTermS) -> LanguageObject.AllTermS
        module Mut_a : LanguageObject.AllTermS
        module Mut_b : LanguageObject.AllTermS
      end)
