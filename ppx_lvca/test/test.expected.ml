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

module Lang (Integer : LanguageObject.AllTermS) = struct

    module Foo =
      struct
        type 'info t =
          | Foo of 'info * Integer.t
          | Bar of 'info * ('info, Lvca_util.Void.t) Pattern.t * string * 'info t

        module Plain =
          struct
            type t =
              | Foo of Integer.t
              | Bar of (unit, Lvca_util.Void.t) Pattern.t * string * t
        end

        let rec to_plain =
          function
          | Foo (_, x1) -> Plain.Foo x1
          | Bar (_, x1, x2, x3) -> Plain.Bar (x1, x2, to_plain x3)

        let rec of_plain =
          function
          | Plain.Foo x1 -> Foo ((), x1)
          | Plain.Bar (x1, x2, x3) -> Bar ((), x1, x2, of_plain x3)

        let equal ~info_eq t1 t2 = match t1, t2 with
          | Foo (i1, a1), Foo (i2, a2) -> info_eq i1 i2 && Integer.equal ~info_eq a1 a2
          | Bar (i1, a1, b1, c1), Bar (i2, a2, b2, c2) -> info_eq i1 i2 && Pattern.equal info_eq Lvca_util.Void.(=) a1 a2 && String.(b1 = b2) && equal ~info_eq c1 c2
          | _, _ -> false

        let info = function | Foo (i, _) -> i | Bar (i, _, _, _) -> i

        let rec map_info ~f = function
          | Foo (i, a) -> Foo (f i, a)
          | Bar (i, a, b, c) -> Bar (f i, a, b, map_info ~f c)

        (* let pp_generic ~opener ~closer ppf = function ... *)
        (* let of_nominal = function ... *)
        (* let to_nominal = function ... *)
        (* Erase, select? *)
    end
end
