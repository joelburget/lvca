open Lvca_syntax
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
      sig
        type 'a t =
          | Nil 
          | Cons of 'a * 'a Wrapper.Plain.list 
        val pp : 'a_ Fmt.t -> 'a_ t Fmt.t
        val equal : ('a_ -> 'a_ -> bool) -> 'a_ t -> 'a_ t -> bool
        val parse : 'a_ Lvca_parsing.t -> 'a_ t Lvca_parsing.t
        val jsonify :
          'a_ Lvca_util.Json.serializer -> 'a_ t Lvca_util.Json.serializer
        val unjsonify :
          'a_ Lvca_util.Json.deserializer ->
            'a_ t Lvca_util.Json.deserializer
      end
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
            let equal a_of_plain a_to_nominal x y =
              let x =
                (x |> (of_plain a_of_plain)) |> (to_nominal a_to_nominal) in
              let y =
                (y |> (of_plain a_of_plain)) |> (to_nominal a_to_nominal) in
              let open Lvca_syntax.Nominal.Term in
                equal ~info_eq:Base.Unit.(=) (erase x) (erase y)
            let jsonify a_of_plain a_to_nominal tm =
              ((tm |> (of_plain a_of_plain)) |> (to_nominal a_to_nominal)) |>
                Lvca_syntax.Nominal.Term.jsonify
            let unjsonify a_of_nominal a_to_plain json =
              (json |> Lvca_syntax.Nominal.Term.unjsonify) |>
                (Base.Option.bind
                   ~f:(fun tm ->
                         match (of_nominal a_of_nominal) tm with
                         | Ok tm -> Some ((to_plain a_to_plain) tm)
                         | Error _ -> None))
            let pp a_of_plain a_to_nominal ppf tm =
              ((tm |> (of_plain a_of_plain)) |> (to_nominal a_to_nominal)) |>
                (Lvca_syntax.Nominal.Term.pp ppf)
            let parse a_of_nominal a_to_plain =
              let parse_prim =
                Lvca_parsing.fail "Generated parser parse_prim always fails" in
              let open Lvca_parsing in
                (Lvca_syntax.Nominal.Term.parse
                   ~comment:Lvca_parsing.c_comment ~parse_prim)
                  >>=
                  (fun tm ->
                     match (of_nominal a_of_nominal) tm with
                     | Ok tm -> return ((to_plain a_to_plain) tm)
                     | Error _ ->
                         fail "Generated parser failed nominal conversion")
          end
      end
  end 
