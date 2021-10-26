open Lvca_syntax
module Test4 =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type list =
              | Nil of Lvca_syntax.Provenance.t 
              | Cons of Lvca_syntax.Provenance.t * Nominal.Term.t * list 
          end
        module Info =
          struct
            let list =
              function | Types.Nil x0 -> x0 | Types.Cons (x0, _, _) -> x0
          end
        module Equivalent =
          struct
            let rec list ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Nil x0, Types.Nil y0) -> info_eq x0 y0
              | (Types.Cons (x0, x1, x2), Types.Cons (y0, y1, y2)) ->
                  (info_eq x0 y0) &&
                    ((Nominal.Term.equivalent ~info_eq x1 y1) &&
                       (list ~info_eq x2 y2))
              | (_, _) -> false
          end
        module To_nominal =
          struct
            let rec list =
              function
              | Types.Nil x0 ->
                  Lvca_syntax.Nominal.Term.Operator (x0, "Nil", [])
              | Types.Cons (x0, x1, x2) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Cons",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (Nominal.Term.to_nominal x1));
                      Lvca_syntax.Nominal.Scope.Scope ([], (list x2))])
          end
        module Of_nominal =
          struct
            let rec list =
              function
              | Lvca_syntax.Nominal.Term.Operator (x0, "Nil", []) ->
                  Ok (Types.Nil x0)
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Cons", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::(Lvca_syntax.Nominal.Scope.Scope ([], x2))::[])
                  ->
                  (match Nominal.Term.of_nominal x1 with
                   | Error err -> Error err
                   | Ok x1 ->
                       (match list x2 with
                        | Error err -> Error err
                        | Ok x2 -> Ok (Types.Cons (x0, x1, x2))))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals =
            [("term",
               (Lvca_syntax.Abstract_syntax.Kind.Kind
                  ((Lvca_syntax.Provenance.Located
                      (Lvca_syntax.Provenance.Located.Parse_located
                         ((let open Lvca_syntax.Provenance.Parse_located in
                             {
                               input =
                                 Lvca_syntax.Provenance.Parse_input.Input_unknown;
                               range =
                                 (Some
                                    (let open Lvca_provenance.Range in
                                       { start = 7; finish = 8 }))
                             })))), 1)))];
          sort_defs =
            [("list",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ((Lvca_syntax.Provenance.Located
                           (Lvca_syntax.Provenance.Located.Parse_located
                              ((let open Lvca_syntax.Provenance.Parse_located in
                                  {
                                    input =
                                      Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                    range =
                                      (Some
                                         (let open Lvca_provenance.Range in
                                            { start = 21; finish = 23 }))
                                  })))), "Nil",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((Lvca_syntax.Provenance.Located
                                (Lvca_syntax.Provenance.Located.Source_located
                                   {
                                     pos_fname = "syntax/Abstract_syntax.ml";
                                     pos_lnum = 250;
                                     pos_bol = 6489;
                                     pos_cnum = 6540
                                   })), [])));
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 30; finish = 42 }))
                                 })))), "Cons",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 31;
                                                            finish = 35
                                                          }))
                                                })))), "term")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Name
                                    ((Lvca_syntax.Provenance.Located
                                        (Lvca_syntax.Provenance.Located.Parse_located
                                           ((let open Lvca_syntax.Provenance.Parse_located in
                                               {
                                                 input =
                                                   Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 37;
                                                           finish = 41
                                                         }))
                                               })))), "list")))])))])))]
        }
    module List =
      struct
        type t = Wrapper.Types.list =
          | Nil of Lvca_syntax.Provenance.t 
          | Cons of Lvca_syntax.Provenance.t * Nominal.Term.t *
          Wrapper.Types.list 
        let info = Wrapper.Info.list
        let equivalent = Wrapper.Equivalent.list
        let to_nominal = Wrapper.To_nominal.list
        let of_nominal = Wrapper.Of_nominal.list
        let mk_Nil ~info  = Nil info
        let mk_Cons ~info  x_0 x_1 = Cons (info, x_0, x_1)
      end
  end
let test_pattern =
  Lvca_syntax.Pattern.Operator
    ((Lvca_syntax.Provenance.Located
        (Lvca_syntax.Provenance.Located.Parse_located
           (let open Lvca_syntax.Provenance.Parse_located in
              {
                input = Lvca_syntax.Provenance.Parse_input.Input_unknown;
                range =
                  (Some
                     (let open Lvca_provenance.Range in
                        { start = 0; finish = 6 }))
              }))), "foo",
      [Lvca_syntax.Pattern.Var
         ((Lvca_syntax.Provenance.Located
             (Lvca_syntax.Provenance.Located.Parse_located
                ((let open Lvca_syntax.Provenance.Parse_located in
                    {
                      input =
                        Lvca_syntax.Provenance.Parse_input.Input_unknown;
                      range =
                        (Some
                           (let open Lvca_provenance.Range in
                              { start = 4; finish = 5 }))
                    })))), "x")])
let test_language =
  let open Lvca_syntax.Abstract_syntax in
    {
      externals = [];
      sort_defs =
        [("foo",
           (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
              ([],
                [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                   ((Lvca_syntax.Provenance.Located
                       (Lvca_syntax.Provenance.Located.Parse_located
                          ((let open Lvca_syntax.Provenance.Parse_located in
                              {
                                input =
                                  Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                range =
                                  (Some
                                     (let open Lvca_provenance.Range in
                                        { start = 11; finish = 20 }))
                              })))), "foo",
                     (Lvca_syntax.Abstract_syntax.Arity.Arity
                        ((Lvca_syntax.Provenance.Located
                            (Lvca_syntax.Provenance.Located.Source_located
                               {
                                 pos_fname = "syntax/Abstract_syntax.ml";
                                 pos_lnum = 250;
                                 pos_bol = 6489;
                                 pos_cnum = 6540
                               })),
                          [Lvca_syntax.Abstract_syntax.Valence.Valence
                             ([],
                               (Lvca_syntax.Sort.Name
                                  ((Lvca_syntax.Provenance.Located
                                      (Lvca_syntax.Provenance.Located.Parse_located
                                         ((let open Lvca_syntax.Provenance.Parse_located in
                                             {
                                               input =
                                                 Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                               range =
                                                 (Some
                                                    (let open Lvca_provenance.Range in
                                                       {
                                                         start = 12;
                                                         finish = 19
                                                       }))
                                             })))), "integer")))])))])))]
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
           ('a_, Lvca_syntax.Nominal.Conversion_error.t) Result.t)
          ->
          Lvca_syntax.Nominal.Term.t ->
            ('a_ t, Lvca_syntax.Nominal.Conversion_error.t) Result.t
      val info : 'a_ t -> Lvca_syntax.Provenance.t
      val equivalent :
        (?info_eq:(Lvca_syntax.Provenance.t ->
                     Lvca_syntax.Provenance.t -> bool)
           -> 'a_ -> 'a_ -> bool)
          ->
          ?info_eq:(Lvca_syntax.Provenance.t ->
                      Lvca_syntax.Provenance.t -> bool)
            -> 'a_ t -> 'a_ t -> bool
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
            let list =
              function | Types.Nil x0 -> x0 | Types.Cons (x0, _, _) -> x0
          end
        module Equivalent =
          struct
            let rec list
              (a :
                ?info_eq:(Lvca_syntax.Provenance.t ->
                            Lvca_syntax.Provenance.t -> bool)
                  -> _ -> _ -> bool)
              ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Nil x0, Types.Nil y0) -> info_eq x0 y0
              | (Types.Cons (x0, x1, x2), Types.Cons (y0, y1, y2)) ->
                  (info_eq x0 y0) &&
                    ((a ~info_eq x1 y1) && (list a ~info_eq x2 y2))
              | (_, _) -> false
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
                   | Error err -> Error err
                   | Ok x1 ->
                       (match list a x2 with
                        | Error err -> Error err
                        | Ok x2 -> Ok (Types.Cons (x0, x1, x2))))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
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
                       ((Lvca_syntax.Provenance.Located
                           (Lvca_syntax.Provenance.Located.Parse_located
                              ((let open Lvca_syntax.Provenance.Parse_located in
                                  {
                                    input =
                                      Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                    range =
                                      (Some
                                         (let open Lvca_provenance.Range in
                                            { start = 13; finish = 15 }))
                                  })))), "Nil",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((Lvca_syntax.Provenance.Located
                                (Lvca_syntax.Provenance.Located.Source_located
                                   {
                                     pos_fname = "syntax/Abstract_syntax.ml";
                                     pos_lnum = 250;
                                     pos_bol = 6489;
                                     pos_cnum = 6540
                                   })), [])));
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 22; finish = 33 }))
                                 })))), "Cons",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 23;
                                                            finish = 24
                                                          }))
                                                })))), "a")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Ap
                                    ((Lvca_syntax.Provenance.Located
                                        (Lvca_syntax.Provenance.Located.Parse_located
                                           ((let open Lvca_syntax.Provenance.Parse_located in
                                               {
                                                 input =
                                                   Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 26;
                                                           finish = 30
                                                         }))
                                               })))), "list",
                                      (Lvca_syntax.Sort.Cons
                                         ((Lvca_syntax.Provenance.Located
                                             (Lvca_syntax.Provenance.Located.Source_located
                                                {
                                                  pos_fname =
                                                    "syntax/Sort.ml";
                                                  pos_lnum = 44;
                                                  pos_bol = 1417;
                                                  pos_cnum = 1459
                                                })),
                                           (Lvca_syntax.Sort.Name
                                              ((Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Parse_located
                                                     ((let open Lvca_syntax.Provenance.Parse_located in
                                                         {
                                                           input =
                                                             Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                           range =
                                                             (Some
                                                                (let open Lvca_provenance.Range in
                                                                   {
                                                                    start =
                                                                    31;
                                                                    finish =
                                                                    32
                                                                   }))
                                                         })))), "a")),
                                           (Lvca_syntax.Sort.Nil
                                              (Lvca_syntax.Provenance.Located
                                                 (Lvca_syntax.Provenance.Located.Source_located
                                                    {
                                                      pos_fname =
                                                        "syntax/Sort.ml";
                                                      pos_lnum = 43;
                                                      pos_bol = 1372;
                                                      pos_cnum = 1408
                                                    }))))))))])))])))]
        }
    module List =
      struct
        type 'a t = 'a Wrapper.Types.list =
          | Nil of Lvca_syntax.Provenance.t 
          | Cons of Lvca_syntax.Provenance.t * 'a * 'a Wrapper.Types.list 
        let info = Wrapper.Info.list
        let equivalent = Wrapper.Equivalent.list
        let to_nominal = Wrapper.To_nominal.list
        let of_nominal = Wrapper.Of_nominal.list
        let mk_Nil ~info  = Nil info
        let mk_Cons ~info  x_0 x_1 = Cons (info, x_0, x_1)
      end
  end 
module List =
  struct
    type 'a t
    let to_nominal _ _ = Nominal.Term.Var ((failwith "no provenance"), "")
    let of_nominal _ tm = Error (Nominal.Conversion_error.mk_Term tm)
    let equivalent _a ~info_eq:_  _ _ = true
  end
module Maybe = List
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
              | Operator of Lvca_syntax.Provenance.t * term List.t 
            and ('a, 'b) pair_plus =
              | PairPlus of Lvca_syntax.Provenance.t * 'a * 'b * foo 
            and foo =
              | Foo of Lvca_syntax.Provenance.t * Primitive.Integer.t 
              | Bar of Lvca_syntax.Provenance.t * (Pattern.t *
              Lvca_syntax.Single_var.t * foo) 
              | Foo_var of Lvca_syntax.Provenance.t * string 
            and ('a, 'b) pair =
              | Pair of Lvca_syntax.Provenance.t * 'a * 'b 
            and nonempty =
              | Nonempty of Lvca_syntax.Provenance.t * Primitive.String.t *
              Primitive.String.t List.t 
          end
        module Info =
          struct
            let nonempty = function | Types.Nonempty (x0, _, _) -> x0
            let pair = function | Types.Pair (x0, _, _) -> x0
            let foo =
              function
              | Types.Foo (x0, _) -> x0
              | Types.Bar (x0, (_, _, _)) -> x0
              | Types.Foo_var (info, _) -> info
            let pair_plus = function | Types.PairPlus (x0, _, _, _) -> x0
            let term = function | Types.Operator (x0, _) -> x0
            let mut_a = function | Types.Mut_a (x0, _) -> x0
            and mut_b = function | Types.Mut_b (x0, _) -> x0
            let nat = function | Types.Z x0 -> x0 | Types.S (x0, _) -> x0
          end
        module Equivalent =
          struct
            let nonempty ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Nonempty (x0, x1, x2), Types.Nonempty (y0, y1, y2)) ->
                  (info_eq x0 y0) &&
                    ((Primitive.String.equivalent ~info_eq x1 y1) &&
                       (List.equivalent Primitive.String.equivalent ~info_eq
                          x2 y2))
            let pair
              (a :
                ?info_eq:(Lvca_syntax.Provenance.t ->
                            Lvca_syntax.Provenance.t -> bool)
                  -> _ -> _ -> bool)
              (b :
                ?info_eq:(Lvca_syntax.Provenance.t ->
                            Lvca_syntax.Provenance.t -> bool)
                  -> _ -> _ -> bool)
              ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Pair (x0, x1, x2), Types.Pair (y0, y1, y2)) ->
                  (info_eq x0 y0) &&
                    ((a ~info_eq x1 y1) && (b ~info_eq x2 y2))
            let rec foo ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Foo (x0, x1), Types.Foo (y0, y1)) ->
                  (info_eq x0 y0) &&
                    (Primitive.Integer.equivalent ~info_eq x1 y1)
              | (Types.Bar (x0, (x1, x2, x3)), Types.Bar (y0, (y1, y2, y3)))
                  ->
                  (info_eq x0 y0) &&
                    ((Lvca_syntax.Pattern.equivalent ~info_eq x1 y1) &&
                       ((Lvca_syntax.Single_var.equivalent ~info_eq x2 y2) &&
                          (foo ~info_eq x3 y3)))
              | (Types.Foo_var (i1, n1), Types.Foo_var (i2, n2)) ->
                  (info_eq i1 i2) && (let open Base.String in n1 = n2)
              | (_, _) -> false
            let pair_plus
              (a :
                ?info_eq:(Lvca_syntax.Provenance.t ->
                            Lvca_syntax.Provenance.t -> bool)
                  -> _ -> _ -> bool)
              (b :
                ?info_eq:(Lvca_syntax.Provenance.t ->
                            Lvca_syntax.Provenance.t -> bool)
                  -> _ -> _ -> bool)
              ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.PairPlus (x0, x1, x2, x3), Types.PairPlus
                 (y0, y1, y2, y3)) ->
                  (info_eq x0 y0) &&
                    ((a ~info_eq x1 y1) &&
                       ((b ~info_eq x2 y2) && (foo ~info_eq x3 y3)))
            let rec term ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Operator (x0, x1), Types.Operator (y0, y1)) ->
                  (info_eq x0 y0) && (List.equivalent term ~info_eq x1 y1)
            let rec mut_a ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Mut_a (x0, x1), Types.Mut_a (y0, y1)) ->
                  (info_eq x0 y0) && (mut_b ~info_eq x1 y1)
            and mut_b ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Mut_b (x0, x1), Types.Mut_b (y0, y1)) ->
                  (info_eq x0 y0) && (mut_a ~info_eq x1 y1)
            let rec nat ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Z x0, Types.Z y0) -> info_eq x0 y0
              | (Types.S (x0, x1), Types.S (y0, y1)) ->
                  (info_eq x0 y0) && (nat ~info_eq x1 y1)
              | (_, _) -> false
          end
        module To_nominal =
          struct
            let nonempty =
              function
              | Types.Nonempty (x0, x1, x2) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Nonempty",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (Primitive.String.to_nominal x1));
                      Lvca_syntax.Nominal.Scope.Scope
                        ([],
                          (List.to_nominal Primitive.String.to_nominal x2))])
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
                         ([], (Primitive.Integer.to_nominal x1))])
              | Types.Bar (x0, (x1, x2, x3)) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Bar",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([x1;
                          Lvca_syntax.Pattern.Var ((x2.info), (x2.name))],
                           (foo x3))])
              | Types.Foo_var (info, name) ->
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
                         ([], (List.to_nominal term x1))])
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
                  ->
                  (match Primitive.String.of_nominal x1 with
                   | Error err -> Error err
                   | Ok x1 ->
                       (match List.of_nominal Primitive.String.of_nominal x2
                        with
                        | Error err -> Error err
                        | Ok x2 -> Ok (Types.Nonempty (x0, x1, x2))))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let pair a b =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Pair", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::(Lvca_syntax.Nominal.Scope.Scope ([], x2))::[])
                  ->
                  (match a x1 with
                   | Error err -> Error err
                   | Ok x1 ->
                       (match b x2 with
                        | Error err -> Error err
                        | Ok x2 -> Ok (Types.Pair (x0, x1, x2))))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let rec foo =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Foo", (Lvca_syntax.Nominal.Scope.Scope ([], x1))::[])
                  ->
                  (match Primitive.Integer.of_nominal x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.Foo (x0, x1)))
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Bar", (Lvca_syntax.Nominal.Scope.Scope
                   (x1::(Lvca_syntax.Pattern.Var (x2, x3))::[], x4))::[])
                  ->
                  (match foo x4 with
                   | Error err -> Error err
                   | Ok x4 ->
                       Ok
                         (Types.Bar
                            (x0,
                              (x1,
                                (let open Lvca_syntax.Single_var in
                                   { info = x2; name = x3 }), x4))))
              | Lvca_syntax.Nominal.Term.Var (info, name) ->
                  Ok (Types.Foo_var (info, name))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let pair_plus a b =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "PairPlus", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::(Lvca_syntax.Nominal.Scope.Scope
                   ([], x2))::(Lvca_syntax.Nominal.Scope.Scope ([], x3))::[])
                  ->
                  (match a x1 with
                   | Error err -> Error err
                   | Ok x1 ->
                       (match b x2 with
                        | Error err -> Error err
                        | Ok x2 ->
                            (match foo x3 with
                             | Error err -> Error err
                             | Ok x3 -> Ok (Types.PairPlus (x0, x1, x2, x3)))))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let rec term =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Operator", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match List.of_nominal term x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.Operator (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let rec mut_a =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Mut_a", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match mut_b x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.Mut_a (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            and mut_b =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Mut_b", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match mut_a x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.Mut_b (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let rec nat =
              function
              | Lvca_syntax.Nominal.Term.Operator (x0, "Z", []) ->
                  Ok (Types.Z x0)
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "S", (Lvca_syntax.Nominal.Scope.Scope ([], x1))::[])
                  ->
                  (match nat x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.S (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals =
            [("integer",
               (Lvca_syntax.Abstract_syntax.Kind.Kind
                  ((Lvca_syntax.Provenance.Located
                      (Lvca_syntax.Provenance.Located.Parse_located
                         ((let open Lvca_syntax.Provenance.Parse_located in
                             {
                               input =
                                 Lvca_syntax.Provenance.Parse_input.Input_unknown;
                               range =
                                 (Some
                                    (let open Lvca_provenance.Range in
                                       { start = 11; finish = 12 }))
                             })))), 1)));
            ("string",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 ((Lvca_syntax.Provenance.Located
                     (Lvca_syntax.Provenance.Located.Parse_located
                        ((let open Lvca_syntax.Provenance.Parse_located in
                            {
                              input =
                                Lvca_syntax.Provenance.Parse_input.Input_unknown;
                              range =
                                (Some
                                   (let open Lvca_provenance.Range in
                                      { start = 22; finish = 23 }))
                            })))), 1)));
            ("maybe",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 ((Lvca_syntax.Provenance.Located
                     (Lvca_syntax.Provenance.Located.Parse_located
                        ((let open Lvca_syntax.Provenance.Parse_located in
                            {
                              input =
                                Lvca_syntax.Provenance.Parse_input.Input_unknown;
                              range =
                                (Some
                                   (let open Lvca_provenance.Range in
                                      { start = 32; finish = 38 }))
                            })))), 2)));
            ("list",
              (Lvca_syntax.Abstract_syntax.Kind.Kind
                 ((Lvca_syntax.Provenance.Located
                     (Lvca_syntax.Provenance.Located.Parse_located
                        ((let open Lvca_syntax.Provenance.Parse_located in
                            {
                              input =
                                Lvca_syntax.Provenance.Parse_input.Input_unknown;
                              range =
                                (Some
                                   (let open Lvca_provenance.Range in
                                      { start = 46; finish = 52 }))
                            })))), 2)))];
          sort_defs =
            [("foo",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ((Lvca_syntax.Provenance.Located
                           (Lvca_syntax.Provenance.Located.Parse_located
                              ((let open Lvca_syntax.Provenance.Parse_located in
                                  {
                                    input =
                                      Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                    range =
                                      (Some
                                         (let open Lvca_provenance.Range in
                                            { start = 68; finish = 77 }))
                                  })))), "Foo",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((Lvca_syntax.Provenance.Located
                                (Lvca_syntax.Provenance.Located.Source_located
                                   {
                                     pos_fname = "syntax/Abstract_syntax.ml";
                                     pos_lnum = 250;
                                     pos_bol = 6489;
                                     pos_cnum = 6540
                                   })),
                              [Lvca_syntax.Abstract_syntax.Valence.Valence
                                 ([],
                                   (Lvca_syntax.Sort.Name
                                      ((Lvca_syntax.Provenance.Located
                                          (Lvca_syntax.Provenance.Located.Parse_located
                                             ((let open Lvca_syntax.Provenance.Parse_located in
                                                 {
                                                   input =
                                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                   range =
                                                     (Some
                                                        (let open Lvca_provenance.Range in
                                                           {
                                                             start = 69;
                                                             finish = 76
                                                           }))
                                                 })))), "integer")))])));
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 85; finish = 105 }))
                                 })))), "Bar",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([Lvca_syntax.Abstract_syntax.Sort_slot.Sort_pattern
                                    {
                                      pattern_sort =
                                        (Lvca_syntax.Sort.Name
                                           ((Lvca_syntax.Provenance.Located
                                               (Lvca_syntax.Provenance.Located.Parse_located
                                                  ((let open Lvca_syntax.Provenance.Parse_located in
                                                      {
                                                        input =
                                                          Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                        range =
                                                          (Some
                                                             (let open Lvca_provenance.Range in
                                                                {
                                                                  start = 86;
                                                                  finish = 89
                                                                }))
                                                      })))), "foo"));
                                      var_sort =
                                        (Lvca_syntax.Sort.Name
                                           ((Lvca_syntax.Provenance.Located
                                               (Lvca_syntax.Provenance.Located.Parse_located
                                                  ((let open Lvca_syntax.Provenance.Parse_located in
                                                      {
                                                        input =
                                                          Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                        range =
                                                          (Some
                                                             (let open Lvca_provenance.Range in
                                                                {
                                                                  start = 90;
                                                                  finish = 93
                                                                }))
                                                      })))), "foo"))
                                    };
                                 Lvca_syntax.Abstract_syntax.Sort_slot.Sort_binding
                                   (Lvca_syntax.Sort.Name
                                      ((Lvca_syntax.Provenance.Located
                                          (Lvca_syntax.Provenance.Located.Parse_located
                                             ((let open Lvca_syntax.Provenance.Parse_located in
                                                 {
                                                   input =
                                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                   range =
                                                     (Some
                                                        (let open Lvca_provenance.Range in
                                                           {
                                                             start = 96;
                                                             finish = 99
                                                           }))
                                                 })))), "foo"))],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 101;
                                                            finish = 104
                                                          }))
                                                })))), "foo")))])))])));
            ("nat",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 115; finish = 117 }))
                                 })))), "Z",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })), [])));
                   Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                     ((Lvca_syntax.Provenance.Located
                         (Lvca_syntax.Provenance.Located.Parse_located
                            ((let open Lvca_syntax.Provenance.Parse_located in
                                {
                                  input =
                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 121; finish = 126 }))
                                })))), "S",
                       (Lvca_syntax.Abstract_syntax.Arity.Arity
                          ((Lvca_syntax.Provenance.Located
                              (Lvca_syntax.Provenance.Located.Source_located
                                 {
                                   pos_fname = "syntax/Abstract_syntax.ml";
                                   pos_lnum = 250;
                                   pos_bol = 6489;
                                   pos_cnum = 6540
                                 })),
                            [Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Name
                                    ((Lvca_syntax.Provenance.Located
                                        (Lvca_syntax.Provenance.Located.Parse_located
                                           ((let open Lvca_syntax.Provenance.Parse_located in
                                               {
                                                 input =
                                                   Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 122;
                                                           finish = 125
                                                         }))
                                               })))), "nat")))])))])));
            ("pair",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None); ("b", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 144; finish = 150 }))
                                 })))), "Pair",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 145;
                                                            finish = 146
                                                          }))
                                                })))), "a")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Name
                                    ((Lvca_syntax.Provenance.Located
                                        (Lvca_syntax.Provenance.Located.Parse_located
                                           ((let open Lvca_syntax.Provenance.Parse_located in
                                               {
                                                 input =
                                                   Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 148;
                                                           finish = 149
                                                         }))
                                               })))), "b")))])))])));
            ("pair_plus",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None); ("b", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 176; finish = 187 }))
                                 })))), "PairPlus",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 177;
                                                            finish = 178
                                                          }))
                                                })))), "a")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Name
                                    ((Lvca_syntax.Provenance.Located
                                        (Lvca_syntax.Provenance.Located.Parse_located
                                           ((let open Lvca_syntax.Provenance.Parse_located in
                                               {
                                                 input =
                                                   Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 180;
                                                           finish = 181
                                                         }))
                                               })))), "b")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Name
                                    ((Lvca_syntax.Provenance.Located
                                        (Lvca_syntax.Provenance.Located.Parse_located
                                           ((let open Lvca_syntax.Provenance.Parse_located in
                                               {
                                                 input =
                                                   Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 183;
                                                           finish = 186
                                                         }))
                                               })))), "foo")))])))])));
            ("nonempty",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 209; finish = 230 }))
                                 })))), "Nonempty",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 210;
                                                            finish = 216
                                                          }))
                                                })))), "string")));
                             Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Ap
                                    ((Lvca_syntax.Provenance.Located
                                        (Lvca_syntax.Provenance.Located.Parse_located
                                           ((let open Lvca_syntax.Provenance.Parse_located in
                                               {
                                                 input =
                                                   Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 218;
                                                           finish = 222
                                                         }))
                                               })))), "list",
                                      (Lvca_syntax.Sort.Cons
                                         ((Lvca_syntax.Provenance.Located
                                             (Lvca_syntax.Provenance.Located.Source_located
                                                {
                                                  pos_fname =
                                                    "syntax/Sort.ml";
                                                  pos_lnum = 44;
                                                  pos_bol = 1417;
                                                  pos_cnum = 1459
                                                })),
                                           (Lvca_syntax.Sort.Name
                                              ((Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Parse_located
                                                     ((let open Lvca_syntax.Provenance.Parse_located in
                                                         {
                                                           input =
                                                             Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                           range =
                                                             (Some
                                                                (let open Lvca_provenance.Range in
                                                                   {
                                                                    start =
                                                                    223;
                                                                    finish =
                                                                    229
                                                                   }))
                                                         })))), "string")),
                                           (Lvca_syntax.Sort.Nil
                                              (Lvca_syntax.Provenance.Located
                                                 (Lvca_syntax.Provenance.Located.Source_located
                                                    {
                                                      pos_fname =
                                                        "syntax/Sort.ml";
                                                      pos_lnum = 43;
                                                      pos_bol = 1372;
                                                      pos_cnum = 1408
                                                    }))))))))])))])));
            ("term",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 248; finish = 259 }))
                                 })))), "Operator",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 249;
                                                            finish = 253
                                                          }))
                                                })))), "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((Lvca_syntax.Provenance.Located
                                              (Lvca_syntax.Provenance.Located.Source_located
                                                 {
                                                   pos_fname =
                                                     "syntax/Sort.ml";
                                                   pos_lnum = 44;
                                                   pos_bol = 1417;
                                                   pos_cnum = 1459
                                                 })),
                                            (Lvca_syntax.Sort.Name
                                               ((Lvca_syntax.Provenance.Located
                                                   (Lvca_syntax.Provenance.Located.Parse_located
                                                      ((let open Lvca_syntax.Provenance.Parse_located in
                                                          {
                                                            input =
                                                              Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                            range =
                                                              (Some
                                                                 (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    254;
                                                                    finish =
                                                                    258
                                                                    }))
                                                          })))), "term")),
                                            (Lvca_syntax.Sort.Nil
                                               (Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Source_located
                                                     {
                                                       pos_fname =
                                                         "syntax/Sort.ml";
                                                       pos_lnum = 43;
                                                       pos_bol = 1372;
                                                       pos_cnum = 1408
                                                     }))))))))])))])));
            ("mut_a",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 275; finish = 282 }))
                                 })))), "Mut_a",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 276;
                                                            finish = 281
                                                          }))
                                                })))), "mut_b")))])))])));
            ("mut_b",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 297; finish = 304 }))
                                 })))), "Mut_b",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 298;
                                                            finish = 303
                                                          }))
                                                })))), "mut_a")))])))])))]
        }
    module Foo =
      struct
        type t = Wrapper.Types.foo =
          | Foo of Lvca_syntax.Provenance.t * Primitive.Integer.t 
          | Bar of Lvca_syntax.Provenance.t * (Pattern.t *
          Lvca_syntax.Single_var.t * Wrapper.Types.foo) 
          | Foo_var of Lvca_syntax.Provenance.t * string 
        let info = Wrapper.Info.foo
        let equivalent = Wrapper.Equivalent.foo
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
        let equivalent = Wrapper.Equivalent.nat
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
        let equivalent = Wrapper.Equivalent.pair
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
        let equivalent = Wrapper.Equivalent.pair_plus
        let to_nominal = Wrapper.To_nominal.pair_plus
        let of_nominal = Wrapper.Of_nominal.pair_plus
        let mk_PairPlus ~info  x_0 x_1 x_2 = PairPlus (info, x_0, x_1, x_2)
      end
    module Nonempty =
      struct
        type t = Wrapper.Types.nonempty =
          | Nonempty of Lvca_syntax.Provenance.t * Primitive.String.t *
          Primitive.String.t List.t 
        let info = Wrapper.Info.nonempty
        let equivalent = Wrapper.Equivalent.nonempty
        let to_nominal = Wrapper.To_nominal.nonempty
        let of_nominal = Wrapper.Of_nominal.nonempty
        let mk_Nonempty ~info  x_0 x_1 = Nonempty (info, x_0, x_1)
      end
    module Term =
      struct
        type t = Wrapper.Types.term =
          | Operator of Lvca_syntax.Provenance.t * Wrapper.Types.term List.t 
        let info = Wrapper.Info.term
        let equivalent = Wrapper.Equivalent.term
        let to_nominal = Wrapper.To_nominal.term
        let of_nominal = Wrapper.Of_nominal.term
        let mk_Operator ~info  x_0 = Operator (info, x_0)
      end
    module Mut_a =
      struct
        type t = Wrapper.Types.mut_a =
          | Mut_a of Lvca_syntax.Provenance.t * Wrapper.Types.mut_b 
        let info = Wrapper.Info.mut_a
        let equivalent = Wrapper.Equivalent.mut_a
        let to_nominal = Wrapper.To_nominal.mut_a
        let of_nominal = Wrapper.Of_nominal.mut_a
        let mk_Mut_a ~info  x_0 = Mut_a (info, x_0)
      end
    module Mut_b =
      struct
        type t = Wrapper.Types.mut_b =
          | Mut_b of Lvca_syntax.Provenance.t * Wrapper.Types.mut_a 
        let info = Wrapper.Info.mut_b
        let equivalent = Wrapper.Equivalent.mut_b
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
          (t, Lvca_syntax.Nominal.Conversion_error.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val equivalent :
        ?info_eq:(Lvca_syntax.Provenance.t ->
                    Lvca_syntax.Provenance.t -> bool)
          -> t -> t -> bool
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
        module Equivalent =
          struct
            let rec ifz ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Ifz (x0, x1, (x2, x3), x4), Types.Ifz
                 (y0, y1, (y2, y3), y4)) ->
                  (info_eq x0 y0) &&
                    ((ifz ~info_eq x1 y1) &&
                       ((Lvca_syntax.Single_var.equivalent ~info_eq x2 y2) &&
                          ((ifz ~info_eq x3 y3) && (ifz ~info_eq x4 y4))))
              | (Types.Ifz_var (i1, n1), Types.Ifz_var (i2, n2)) ->
                  (info_eq i1 i2) && (let open Base.String in n1 = n2)
              | (_, _) -> false
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
              | Types.Ifz_var (info, name) ->
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
                   | Error err -> Error err
                   | Ok x1 ->
                       (match ifz x4 with
                        | Error err -> Error err
                        | Ok x4 ->
                            (match ifz x5 with
                             | Error err -> Error err
                             | Ok x5 ->
                                 Ok
                                   (Types.Ifz
                                      (x0, x1,
                                        ((let open Lvca_syntax.Single_var in
                                            { info = x2; name = x3 }), x4),
                                        x5)))))
              | Lvca_syntax.Nominal.Term.Var (info, name) ->
                  Ok (Types.Ifz_var (info, name))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
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
                       ((Lvca_syntax.Provenance.Located
                           (Lvca_syntax.Provenance.Located.Parse_located
                              ((let open Lvca_syntax.Provenance.Parse_located in
                                  {
                                    input =
                                      Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                    range =
                                      (Some
                                         (let open Lvca_provenance.Range in
                                            { start = 10; finish = 30 }))
                                  })))), "Ifz",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((Lvca_syntax.Provenance.Located
                                (Lvca_syntax.Provenance.Located.Source_located
                                   {
                                     pos_fname = "syntax/Abstract_syntax.ml";
                                     pos_lnum = 250;
                                     pos_bol = 6489;
                                     pos_cnum = 6540
                                   })),
                              [Lvca_syntax.Abstract_syntax.Valence.Valence
                                 ([],
                                   (Lvca_syntax.Sort.Name
                                      ((Lvca_syntax.Provenance.Located
                                          (Lvca_syntax.Provenance.Located.Parse_located
                                             ((let open Lvca_syntax.Provenance.Parse_located in
                                                 {
                                                   input =
                                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                   range =
                                                     (Some
                                                        (let open Lvca_provenance.Range in
                                                           {
                                                             start = 11;
                                                             finish = 14
                                                           }))
                                                 })))), "ifz")));
                              Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([Lvca_syntax.Abstract_syntax.Sort_slot.Sort_binding
                                    (Lvca_syntax.Sort.Name
                                       ((Lvca_syntax.Provenance.Located
                                           (Lvca_syntax.Provenance.Located.Parse_located
                                              ((let open Lvca_syntax.Provenance.Parse_located in
                                                  {
                                                    input =
                                                      Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                    range =
                                                      (Some
                                                         (let open Lvca_provenance.Range in
                                                            {
                                                              start = 16;
                                                              finish = 19
                                                            }))
                                                  })))), "ifz"))],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 21;
                                                            finish = 24
                                                          }))
                                                })))), "ifz")));
                              Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 26;
                                                            finish = 29
                                                          }))
                                                })))), "ifz")))])))])))]
        }
    module Ifz =
      struct
        type t = Wrapper.Types.ifz =
          | Ifz of Lvca_syntax.Provenance.t * Wrapper.Types.ifz *
          (Lvca_syntax.Single_var.t * Wrapper.Types.ifz) * Wrapper.Types.ifz
          
          | Ifz_var of Lvca_syntax.Provenance.t * string 
        let info = Wrapper.Info.ifz
        let equivalent = Wrapper.Equivalent.ifz
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
              | List_external of Lvca_syntax.Provenance.t * Nominal.Term.t
              list 
            and list_predefined =
              | List_predefined of Lvca_syntax.Provenance.t * predefined list 
            and list_list_string_2 =
              | List_list_string_2 of Lvca_syntax.Provenance.t *
              Nominal.Term.t list_list_a 
            and list_list_string_1 =
              | List_list_string_1 of Lvca_syntax.Provenance.t *
              Nominal.Term.t list list 
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
            let list =
              function | Types.Nil x0 -> x0 | Types.Cons (x0, _, _) -> x0
            let predefined = function | Types.Predefined x0 -> x0
            let list_list_predefined_1 =
              function | Types.List_list_predefined_1 (x0, _) -> x0
            let list_list_a = function | Types.List_list_a (x0, _) -> x0
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
        module Equivalent =
          struct
            let rec list
              (a :
                ?info_eq:(Lvca_syntax.Provenance.t ->
                            Lvca_syntax.Provenance.t -> bool)
                  -> _ -> _ -> bool)
              ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Nil x0, Types.Nil y0) -> info_eq x0 y0
              | (Types.Cons (x0, x1, x2), Types.Cons (y0, y1, y2)) ->
                  (info_eq x0 y0) &&
                    ((a ~info_eq x1 y1) && (list a ~info_eq x2 y2))
              | (_, _) -> false
            let predefined ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Predefined x0, Types.Predefined y0) -> info_eq x0 y0
            let list_list_predefined_1 ?(info_eq= fun _ -> fun _ -> true)  t1
              t2 =
              match (t1, t2) with
              | (Types.List_list_predefined_1 (x0, x1),
                 Types.List_list_predefined_1 (y0, y1)) ->
                  (info_eq x0 y0) && (list (list predefined) ~info_eq x1 y1)
            let list_list_a
              (a :
                ?info_eq:(Lvca_syntax.Provenance.t ->
                            Lvca_syntax.Provenance.t -> bool)
                  -> _ -> _ -> bool)
              ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.List_list_a (x0, x1), Types.List_list_a (y0, y1)) ->
                  (info_eq x0 y0) && (list (list a) ~info_eq x1 y1)
            let list_list_predefined_2 ?(info_eq= fun _ -> fun _ -> true)  t1
              t2 =
              match (t1, t2) with
              | (Types.List_list_predefined_2 (x0, x1),
                 Types.List_list_predefined_2 (y0, y1)) ->
                  (info_eq x0 y0) && (list_list_a predefined ~info_eq x1 y1)
            let list_list_string_1 ?(info_eq= fun _ -> fun _ -> true)  t1 t2
              =
              match (t1, t2) with
              | (Types.List_list_string_1 (x0, x1), Types.List_list_string_1
                 (y0, y1)) ->
                  (info_eq x0 y0) &&
                    (list (list Nominal.Term.equivalent) ~info_eq x1 y1)
            let list_list_string_2 ?(info_eq= fun _ -> fun _ -> true)  t1 t2
              =
              match (t1, t2) with
              | (Types.List_list_string_2 (x0, x1), Types.List_list_string_2
                 (y0, y1)) ->
                  (info_eq x0 y0) &&
                    (list_list_a Nominal.Term.equivalent ~info_eq x1 y1)
            let list_predefined ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.List_predefined (x0, x1), Types.List_predefined
                 (y0, y1)) ->
                  (info_eq x0 y0) && (list predefined ~info_eq x1 y1)
            let list_external ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.List_external (x0, x1), Types.List_external (y0, y1))
                  ->
                  (info_eq x0 y0) &&
                    (list Nominal.Term.equivalent ~info_eq x1 y1)
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
                         ([], (list (list Nominal.Term.to_nominal) x1))])
            let list_list_string_2 =
              function
              | Types.List_list_string_2 (x0, x1) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "List_list_string_2",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([], (list_list_a Nominal.Term.to_nominal x1))])
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
                         ([], (list Nominal.Term.to_nominal x1))])
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
                   | Error err -> Error err
                   | Ok x1 ->
                       (match list a x2 with
                        | Error err -> Error err
                        | Ok x2 -> Ok (Types.Cons (x0, x1, x2))))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let predefined =
              function
              | Lvca_syntax.Nominal.Term.Operator (x0, "Predefined", []) ->
                  Ok (Types.Predefined x0)
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let list_list_predefined_1 =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_predefined_1",
                   (Lvca_syntax.Nominal.Scope.Scope ([], x1))::[])
                  ->
                  (match list (list predefined) x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.List_list_predefined_1 (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let list_list_a a =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_a", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list (list a) x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.List_list_a (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let list_list_predefined_2 =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_predefined_2",
                   (Lvca_syntax.Nominal.Scope.Scope ([], x1))::[])
                  ->
                  (match list_list_a predefined x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.List_list_predefined_2 (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let list_list_string_1 =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_string_1", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list (list Nominal.Term.of_nominal) x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.List_list_string_1 (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let list_list_string_2 =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_list_string_2", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list_list_a Nominal.Term.of_nominal x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.List_list_string_2 (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let list_predefined =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_predefined", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list predefined x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.List_predefined (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let list_external =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "List_external", (Lvca_syntax.Nominal.Scope.Scope
                   ([], x1))::[])
                  ->
                  (match list Nominal.Term.of_nominal x1 with
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.List_external (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals =
            [("string",
               (Lvca_syntax.Abstract_syntax.Kind.Kind
                  ((Lvca_syntax.Provenance.Located
                      (Lvca_syntax.Provenance.Located.Parse_located
                         ((let open Lvca_syntax.Provenance.Parse_located in
                             {
                               input =
                                 Lvca_syntax.Provenance.Parse_input.Input_unknown;
                               range =
                                 (Some
                                    (let open Lvca_provenance.Range in
                                       { start = 10; finish = 11 }))
                             })))), 1)))];
          sort_defs =
            [("predefined",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                  ([],
                    [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                       ((Lvca_syntax.Provenance.Located
                           (Lvca_syntax.Provenance.Located.Parse_located
                              ((let open Lvca_syntax.Provenance.Parse_located in
                                  {
                                    input =
                                      Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                    range =
                                      (Some
                                         (let open Lvca_provenance.Range in
                                            { start = 37; finish = 39 }))
                                  })))), "Predefined",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((Lvca_syntax.Provenance.Located
                                (Lvca_syntax.Provenance.Located.Source_located
                                   {
                                     pos_fname = "syntax/Abstract_syntax.ml";
                                     pos_lnum = 250;
                                     pos_bol = 6489;
                                     pos_cnum = 6540
                                   })), [])))])));
            ("list",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 53; finish = 55 }))
                                 })))), "Nil",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })), [])));
                   Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                     ((Lvca_syntax.Provenance.Located
                         (Lvca_syntax.Provenance.Located.Parse_located
                            ((let open Lvca_syntax.Provenance.Parse_located in
                                {
                                  input =
                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                  range =
                                    (Some
                                       (let open Lvca_provenance.Range in
                                          { start = 62; finish = 73 }))
                                })))), "Cons",
                       (Lvca_syntax.Abstract_syntax.Arity.Arity
                          ((Lvca_syntax.Provenance.Located
                              (Lvca_syntax.Provenance.Located.Source_located
                                 {
                                   pos_fname = "syntax/Abstract_syntax.ml";
                                   pos_lnum = 250;
                                   pos_bol = 6489;
                                   pos_cnum = 6540
                                 })),
                            [Lvca_syntax.Abstract_syntax.Valence.Valence
                               ([],
                                 (Lvca_syntax.Sort.Name
                                    ((Lvca_syntax.Provenance.Located
                                        (Lvca_syntax.Provenance.Located.Parse_located
                                           ((let open Lvca_syntax.Provenance.Parse_located in
                                               {
                                                 input =
                                                   Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                 range =
                                                   (Some
                                                      (let open Lvca_provenance.Range in
                                                         {
                                                           start = 63;
                                                           finish = 64
                                                         }))
                                               })))), "a")));
                            Lvca_syntax.Abstract_syntax.Valence.Valence
                              ([],
                                (Lvca_syntax.Sort.Ap
                                   ((Lvca_syntax.Provenance.Located
                                       (Lvca_syntax.Provenance.Located.Parse_located
                                          ((let open Lvca_syntax.Provenance.Parse_located in
                                              {
                                                input =
                                                  Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                range =
                                                  (Some
                                                     (let open Lvca_provenance.Range in
                                                        {
                                                          start = 66;
                                                          finish = 70
                                                        }))
                                              })))), "list",
                                     (Lvca_syntax.Sort.Cons
                                        ((Lvca_syntax.Provenance.Located
                                            (Lvca_syntax.Provenance.Located.Source_located
                                               {
                                                 pos_fname = "syntax/Sort.ml";
                                                 pos_lnum = 44;
                                                 pos_bol = 1417;
                                                 pos_cnum = 1459
                                               })),
                                          (Lvca_syntax.Sort.Name
                                             ((Lvca_syntax.Provenance.Located
                                                 (Lvca_syntax.Provenance.Located.Parse_located
                                                    ((let open Lvca_syntax.Provenance.Parse_located in
                                                        {
                                                          input =
                                                            Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                          range =
                                                            (Some
                                                               (let open Lvca_provenance.Range in
                                                                  {
                                                                    start =
                                                                    71;
                                                                    finish =
                                                                    72
                                                                  }))
                                                        })))), "a")),
                                          (Lvca_syntax.Sort.Nil
                                             (Lvca_syntax.Provenance.Located
                                                (Lvca_syntax.Provenance.Located.Source_located
                                                   {
                                                     pos_fname =
                                                       "syntax/Sort.ml";
                                                     pos_lnum = 43;
                                                     pos_bol = 1372;
                                                     pos_cnum = 1408
                                                   }))))))))])))])));
            ("list_external",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 104; finish = 117 }))
                                 })))), "List_external",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 105;
                                                            finish = 109
                                                          }))
                                                })))), "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((Lvca_syntax.Provenance.Located
                                              (Lvca_syntax.Provenance.Located.Source_located
                                                 {
                                                   pos_fname =
                                                     "syntax/Sort.ml";
                                                   pos_lnum = 44;
                                                   pos_bol = 1417;
                                                   pos_cnum = 1459
                                                 })),
                                            (Lvca_syntax.Sort.Name
                                               ((Lvca_syntax.Provenance.Located
                                                   (Lvca_syntax.Provenance.Located.Parse_located
                                                      ((let open Lvca_syntax.Provenance.Parse_located in
                                                          {
                                                            input =
                                                              Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                            range =
                                                              (Some
                                                                 (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    110;
                                                                    finish =
                                                                    116
                                                                    }))
                                                          })))), "string")),
                                            (Lvca_syntax.Sort.Nil
                                               (Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Source_located
                                                     {
                                                       pos_fname =
                                                         "syntax/Sort.ml";
                                                       pos_lnum = 43;
                                                       pos_bol = 1372;
                                                       pos_cnum = 1408
                                                     }))))))))])))])));
            ("list_predefined",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 152; finish = 169 }))
                                 })))), "List_predefined",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 153;
                                                            finish = 157
                                                          }))
                                                })))), "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((Lvca_syntax.Provenance.Located
                                              (Lvca_syntax.Provenance.Located.Source_located
                                                 {
                                                   pos_fname =
                                                     "syntax/Sort.ml";
                                                   pos_lnum = 44;
                                                   pos_bol = 1417;
                                                   pos_cnum = 1459
                                                 })),
                                            (Lvca_syntax.Sort.Name
                                               ((Lvca_syntax.Provenance.Located
                                                   (Lvca_syntax.Provenance.Located.Parse_located
                                                      ((let open Lvca_syntax.Provenance.Parse_located in
                                                          {
                                                            input =
                                                              Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                            range =
                                                              (Some
                                                                 (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    158;
                                                                    finish =
                                                                    168
                                                                    }))
                                                          })))),
                                                 "predefined")),
                                            (Lvca_syntax.Sort.Nil
                                               (Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Source_located
                                                     {
                                                       pos_fname =
                                                         "syntax/Sort.ml";
                                                       pos_lnum = 43;
                                                       pos_bol = 1372;
                                                       pos_cnum = 1408
                                                     }))))))))])))])));
            ("list_list_a",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([("a", None)],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 199; finish = 214 }))
                                 })))), "List_list_a",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 200;
                                                            finish = 204
                                                          }))
                                                })))), "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((Lvca_syntax.Provenance.Located
                                              (Lvca_syntax.Provenance.Located.Source_located
                                                 {
                                                   pos_fname =
                                                     "syntax/Sort.ml";
                                                   pos_lnum = 44;
                                                   pos_bol = 1417;
                                                   pos_cnum = 1459
                                                 })),
                                            (Lvca_syntax.Sort.Ap
                                               ((Lvca_syntax.Provenance.Located
                                                   (Lvca_syntax.Provenance.Located.Parse_located
                                                      ((let open Lvca_syntax.Provenance.Parse_located in
                                                          {
                                                            input =
                                                              Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                            range =
                                                              (Some
                                                                 (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    206;
                                                                    finish =
                                                                    210
                                                                    }))
                                                          })))), "list",
                                                 (Lvca_syntax.Sort.Cons
                                                    ((Lvca_syntax.Provenance.Located
                                                        (Lvca_syntax.Provenance.Located.Source_located
                                                           {
                                                             pos_fname =
                                                               "syntax/Sort.ml";
                                                             pos_lnum = 44;
                                                             pos_bol = 1417;
                                                             pos_cnum = 1459
                                                           })),
                                                      (Lvca_syntax.Sort.Name
                                                         ((Lvca_syntax.Provenance.Located
                                                             (Lvca_syntax.Provenance.Located.Parse_located
                                                                ((let open Lvca_syntax.Provenance.Parse_located in
                                                                    {
                                                                    input =
                                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                                    range =
                                                                    (Some
                                                                    (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    211;
                                                                    finish =
                                                                    212
                                                                    }))
                                                                    })))),
                                                           "a")),
                                                      (Lvca_syntax.Sort.Nil
                                                         (Lvca_syntax.Provenance.Located
                                                            (Lvca_syntax.Provenance.Located.Source_located
                                                               {
                                                                 pos_fname =
                                                                   "syntax/Sort.ml";
                                                                 pos_lnum =
                                                                   43;
                                                                 pos_bol =
                                                                   1372;
                                                                 pos_cnum =
                                                                   1408
                                                               }))))))),
                                            (Lvca_syntax.Sort.Nil
                                               (Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Source_located
                                                     {
                                                       pos_fname =
                                                         "syntax/Sort.ml";
                                                       pos_lnum = 43;
                                                       pos_bol = 1372;
                                                       pos_cnum = 1408
                                                     }))))))))])))])));
            ("list_list_string_1",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 255; finish = 275 }))
                                 })))), "List_list_string_1",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 256;
                                                            finish = 260
                                                          }))
                                                })))), "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((Lvca_syntax.Provenance.Located
                                              (Lvca_syntax.Provenance.Located.Source_located
                                                 {
                                                   pos_fname =
                                                     "syntax/Sort.ml";
                                                   pos_lnum = 44;
                                                   pos_bol = 1417;
                                                   pos_cnum = 1459
                                                 })),
                                            (Lvca_syntax.Sort.Ap
                                               ((Lvca_syntax.Provenance.Located
                                                   (Lvca_syntax.Provenance.Located.Parse_located
                                                      ((let open Lvca_syntax.Provenance.Parse_located in
                                                          {
                                                            input =
                                                              Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                            range =
                                                              (Some
                                                                 (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    262;
                                                                    finish =
                                                                    266
                                                                    }))
                                                          })))), "list",
                                                 (Lvca_syntax.Sort.Cons
                                                    ((Lvca_syntax.Provenance.Located
                                                        (Lvca_syntax.Provenance.Located.Source_located
                                                           {
                                                             pos_fname =
                                                               "syntax/Sort.ml";
                                                             pos_lnum = 44;
                                                             pos_bol = 1417;
                                                             pos_cnum = 1459
                                                           })),
                                                      (Lvca_syntax.Sort.Name
                                                         ((Lvca_syntax.Provenance.Located
                                                             (Lvca_syntax.Provenance.Located.Parse_located
                                                                ((let open Lvca_syntax.Provenance.Parse_located in
                                                                    {
                                                                    input =
                                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                                    range =
                                                                    (Some
                                                                    (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    267;
                                                                    finish =
                                                                    273
                                                                    }))
                                                                    })))),
                                                           "string")),
                                                      (Lvca_syntax.Sort.Nil
                                                         (Lvca_syntax.Provenance.Located
                                                            (Lvca_syntax.Provenance.Located.Source_located
                                                               {
                                                                 pos_fname =
                                                                   "syntax/Sort.ml";
                                                                 pos_lnum =
                                                                   43;
                                                                 pos_bol =
                                                                   1372;
                                                                 pos_cnum =
                                                                   1408
                                                               }))))))),
                                            (Lvca_syntax.Sort.Nil
                                               (Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Source_located
                                                     {
                                                       pos_fname =
                                                         "syntax/Sort.ml";
                                                       pos_lnum = 43;
                                                       pos_bol = 1372;
                                                       pos_cnum = 1408
                                                     }))))))))])))])));
            ("list_list_string_2",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 316; finish = 336 }))
                                 })))), "List_list_string_2",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 317;
                                                            finish = 328
                                                          }))
                                                })))), "list_list_a",
                                       (Lvca_syntax.Sort.Cons
                                          ((Lvca_syntax.Provenance.Located
                                              (Lvca_syntax.Provenance.Located.Source_located
                                                 {
                                                   pos_fname =
                                                     "syntax/Sort.ml";
                                                   pos_lnum = 44;
                                                   pos_bol = 1417;
                                                   pos_cnum = 1459
                                                 })),
                                            (Lvca_syntax.Sort.Name
                                               ((Lvca_syntax.Provenance.Located
                                                   (Lvca_syntax.Provenance.Located.Parse_located
                                                      ((let open Lvca_syntax.Provenance.Parse_located in
                                                          {
                                                            input =
                                                              Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                            range =
                                                              (Some
                                                                 (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    329;
                                                                    finish =
                                                                    335
                                                                    }))
                                                          })))), "string")),
                                            (Lvca_syntax.Sort.Nil
                                               (Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Source_located
                                                     {
                                                       pos_fname =
                                                         "syntax/Sort.ml";
                                                       pos_lnum = 43;
                                                       pos_bol = 1372;
                                                       pos_cnum = 1408
                                                     }))))))))])))])));
            ("list_list_predefined_1",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 385; finish = 409 }))
                                 })))), "List_list_predefined_1",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 386;
                                                            finish = 390
                                                          }))
                                                })))), "list",
                                       (Lvca_syntax.Sort.Cons
                                          ((Lvca_syntax.Provenance.Located
                                              (Lvca_syntax.Provenance.Located.Source_located
                                                 {
                                                   pos_fname =
                                                     "syntax/Sort.ml";
                                                   pos_lnum = 44;
                                                   pos_bol = 1417;
                                                   pos_cnum = 1459
                                                 })),
                                            (Lvca_syntax.Sort.Ap
                                               ((Lvca_syntax.Provenance.Located
                                                   (Lvca_syntax.Provenance.Located.Parse_located
                                                      ((let open Lvca_syntax.Provenance.Parse_located in
                                                          {
                                                            input =
                                                              Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                            range =
                                                              (Some
                                                                 (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    392;
                                                                    finish =
                                                                    396
                                                                    }))
                                                          })))), "list",
                                                 (Lvca_syntax.Sort.Cons
                                                    ((Lvca_syntax.Provenance.Located
                                                        (Lvca_syntax.Provenance.Located.Source_located
                                                           {
                                                             pos_fname =
                                                               "syntax/Sort.ml";
                                                             pos_lnum = 44;
                                                             pos_bol = 1417;
                                                             pos_cnum = 1459
                                                           })),
                                                      (Lvca_syntax.Sort.Name
                                                         ((Lvca_syntax.Provenance.Located
                                                             (Lvca_syntax.Provenance.Located.Parse_located
                                                                ((let open Lvca_syntax.Provenance.Parse_located in
                                                                    {
                                                                    input =
                                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                                    range =
                                                                    (Some
                                                                    (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    397;
                                                                    finish =
                                                                    407
                                                                    }))
                                                                    })))),
                                                           "predefined")),
                                                      (Lvca_syntax.Sort.Nil
                                                         (Lvca_syntax.Provenance.Located
                                                            (Lvca_syntax.Provenance.Located.Source_located
                                                               {
                                                                 pos_fname =
                                                                   "syntax/Sort.ml";
                                                                 pos_lnum =
                                                                   43;
                                                                 pos_bol =
                                                                   1372;
                                                                 pos_cnum =
                                                                   1408
                                                               }))))))),
                                            (Lvca_syntax.Sort.Nil
                                               (Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Source_located
                                                     {
                                                       pos_fname =
                                                         "syntax/Sort.ml";
                                                       pos_lnum = 43;
                                                       pos_bol = 1372;
                                                       pos_cnum = 1408
                                                     }))))))))])))])));
            ("list_list_predefined_2",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 458; finish = 482 }))
                                 })))), "List_list_predefined_2",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Ap
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 459;
                                                            finish = 470
                                                          }))
                                                })))), "list_list_a",
                                       (Lvca_syntax.Sort.Cons
                                          ((Lvca_syntax.Provenance.Located
                                              (Lvca_syntax.Provenance.Located.Source_located
                                                 {
                                                   pos_fname =
                                                     "syntax/Sort.ml";
                                                   pos_lnum = 44;
                                                   pos_bol = 1417;
                                                   pos_cnum = 1459
                                                 })),
                                            (Lvca_syntax.Sort.Name
                                               ((Lvca_syntax.Provenance.Located
                                                   (Lvca_syntax.Provenance.Located.Parse_located
                                                      ((let open Lvca_syntax.Provenance.Parse_located in
                                                          {
                                                            input =
                                                              Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                            range =
                                                              (Some
                                                                 (let open Lvca_provenance.Range in
                                                                    {
                                                                    start =
                                                                    471;
                                                                    finish =
                                                                    481
                                                                    }))
                                                          })))),
                                                 "predefined")),
                                            (Lvca_syntax.Sort.Nil
                                               (Lvca_syntax.Provenance.Located
                                                  (Lvca_syntax.Provenance.Located.Source_located
                                                     {
                                                       pos_fname =
                                                         "syntax/Sort.ml";
                                                       pos_lnum = 43;
                                                       pos_bol = 1372;
                                                       pos_cnum = 1408
                                                     }))))))))])))])))]
        }
    module Predefined =
      struct
        type t = Wrapper.Types.predefined =
          | Predefined of Lvca_syntax.Provenance.t 
        let info = Wrapper.Info.predefined
        let equivalent = Wrapper.Equivalent.predefined
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
        let equivalent = Wrapper.Equivalent.list
        let to_nominal = Wrapper.To_nominal.list
        let of_nominal = Wrapper.Of_nominal.list
        let mk_Nil ~info  = Nil info
        let mk_Cons ~info  x_0 x_1 = Cons (info, x_0, x_1)
      end
    module List_external =
      struct
        type t = Wrapper.Types.list_external =
          | List_external of Lvca_syntax.Provenance.t * Nominal.Term.t
          Wrapper.Types.list 
        let info = Wrapper.Info.list_external
        let equivalent = Wrapper.Equivalent.list_external
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
        let equivalent = Wrapper.Equivalent.list_predefined
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
        let equivalent = Wrapper.Equivalent.list_list_a
        let to_nominal = Wrapper.To_nominal.list_list_a
        let of_nominal = Wrapper.Of_nominal.list_list_a
        let mk_List_list_a ~info  x_0 = List_list_a (info, x_0)
      end
    module List_list_string_1 =
      struct
        type t = Wrapper.Types.list_list_string_1 =
          | List_list_string_1 of Lvca_syntax.Provenance.t * Nominal.Term.t
          Wrapper.Types.list Wrapper.Types.list 
        let info = Wrapper.Info.list_list_string_1
        let equivalent = Wrapper.Equivalent.list_list_string_1
        let to_nominal = Wrapper.To_nominal.list_list_string_1
        let of_nominal = Wrapper.Of_nominal.list_list_string_1
        let mk_List_list_string_1 ~info  x_0 = List_list_string_1 (info, x_0)
      end
    module List_list_string_2 =
      struct
        type t = Wrapper.Types.list_list_string_2 =
          | List_list_string_2 of Lvca_syntax.Provenance.t * Nominal.Term.t
          Wrapper.Types.list_list_a 
        let info = Wrapper.Info.list_list_string_2
        let equivalent = Wrapper.Equivalent.list_list_string_2
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
        let equivalent = Wrapper.Equivalent.list_list_predefined_1
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
        let equivalent = Wrapper.Equivalent.list_list_predefined_2
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
          | Sort of Lvca_syntax.Provenance.t * Sort.t 
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
          (t, Lvca_syntax.Nominal.Conversion_error.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val equivalent :
        ?info_eq:(Lvca_syntax.Provenance.t ->
                    Lvca_syntax.Provenance.t -> bool)
          -> t -> t -> bool
      val mk_Rec : info:Lvca_syntax.Provenance.t -> t
      val mk_No_rec : info:Lvca_syntax.Provenance.t -> t
    end
    module Ty :
    sig
      type t = Wrapper.Types.ty =
        | Sort of Lvca_syntax.Provenance.t * Sort.t 
        | Arrow of Lvca_syntax.Provenance.t * Wrapper.Types.ty *
        Wrapper.Types.ty 
      val to_nominal : t -> Lvca_syntax.Nominal.Term.t
      val of_nominal :
        Lvca_syntax.Nominal.Term.t ->
          (t, Lvca_syntax.Nominal.Conversion_error.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val equivalent :
        ?info_eq:(Lvca_syntax.Provenance.t ->
                    Lvca_syntax.Provenance.t -> bool)
          -> t -> t -> bool
      val mk_Sort : info:Lvca_syntax.Provenance.t -> Sort.t -> t
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
          (t, Lvca_syntax.Nominal.Conversion_error.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val equivalent :
        ?info_eq:(Lvca_syntax.Provenance.t ->
                    Lvca_syntax.Provenance.t -> bool)
          -> t -> t -> bool
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
          (t, Lvca_syntax.Nominal.Conversion_error.t) Result.t
      val info : t -> Lvca_syntax.Provenance.t
      val equivalent :
        ?info_eq:(Lvca_syntax.Provenance.t ->
                    Lvca_syntax.Provenance.t -> bool)
          -> t -> t -> bool
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
           ('a_, Lvca_syntax.Nominal.Conversion_error.t) Result.t)
          ->
          Lvca_syntax.Nominal.Term.t ->
            ('a_ t, Lvca_syntax.Nominal.Conversion_error.t) Result.t
      val info : 'a_ t -> Lvca_syntax.Provenance.t
      val equivalent :
        (?info_eq:(Lvca_syntax.Provenance.t ->
                     Lvca_syntax.Provenance.t -> bool)
           -> 'a_ -> 'a_ -> bool)
          ->
          ?info_eq:(Lvca_syntax.Provenance.t ->
                      Lvca_syntax.Provenance.t -> bool)
            -> 'a_ t -> 'a_ t -> bool
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
            let option =
              function | Types.None x0 -> x0 | Types.Some (x0, _) -> x0
          end
        module Equivalent =
          struct
            let option
              (a :
                ?info_eq:(Lvca_syntax.Provenance.t ->
                            Lvca_syntax.Provenance.t -> bool)
                  -> _ -> _ -> bool)
              ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.None x0, Types.None y0) -> info_eq x0 y0
              | (Types.Some (x0, x1), Types.Some (y0, y1)) ->
                  (info_eq x0 y0) && (a ~info_eq x1 y1)
              | (_, _) -> false
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
                   | Error err -> Error err
                   | Ok x1 -> Ok (Types.Some (x0, x1)))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
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
                       ((Lvca_syntax.Provenance.Located
                           (Lvca_syntax.Provenance.Located.Parse_located
                              ((let open Lvca_syntax.Provenance.Parse_located in
                                  {
                                    input =
                                      Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                    range =
                                      (Some
                                         (let open Lvca_provenance.Range in
                                            { start = 16; finish = 18 }))
                                  })))), "None",
                         (Lvca_syntax.Abstract_syntax.Arity.Arity
                            ((Lvca_syntax.Provenance.Located
                                (Lvca_syntax.Provenance.Located.Source_located
                                   {
                                     pos_fname = "syntax/Abstract_syntax.ml";
                                     pos_lnum = 250;
                                     pos_bol = 6489;
                                     pos_cnum = 6540
                                   })), [])));
                    Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 25; finish = 28 }))
                                 })))), "Some",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 26;
                                                            finish = 27
                                                          }))
                                                })))), "a")))])))])))]
        }
    module Option =
      struct
        type 'a t = 'a Wrapper.Types.option =
          | None of Lvca_syntax.Provenance.t 
          | Some of Lvca_syntax.Provenance.t * 'a 
        let info = Wrapper.Info.option
        let equivalent = Wrapper.Equivalent.option
        let to_nominal = Wrapper.To_nominal.option
        let of_nominal = Wrapper.Of_nominal.option
        let mk_None ~info  = None info
        let mk_Some ~info  x_0 = Some (info, x_0)
      end
  end 
module Empty =
  struct
    module Wrapper =
      struct
        module Types = struct type empty = | end
        module Info =
          struct let empty = function | (_ : Types.empty) -> . end
        module Equivalent =
          struct let empty = function | (_ : Types.empty) -> . end
        module To_nominal =
          struct let empty = function | (_ : Types.empty) -> . end
        module Of_nominal =
          struct
            let empty =
              function
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals = [];
          sort_defs =
            [("empty",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def ([], [])))]
        }
    module Empty =
      struct
        type t = Wrapper.Types.empty = |
        let info = Wrapper.Info.empty
        let equivalent = Wrapper.Equivalent.empty
        let to_nominal = Wrapper.To_nominal.empty
        let of_nominal = Wrapper.Of_nominal.empty
      end
  end
module Empty_as_var =
  struct
    module Wrapper =
      struct
        module Types =
          struct
            type foo =
              | Foo of Lvca_syntax.Provenance.t * (Lvca_syntax.Single_var.t *
              empty) 
            and empty =
              | Empty_var of Lvca_syntax.Provenance.t * string 
          end
        module Info =
          struct
            let empty = function | Types.Empty_var (info, _) -> info
            let foo = function | Types.Foo (x0, (_, _)) -> x0
          end
        module Equivalent =
          struct
            let empty ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Empty_var (i1, n1), Types.Empty_var (i2, n2)) ->
                  (info_eq i1 i2) && (let open Base.String in n1 = n2)
            let foo ?(info_eq= fun _ -> fun _ -> true)  t1 t2 =
              match (t1, t2) with
              | (Types.Foo (x0, (x1, x2)), Types.Foo (y0, (y1, y2))) ->
                  (info_eq x0 y0) &&
                    ((Lvca_syntax.Single_var.equivalent ~info_eq x1 y1) &&
                       (empty ~info_eq x2 y2))
          end
        module To_nominal =
          struct
            let empty =
              function
              | Types.Empty_var (info, name) ->
                  Lvca_syntax.Nominal.Term.Var (info, name)
            let foo =
              function
              | Types.Foo (x0, (x1, x2)) ->
                  Lvca_syntax.Nominal.Term.Operator
                    (x0, "Foo",
                      [Lvca_syntax.Nominal.Scope.Scope
                         ([Lvca_syntax.Pattern.Var ((x1.info), (x1.name))],
                           (empty x2))])
          end
        module Of_nominal =
          struct
            let empty =
              function
              | Lvca_syntax.Nominal.Term.Var (info, name) ->
                  Ok (Types.Empty_var (info, name))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
            let foo =
              function
              | Lvca_syntax.Nominal.Term.Operator
                  (x0, "Foo", (Lvca_syntax.Nominal.Scope.Scope
                   ((Lvca_syntax.Pattern.Var (x1, x2))::[], x3))::[])
                  ->
                  (match empty x3 with
                   | Error err -> Error err
                   | Ok x3 ->
                       Ok
                         (Types.Foo
                            (x0,
                              ((let open Lvca_syntax.Single_var in
                                  { info = x1; name = x2 }), x3))))
              | tm ->
                  let err =
                    Lvca_syntax.Nominal.Conversion_error.mk_Term
                      ~provenance:(Lvca_syntax.Provenance.Located
                                     (Lvca_syntax.Provenance.Located.Source_located
                                        {
                                          pos_fname =
                                            "ppx_lvca/Module_builder.ml";
                                          pos_lnum = 884;
                                          pos_bol = 27499;
                                          pos_cnum = 27533
                                        })) tm in
                  Error err
          end
      end
    module Types = Wrapper.Types
    let language =
      let open Lvca_syntax.Abstract_syntax in
        {
          externals = [];
          sort_defs =
            [("empty",
               (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def ([], [])));
            ("foo",
              (Lvca_syntax.Abstract_syntax.Sort_def.Sort_def
                 ([],
                   [Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
                      ((Lvca_syntax.Provenance.Located
                          (Lvca_syntax.Provenance.Located.Parse_located
                             ((let open Lvca_syntax.Provenance.Parse_located in
                                 {
                                   input =
                                     Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                   range =
                                     (Some
                                        (let open Lvca_provenance.Range in
                                           { start = 20; finish = 34 }))
                                 })))), "Foo",
                        (Lvca_syntax.Abstract_syntax.Arity.Arity
                           ((Lvca_syntax.Provenance.Located
                               (Lvca_syntax.Provenance.Located.Source_located
                                  {
                                    pos_fname = "syntax/Abstract_syntax.ml";
                                    pos_lnum = 250;
                                    pos_bol = 6489;
                                    pos_cnum = 6540
                                  })),
                             [Lvca_syntax.Abstract_syntax.Valence.Valence
                                ([Lvca_syntax.Abstract_syntax.Sort_slot.Sort_binding
                                    (Lvca_syntax.Sort.Name
                                       ((Lvca_syntax.Provenance.Located
                                           (Lvca_syntax.Provenance.Located.Parse_located
                                              ((let open Lvca_syntax.Provenance.Parse_located in
                                                  {
                                                    input =
                                                      Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                    range =
                                                      (Some
                                                         (let open Lvca_provenance.Range in
                                                            {
                                                              start = 21;
                                                              finish = 26
                                                            }))
                                                  })))), "empty"))],
                                  (Lvca_syntax.Sort.Name
                                     ((Lvca_syntax.Provenance.Located
                                         (Lvca_syntax.Provenance.Located.Parse_located
                                            ((let open Lvca_syntax.Provenance.Parse_located in
                                                {
                                                  input =
                                                    Lvca_syntax.Provenance.Parse_input.Input_unknown;
                                                  range =
                                                    (Some
                                                       (let open Lvca_provenance.Range in
                                                          {
                                                            start = 28;
                                                            finish = 33
                                                          }))
                                                })))), "empty")))])))])))]
        }
    module Empty =
      struct
        type t = Wrapper.Types.empty =
          | Empty_var of Lvca_syntax.Provenance.t * string 
        let info = Wrapper.Info.empty
        let equivalent = Wrapper.Equivalent.empty
        let to_nominal = Wrapper.To_nominal.empty
        let of_nominal = Wrapper.Of_nominal.empty
        let mk_Empty_var ~info  name = Empty_var (info, name)
      end
    module Foo =
      struct
        type t = Wrapper.Types.foo =
          | Foo of Lvca_syntax.Provenance.t * (Lvca_syntax.Single_var.t *
          Wrapper.Types.empty) 
        let info = Wrapper.Info.foo
        let equivalent = Wrapper.Equivalent.foo
        let to_nominal = Wrapper.To_nominal.foo
        let of_nominal = Wrapper.Of_nominal.foo
        let mk_Foo ~info  x_0 = Foo (info, x_0)
      end
  end
