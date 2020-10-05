open Base

module CR = struct
  exception PrecisionOverflowException

  let big0 = Bigint.of_int 0
  let big1 = Bigint.of_int 1
  let bigm1 = Bigint.of_int (-1)
  let big2 = Bigint.of_int 2
  let bigm2 = Bigint.of_int (-2)
  let big10 = Bigint.of_int 10

  let shift k n =
    if n = 0 then k
    else if n < 0 then Bigint.shift_right k (-n)
    else Bigint.shift_left k n

(*
  type t =
    { approxmiate: int -> Bigint.t
    (** Give a scaled approximation accurate to 2**n *)
    ; min_prec: int
    (** The smallest precision value with which the above has been called? *)
    ; max_appr: Bigint.t
    (** The scaled approximation corresponding to min_prec *)
    ; appr_valid: bool
    (** min_prec and max_val are valid *)
    }
  *)

  type base =
    { min_prec: int
    ; max_appr: Bigint.t
    ; appr_valid: bool
    }

  let start_base = { min_prec = 0; max_appr = big0; appr_valid = false }

  type cr =
    (* | SlowCR of int * int *)
    | IntCR of Bigint.t
    | AssumedIntCR of t
    | AddCR of t * t
    | ShiftedCR of t * int
    | NegCR of t
    | SelectCR of t * int * t * t
    | MultCR of t * t
    | InvCR of t
    | PrescaledExpCR of t
    | PrescaledCosCR of t
    | IntegralAtanCR of int
    | PrescaledLnCR of t
    | PrescaledAsinCR of t
    | SqrtCR of t
    | GlPiCR

  and t =
    { base: base
    ; cr: cr
    }

  let big_signum : Bigint.t -> int
    = fun i ->
      let open Bigint in
      if i = big0 then 0 else if i > big0 then 1 else -1

  (* constructors *)

  let of_cr cr = { base = start_base; cr }

  let add : t -> t -> t
    = fun x y -> of_cr (AddCR (x, y))
  let shift_left : t -> int -> t
    (* TODO: check_prec? *)
    = fun x n -> of_cr (ShiftedCR (x, n))
  let shift_right : t -> int -> t
    (* TODO: check_prec? *)
    = fun x n -> of_cr (ShiftedCR (x, -n))
  let assume_int : t -> t
    = fun x -> of_cr (AssumedIntCR x)
  let negate : t -> t
    = fun x -> of_cr (NegCR x)
  let subtract x y = of_cr (AddCR (x, negate y))
  let multiply x y = of_cr (MultCR (x, y))
  let inverse x = of_cr (InvCR x)
  let divide x y = of_cr (MultCR (x, inverse y))

  let pi = of_cr GlPiCR
  let half_pi = shift_right pi 1

  (*
  let cos : t -> t
    = failwith "TODO"

  let sin x = subtract half_pi x |> cos

  let asin : t -> t
    = failwith "TODO"

  let acos x = subtract half_pi x |> asin

  (* let low_ln_limit = big8 *)

  let ln : t -> t
    = failwith "TODO"
    *)

  let sqrt x = of_cr (SqrtCR x)

  let bound_log2 : int -> int
    = fun n ->
      Float.of_int (Int.abs n + 1) /. Float.log 2.0
        |> Float.log
        |> Float.round_up
        |> Int.of_float

  let of_bigint : Bigint.t -> t
    = fun x -> of_cr (IntCR x)

  let of_int : int -> t
    = fun x -> x |> Bigint.of_int |> of_bigint

  (* TODO of_float *)

  (* end constructors *)

  let scale k n =
    if n >= 0
    then Bigint.shift_left k n
    else
      let big_add = Bigint.(+) in
      let adj_k = big_add (shift k (n + 1)) big1 in
      Bigint.shift_left adj_k 1
  ;;

  let check_prec n =
    let high = n lsr 28 in
    let high_shifted = n lsr 29 in
    if high lxor high_shifted <> 0 then (
      Caml.Printf.printf
        "n: %n\nhigh: %n\nhigh_shifted: %n\nhigh lxor high_shifted: %n\n"
        n high high_shifted (high lxor high_shifted);
      raise PrecisionOverflowException
    )
  ;;

  (* TODO:
   * iter_msd?
   * simple_ln
   * atan_reciprocal
   *)

  (*
  let compare x y ~relative_tolerance ~absolute_tolerance
    = failwith "TOOD"
    *)

  let known_msd : t -> int
    = fun t ->
      let length =
        if big_signum t.base.max_appr >= 0
        then t.base.max_appr |> Bigint.Unstable.bin_size_t
        else t.base.max_appr |> Bigint.neg |> Bigint.Unstable.bin_size_t
      in
      t.base.min_prec + length - 1

  let msd : t -> int -> int
    = fun t _n ->
      if (not t.base.appr_valid ||
          Bigint.compare t.base.max_appr big1 <= 0 ||
          Bigint.compare t.base.max_appr bigm1 >= 0)
      then
        (* XXX
        get_appr (n - 1)
        *)
        if (Bigint.compare (Bigint.abs t.base.max_appr) big1 <= 0)
        then Int.min_value
        else known_msd t
      else known_msd t

  let rec signum_a : t -> int -> int
    = fun op a ->
      (* TODO
      if appr_valid
      then
        let quick_try = signum max_appr in
        if quick_try <> 0 then quick_try
        *)
      let needed_prec = a - 1 in
      let this_appr = get_appr op needed_prec in
      big_signum this_appr

  and signum : t -> int
    = fun t -> signum' t (-20)

  and signum' : t -> int -> int
    = fun t a ->
      (* TODO check_prec a *)
      let result = signum_a t a in
      if result <> 0
      then result
      else signum' t (a * 2)

  and approximate : t -> int -> Bigint.t
    = fun { cr; _ } p -> match cr with
      | IntCR value -> scale value (-p)
      | AssumedIntCR value ->
        if p >= 0
        then get_appr value p
        else scale (get_appr value 0) (-p)
      | AddCR (op1, op2) ->
        let (+) = Bigint.(+) in
        scale (get_appr op1 (p - 2) + get_appr op2 (p - 2)) (-2)
      | ShiftedCR (op, count) ->
        get_appr op (p - count)
      | NegCR op -> Bigint.neg (get_appr op p)
      | SelectCR (selector, selector_sign, op1, op2) ->
        if (selector_sign < 0) then get_appr op1 p
        else if (selector_sign > 0) then get_appr op2 p
        else
          let op1_appr = get_appr op1 (p - 1) in
          let op2_appr = get_appr op2 (p - 1) in
          let diff = Bigint.(abs (op1_appr - op2_appr)) in
          if Bigint.(diff < big1) then scale op1_appr (-1)
          else if signum selector < 0 then (* XXX mutate *) scale op1_appr (-1)
          else (* XXX mutate *) scale op2_appr (-1)
      | MultCR (x, y) -> approximate_mult_cr x y p
      | InvCR _ -> failwith "TODO InvCR"
      | PrescaledExpCR _ -> failwith "TODO PrescaledExpCR"
      | PrescaledCosCR _ -> failwith "TODO PrescaledCosCR"
      | IntegralAtanCR _ -> failwith "TODO IntegralAtanCR"
      | PrescaledLnCR _ -> failwith "TODO PrescaledLnCR"
      | PrescaledAsinCR _ -> failwith "TODO PrescaledAsinCR"
      | SqrtCR _ -> failwith "TODO SqrtCR"
      | GlPiCR -> failwith "TODO GlPiCR"

  and approximate_mult_cr : t -> t -> int -> Bigint.t
    = fun op1 op2 p ->
      let half_prec = (p lsr 1) - 1 in
      let msd_op1 = msd op1 half_prec in

      (* XXX
      let msd_op2 =
        if msd_op1 = Int.min_value
        then
          msd op2 half_prec
          (* TODO: if msd_op2 = Int.min_value return big0 *)
        else
          failwith "TODO"
      in
      *)

      let prec2 = p - msd_op1 - 3 in
      let appr2 = get_appr op2 prec2 in
      if big_signum appr2 = 0
      then big0
      else
        let msd_op2 = known_msd op2 in
        let prec1 = p - msd_op2 - 3 in
        let appr1 = get_appr op1 prec1 in
        let scale_digits = prec1 + prec2 - p in
        scale Bigint.(appr1 * appr2) scale_digits

  and get_appr : t -> int -> Bigint.t
    = fun ({ base = { min_prec; max_appr; appr_valid }; _ } as t) precision ->
    check_prec precision;
    if appr_valid && precision >= min_prec
    then scale max_appr (min_prec - precision)
    else
      let result = approximate t precision in
      (* TODO
      min_prec <- precision;
      max_appr <- result;
      appr_valid <- true;
      *)
      result
  ;;

  (* more constructors (that use get_appr) *)
  let select selector x y
    = of_cr (SelectCR (selector, big_signum @@ get_appr selector (-20), x, y))
  let max x y = select (subtract x y) y x
  let min x y = select (subtract x y) x y
  let abs x = select x (negate x) x

  let rec exp x =
    let low_prec = -10 in
    let rough_appr = get_appr x low_prec in
    if (Bigint.compare rough_appr big2 > 0 || Bigint.compare rough_appr bigm2 < 0)
    then
      let square_root = exp @@ shift_right x 1 in
      multiply square_root square_root
    else
      of_cr (PrescaledExpCR x)
  (* end more constructors *)

  let rec to_string : ?digits:int -> ?radix:int -> t -> string
    = fun ?digits:(digits=10) ?radix:(radix=10) t ->
      let scaled_cr =
        if radix = 16
        then shift_left t (4 * digits)
        else
          let scale_factor =
            radix |> Bigint.of_int |> Bigint.pow (Bigint.of_int digits)
          in
          Caml.Printf.printf "scale_factor: %s\n" (Bigint.to_string scale_factor);
          multiply t (of_cr (IntCR scale_factor))
      in
      let scaled_int = get_appr scaled_cr 0 in
      let scaled_string : string = scaled_int
        |> of_bigint
        |> abs
        |> to_string ~radix
      in
      let result =
        if digits = 0
        then scaled_string
        else
          let len = String.length scaled_string in
          let len, scaled_string = if len <= digits
            then
              let z = String.make (digits + 1 - len) '0' in
              digits + 1, z ^ scaled_string
            else len, scaled_string
          in
          let whole = scaled_string |> String.subo ~pos:0 ~len:(len - digits) in
          let fraction = scaled_string |> String.subo ~pos:(len - digits) in
          whole ^ "." ^ fraction
      in
        if big_signum scaled_int < 0
        then "-" ^ result
        else result

  (* to_string_float_rep *)

  let bigint_value : t -> Bigint.t
    = fun t -> get_appr t 0
end

let%test_module "Calculator" = (module struct
  let%expect_test _ =
    Caml.Printf.printf "%s\n" (0 |> CR.of_int |> CR.to_string);
    [%expect{| 0 |}]

end);;
