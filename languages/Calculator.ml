open Base

exception EarlyReturn of Bigint.t

module CR = struct
  exception PrecisionOverflowException

  let zero, one, minus_one, neg, (lxor), (+), (-), ( * ), (=), (<>), (>=), (<=), (<), (>) =
    Int32.(zero, one, minus_one, neg, (lxor), (+), (-), ( * ), (=), (<>), (>=), (<=), (<), (>))
  let two = Int32.of_int_exn 2
  let three = Int32.of_int_exn 3
  let four = Int32.of_int_exn 4
  let ten = Int32.of_int_exn 10
  let sixteen = Int32.of_int_exn 16
  let twenty = Int32.of_int_exn 20

  let big0 = Bigint.of_int 0
  let big1 = Bigint.of_int 1
  let bigm1 = Bigint.of_int (-1)
  let big2 = Bigint.of_int 2
  let bigm2 = Bigint.of_int (-2)
  let big10 = Bigint.of_int 10

  let shift k n =
    let n' = Int32.to_int_exn n in
    if n = zero then k
    else if n < zero then Bigint.shift_right k (-n')
    else Bigint.shift_left k n'

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
    { mutable min_prec: int32
    ; mutable max_appr: Bigint.t
    ; mutable appr_valid: bool
    }

  let base_to_string { min_prec; max_appr; appr_valid } =
    Printf.sprintf "{ min_prec = %s; max_appr = %s; appr_valid = %b }"
      (Int32.to_string min_prec)
      (Bigint.to_string max_appr)
      appr_valid

  let start_base = { min_prec = Int32.zero; max_appr = big0; appr_valid = false }

  type cr =
    (* | SlowCR of int * int *)
    | IntCR of Bigint.t
    | AssumedIntCR of t
    | AddCR of t * t
    | ShiftedCR of t * int32
    | NegCR of t
    | SelectCR of t * int32 * t * t
    | MultCR of t * t
    | InvCR of t
    | PrescaledExpCR of t
    | PrescaledCosCR of t
    | IntegralAtanCR of int32
    | PrescaledLnCR of t
    | PrescaledAsinCR of t
    | SqrtCR of t
    | GlPiCR

  and t =
    { base: base
    ; cr: cr
    }

  let debug_to_string { base; _ } =
    Printf.sprintf "{ base = %s; cr = _ }" (base_to_string base)

  let big_signum : Bigint.t -> int32
    = fun i ->
      let open Bigint in
      if i = big0 then Int32.zero else if i > big0 then Int32.one else Int32.minus_one

  (* constructors *)

  let of_cr cr = { base = start_base; cr }

  let add : t -> t -> t
    = fun x y -> of_cr (AddCR (x, y))
  let shift_left : t -> int32 -> t
    (* TODO: check_prec? *)
    = fun x n -> of_cr (ShiftedCR (x, n))
  let shift_right : t -> int32 -> t
    (* TODO: check_prec? *)
    = fun x n -> of_cr (ShiftedCR (x, neg n))
  let assume_int : t -> t
    = fun x -> of_cr (AssumedIntCR x)
  let negate : t -> t
    = fun x -> of_cr (NegCR x)
  let subtract x y = of_cr (AddCR (x, negate y))
  let multiply x y = of_cr (MultCR (x, y))
  let inverse x = of_cr (InvCR x)
  let divide x y = of_cr (MultCR (x, inverse y))

  let pi = of_cr GlPiCR
  let half_pi = shift_right pi one

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

  let bound_log2 : int32 -> int32
    = fun n ->
      Float.of_int (Int.of_int32_exn (Int32.abs n + one)) /. Float.log 2.0
        |> Float.log
        |> Float.round_up
        |> Int32.of_float

  let of_bigint : Bigint.t -> t
    = fun x -> of_cr (IntCR x)

  let of_int : int -> t
    = fun x -> x |> Bigint.of_int |> of_bigint

  (* TODO of_float *)

  (* end constructors *)

  let scale k n =
    let n' = Int32.to_int_exn n in
    if n >= zero
    then Bigint.shift_left k n'
    else
      let big_add = Bigint.(+) in
      let adj_k = big_add (shift k (n + one)) big1 in
      Bigint.shift_left adj_k 1
  ;;

  (* Check that precision is at least a factor of 8 from overflowing the int32 used to
   * hold the precision spec. *)
  let check_prec n =
    let high = Int32.shift_right n 28 in
    let high_shifted =  Int32.shift_right n 29 in
    if high lxor high_shifted <> zero then (
      (*
      let h = Int32.Hex.to_string in
      debug_printf
        {|n: %s
high: %s
high_shifted: %s
high lxor high_shifted: %s
|}
        (h n) (h high) (h high_shifted) (h (high lxor high_shifted));
        *)
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

  let rec signum_a : t -> int32 -> int32
    = fun op a ->
      (* TODO
      if appr_valid
      then
        let quick_try = signum max_appr in
        if quick_try <> 0 then quick_try
        *)
      let needed_prec = Int32.(a - one) in
      let this_appr = get_appr op needed_prec in
      big_signum this_appr

  and signum : t -> int32
    = fun t -> signum' t (neg twenty)

  and signum' : t -> int32 -> int32
    = fun t a ->
      (* TODO check_prec a *)
      let result = signum_a t a in
      if Int32.(result <> zero)
      then result
      else signum' t (a * two)

  and approximate : t -> int32 -> Bigint.t
    = fun { cr; _ } p -> match cr with
      | IntCR value -> scale value (neg p)
      | AssumedIntCR value ->
        if p >= zero
        then get_appr value p
        else scale (get_appr value zero) (neg p)
      | AddCR (op1, op2) ->
        let (+) = Bigint.(+) in
        scale (get_appr op1 (p - two) + get_appr op2 (p - two)) (neg two)
      | ShiftedCR (op, count) ->
        get_appr op (p - count)
      | NegCR op -> Bigint.neg (get_appr op p)
      | SelectCR (selector, selector_sign, op1, op2) ->
        if (selector_sign < zero) then get_appr op1 p
        else if (selector_sign > zero) then get_appr op2 p
        else
          let op1_appr = get_appr op1 (p - one) in
          let op2_appr = get_appr op2 (p - one) in
          let diff = Bigint.(abs (op1_appr - op2_appr)) in
          if Bigint.(diff < big1) then scale op1_appr (neg one)
          else if signum selector < zero then (* XXX mutate *) scale op1_appr (neg one)
          else (* XXX mutate *) scale op2_appr (neg one)
      | MultCR (x, y) -> approximate_mult_cr x y p
      | InvCR _ -> failwith "TODO InvCR"
      | PrescaledExpCR _ -> failwith "TODO PrescaledExpCR"
      | PrescaledCosCR _ -> failwith "TODO PrescaledCosCR"
      | IntegralAtanCR _ -> failwith "TODO IntegralAtanCR"
      | PrescaledLnCR _ -> failwith "TODO PrescaledLnCR"
      | PrescaledAsinCR _ -> failwith "TODO PrescaledAsinCR"
      | SqrtCR _ -> failwith "TODO SqrtCR"
      | GlPiCR -> failwith "TODO GlPiCR"

  and approximate_mult_cr : t -> t -> int32 -> Bigint.t
    = fun op1 op2 p ->
      try
        let half_prec = (Int32.shift_right p 1) - one in
        (* debug_printf "p: %li, half_prec: %li\n" p half_prec; *)
        let msd_op1 = msd op1 half_prec in
        (* debug_printf "msd_op1: %li\n" msd_op1; *)

        let op1, op2, msd_op1 =
          if msd_op1 = Int32.min_value
          then
            let msd_op2 = msd op2 half_prec in
            (* debug_printf "msd_op2: %li\n" msd_op2; *)
            (* Product is small enough that zero will do as an approximation *)
            if msd_op2 = Int32.min_value then raise (EarlyReturn big0);
            (* Swap operands so larger is first *)
            op2, op1, msd_op2
          else
            op1, op2, msd_op1
        in

        (*
        debug_printf "p: %li, msd_op1: %li, p - msd_op1 - 3: %li\n"
          p msd_op1 (p - msd_op1 - three);
          *)
        let prec2 = p - msd_op1 - three in
        let appr2 = get_appr op2 prec2 in
        if big_signum appr2 = zero
        then big0
        else
          let msd_op2 = known_msd op2 in
          let prec1 = p - msd_op2 - three in
          let appr1 = get_appr op1 prec1 in
          let scale_digits = prec1 + prec2 - p in
          scale Bigint.(appr1 * appr2) scale_digits
      with
        EarlyReturn i -> i

  (* Return [value / 2 ** prec] rounded to an integer. *)
  and get_appr : t -> int32 -> Bigint.t
    = fun ({ base = { min_prec; max_appr; appr_valid }; _ } as t) precision ->
    (* debug_printf "precision: %li\n" precision; *)
    check_prec precision;
    if appr_valid && precision >= min_prec
    then scale max_appr (min_prec - precision)
    else
      let result = approximate t precision in
      t.base.min_prec <- precision;
      t.base.max_appr <- result;
      t.base.appr_valid <- true;
      result

  (* Position of the most significant digit.
     If [msd x = n] then [pow 2 (n - 1) < abs x < pow 2 (n + 1)]
     This version assumes that max_appr is valid and sufficiently removed from zero that
     the msd is determined.
   *)
  and known_msd : t -> int32
    = fun t ->
      let length =
        if big_signum t.base.max_appr >= zero
        then t.base.max_appr |> Bigint.Unstable.bin_size_t
        else t.base.max_appr |> Bigint.neg |> Bigint.Unstable.bin_size_t
      in
      (*
      debug_printf {|min_prec: %li, length: %i, known_msd -> %li|}
        t.base.min_prec
        length
        (t.base.min_prec + Int32.of_int_exn length - one)
        ;
        *)
      t.base.min_prec + Int32.of_int_exn length - one

  (* Most significant digit. Returns [Int32.min_value] if the correct answer [< n].
     TODO: return option?
   *)
  and msd : t -> int32 -> int32
    = fun t n ->
      if Int.(not t.base.appr_valid ||
          Bigint.compare t.base.max_appr big1 <= 0 ||
          Bigint.compare t.base.max_appr bigm1 >= 0)
      then
        let (_ : Bigint.t) = get_appr t (n - one) in
        (*
        debug_printf {|Bigint.abs t.base.max_appr: %s
Bigint.compare (Bigint.abs t.base.max_appr) big1: %i
|}
          (Bigint.abs t.base.max_appr |> Bigint.to_string)
          (Bigint.compare (Bigint.abs t.base.max_appr) big1) (* TODO: use <= ? *)
        ;
        *)
        if Int.(Bigint.compare (Bigint.abs t.base.max_appr) big1 <= 0)
        then (
          (* debug_printf "msd: returning Int32.min_value\n"; *)
          (* msd arbitrarily far to the right *)
          Int32.min_value
        )
        else (
          (* debug_printf "msd: returning known_msd t\n"; *)
          known_msd t
        )
      else known_msd t
  ;;

  (* more constructors (that use get_appr) *)
  let select selector x y
    = of_cr (SelectCR (selector, neg twenty |> get_appr selector |> big_signum, x, y))
  let max x y = select (subtract x y) y x
  let min x y = select (subtract x y) x y
  let abs x = select x (negate x) x

  let rec exp x =
    let low_prec = neg ten in
    let rough_appr = get_appr x low_prec in
    if Int.(Bigint.compare rough_appr big2 > 0 || Bigint.compare rough_appr bigm2 < 0)
    then
      let square_root = exp @@ shift_right x one in
      multiply square_root square_root
    else
      of_cr (PrescaledExpCR x)
  (* end more constructors *)

  let to_string : ?digits:int32 -> ?radix:int32 -> t -> string
    = fun ?digits:(digits=ten) ?radix:(radix=ten) t ->
      let scaled_cr =
        if radix = sixteen
        then shift_left t (four * digits)
        else
          let scale_factor =
            radix |> Bigint.of_int32 |> Bigint.pow (Bigint.of_int32 digits)
          in
          (* debug_printf "scale_factor: %s\n" (Bigint.to_string scale_factor); *)
          multiply t (of_cr (IntCR scale_factor))
      in
      (* Caml.Printf.printf "scaled_cr: %s\n" (debug_to_string scaled_cr); *)
      let scaled_int = get_appr scaled_cr zero in
      (* Caml.Printf.printf "scaled_int: %s\n" (Bigint.to_string scaled_int); *)
      let scaled_string : string = scaled_int
        (* |> of_bigint *)
        |> Bigint.abs
        |> Bigint.to_string (* ~radix *)
      in
      let result =
        if digits = zero
        then scaled_string
        else
          let len = scaled_string |> String.length |> Int32.of_int_exn in
          let len, scaled_string = if len <= digits
            then
              let z = String.make (Int.of_int32_exn (digits + one - len)) '0' in
              digits + one, z ^ scaled_string
            else len, scaled_string
          in
          let splitting_pos = Int.of_int32_exn (len - digits) in
          let whole = scaled_string |> String.subo ~pos:0 ~len:splitting_pos in
          let fraction = scaled_string |> String.subo ~pos:splitting_pos in
          whole ^ "." ^ fraction
      in
        if big_signum scaled_int < zero
        then "-" ^ result
        else result

  (* to_string_float_rep *)

  let bigint_value : t -> Bigint.t
    = fun t -> get_appr t Int32.zero
end

let%test_module "Calculator" = (module struct
  let%expect_test _ =
    Caml.Printf.printf "%s\n" (0 |> CR.of_int |> CR.to_string);
    [%expect{| 0.0000000000 |}]

end);;
