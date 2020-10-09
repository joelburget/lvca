open Base

exception EarlyReturn
exception ArithmeticError of string

module I32 = struct
  let two = Int32.of_int_exn 2
  let three = Int32.of_int_exn 3
  let four = Int32.of_int_exn 4
  let five = Int32.of_int_exn 5
  let six = Int32.of_int_exn 6
  let ten = Int32.of_int_exn 10
  let minus_ten = Int32.of_int_exn (-10)
  let sixteen = Int32.of_int_exn 16
  let twenty = Int32.of_int_exn 20
  let thirty = Int32.of_int_exn 30
  let fifty = Int32.of_int_exn 50
  let sixty = Int32.of_int_exn 60
  let hundred = Int32.of_int_exn 100
  let minus_52 =Int32.of_int_exn (-52)
end

module CR = struct
  exception PrecisionOverflowException

  let neg, (+), (-), ( * ), (=), (<>), (>=), (<=), (<), (>) =
    Int32.(neg, (+), (-), ( * ), (=), (<>), (>=), (<=), (<), (>))

  let big0 = Bigint.of_int 0
  let big1 = Bigint.of_int 1
  let bigm1 = Bigint.of_int (-1)
  let big2 = Bigint.of_int 2
  let bigm2 = Bigint.of_int (-2)
  let big3 = Bigint.of_int 3
  let big4 = Bigint.of_int 4
  let big8 = Bigint.of_int 8
  let big10 = Bigint.of_int 10
  let big750 = Bigint.of_int 750
  let bigm750 = Bigint.of_int (-750)

  let string_of_scaled_int scaled_int digits =
    let scaled_string : string = Bigint.(scaled_int |> abs |> to_string) in
    let result =
      if digits = Int32.zero
      then scaled_string
      else
        let len = scaled_string |> String.length |> Int32.of_int_exn in
        let len, scaled_string = if len <= digits
          then
            let z = String.make (Int.of_int32_exn (digits + Int32.one - len)) '0' in
            digits + Int32.one, z ^ scaled_string
          else len, scaled_string
        in
        let splitting_pos = Int.of_int32_exn (len - digits) in
        let whole = scaled_string |> String.subo ~pos:0 ~len:splitting_pos in
        let fraction = scaled_string |> String.subo ~pos:splitting_pos in
        whole ^ "." ^ fraction
    in
      if Bigint.(scaled_int < zero)
      then "-" ^ result
      else result

  let big_shift_left bi i32 = Bigint.shift_left bi (Int.of_int32_exn i32)

  let shift_left_allow_neg bi i =
    let open Int in
    if i < 0
    then Bigint.shift_right bi (neg i)
    else Bigint.shift_left bi i

  let numbits bi = bi |> Bigint.to_zarith_bigint |> Z.numbits

  let shift k n =
    let n' = Int32.to_int_exn n in
    if n = Int32.zero then k
    else if n < Int32.zero then Bigint.shift_right k (-n')
    else big_shift_left k n

  type iterative_adjustments =
    { op_prec : int32
    ; calc_precision : int32
    }

    (*
  type iterative_params =
    {
    }
  *)

  type base =
    { mutable min_prec: int32
    (** The smallest precision value with which the above has been called? *)
    ; mutable max_appr: Bigint.t
    (** The scaled approximation corresponding to min_prec *)
    ; mutable appr_valid: bool
    (** min_prec and max_val are valid *)
    }

  let base_to_string { min_prec; max_appr; appr_valid } =
    Printf.sprintf "{ min_prec = %s; max_appr = %s; appr_valid = %b }"
      (Int32.to_string min_prec)
      (Bigint.to_string max_appr)
      appr_valid

  let new_base () = { min_prec = Int32.zero; max_appr = big0; appr_valid = false }

  type cr =
    (* | SlowCR of int * int *)
    | IntCR of Bigint.t
    | AssumedIntCR of t
    | AddCR of t * t
    | ShiftedCR of t * int32
    | NegCR of t
    | SelectCR of selector
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

  and selector =
    { selector : t
    ; mutable selector_sign: int32
    ; op1: t
    ; op2: t
    }

  let rec debug_cr_to_string = function
    | IntCR i -> Printf.sprintf "IntCR %s" (Bigint.to_string i)
    | AssumedIntCR op -> Printf.sprintf "AssumedIntCR (%s)" (debug_to_string op)
    | AddCR (op1, op2)
    -> Printf.sprintf "AddCR (%s, %s)" (debug_to_string op1) (debug_to_string op2)
    | ShiftedCR (op, shift)
    -> Printf.sprintf "ShiftedCR (%s, %li)" (debug_to_string op) shift
    | NegCR op
    -> Printf.sprintf "NegCR (%s)" (debug_to_string op)
    | SelectCR { selector; selector_sign; op1; op2 }
    -> Printf.sprintf "SelectCR (%s, %li, %s, %s)"
      (debug_to_string selector) selector_sign (debug_to_string op1) (debug_to_string op2)
    | MultCR (op1, op2)
    -> Printf.sprintf "MultCR (%s, %s)" (debug_to_string op1) (debug_to_string op2)
    | InvCR op
    -> Printf.sprintf "InvCR (%s)" (debug_to_string op)
    | PrescaledExpCR op
    -> Printf.sprintf "PrescaledExpCR (%s)" (debug_to_string op)
    | PrescaledCosCR op
    -> Printf.sprintf "PrescaledCosCR (%s)" (debug_to_string op)
    | IntegralAtanCR i
    -> Printf.sprintf "IntegralAtanCR (%li)" i
    | PrescaledLnCR op
    -> Printf.sprintf "PrescaledLnCR (%s)" (debug_to_string op)
    | PrescaledAsinCR op
    -> Printf.sprintf "PrescaledAsinCR (%s)" (debug_to_string op)
    | SqrtCR op
    -> Printf.sprintf "SqrtCR (%s)" (debug_to_string op)
    | GlPiCR -> "GlPiCR"

  and debug_to_string { base; cr } =
    Printf.sprintf "{ base = %s; cr = %s }" (base_to_string base) (debug_cr_to_string cr)

  type operator =
    | App
    | Negate
    | Mul
    | Div
    | Shift
    | Add
    | Sub

  (* An odd amalgamation of Haskell and OCaml rules *)
  let prec : operator -> int
    = function
      | App -> 10
      | Negate -> 8
      | Mul | Div -> 7
      | Shift -> 6
      | Add | Sub -> 5

  (* TODO: this should use associativity as well *)
  let parens_prec op_prec env_prec str =
    if Int.(op_prec <= env_prec)
    then "(" ^ str ^ ")"
    else str

  let rec to_string' prec { cr; _ } = cr_to_string prec cr

  and cr_to_string ambient_prec = function
    | IntCR i -> Bigint.to_string i
    | AssumedIntCR op -> to_string' ambient_prec op
    | AddCR (op1, op2)
    -> let prec' = prec Add in
       parens_prec prec' ambient_prec
         (Printf.sprintf "%s + %s" (to_string' prec' op1) (to_string' prec' op2))
    | ShiftedCR (op, shift)
    -> let prec' = prec Shift in
       parens_prec prec' ambient_prec
         (Printf.sprintf "%s << %li" (to_string' prec' op) shift)
    | NegCR op
    -> let prec' = prec Negate in
       parens_prec prec' ambient_prec (Printf.sprintf "-%s" (to_string' prec' op))
    | SelectCR { selector; selector_sign; op1; op2 }
    (* TODO: this style is inconsistent with the others *)
    -> Printf.sprintf "select(%s, %li, %s, %s)"
             (to_string' 0 selector) selector_sign (to_string' 0 op1) (to_string' 0 op2)
    | MultCR (op1, op2)
    -> let prec' = prec Mul in
       parens_prec prec' ambient_prec
         (Printf.sprintf "%s * %s" (to_string' prec' op1) (to_string' prec' op2))
    | InvCR op
    -> let prec' = prec Div in
       parens_prec prec' ambient_prec
         (Printf.sprintf "1 / %s" (to_string' prec' op))
    | PrescaledExpCR op
    -> Printf.sprintf "exp %s" (to_string' (prec App) op)
    | PrescaledCosCR op
    -> Printf.sprintf "cos %s" (to_string' (prec App) op)
    | IntegralAtanCR i
    -> Printf.sprintf "atan %li" i
    | PrescaledLnCR op
    -> Printf.sprintf "ln %s" (to_string' (prec App) op)
    | PrescaledAsinCR op
    -> Printf.sprintf "asin %s" (to_string' (prec App) op)
    | SqrtCR op
    -> Printf.sprintf "sqrt %s" (to_string' (prec App) op)
    | GlPiCR -> "pi"

  let to_string = to_string' 0

  let big_signum : Bigint.t -> int32
    = fun i ->
      let open Bigint in
      if i = big0 then Int32.zero else if i > big0 then Int32.one else Int32.minus_one

  (* Check that precision is at least a factor of 8 from overflowing the int32 used to
   * hold the precision spec. *)
  let check_prec n =
    let open Int32 in
    let high = shift_right n 28 in
    let high_shifted = shift_right n 29 in
    if high lxor high_shifted <> zero then raise PrecisionOverflowException
  ;;

  (* constructors *)

  let of_cr cr = { base = new_base (); cr }

  let add : t -> t -> t
    = fun x y -> of_cr (AddCR (x, y))
  let shift_left : t -> int32 -> t
    = fun x n ->
      check_prec n;
      of_cr (ShiftedCR (x, n))
  let shift_right : t -> int32 -> t
    = fun x n ->
      check_prec n;
      of_cr (ShiftedCR (x, neg n))
  let assume_int : t -> t
    = fun x -> of_cr (AssumedIntCR x)
  let negate : t -> t
    = fun x -> of_cr (NegCR x)
  let subtract x y = of_cr (AddCR (x, negate y))
  let multiply x y = of_cr (MultCR (x, y))
  let inverse x = of_cr (InvCR x)
  let divide x y = of_cr (MultCR (x, inverse y))

  let pi = of_cr GlPiCR
  let half_pi = shift_right pi Int32.one

  let sqrt x = of_cr (SqrtCR x)

  let bound_log2 : int32 -> int32
    = fun n ->
      let x = Float.of_int (Int.of_int32_exn (Int32.abs n + Int32.one)) in
      Int32.of_float (Float.round_up (Float.log x /. Float.log 2.0))

  let of_bigint : Bigint.t -> t
    = fun x -> of_cr (IntCR x)

  let of_int : int -> t
    = fun x -> x |> Bigint.of_int |> of_bigint

  let of_int32 : int32 -> t
    = fun x -> x |> Int32.to_int_exn |> of_int

  let one = of_int 1
  let minus_one = of_int (-1)
  let two = of_int 2
  let three = of_int 3
  let minus_fifty_two = of_int (-52)

  let of_float n =
    (* TODO: throw for NaN / infinite *)
    let negative = Float.ieee_negative n in
    let pre_mantissa = Int63.to_int_exn (Float.ieee_mantissa n) in
    let exp = Int.(Float.ieee_exponent n - 1023) |> Int32.of_int_exn in

    let p1 = shift_left one exp in
    let p2 = shift_left one I32.minus_52 in

    let mantissa = add one (multiply (of_int pre_mantissa) p2) in
    let result = multiply p1 mantissa in

    if negative then negate result else result

  (* end constructors *)

  (* Multiply by 2**n, rounding result *)
  let scale k n =
    if n >= Int32.zero
    then big_shift_left k n
    else
      (* TODO: Seems like an odd way to do this if I'm understanding *)
      let big_add = Bigint.(+) in
      let adj_k = big_add (shift k (n + Int32.one)) big1 in
      Bigint.shift_right adj_k 1
  ;;

  (* TODO:
   * simple_ln
   * atan_reciprocal
   *)

  (*
  let compare x y ~relative_tolerance ~absolute_tolerance
    = failwith "TOOD"
    *)

  let pi_b_prec : int32 Queue.t
    = Queue.of_list [Int32.zero] (* Initial entry unused *)

  let pi_b_val : Bigint.t Queue.t
    = Queue.of_list [big0] (* Initial entry unused *)

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
    = fun op -> signum' op (neg I32.twenty)

  and signum' : t -> int32 -> int32
    = fun op a ->
      (* TODO check_prec a *)
      let result = signum_a op a in
      if Int32.(result <> zero) then result else signum' op (a * I32.two)

  (* Give a scaled approximation accurate to 2**n *)
  and approximate : t -> int32 -> Bigint.t
    = fun ({ cr; _ } as t) p ->
      match cr with
      | IntCR value -> scale value (neg p)
      | AssumedIntCR value ->
        if p >= Int32.zero
        then get_appr value p
        else scale (get_appr value Int32.zero) (neg p)
      | AddCR (op1, op2) ->
        let (+) = Bigint.(+) in
        scale (get_appr op1 (p - I32.two) + get_appr op2 (p - I32.two)) (neg I32.two)
      | ShiftedCR (op, count) -> get_appr op (p - count)
      | NegCR op -> Bigint.neg (get_appr op p)
      | SelectCR selector ->
        if (selector.selector_sign < Int32.zero) then get_appr selector.op1 p
        else if (selector.selector_sign > Int32.zero) then get_appr selector.op2 p
        else
          let op1_appr = get_appr selector.op1 (p - Int32.one) in
          let op2_appr = get_appr selector.op2 (p - Int32.one) in
          let diff = Bigint.(abs (op1_appr - op2_appr)) in
          if Bigint.(diff <= big1) then scale op1_appr Int32.minus_one
          else if signum selector.selector < Int32.zero
          then (
            selector.selector_sign <- Int32.minus_one;
            scale op1_appr Int32.minus_one
          )
          else (
            selector.selector_sign <- Int32.one;
            scale op2_appr Int32.minus_one
          )
      | MultCR (x, y) -> approximate_mult_cr x y p
      | InvCR op -> approximate_inv_cr op p
      | PrescaledExpCR op -> approximate_prescaled_exp_cr op p
      | PrescaledCosCR op -> approximate_prescaled_cos_cr op p
      | IntegralAtanCR op -> approximate_integral_atan_cr op p
      | PrescaledLnCR op -> approximate_prescaled_ln_cr op p
      | PrescaledAsinCR op -> approximate_prescaled_asin_cr op p
      | SqrtCR op -> approximate_sqrt_cr t op p
      | GlPiCR -> approximate_pi_cr p

  and approximate_mult_cr : t -> t -> int32 -> Bigint.t
    = fun op1 op2 p ->
      try
        let half_prec = Int32.shift_right p 1 - Int32.one in
        (* debug_printf "p: %li, half_prec: %li\n" p half_prec; *)
        let msd_op1 = msd op1 half_prec in
        (* debug_printf "msd_op1: %li\n" msd_op1; *)

        let op1, op2, msd_op1 =
          if msd_op1 = Int32.min_value
          then
            let msd_op2 = msd op2 half_prec in
            (* debug_printf "msd_op2: %li\n" msd_op2; *)
            (* Product is small enough that zero will do as an approximation *)
            if msd_op2 = Int32.min_value then raise EarlyReturn;
            (* Swap operands so larger is first *)
            op2, op1, msd_op2
          else
            op1, op2, msd_op1
        in

        let prec2 = p - msd_op1 - I32.three in
        let appr2 = get_appr op2 prec2 in
        (*
        Caml.Printf.printf "p: %li, msd_op1: %li, prec2: %li, appr2: %s\n"
          p msd_op1 prec2 (appr2 |> Bigint.to_string);
          *)
        if Bigint.(appr2 = zero) then raise EarlyReturn;
        let msd_op2 = known_msd op2 in
        let prec1 = p - msd_op2 - I32.three in
        let appr1 = get_appr op1 prec1 in
        let scale_digits = prec1 + prec2 - p in
        scale Bigint.(appr1 * appr2) scale_digits
      with
        EarlyReturn -> big0
        (* Caml.Printf.printf "early return -> %s\n" (Bigint.to_string i); *)

  and approximate_inv_cr op p =
    let open Int32 in
    let msd = iter_msd op Int32.min_value in
    let inv_msd = Int32.one - msd in
    let digits_needed = inv_msd - p - I32.three in
    let prec_needed = msd - digits_needed in
    let log_scale_factor = neg p - prec_needed in
    if log_scale_factor < Int32.zero then big0
    else
      let open Bigint in
      let dividend = big_shift_left big1 log_scale_factor in
      let scaled_divisor = get_appr op prec_needed in
      let abs_scaled_divisor = abs scaled_divisor in
      let adj_dividend = dividend + shift_right abs_scaled_divisor 1 in
      let result = adj_dividend / abs_scaled_divisor in
      if scaled_divisor < zero then neg result else result

  and approximate_prescaled_asin_cr op p =
    let open Int32 in
    if p >= I32.two then big0
    else
      let iterations_needed = neg I32.three * p / I32.two + I32.four in
      let calc_precision = p - bound_log2 (I32.two * iterations_needed) - I32.four in
      let op_prec = p - I32.three in
      let op_appr = get_appr op op_prec in
      let exp = ref 1 in
      let start_term_val = op_prec - calc_precision |> big_shift_left op_appr
      in
      let current_term = ref start_term_val in
      let current_sum = ref start_term_val in
      let current_factor = ref start_term_val in
      let max_trunc_error = big_shift_left big1 (p - I32.four - calc_precision) in
      while Bigint.(abs !current_term >= max_trunc_error) do
        exp := Int.(!exp + 2);
        let open Bigint in
        current_factor := !current_factor * Bigint.of_int Int.(!exp - 2);
        current_factor := scale (!current_factor * op_appr) Int32.(op_prec + I32.two);
        current_factor := !current_factor * op_appr;
        let divisor = Bigint.of_int Int.(!exp - 1) in
        current_factor := !current_factor / divisor;
        current_factor := scale !current_factor Int32.(op_prec - I32.two);
        current_term := !current_factor / Bigint.of_int !exp;
        current_sum := !current_sum + !current_term;
      done;
      scale !current_sum Int32.(calc_precision - p)

  and approximate_pi_cr p =
    let pi_tolerance = big4 in
    let sqrt_half = sqrt (shift_right one Int32.one) in
    if p >= Int32.zero
    then scale big3 (neg p)
    else
      let extra_eval_prec =
        Int32.of_float
          (Float.round_up (Float.log (Int32.to_float (neg p)) /. Float.log 2.0))
          + I32.ten
      in
      let eval_prec = p - extra_eval_prec in
      (* Caml.Printf.printf "extra_eval_prec: %li, eval_prec: %li\n" extra_eval_prec eval_prec; *)
      let a = ref (big_shift_left big1 (neg eval_prec)) in
      let b = ref (get_appr sqrt_half eval_prec) in
      let t = ref (big_shift_left big1 (neg eval_prec - I32.two)) in
      let n = ref 0 in

      (*
      Caml.Printf.printf "!a: %s, !b: %s\n" (Bigint.to_string !a) (Bigint.to_string !b);
      Caml.Printf.printf "!a - !b - pi_tolerance: %s\n"
        Bigint.(!a - !b - pi_tolerance |> to_string);
        *)
      while Bigint.(!a - !b - pi_tolerance > big0) do
        let next_a = Bigint.(shift_right (!a + !b) 1) in
        let a_diff = Bigint.(!a - next_a) in
        let b_prod = Bigint.(shift_right (!a * !b)
          (eval_prec |> Int32.neg |> Int.of_int32_exn))
        in
        let b_prod_as_cr = shift_right (of_bigint b_prod) (neg eval_prec) in
        let next_b =
          (*
          Caml.Printf.printf "Queue.length pi_b_prec: %i, !n + 1: %i\n"
            (Queue.length pi_b_prec) Int.(!n + 1);
            *)
          if Int.(Queue.length pi_b_prec = !n + 1)
          then (
            (* Add an n+1st slot *)
            let next_b_as_cr = sqrt b_prod_as_cr in
            let next_b = get_appr next_b_as_cr eval_prec in
            let scaled_next_b = scale next_b (neg extra_eval_prec) in
            Queue.enqueue pi_b_prec p;
            Queue.enqueue pi_b_val scaled_next_b;
            next_b
          )
          else (
            let next_b_as_cr =
              { base =
                { min_prec = Queue.get pi_b_prec Int.(!n + 1)
                ; max_appr = Queue.get pi_b_val Int.(!n + 1)
                ; appr_valid = true
                }
              ; cr = SqrtCR b_prod_as_cr
              }
            in
            let next_b = get_appr next_b_as_cr eval_prec in
            Queue.set pi_b_prec Int.(!n + 1) p;
            Queue.set pi_b_val Int.(!n + 1) (scale next_b (neg extra_eval_prec));
            next_b
          )
        in
        let next_t =
          (* shift distance is usually negative *)
          Bigint.(
            !t - (shift_left_allow_neg (a_diff * a_diff) Int.(!n + of_int32_exn eval_prec)))
        in
        (*
        Caml.Printf.printf {|next_a: %s
next_b: %s
next_t: %s
|} (Bigint.to_string next_a) (Bigint.to_string next_b) (Bigint.to_string next_t);
*)
        a := next_a;
        b := next_b;
        t := next_t;
        Int.incr n;
      done;

      let sum = Bigint.(!a + !b) in
      let result = Bigint.(shift_right (sum * sum / !t) 2) in
      scale result (neg extra_eval_prec)

  and approximate_sqrt_cr t op p =
    let open Int32 in
    (* Convervative estimate of number of significant bits in double precision
     * computation*)
    let fp_prec = I32.fifty in
    let fp_op_prec = I32.sixty in
    let max_op_prec_needed = I32.two * p - Int32.one in
    let msd = iter_msd op max_op_prec_needed in
    if msd <= max_op_prec_needed then big0 else
      let result_msd = msd / I32.two in
      let result_digits = result_msd - p in
      if result_digits > fp_prec
      then
        let appr_digits = result_digits / I32.two + I32.six in
        let appr_prec = result_msd - appr_digits in
        let prod_prec = I32.two * appr_prec in
        let op_appr = get_appr op prod_prec in
        let last_appr = get_appr t appr_prec in
        let open Bigint in
        let prod_prec_scaled_numerator = last_appr * last_appr + op_appr in
        let scaled_numerator = scale prod_prec_scaled_numerator Int32.(appr_prec - p) in
        let shifted_result = scaled_numerator / last_appr in
        shift_right (shifted_result + big1) 1
      else
        let op_prec = (msd - fp_op_prec) land lnot Int32.one in
        let working_prec = op_prec - fp_op_prec in
        let scaled_bi_appr = big_shift_left (get_appr op op_prec) fp_op_prec in
        let scaled_appr = Bigint.to_float scaled_bi_appr in
        if Float.(scaled_appr < 0.0) then raise (ArithmeticError "sqrt(negative)");
        let scaled_fp_sqrt = Float.sqrt scaled_appr in
        let scaled_sqrt = Bigint.of_float scaled_fp_sqrt in
        let shift_count = working_prec / I32.two - p in
        shift scaled_sqrt shift_count

  (*
  and iterative' params p =
    if p >= Int32.one then big0
    else
      let iterations_needed = params.iterations_needed p in
      let calc_precision = params.calc_precision iterations_needed in
      while Bigint.(abs !current_term >= max_trunc_error) do
        current_term := loop n;
        current_sum := !current_sum + !current_term
      done;
      scale !current_sum Inte32.(calc_precision - p)
  *)

  and approximate_prescaled_ln_cr op p =
    let open Int32 in
    if p >= Int32.one then big0
    else
      let iterations_needed = neg p in
      let calc_precision = p - bound_log2 (I32.two * iterations_needed) - I32.four in
      let op_prec = p - I32.three in
      let op_appr = get_appr op op_prec in
      let x_nth_val = scale op_appr (op_prec - calc_precision) in
      let x_nth = ref x_nth_val in
      let current_term = ref x_nth_val in
      let current_sum = ref x_nth_val in
      let n = ref 1 in
      let current_sign = ref 1 in
      let max_trunc_error = big_shift_left big1 (p - I32.four - calc_precision) in
      while Bigint.(abs !current_term >= max_trunc_error) do
        let open Bigint in
        Int.incr n;
        current_sign := Int.neg !current_sign;
        x_nth := scale (!x_nth * op_appr) op_prec;
        current_term := !x_nth / Bigint.of_int Int.(!n * !current_sign);
        current_sum := !current_sum + !current_term;
      done;
      scale !current_sum Int32.(calc_precision - p)

  and approximate_integral_atan_cr op p =
    let open Int32 in
    if p >= Int32.one then big0
    else
      let iterations_needed = neg p / I32.two + I32.two in
      let calc_precision = p - bound_log2 (I32.two * iterations_needed) - I32.two in
      let scaled_1 = calc_precision |> neg |> big_shift_left big1
      in
      let big_op = Bigint.of_int32_exn op in
      let big_op_squared = Bigint.of_int32_exn (op * op) in
      let op_inverse = Bigint.(scaled_1 / big_op) in
      let current_power = ref op_inverse in
      let current_term = ref op_inverse in
      let current_sum = ref op_inverse in
      let current_sign = ref 1 in
      let n = ref 1 in
      let max_trunc_error = big_shift_left big1 (p - I32.two - calc_precision) in
      while Bigint.(abs !current_term >= max_trunc_error) do
        n := Int.(!n + 2);
        let open Bigint in
        current_power := !current_power / big_op_squared;
        current_sign := Int.neg !current_sign;
        current_term := !current_power / Bigint.of_int Int.(!current_sign * !n);
        current_sum := !current_sum + !current_term;
      done;
      scale !current_sum Int32.(calc_precision - p)

  and approximate_prescaled_cos_cr op p =
    let open Int32 in
    if p >= Int32.one then big0
    else
      let iterations_needed = neg p / I32.two + I32.four in
      let calc_precision = p - bound_log2 (I32.two * iterations_needed) - I32.four in
      let op_prec = p - I32.two in
      let op_appr = get_appr op op_prec in
      let scaled_1 = big_shift_left big1 (neg calc_precision) in
      let current_term = ref scaled_1 in
      let current_sum = ref scaled_1 in
      let n = ref 0 in
      let max_trunc_error = big_shift_left big1 (p - I32.four - calc_precision) in
      while Bigint.(abs !current_term >= max_trunc_error) do
        n := Int.(!n + 2);
        let open Bigint in
        (* current_term := - current_term * op * op / n * (n - 1) *)
        let numerator = scale
          ((scale (!current_term * op_appr) op_prec) * op_appr)
          op_prec
        in
        let divisor = Bigint.(of_int (Int.neg !n) * of_int Int.(!n - 1)) in
        current_term := numerator / divisor;
        current_sum := !current_sum + !current_term;
      done;
      scale !current_sum Int32.(calc_precision - p)

  and approximate_prescaled_exp_cr op p =
    let open Int32 in
    if p >= Int32.one then big0
    else
      let iterations_needed = neg p / I32.two + I32.two in
      let calc_precision = p - bound_log2 (I32.two * iterations_needed) - I32.four in
      let op_prec = p - I32.three in
      let op_appr = get_appr op op_prec in
      let scaled_1 = calc_precision
        |> neg
        |> big_shift_left big1
      in
      let current_term = ref scaled_1 in
      let current_sum = ref scaled_1 in
      let n = ref 0 in
      let max_trunc_error = big_shift_left big1 (p - I32.four - calc_precision) in
      while Bigint.(abs !current_term >= max_trunc_error) do
        let open Bigint in
        current_term := scale (!current_term * op_appr) op_prec;
        current_term := !current_term / Bigint.of_int !n;
        current_sum := !current_sum + !current_term;
      done;
      scale !current_sum Int32.(calc_precision - p)

  (* Return [value / 2 ** prec] rounded to an integer. *)
  and get_appr : t -> int32 -> Bigint.t
    = fun ({ base = { min_prec; max_appr; appr_valid }; _ } as op) precision ->
    (* debug_printf "precision: %li\n" precision; *)
    check_prec precision;
    if appr_valid && precision >= min_prec
    then (
      (*
      Caml.Printf.printf {|get_appr returning early (appr_valid).
max_appr: %s
min_prec - precision: %li
|} (max_appr |> Bigint.to_string) (min_prec - precision);
*)
      scale max_appr (min_prec - precision)
    )
    else
      (
      let result = approximate op precision in
      op.base.min_prec <- precision;
      op.base.max_appr <- result;
      op.base.appr_valid <- true;
      result
      )

  (* Position of the most significant digit.
     If [msd x = n] then [pow 2 (n - 1) < abs x < pow 2 (n + 1)]
     This version assumes that max_appr is valid and sufficiently removed from zero that
     the msd is determined.
   *)
  and known_msd : t -> int32
    = fun op ->
      let length =
        if Bigint.(op.base.max_appr >= zero)
        then op.base.max_appr |> numbits
        else op.base.max_appr |> Bigint.neg |> numbits
      in
      (*
      Caml.Printf.printf "length %s -> %i\n"
        (Bigint.to_string op.base.max_appr) length;
      Caml.Printf.printf {|min_prec: %li, length: %i, known_msd -> %li
|}
        op.base.min_prec
        length
        (op.base.min_prec + Int32.of_int_exn length - one)
        ;
        *)
      op.base.min_prec + Int32.of_int_exn length - Int32.one

  (* Most significant digit. Returns [Int32.min_value] if the correct answer [< n].
     TODO: return option?
   *)
  and msd : t -> int32 -> int32
    = fun op n ->
      let { max_appr; appr_valid; _ } = op.base in
      if not appr_valid ||
          (* in range [-1, 1] *)
          Bigint.(max_appr >= bigm1 && max_appr <= big1)
      then
        let (_ : Bigint.t) = get_appr op (n - Int32.one) in
        let max_appr = op.base.max_appr in (* get new value after get_appr *)
        (*
        Caml.Printf.printf {|Bigint.abs max_appr: %s
Bigint.(abs max_appr <= big1): %b
get_appr op (n - 1) -> %s
|}
          Bigint.(abs max_appr |> to_string)
          Bigint.(abs max_appr <= big1)
          (i |> Bigint.to_string)
        ;
        *)
        if Bigint.(abs max_appr <= big1)
        then (
          (* Caml.Printf.printf "msd: returning Int32.min_value\n"; *)
          (* msd arbitrarily far to the right *)
          Int32.min_value
        )
        else (
          (* Caml.Printf.printf "msd: returning known_msd op -> %li\n" (known_msd op); *)
          known_msd op
        )
      else known_msd op

    and iter_msd op n = iter_prec_msd op n Int32.zero

    and iter_prec_msd op n prec =
      if prec <= n + I32.thirty then msd op n
      else
        let msd = msd op prec in
        if msd <> Int32.min_value then msd
        else (
          check_prec prec;
          iter_prec_msd op n Int32.(prec * I32.three / I32.two - I32.sixteen)
        )
  ;;

  (* more constructors (that use get_appr) *)
  let select selector x y
    = of_cr (SelectCR {
        selector;
        selector_sign = neg I32.twenty |> get_appr selector |> big_signum;
        op1 = x;
        op2 = y
      })
  let max x y = select (subtract x y) y x
  let min x y = select (subtract x y) x y
  let abs x = select x (negate x) x

  let rec exp x =
    let low_prec = neg I32.ten in
    let rough_appr = get_appr x low_prec in
    if Bigint.(rough_appr > big2) || Bigint.(rough_appr < bigm2)
    then
      let square_root = exp @@ shift_right x Int32.one in
      multiply square_root square_root
    else
      of_cr (PrescaledExpCR x)

  let rec cos : t -> t =
    fun op ->
    let halfpi_multiples = get_appr (divide op pi) Int32.minus_one in
    let abs_halfpi_multiples = Bigint.abs halfpi_multiples in
    if Bigint.(abs_halfpi_multiples >= big2)
    then
      let open Bigint in
      let pi_multiples = scale halfpi_multiples Int32.minus_one in
      let adjustment = multiply pi (of_bigint pi_multiples) in
      (* if bit_and pi_multiples big1 <> big0 *)
      if bit_and pi_multiples big1 <> big0
      then (* Odd number of pi multiples *)
        subtract op adjustment |> cos |> negate
      else (* Even number of pi multiples *)
        subtract op adjustment |> cos
    else (
      if Bigint.(abs (get_appr op Int32.minus_one) >= big2)
      then (
        (* cos(2x) = 2 * cos(x)^2 - 1 *)
        let cos_half = cos (shift_right op Int32.one) in
        subtract (shift_left (multiply cos_half cos_half) Int32.one) one
      )
      else of_cr (PrescaledCosCR op)
    )

  let sin x = cos (subtract half_pi x)

  let rec asin : t -> t
    = fun op ->
      (* Caml.Printf.printf "asin %s\n" (debug_to_string op); *)
      let rough_appr = get_appr op I32.minus_ten in
      if Bigint.(rough_appr > big750)
      then
        (* asin(x) = acos(sqrt(1 - x^2)) *)
        subtract one (multiply op op) |> sqrt |> acos
      else
        if Bigint.(rough_appr < bigm750)
        then
          (* asin(-x) = -asin(x) <=> asin(x) = -asin(-x) *)
          op |> negate |> asin |> negate
        else of_cr (PrescaledAsinCR op)

  and acos x = subtract half_pi (asin x) (* asin (subtract half_pi x) *)

  let low_ln_limit = big8
  let high_ln_limit = Bigint.of_int Int.(16 + 8)
  let scaled_4 = Bigint.of_int Int.(4 * 16)

  let simple_ln op = of_cr (PrescaledLnCR (subtract op one))

  let ten_ninths = divide (of_int 10) (of_int 9)
  let twentyfive_twentyfourths = divide (of_int 25) (of_int 24)
  let eightyone_eightyeths = divide (of_int 81) (of_int 80)
  let ln2_1 = multiply (of_int 7) (simple_ln ten_ninths)
  let ln2_2 = multiply two (simple_ln twentyfive_twentyfourths)
  let ln2_3 = multiply three (simple_ln eightyone_eightyeths)
  let ln2 = add (subtract ln2_1 ln2_2) ln2_3

  let rec ln : t -> t
    = fun op ->
      let low_prec = Int32.neg I32.four in
      let rough_appr = get_appr op low_prec in
      if Bigint.(rough_appr < big0) then raise (ArithmeticError "ln(negative)");
      if Bigint.(rough_appr <= low_ln_limit)
      then op |> inverse |> ln |> negate
      else
        if Bigint.(rough_appr >= high_ln_limit)
        then
          if Bigint.(rough_appr <= scaled_4)
          then
            let quarter = op |> sqrt |> sqrt |> ln in
            shift_left quarter I32.two
          else
            let extra_bits = Int32.of_int_exn (numbits rough_appr) - I32.three in
            let scaled_result = shift_right op extra_bits |> ln in
            add scaled_result (of_int32 extra_bits |> multiply ln2)
        else
          simple_ln op

  (* end more constructors *)

  let eval_to_string : ?digits:int32 -> ?radix:int32 -> t -> string
    = fun ?digits:(digits=I32.ten) ?radix:(radix=I32.ten) op ->
      let scaled_cr =
        if radix = I32.sixteen
        then shift_left op (I32.four * digits)
        else
          let scale_factor = Bigint.(pow (of_int32 radix) (of_int32 digits)) in
          (* Caml.Printf.printf "scale_factor: %s\n" (Bigint.to_string scale_factor); *)
          multiply op (of_cr (IntCR scale_factor))
      in
      (* Caml.Printf.printf "scaled_cr: %s\n" (to_string scaled_cr); *)
      let scaled_int = get_appr scaled_cr Int32.zero in
      (* Caml.Printf.printf "scaled_int: %s\n" (Bigint.to_string scaled_int); *)
      (* TODO: radix *)
      string_of_scaled_int scaled_int digits

  (* to_string_float_rep *)

  let bigint_value : t -> Bigint.t
    = fun op -> get_appr op Int32.zero
end

let%test_module "Calculator" = (module struct
  let zero = CR.of_int 0
  let one = CR.of_int 1
  let two = CR.of_int 2
  let three = CR.add two one
  let four = CR.add two two
  let i38923 = CR.of_int 38923
  let half = CR.(divide one two)

  let print ?digits:(digits=I32.ten) cr
    = Caml.Printf.printf "%s\n" (CR.eval_to_string cr ~digits)

  let%expect_test _ =
    print zero;
    [%expect{| 0.0000000000 |}]

  let%expect_test _ =
    print one;
    [%expect{| 1.0000000000 |}]

  let%expect_test _ =
    print three;
    [%expect{| 3.0000000000 |}]

  let%expect_test _ =
    print four;
    [%expect{| 4.0000000000 |}]

  let%expect_test _ =
    print (CR.multiply zero one);
    [%expect{| 0.0000000000 |}]

  let%expect_test _ =
    print i38923;
    [%expect{| 38923.0000000000 |}]

  let%expect_test _ =
    print (CR.inverse two);
    [%expect{| 0.5000000000 |}]

  let%expect_test _ =
    print (CR.inverse three);
    [%expect{| 0.3333333333 |}]

  let%expect_test _ =
    print (CR.shift_left one Int32.one);
    [%expect{| 2.0000000000 |}]

  let%expect_test _ =
    print (CR.shift_right two Int32.one);
    [%expect{| 1.0000000000 |}]

  let%expect_test _ =
    print (CR.add one one);
    [%expect{| 2.0000000000 |}]

  let%expect_test _ =
    print CR.pi ~digits:I32.hundred;
    [%expect{| 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680 |}]

  let%expect_test _ =
    print (CR.sqrt one);
    [%expect{| 1.0000000000 |}]

  let%expect_test _ =
    print (CR.sqrt two) ~digits:I32.hundred;
    [%expect{| 1.4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727 |}]

  let%expect_test _ =
    print (CR.sqrt four);
    [%expect{| 2.0000000000 |}]

  let%expect_test _ =
    print CR.half_pi;
    [%expect{| 1.5707963268 |}]

  let%expect_test _ =
    print CR.(asin one);
    [%expect{| 1.5707963268 |}]

  let%expect_test _ =
    print CR.(asin (negate one));
    [%expect{| -1.5707963268 |}]

  let%expect_test _ =
    print CR.(asin zero);
    [%expect{| 0.0000000000 |}]

  let%expect_test _ =
    print (CR.sin zero);
    [%expect{| 0.0000000000 |}]

  let%expect_test _ =
    print CR.(sin half_pi);
    print CR.(sin pi);
    print (CR.sin half);
    [%expect{|
      1.0000000000
      0.0000000000
      0.4794255386
    |}]

  let%expect_test _ =
    print CR.(of_float 0.684413);
    print CR.(of_float 0.684414);
    [%expect{|
      0.6844130000
      0.6844140000 |}]

  let%expect_test _ =
    print (CR.sin one);
    print (CR.sin two);
    print (CR.sin three);
    [%expect{|
      0.8414709848
      0.9092974268
      0.1411200081 |}]

  let%expect_test _ =
    print CR.(asin (sin half));
    [%expect{| 0.5000000000 |}]

  let%expect_test _ =
    print CR.(cos one);
    print CR.(cos three);
    [%expect{|
      0.5403023059
      -0.9899924966 |}]

  let%expect_test _ =
    print CR.(asin (sin one));
    [%expect{| 1.0000000000 |}]

  let%expect_test _ =
    print CR.(acos (cos one));
    [%expect{| 1.0000000000 |}]

    (*
  let%expect_test _ =
    print CR.(atan (tan one));
    [%expect{| 1.0000000000 |}]

  let%expect_test _ =
    print CR.(atan (tan (negate one)));
    [%expect{| -1.0000000000 |}]
    *)

    (*
  let thousand = Bigint.of_int 1000
  let million = Bigint.of_int 1000000
  let huge = million * million * thousand
  let%expect_test _ =
    print CR.(tan (atan huge));
    *)

  let sqrt13 = CR.(of_int 13 |> sqrt)

  let%expect_test _ =
    print CR.(multiply sqrt13 sqrt13);
    [%expect{| 13.0000000000 |}]

end);;
