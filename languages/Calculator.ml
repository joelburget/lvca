
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

  class virtual base = object(self)
    val mutable min_prec = 0
    val mutable max_appr = Bigint.of_int 0
    val mutable appr_valid = false

    method virtual approximate : int -> Bigint.t

    method check_prec n =
      let high = n lsr 28 in
      let high_shifted = n lsr 29 in
      if high lxor high_shifted <> 0 then raise PrecisionOverflowException

    method scale k n =
      if n >= 0
      then Bigint.shift_left k n
      else
        let big_add = Bigint.(+) in
        let adj_k = big_add (shift k (n + 1)) big1 in
        Bigint.shift_left adj_k 1

    method get_appr precision =
      self#check_prec precision;
      if appr_valid && precision >= min_prec
      then self#scale max_appr (min_prec - precision)
      else
        let result = self#approximate precision in
        min_prec <- precision;
        max_appr <- result;
        appr_valid <- true;
        result
  end
  ;;

  class int_cr = object
    inherit base as super
    val value = Bigint.of_int 0

    method approximate p =
      super#scale value (-p)
  end
  ;;

  class add_cr op1 op2 = object
    inherit base as super
    val op1 = op1
    val op2 = op2

    method approximate p =
      super#scale (add_cr (op1#get_appr (p - 2)) (op2#get_appr (p - 2))) (-2)
  end
  ;;

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

  let bound_log2 : int -> int
    = fun n ->
      let abs_n = abs n in
      Int.of_float @@ ceil @@ log (Float.of_int (abs_n + 1) /. log 2.0)

  (* TODO: check_prec? *)

  (*
  let of_bigint : Bigint.t -> t
    =
  *)
end
