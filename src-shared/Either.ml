type ('a, 'b) t =
  | Left of 'a
  | Right of 'b

let map f = function
  | Left a -> Left a
  | Right b -> Right (f b)
;;

let ( >>= ) e f =
  match e with
  | Left a -> Left a
  | Right b -> f b
;;
