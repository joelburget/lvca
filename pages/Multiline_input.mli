val mk
  :  ?autofocus:bool
  -> ?border:bool
  -> ?rows:int option
  -> ?cols:int
  -> ?at:Brr.At.t list
  -> string Note.signal
  -> Brr.El.t * Common.input_event Note.event
