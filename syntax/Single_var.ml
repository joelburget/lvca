type 'info t =
  { info : 'info
  ; name : string
  }

module Plain = struct
  type t = { name : string }
end
