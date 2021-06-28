type 'info t =
  { info : 'info
  ; name : string
  }

let map_info ~f { info; name } = { info = f info; name }

module Plain = struct
  type t = { name : string }
end
