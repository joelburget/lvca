type 'comment t =
  { comment : 'comment option
  ; range : Opt_range.t
  }

let none = { comment = None; range = None }
let get_range { range; _ } = range
let get_comment { comment; _ } = comment
