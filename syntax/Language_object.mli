(** Derive helpers (an extended language object) from the basics. *)
module Extend (Object : Language_object_intf.S) :
  Language_object_intf.Extended_s
    with type 'info t = 'info Object.t
     and module Plain = Object.Plain

(** Properties of parsing and pretty-printing that should hold for any language object. *)
module Check_parse_pretty (Object : Language_object_intf.S) :
  Properties_intf.Parse_pretty_s with type 'info t = 'info Object.t

(** Properties of json serialization / deserialization that should hold for any language
    object. *)
module Check_json (Object : Language_object_intf.S) :
  Properties_intf.Json_s with type 'info t = 'info Object.t

(** Check json serialization / deserialization and parsing / pretty-printing properties *)
module Check_properties (Object : Language_object_intf.S) :
  Properties_intf.S with type 'info t = 'info Object.t
