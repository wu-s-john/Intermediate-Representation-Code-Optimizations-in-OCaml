type t = {
  name : string;
  typ : Type.t; [@key "type"]
}
[@@deriving yojson]
