open Core

type t = {
  name : string;
  typ : Type.t; [@key "type"]
}
[@@deriving yojson, eq, hash, sexp]
