open Core

type t = {
  name : string;
  typ : Type.t; [@key "type"]
}
[@@deriving compare, equal, sexp, hash, yojson]
