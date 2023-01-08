open Core

type t = string [@@deriving compare, equal, sexp, hash, yojson]