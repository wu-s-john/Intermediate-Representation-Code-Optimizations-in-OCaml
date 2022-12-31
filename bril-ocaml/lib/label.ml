open Core

module T = struct
  type t = string option [@@deriving compare, equal, sexp, hash, to_yojson]
end

include Comparable.Make (T)
