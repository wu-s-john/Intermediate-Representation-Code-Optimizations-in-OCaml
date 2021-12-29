type t = {
  name : string;
  typ : Type.t;
} [@@deriving compare, equal, sexp, hash, yojson]