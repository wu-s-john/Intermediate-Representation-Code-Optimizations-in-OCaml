open! Core

type t = string * Bril_type.t [@@deriving compare, equal, sexp_of]