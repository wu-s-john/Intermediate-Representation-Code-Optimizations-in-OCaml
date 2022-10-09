open! Core

type t =
  | Int of int
  | Bool of bool
[@@deriving compare, equal, sexp_of]

let to_string = function
  | Int i -> Int.to_string i
  | Bool b -> Bool.to_string b