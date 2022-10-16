open Core

module type Key = sig
  type t [@@deriving sexp, compare, hash]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module type S = sig
  module Key : Key

  type t

  val get_key : t -> Key.t
  val children : t -> Key.t list
end
