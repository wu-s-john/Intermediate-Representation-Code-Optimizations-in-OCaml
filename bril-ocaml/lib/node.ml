open Core

module type S = sig
  module Key : sig
    type t [@@deriving sexp, compare, hash]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  type t

  val get_key : t -> Key.t
  val children : t -> Key.t list
end