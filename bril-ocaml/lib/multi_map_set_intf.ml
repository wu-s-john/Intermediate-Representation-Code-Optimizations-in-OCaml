open Core

module type Elem = sig
  type t [@@deriving to_yojson]

  include Comparable with type t := t
end

module type S = sig
  module Key : Elem
  module Value : Elem

  type t = Value.Set.t Key.Map.t [@@deriving eq, sexp, to_yojson]

  val upsert : t -> Key.t -> Value.t -> t
  val mem : t -> Key.t -> Value.t -> bool
  val merge : t -> t -> t
  val empty : t
  val remove : t -> Key.t -> t
  val get : t -> Key.t -> Value.Set.t
  val of_alist : (Key.t * Value.t) list -> t
end