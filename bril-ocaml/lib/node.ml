open Core

module type Key = sig
  type t [@@deriving sexp, compare, hash]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module type S = sig
  module Key : Key

  type t

  val key : t -> Key.t
end

module type Key_with_yojson = sig
  type t [@@deriving to_yojson]

  include Key with type t := t
  include Renderable.S with type t := t
end

module type Homogeneous_intf = sig
  include S

  val render : t -> string
  val children : t -> Key.t list
end

module Make_from_multimap
    (Key : Key_with_yojson)
    (Multi_map_set : Multi_map_set.S with type Key.t = Key.t and type Value.t = Key.t) :
  Homogeneous_intf with module Key = Key and type t = Multi_map_set.t * Key.t = struct
  type t = Multi_map_set.t * Key.t

  module Key = Key

  let render (_, key) = Key.render key
  let key (_, t) = t
  let children (multimap, t) = Multi_map_set.get multimap t |> Set.to_list
end