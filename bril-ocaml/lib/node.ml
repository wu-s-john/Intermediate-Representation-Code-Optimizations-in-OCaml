open Core

module type S = sig
  module Key : sig
    type t [@@deriving sexp, compare, hash]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  type t

  val get_key : t -> Key.t
end

module type Mapper = sig
  type _in
  type key
  type t

  val f : _in -> t
  val contra_f : t -> _in
end

module Make_map (Node : S) (F : Mapper with type _in := Node.t and type key := Node.Key.t) :
  S with type t = F.t and module Key = Node.Key = struct
  type t = F.t

  module Key = Node.Key

  let get_key t = Node.get_key @@ F.contra_f t
end
