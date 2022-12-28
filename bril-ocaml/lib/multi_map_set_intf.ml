open Core

module type Value_with_key = sig
  type t
  type key

  val key : t -> key
end

module type S = sig
  module Key : Comparable
  module Value : Comparable

  type t = Value.Set.t Key.Map.t [@@deriving eq, sexp, compare]

  val to_yojson
    :  key_to_yojson:(Key.t -> Yojson.Safe.t) ->
    value_to_yojson:(Value.t -> Yojson.Safe.t) ->
    t ->
    Yojson.Safe.t

  val upsert : t -> Key.t -> Value.t -> t
  val mem : t -> Key.t -> Value.t -> bool
  val merge : t -> t -> t
  val empty : t
  val remove : t -> Key.t -> t
  val get : t -> Key.t -> Value.Set.t

  val of_alist
    :  (module Value_with_key with type t = Value.t and type key = Key.t) ->
    (Key.t * Value.t) list ->
    t

  val to_alist : t -> (Key.t * Value.t) list
end