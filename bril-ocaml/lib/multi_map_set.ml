open Core

module type Elem = sig
  include Hashable
  include Comparable with type t := t
end

module type S = sig
  module Key : Elem
  module Value : Elem

  type t = Value.Set.t Key.Map.t [@@deriving eq]

  val upsert : t -> Key.t -> Value.t -> t
  val mem : t -> Key.t -> Value.t -> bool
  val merge : t -> t -> t
  val empty : t
  val remove : t -> Key.t -> t
end

module Make (Key : Elem) (Value : Elem) : S with module Key = Key and module Value = Value = struct
  module Key = Key
  module Value = Value

  type t = Value.Set.t Key.Map.t [@@deriving eq]

  let upsert (t : t) (key : Key.t) (value : Value.t) =
    Map.update
      t
      key
      ~f:(Option.value_map ~default:(Value.Set.singleton value) ~f:(fun set -> Set.add set value))

  let mem (t : t) (key : Key.t) (value : Value.t) =
    Map.find t key |> Option.value_map ~default:false ~f:(fun value_set -> Set.mem value_set value)

  let merge (t1 : t) (t2 : t) : t =
    Map.to_alist t2
    |> List.bind ~f:(fun (key, value_set) ->
           Set.to_list value_set |> List.map ~f:(fun value -> (key, value)))
    |> List.fold ~init:t1 ~f:(fun t (key, value) -> upsert t key value)

  let empty = Key.Map.empty
  let remove (t : t) (key : Key.t) = Map.remove t key
end
