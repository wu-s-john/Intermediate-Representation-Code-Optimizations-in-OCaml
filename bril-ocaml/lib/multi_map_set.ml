open Core
include Multi_map_set_intf

module Make (Key : Comparable) (Value : Comparable) = struct
  module Key = Key
  module Value = Value

  type t = Value.Set.t Key.Map.t [@@deriving eq, sexp, compare]

  let to_yojson
      ~(key_to_yojson : Key.t -> Yojson.Safe.t)
      ~(value_to_yojson : Value.t -> Yojson.Safe.t)
      (t : t)
      : Yojson.Safe.t
    =
    let result =
      Key.Map.to_alist t
      |> List.map ~f:(fun (k, v) ->
             `Assoc
               [
                 ("key", key_to_yojson k);
                 ("value", `List (List.map ~f:value_to_yojson @@ Value.Set.to_list v));
               ])
    in
    `List result

  let upsert (t : t) (key : Key.t) (value : Value.t) =
    Map.update
      t
      key
      ~f:(Option.value_map ~default:(Value.Set.singleton value) ~f:(fun set -> Set.add set value))

  let mem (t : t) (key : Key.t) (value : Value.t) =
    Map.find t key |> Option.value_map ~default:false ~f:(fun value_set -> Set.mem value_set value)

  (* TODO:: This should use sequence because it is more optimized  *)
  let merge (t1 : t) (t2 : t) : t =
    Map.to_alist t2
    |> List.bind ~f:(fun (key, value_set) ->
           Set.to_list value_set |> List.map ~f:(fun value -> (key, value)))
    |> List.fold ~init:t1 ~f:(fun t (key, value) -> upsert t key value)

  let empty = Key.Map.empty
  let remove (t : t) (key : Key.t) = Map.remove t key
  let get (t : t) (key : Key.t) = Map.find t key |> Option.value ~default:Value.Set.empty

  let to_alist (t : t) : (Key.t * Value.t) list =
    Map.to_alist t
    |> List.bind ~f:(fun (key, value_set) ->
           Set.to_list value_set |> List.map ~f:(fun value -> (key, value)))

  let to_list (t : t) : Value.t list = Map.data t |> List.bind ~f:Set.to_list

  let of_alist
      (module Key_value : Value_with_key with type t = Value.t and type key = Key.t)
      (list : (Key.t * Value.t) list)
    =
    let initial_set = Key.Map.of_alist_multi list |> Map.map ~f:Value.Set.of_list in
    let domain =
      List.map list ~f:(fun (_, value) -> Key_value.key value)
      @ List.map list ~f:(fun (key, _) -> key)
      |> Key.Set.of_list
    in
    Key.Set.fold domain ~init:initial_set ~f:(fun set key ->
        match Map.add set ~key ~data:Value.Set.empty with
        | `Ok new_set -> new_set
        | `Duplicate -> set)
end
