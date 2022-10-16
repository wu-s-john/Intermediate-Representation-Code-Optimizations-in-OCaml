open Core
include Multi_map_set_intf

module Make (Key : Elem) (Value : Elem) : S with module Key = Key and module Value = Value = struct
  module Key = Key
  module Value = Value

  type t = Value.Set.t Key.Map.t [@@deriving eq, sexp]

  let to_yojson (t : t) : Yojson.Safe.t =
    let result =
      Key.Map.to_alist t
      |> List.map ~f:(fun (k, v) ->
             `Assoc
               [
                 ("key", Key.to_yojson k);
                 ("value", [%to_yojson: Value.t list] @@ Value.Set.to_list v);
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

  let of_alist (list : (Key.t * Value.t) list) =
    Key.Map.of_alist_multi list |> Map.map ~f:Value.Set.of_list
end
