open Core

module type Poly_intf = Undirected_graph_intf.Poly_intf

module Poly = struct
  type ('key, 'node) value = {
    neighbors : 'key Hash_set.t;
    node : 'node;
  }

  type ('key, 'node) t = {
    map : ('key, ('key, 'node) value) Hashtbl.t;
    get_key : 'node -> 'key;
    hash_module : (module Hashable with type t = 'key);
  }

  let nodes t = Hashtbl.data t.map |> List.map ~f:(fun { node; _ } -> node)
  let keys t = Hashtbl.keys t.map

  let neighbors t key =
    match Hashtbl.find t.map key with
    | None -> []
    | Some { neighbors; _ } ->
      Hash_set.to_list neighbors |> List.map ~f:(fun key -> (Hashtbl.find_exn t.map key).node)

  let maximum_out_degree t =
    Hashtbl.data t.map
    |> List.map ~f:(fun { neighbors; _ } -> Hash_set.length neighbors)
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0

  let of_alist (type key) (module Key : Node.Key with type t = key) (list : (key * key) list) =
    let map =
      Key.Table.of_alist_multi list
      |> Hashtbl.to_alist
      |> Key.Table.create_mapped
           ~get_key:(fun (key, _) -> key)
           ~get_data:(fun (key, neighbors) ->
             let neighbors = Hash_set.of_list (module Key) neighbors in
             { neighbors; node = key })
    in
    match map with
    | `Ok map -> { map; get_key = Fn.id; hash_module = (module Key) }
    | `Duplicate_keys _key -> failwith "This should be impossible"

  let create
      (type node key)
      (module Node : Node.S with type t = node and type Key.t = key)
      (nodes : node list)
      (edges : (key * key) list)
    =
    let edge_map = Node.Key.Table.of_alist_multi edges in
    let map =
      Node.Key.Table.create_mapped nodes ~get_key:Node.key ~get_data:(fun node ->
          let neighbors =
            match Node.Key.Table.find edge_map (Node.key node) with
            | None -> Hash_set.create (module Node.Key)
            | Some neighbors -> Hash_set.of_list (module Node.Key) neighbors
          in
          { neighbors; node })
    in
    match map with
    | `Ok map -> Some { map; get_key = Node.key; hash_module = (module Node.Key) }
    | `Duplicate_keys _key -> None
end