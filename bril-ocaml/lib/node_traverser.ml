open Core
include Node_traverser_intf

module Poly : Poly_intf = struct
  type ('key, 'node) value = {
    predecessors : 'key Hash_set.t;
    node : 'node;
  }

  type ('key, 'node) t = {
    map : ('key, ('key, 'node) value) Hashtbl.t;
    root : 'node;
    children : 'node -> 'key list;
    get_key : 'node -> 'key;
    hash_module : (module Hashable with type t = 'key);
  }

  let predecessors ({ map; _ } : ('key, 'node) t) (key : 'key) =
    Hashtbl.find map key
    |> Option.map ~f:(fun { predecessors; _ } ->
           Hash_set.to_list predecessors
           |> List.filter_map ~f:(fun parent_key ->
                  Hashtbl.find map parent_key |> Option.map ~f:(fun { node; _ } -> node)))

  let successors ({ map; children; _ } : ('key, 'node) t) (key : 'key) =
    Hashtbl.find map key
    |> Option.map ~f:(fun { node; _ } ->
           List.filter_map (children node) ~f:(fun child_key ->
               Hashtbl.find map child_key |> Option.map ~f:(fun { node; _ } -> node)))

  let reverse_postorder
      (type key node)
      ({ root; map; hash_module = (module Hash); children; get_key } : (key, node) t)
    =
    let queue : (key, node) Hash_queue.t = Hash.Hash_queue.create () in
    let rec go (node : node) : unit =
      match Hash_queue.enqueue_back queue (get_key node) node with
      | `Key_already_present -> ()
      | `Ok -> List.iter (children node) ~f:(fun child -> go (Hashtbl.find_exn map child).node)
    in
    go root;
    Hash_queue.to_list queue

  let nodes { map; _ } = Hashtbl.data map |> List.map ~f:(fun { node; _ } -> node)
  let keys { map; _ } = Hashtbl.keys map
  let root { root; _ } = root

  let compute_predecessor_map
      (type node key)
      (module Node : Node_intf with type t = node and type Key.t = key)
      (list : (key * node) list)
      : (key, key Hash_set.t) Hashtbl.t
    =
    let map : Node.Key.Hash_set.t Node.Key.Table.t = Node.Key.Table.create () in
    List.iter list ~f:(fun (key, node) ->
        List.iter (Node.children node) ~f:(fun child_key ->
            let child_predecessors =
              Hashtbl.find_or_add map child_key ~default:(fun () -> Node.Key.Hash_set.create ())
            in
            Hash_set.add child_predecessors key));
    map

  let find_root
      (type node key)
      (module Node : Node_intf with type t = node and type Key.t = key)
      (predecessors_map : (key, (key, node) value) Hashtbl.t)
      : Node.t option
    =
    let possible_root_nodes =
      Hashtbl.filter_mapi predecessors_map ~f:(fun ~key:_ ~data:{ predecessors; node } ->
          if Hash_set.is_empty predecessors then Some node else None)
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (_, node) -> node)
    in
    match possible_root_nodes with
    | [ node ] -> Some node
    | _ -> None

  let of_alist
      (type node key)
      (module Node : Node_intf with type t = node and type Key.t = key)
      list
    =
    let open Option.Let_syntax in
    let predecessors_map = compute_predecessor_map (module Node) list in
    let%bind map =
      match
        Node.Key.Table.create_mapped
          list
          ~get_key:(fun (key, _) -> key)
          ~get_data:(fun (key, value) ->
            {
              predecessors =
                Hashtbl.find_or_add predecessors_map key ~default:(fun () ->
                    Node.Key.Hash_set.create ());
              node = value;
            })
      with
      | `Duplicate_keys _ -> None
      | `Ok map -> Some map
    in
    let%map root = find_root (module Node) map in
    { map; root; children = Node.children; get_key = Node.get_key; hash_module = (module Node.Key) }

  let map
      (type node_in node_out key)
      (module Node : Node_intf with type t = node_out and type Key.t = key)
      (t : (key, node_in) t)
      ~(f : node_in -> node_out)
      : (key, node_out) t
    =
    let map =
      Hashtbl.map t.map ~f:(fun { node; predecessors } -> { predecessors; node = f node })
    in
    {
      map;
      root = f t.root;
      children = Node.children;
      get_key = Node.get_key;
      hash_module = (module Node.Key);
    }

  (*  This is a quick way to construct a mapping of new mapping  *)
  let inv_map
      (t : ('key, 'node_in) t)
      ~(contra_f : 'node_out -> 'node_in)
      ~(f : 'node_in -> 'node_out)
      : ('key, 'node_out) t
    =
    let map =
      Hashtbl.map t.map ~f:(fun { node; predecessors } -> { predecessors; node = f node })
    in
    {
      map;
      root = f t.root;
      children = Fn.compose t.children contra_f;
      get_key = Fn.compose t.get_key contra_f;
      hash_module = t.hash_module;
    }

  let find { map; _ } key = Hashtbl.find map key |> Option.map ~f:(fun { node; _ } -> node)
  let find_exn t key = Option.value_exn (find t key)

  let map_inplace { map; _ } ~f =
    Hashtbl.map_inplace map ~f:(fun { node; predecessors } -> { predecessors; node = f node })

  let update { map; get_key; _ } node =
    Hashtbl.change
      map
      (get_key node)
      ~f:(Option.map ~f:(fun { predecessors; _ } -> { predecessors; node }))

  let edges { map; _ } =
    Hashtbl.to_alist map
    |> List.bind ~f:(fun (source, { predecessors; _ }) ->
           List.map (Hash_set.to_list predecessors) ~f:(fun predecessor -> (predecessor, source)))
end