open Core
include Node_traverser_intf

module Poly = struct
  type ('key, 'node) value = {
    predecessors : 'key Hash_set.t;
    children : 'key Hash_set.t;
    node : 'node;
  }

  type ('key, 'node) t = {
    map : ('key, ('key, 'node) value) Hashtbl.t;
    root : 'node;
    get_key : 'node -> 'key;
    hash_module : (module Hashable with type t = 'key);
  }

  let maximum_out_degree { map; _ } =
    Hashtbl.data map
    |> List.map ~f:(fun { children; _ } -> Hash_set.length children)
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0

  let predecessors ({ map; _ } : ('key, 'node) t) (key : 'key) =
    Hashtbl.find map key
    |> Option.map ~f:(fun { predecessors; _ } ->
           Hash_set.to_list predecessors
           |> List.filter_map ~f:(fun parent_key ->
                  Hashtbl.find map parent_key |> Option.map ~f:(fun { node; _ } -> node)))

  let successors ({ map; _ } : ('key, 'node) t) (key : 'key) =
    Hashtbl.find map key
    |> Option.map ~f:(fun { children; _ } ->
           children
           |> Hash_set.to_list
           |> List.filter_map ~f:(fun child_key ->
                  Hashtbl.find map child_key |> Option.map ~f:(fun { node; _ } -> node)))

  let reverse_postorder
      (type key node)
      ({ root; map; hash_module = (module Hash); get_key } : (key, node) t)
    =
    let queue : (key, node) Hash_queue.t = Hash.Hash_queue.create () in
    let rec go (node : node) : unit =
      let key = get_key node in
      match Hash_queue.enqueue_back queue key node with
      | `Key_already_present -> ()
      | `Ok ->
        (Hashtbl.find_exn map key).children
        |> Hash_set.to_list
        |> List.iter ~f:(fun child -> go (Hashtbl.find_exn map child).node)
    in
    go root;
    Hash_queue.to_list queue

  let fold (t : ('key, 'node) t) ~(init : 'acc) ~f = reverse_postorder t |> List.fold ~init ~f
  let nodes { map; _ } = Hashtbl.data map |> List.map ~f:(fun { node; _ } -> node)
  let keys { map; _ } = Hashtbl.keys map
  let root { root; _ } = root

  let compute_predecessor_map
      (type node key)
      (module Node : Node.S with type t = node and type Key.t = key)
      ~(get_children : Node.t -> Node.Key.t list)
      (list : node list)
      : (key, key Hash_set.t) Hashtbl.t
    =
    let map : Node.Key.Hash_set.t Node.Key.Table.t = Node.Key.Table.create () in
    List.iter list ~f:(fun node ->
        List.iter (get_children node) ~f:(fun child_key ->
            let child_predecessors =
              Hashtbl.find_or_add map child_key ~default:(fun () -> Node.Key.Hash_set.create ())
            in
            Hash_set.add child_predecessors (Node.key node)));
    map

  let find_root
      (type node key)
      (module Node : Node.S with type t = node and type Key.t = key)
      (predecessors_map : (key, (key, node) value) Hashtbl.t)
      : Node.t option
    =
    let possible_root_nodes =
      Hashtbl.filter_mapi predecessors_map ~f:(fun ~key:_ ~data:{ predecessors; node; _ } ->
          if Hash_set.is_empty predecessors then Some node else None)
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (_, node) -> node)
    in
    match possible_root_nodes with
    | [ node ] -> Some node
    | _ -> None

  let of_list
      (type node key)
      (module Node : Node.S with type t = node and type Key.t = key)
      ~(get_children : Node.t -> Node.Key.t list)
      list
    =
    let open Option.Let_syntax in
    let predecessors_map = compute_predecessor_map (module Node) ~get_children list in
    let%bind map =
      match
        Node.Key.Table.create_mapped
          list
          ~get_key:(fun node -> Node.key node)
          ~get_data:(fun node ->
            {
              predecessors =
                Hashtbl.find_or_add predecessors_map (Node.key node) ~default:(fun () ->
                    Node.Key.Hash_set.create ());
              node;
              children = Node.Key.Hash_set.of_list (get_children node);
            })
      with
      | `Duplicate_keys _ -> None
      | `Ok map -> Some map
    in
    let%map root = find_root (module Node) map in
    { map; root; get_key = Node.key; hash_module = (module Node.Key) }

  let to_map
      (type key witness)
      (module Comparable : Comparable.S with type t = key and type comparator_witness = witness)
      t
      ~f
    =
    Hashtbl.to_alist t.map
    |> List.map ~f:(fun (key, { node; _ }) -> (key, f node))
    |> Comparable.Map.of_alist_exn

  let create
      (type node key)
      (module Node : Node.S with type t = node and type Key.t = key)
      (nodes : node list)
      (edges : (key * key) list)
    =
    let open Option.Let_syntax in
    let children_map = Node.Key.Map.of_alist_multi edges in
    let predecessors_map =
      compute_predecessor_map
        (module Node)
        ~get_children:(fun node -> Map.find_multi children_map (Node.key node))
        nodes
    in
    let%bind map =
      match
        Node.Key.Table.create_mapped
          nodes
          ~get_key:(fun node -> Node.key node)
          ~get_data:(fun node ->
            {
              predecessors =
                Hashtbl.find_or_add predecessors_map (Node.key node) ~default:(fun () ->
                    Node.Key.Hash_set.create ());
              node;
              children =
                Node.Key.Hash_set.of_list
                  (Map.find children_map (Node.key node) |> Option.value ~default:[]);
            })
      with
      | `Duplicate_keys _ -> None
      | `Ok map -> Some map
    in
    let%map root = find_root (module Node) map in
    { map; root; get_key = Node.key; hash_module = (module Node.Key) }

  let map
      (type node_in node_out key)
      (module Node : Node.S with type t = node_out and type Key.t = key)
      (t : (key, node_in) t)
      ~(f : node_in -> node_out)
      : (key, node_out) t
    =
    let map =
      Hashtbl.map t.map ~f:(fun { node; predecessors; children } ->
          { children; predecessors; node = f node })
    in
    { map; root = f t.root; get_key = Node.key; hash_module = (module Node.Key) }

  (*  This is a quick way to construct a mapping of new mapping  *)
  let inv_map
      (t : ('key, 'node_in) t)
      ~(contra_f : 'node_out -> 'node_in)
      ~(f : 'node_in -> 'node_out)
      : ('key, 'node_out) t
    =
    let map =
      Hashtbl.map t.map ~f:(fun { node; predecessors; children } ->
          { children; predecessors; node = f node })
    in
    { map; root = f t.root; get_key = Fn.compose t.get_key contra_f; hash_module = t.hash_module }

  let find { map; _ } key = Hashtbl.find map key |> Option.map ~f:(fun { node; _ } -> node)
  let find_exn t key = Option.value_exn (find t key)

  let map_inplace { map; _ } ~f =
    Hashtbl.map_inplace map ~f:(fun { node; predecessors; children } ->
        { children; predecessors; node = f node })

  let update { map; get_key; _ } node =
    Hashtbl.change
      map
      (get_key node)
      ~f:(Option.map ~f:(fun { children; predecessors; _ } -> { predecessors; node; children }))

  let edges { map; _ } =
    Hashtbl.to_alist map
    |> List.bind ~f:(fun (source, { predecessors; _ }) ->
           List.map (Hash_set.to_list predecessors) ~f:(fun predecessor -> (predecessor, source)))
end