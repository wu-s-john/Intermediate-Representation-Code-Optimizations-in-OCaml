open Core

module type S = sig
  type t
  type key
  type node

  val predecessors : t -> key -> node list option
  val successors : t -> key -> node list option
  val reverse_postorder : t -> node list
  val keys : t -> key list
  val root : t -> node
  val of_alist : (key * node) list -> t option
  val map_inplace : t -> f:(node -> node) -> t (* Could probably be in another module *)
end

module type Poly_intf = sig
  type ('key, 'node) t

  val predecessors : ('key, 'node) t -> 'key -> 'node list option
  val successors : ('key, 'node) t -> 'key -> 'node list option
  val reverse_postorder : ('key, 'node) t -> 'node list
  val keys : ('key, 'node) t -> 'key list

  val of_alist
    :  (module Node.S with type t = 'node and type Key.t = 'key) ->
    ('key * 'node) list ->
    ('key, 'node) t option

  val map_inplace : ('key, 'node) t -> f:('node -> 'node) -> ('key, 'node) t

  val map
    :  (module Node.S with type t = 'node and type Key.t = 'key) ->
    ('key, 'node_in) t ->
    f:('node_in -> 'node_out) ->
    ('key, 'node_out) t
end

module Node = struct
  module type S = sig
    include Node.S

    val children : t -> Key.t list
  end

  module Make_map (N : S) (F : Node.Mapper with type _in := N.t and type key := N.Key.t) :
    S with type t = F.t and module Key = N.Key = struct
    include Node.Make_map (N) (F)

    let children (t : t) = N.children (F.contra_f t)
  end
end

module Make (Node : Node.S) : S with type key := Node.Key.t and type node := Node.t = struct
  type value = {
    predecessors : Node.Key.Hash_set.t;
    node : Node.t;
  }

  type t = {
    map : (Node.Key.t, value) Hashtbl.t;
    root : Node.t;
  }

  let root (t : t) = t.root
  let keys (t : t) = Hashtbl.keys t.map

  let successors ({ map; _ } : t) (key : Node.Key.t) : Node.t list option =
    Hashtbl.find map key
    |> Option.map ~f:(fun { node; _ } ->
           List.filter_map (Node.children node) ~f:(fun child_key ->
               Hashtbl.find map child_key |> Option.map ~f:(fun { node; _ } -> node)))

  let predecessors ({ map; _ } : t) (key : Node.Key.t) : Node.t list option =
    Hashtbl.find map key
    |> Option.map ~f:(fun { predecessors; _ } ->
           Hash_set.to_list predecessors
           |> List.filter_map ~f:(fun parent_key ->
                  Hashtbl.find map parent_key |> Option.map ~f:(fun { node; _ } -> node)))

  (* Find the root from a predecessors map by determining if a root has no predecessors *)
  let find_root (predecessors_map : (Node.Key.t, value) Hashtbl.t) : Node.t option =
    let possible_root_nodes =
      Hashtbl.filter_mapi predecessors_map ~f:(fun ~key:_ ~data:{ predecessors; node } ->
          if Hash_set.is_empty predecessors then Some node else None)
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (_, node) -> node)
    in
    match possible_root_nodes with
    | [ node ] -> Some node
    | _ -> None

  let reverse_postorder ({ root; map } : t) : Node.t list =
    let queue : (Node.Key.t, Node.t) Hash_queue.t = Node.Key.Hash_queue.create () in
    let rec go (node : Node.t) : unit =
      match Hash_queue.enqueue_back queue (Node.get_key node) node with
      | `Key_already_present -> ()
      | `Ok -> List.iter (Node.children node) ~f:(fun child -> go (Hashtbl.find_exn map child).node)
    in
    go root;
    Hash_queue.to_list queue

  let compute_predecessor_map (list : (Node.Key.t * Node.t) list)
      : (Node.Key.t, Node.Key.Hash_set.t) Hashtbl.t
    =
    let map : Node.Key.Hash_set.t Node.Key.Table.t = Node.Key.Table.create () in
    List.iter list ~f:(fun (key, node) ->
        List.iter (Node.children node) ~f:(fun child_key ->
            let child_predecessors =
              Hashtbl.find_or_add map child_key ~default:(fun () -> Node.Key.Hash_set.create ())
            in
            Hash_set.add child_predecessors key));
    map

  let of_alist (list : (Node.Key.t * Node.t) list) =
    let open Option.Let_syntax in
    let predecessors_map : (Node.Key.t, Node.Key.Hash_set.t) Hashtbl.t =
      compute_predecessor_map list
    in
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
    let%map root = find_root map in
    { map; root }

  let map_inplace { root = _; map } ~f : t =
    Hashtbl.map_inplace map ~f:(fun { predecessors; node } -> { predecessors; node = f node });
    let new_root = find_root map in
    { root = Option.value_exn new_root; map }
end
