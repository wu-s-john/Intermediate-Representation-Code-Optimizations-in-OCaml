open Core

module type S = sig
  type t
  type key
  type node

  val predecessors : t -> key -> node list
  val successors : t -> key -> node list
  val reverse_postorder : t -> node list
  val keys : t -> key list
  val root : t -> node

  (* This makes the graph algorithm less general since it assumes that there is at least one root*)

  val of_alist : (key * node) list -> t option
end

(* S with type key := Node.Key.t and type node := Node.t *)

module type Node_intf = sig
  include Node.S

  val children : t -> t list
end

module Make (Node : Node_intf) = struct
  type value = {
    predecessors : Node.Key.Hash_set.t;
    node : Node.t;
  }

  type t = {
    map : (Node.Key.t, value) Hashtbl.t;
    root : Node.t;
  }

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

  let compute_predecessor_map (list : (Node.Key.t * Node.t) list)
      : (Node.Key.t, Node.Key.Hash_set.t) Hashtbl.t
    =
    let map : Node.Key.Hash_set.t Node.Key.Table.t = Node.Key.Table.create () in
    List.iter list ~f:(fun (key, node) ->
        List.iter (Node.children node) ~f:(fun child ->
            let child_predecessors =
              Hashtbl.find_or_add map (Node.get_key child) ~default:(fun () ->
                  Node.Key.Hash_set.create ())
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
            { predecessors = Hashtbl.find_exn predecessors_map key; node = value })
      with
      | `Duplicate_keys _ -> None
      | `Ok map -> Some map
    in
    let%map root = find_root map in
    { map; root }
end
