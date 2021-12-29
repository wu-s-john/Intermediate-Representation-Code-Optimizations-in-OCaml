open Core

module type S = sig
  type t
  type key
  type node

  val get_dominators : t -> (key, key list) Hashtbl.t
end

module Graph
    (Node : Node.S)
    (Traverser : Node_traverser.S with type key := Node.Key.t with type node := Node.t) =
struct
  (* Simple maping from node to map  *)
  type t = { traverser : Traverser.t }

  let compute_dominators ({ traverser; _ } : t) : (Node.Key.t, Node.Key.Set.t) Hashtbl.t =
    let post_order_nodes =
      Traverser.reverse_postorder traverser |> List.map ~f:Node.get_key |> Node.Key.Set.of_list
    in
    let dominator_map : (Node.Key.t, Node.Key.Set.t) Hashtbl.t =
      Traverser.keys traverser
      |> List.map ~f:(fun key -> (key, Node.Key.Set.singleton key))
      |> Node.Key.Table.of_alist_exn
    in
    let rec go (nodes : Node.Key.Set.t) : unit =
      if Set.is_empty nodes then ()
      else
        Set.filter nodes ~f:(fun key ->
            let singleton_key = Node.Key.Set.singleton key in
            let current_dominators =
              Hashtbl.find_or_add dominator_map key ~default:(fun () -> singleton_key)
            in
            let predecessors = Traverser.predecessors traverser key |> Option.to_list |> List.concat |> List.map ~f:Node.get_key in
            let new_dominators =
              Set.union singleton_key
              @@ Option.value
                   ~default:singleton_key
                   (List.reduce
                      (List.map predecessors ~f:(fun key ->
                           Hashtbl.find_or_add dominator_map key ~default:(fun () -> singleton_key)))
                      ~f:Set.inter)
            in
            not @@ Set.equal current_dominators new_dominators)
        |> go
    in
    go post_order_nodes;
    dominator_map
end