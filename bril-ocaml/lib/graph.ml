open Core

module type S = sig
  type t

  module Node : Node.S

  type dfs_tree_result = {
    arrival_number : int;
    children : Node.Key.Set.t;
  }

  val get_dominators : t -> (Node.Key.t, Node.Key.t list) Hashtbl.t
end

module Graph
    (Node : Node.S)
    (Traverser : Node_traverser.S with type key := Node.Key.t with type node := Node.t) =
struct
  type t = { traverser : Traverser.t }
  type dominator_set = (Node.Key.t, Node.Key.Set.t) Hashtbl.t

  let compute_dominators ({ traverser } : t) : dominator_set =
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
            let predecessors =
              Traverser.predecessors traverser key
              |> Option.to_list
              |> List.concat
              |> List.map ~f:Node.get_key
            in
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

  type dfs_tree_result = {
    arrival_number : int;
    children : Node.Key.Set.t;
  }

  let dfs_tree (t : t) : (Node.Key.t, dfs_tree_result) Hashtbl.t =
    let tree : (Node.Key.t, dfs_tree_result) Hashtbl.t = Node.Key.Table.create () in
    let rec go (node : Node.Key.t) (arrival_number : int ref) =
      if not @@ Hashtbl.mem tree node then
        List.iter
          (List.concat @@ Option.to_list @@ Traverser.successors t.traverser node)
          ~f:(fun child ->
            Hashtbl.update
              tree
              node
              ~f:
                (Option.value_map
                   ~default:
                     {
                       arrival_number = !arrival_number;
                       children = Node.Key.Set.singleton @@ Node.get_key child;
                     }
                   ~f:(fun { children; _ } ->
                     {
                       arrival_number = !arrival_number;
                       children = Set.add children @@ Node.get_key child;
                     }));
            arrival_number := !arrival_number + 1;
            go (Node.get_key child) arrival_number)
    in
    go (Traverser.root t.traverser |> Node.get_key) (ref 0);
    tree

  (* compute dominator tree: It's basically like defining them like a trie  *)

  type node_with_dominator = int * Node.Key.t [@@deriving sexp, hash, compare]

  let rec add_ancestor_trail
      (trie : (Node.Key.t, Node.Key.Hash_set.t) Hashtbl.t)
      (dominator_sequence : Node.Key.t list)
      : unit
    =
    match dominator_sequence with
    | parent :: child :: rest ->
      let children_set =
        Hashtbl.find_or_add trie parent ~default:(fun () -> Node.Key.Hash_set.create ())
      in
      Hash_set.add children_set child;
      add_ancestor_trail trie (child :: rest)
    | _ -> ()

  let dominator_tree (t : t) (dominator_set : dominator_set)
      : (Node.Key.t, Node.Key.Hash_set.t) Hashtbl.t
    =
    let tree = dfs_tree t in
    let sorted_dominator_set : (Node.Key.t, Node.Key.t list) Hashtbl.t =
      Hashtbl.map dominator_set ~f:(fun dominators ->
          Set.to_list dominators
          |> List.map ~f:(fun dominator ->
                 ((Hashtbl.find_exn tree dominator).arrival_number, dominator))
          |> List.sort
               ~compare:(Comparable.lift Int.compare ~f:(fun (arrival_number, _) -> arrival_number))
          |> List.map ~f:(fun (_, dominator_list) -> dominator_list))
    in
    let dominator_trie : (Node.Key.t, Node.Key.Hash_set.t) Hashtbl.t = Node.Key.Table.create () in
    Hashtbl.iteri sorted_dominator_set ~f:(fun ~key ~data:dominator_sequence ->
        add_ancestor_trail dominator_trie @@ List.append dominator_sequence [ key ]);
    dominator_trie
end
