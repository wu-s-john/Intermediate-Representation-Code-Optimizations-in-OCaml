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

type ('key) dfs_tree_result = {
  comparable : (module Comparable with type t = 'key);
  arrival_number : int;
  children : C;
}

module type Poly_intf = sig
  type ('key, 'node) t = ('key, 'node) Node_traverser.Poly.t

  val dominators : ('key, 'node) t -> ('key, 'key) t

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

  type dfs_tree = (Node.Key.t, dfs_tree_result) Hashtbl.t

  let dfs_tree (t : t) : dfs_tree =
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

  let edges (graph : ('key, 'values) Hashtbl.t) ~(f : 'values -> 'key list) : ('key * 'key) list =
    Hashtbl.to_alist graph
    |> List.bind ~f:(fun (source, dests) -> f dests |> List.map ~f:(fun dest -> (source, dest)))

  module Edge = struct
    module T = struct
      type t = Node.Key.t * Node.Key.t [@@deriving sexp, compare, eq, hash]
    end

    include Comparable.Make (T)
    include Hashable.Make (T)
  end

  let compute_back_edges ({ traverser } as t : t) (dominator_set : dominator_set)
      : (Node.Key.t * Node.Key.t) list
    =
    let dfs_tree = dfs_tree t in
    let dfs_tree_edges =
      edges dfs_tree ~f:(fun { children; _ } -> Set.to_list children) |> Edge.Set.of_list
    in
    let traverser_edges = Traverser.edges traverser |> Edge.Set.of_list in
    let possible_back_edges = Set.diff traverser_edges dfs_tree_edges in
    Set.filter possible_back_edges ~f:(fun (source, dest) ->
        Hashtbl.find dominator_set source
        |> Option.value_map ~default:false ~f:(fun dominator_set -> Set.mem dominator_set dest))
    |> Set.to_list

  let get_predecessors (traverser : Traverser.t) (current_node : Node.Key.t) : Node.Key.Set.t =
    List.concat (Option.to_list @@ Traverser.predecessors traverser current_node)
    |> List.map ~f:Node.get_key
    |> Node.Key.Set.of_list

  let rec find_max_levels_reverse
      ({ traverser } as t : t)
      (explored : Node.Key.Set.t) (* Tells which child it came from *)
      (current_level : Node.Key.Set.t)
      (desired_dest : Node.Key.t)
      (num_levels : int)
      : int option
    =
    if Set.is_empty current_level then None
    else if Set.mem current_level desired_dest then Some num_levels
    else
      let updated_explored = Set.union explored current_level in
      let possible_next_level =
        List.bind (Set.to_list current_level) ~f:(fun current_node ->
            List.concat (Option.to_list @@ Traverser.predecessors traverser current_node))
        |> List.map ~f:Node.get_key
        |> Node.Key.Set.of_list
      in
      find_max_levels_reverse
        t
        updated_explored
        (Set.diff possible_next_level updated_explored)
        desired_dest
        (num_levels + 1)

  let rec find_all_paths_reverse_helper
      ~(explored : Node.Key.Set.t)
      ({ traverser } as t : t)
      (source : Node.Key.t)
      (dest : Node.Key.t)
      (num_turns : int)
      : Node.Key.t list list
    =
    if Int.equal num_turns 0 && not (Node.Key.equal source dest) then []
    else if Int.equal num_turns 0 && Node.Key.equal source dest then [ [ source ] ]
    else
      let predecessors = get_predecessors traverser source |> Set.to_list in
      List.bind predecessors ~f:(fun predecessor ->
          find_all_paths_reverse_helper
            ~explored:(Set.add explored source)
            t
            predecessor
            dest
            (num_turns - 1)
          |> List.map ~f:(fun path -> source :: path))

  let find_all_paths_reverse (t : t) (source : Node.Key.t) (dest : Node.Key.t)
      : Node.Key.t list list
    =
    Option.value_map
      (find_max_levels_reverse t Node.Key.Set.empty (Node.Key.Set.singleton source) dest 0)
      ~default:[]
      ~f:(fun max_levels ->
        find_all_paths_reverse_helper ~explored:Node.Key.Set.empty t source dest max_levels) |> List.map ~f:(List.rev)

  (* let natural_loops ({ traverser } as t : t) (dominator_set : dominator_set) : Traverser.t list = 
      let back_edges = compute_back_edges t dominator_set in *)

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
