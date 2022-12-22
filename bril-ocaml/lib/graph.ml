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

type 'key dfs_tree_result = {
  comparable : (module Comparable with type t = 'key);
  arrival_number : int;
}

module type Renderable_key = sig
  type t [@@deriving sexp, to_yojson]

  include Node.Key with type t := t
end

module Make (Key : Renderable_key) (Node : Node.S with module Key = Key) = struct
  module Traverser = Node_traverser.Poly

  type t = (Node.Key.t, Node.t) Node_traverser.Poly.t

  module Dominator_set = Multi_map_set.Make (Node.Key) (Node.Key)

  module Dominator_node = struct
    type t = {
      node : Node.t;
      is_root : bool;
      domain : Node.Key.Set.t;
    }

    module Key = Key

    let get_key { node; _ } = Node.get_key node
    let children { node; _ } = Node.children node
  end

  module Node_operations = struct
    type t = Dominator_node.t
    type key = Node.Key.t
    type data = Node.Key.Set.t [@@deriving eq]

    let transform ({ node; _ } : t) (data : data) = Node.Key.Set.add data (Node.get_key node)

    let merge (list : data list) =
      List.reduce list ~f:Set.inter |> Option.value ~default:Node.Key.Set.empty

    let zero { Dominator_node.is_root; domain; node } =
      if is_root then Node.Key.Set.singleton (Node.get_key node) else domain
  end

  module Dominator_worklist = Worklist.Make (Dominator_node) (Node_operations)
  module Multi_map_graph = Multi_map_set.Make (Node.Key) (Node.Key)

  module Dominator_block = struct
    module Key = Key

    type t = {
      node : Node.t;
      dominators : Node.Key.Set.t;
    }

    let get_key { node; _ } = Node.get_key node
    let children { node; _ } = Node.children node
  end

  let compute_dominators (traverser : t) : Dominator_set.t =
    let root_key = Traverser.root traverser |> Node.get_key in
    let domain = Traverser.keys traverser |> Node.Key.Set.of_list in
    let dominator_traverser =
      Traverser.inv_map
        traverser
        ~f:(fun node ->
          { Dominator_node.node; is_root = Node.Key.equal root_key (Node.get_key node); domain })
        ~contra_f:(fun { Dominator_node.node; _ } -> node)
    in
    let worklist_result = Dominator_worklist.run_forward dominator_traverser in
    let result =
      Node_traverser.Poly.to_map (module Key) worklist_result ~f:(fun { out; _ } -> out)
    in
    result

  type dfs_tree_result = {
    arrival_number : int;
    children : Node.Key.Set.t;
  }
  [@@deriving sexp]

  type dfs_tree = dfs_tree_result Node.Key.Map.t [@@deriving sexp]

  let dfs_tree (traverser : t) : dfs_tree =
    let rec go
        (traverser : t)
        (arrival_map : int Key.Map.t)
        (children_map : Key.Set.t Key.Map.t)
        (prev_node : Node.Key.t option)
        (node : Node.Key.t)
        (arrival_number : int)
      =
      match Map.find arrival_map node with
      | Some _ -> (arrival_map, children_map, arrival_number)
      | None ->
        let arrival_map = Map.set arrival_map ~key:node ~data:arrival_number in
        let arrival_number = arrival_number + 1 in
        let new_children_map =
          match prev_node with
          | None -> children_map
          | Some prev_node -> Multi_map_graph.upsert children_map prev_node node
        in
        List.fold
          ~init:(arrival_map, new_children_map, arrival_number)
          (Node_traverser.Poly.successors traverser node |> Option.to_list |> List.concat)
          ~f:(fun (arrival_map, children_map, arrival_number) child ->
            go traverser arrival_map children_map (Some node) (Node.get_key child) arrival_number)
    in
    let (arrival_map, children_map, _) =
      go traverser Key.Map.empty Key.Map.empty None (Traverser.root traverser |> Node.get_key) 0
    in
    let tree =
      Map.fold arrival_map ~init:Key.Map.empty ~f:(fun ~key ~data:arrival_number tree ->
          Map.set
            tree
            ~key
            ~data:
              {
                arrival_number;
                children = Map.find children_map key |> Option.value ~default:Key.Set.empty;
              })
    in
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

  (* After you do a graph traversal and there is an edge that is not a backedge, then it's not a reducible graph *)
  let compute_back_edges (traverser : t) (dominator_set : Dominator_set.t)
      : (Node.Key.t * Node.Key.t) list
    =
    let dfs_tree = dfs_tree traverser |> Map.map ~f:(fun { children; _ } -> children) in
    let dfs_tree_edges = Multi_map_graph.to_alist dfs_tree |> Edge.Set.of_list in
    let traverser_edges = Traverser.edges traverser |> Edge.Set.of_list in
    let possible_back_edges = Set.diff traverser_edges dfs_tree_edges in
    Set.filter possible_back_edges ~f:(fun (source, dest) ->
        Map.find dominator_set source
        |> Option.value_map ~default:false ~f:(fun dominator_set -> Set.mem dominator_set dest))
    |> Set.to_list

  module Natural_loop = Natural_loop.Make (Node.Key)

  (* To compute a natural loop. Essentially, you start from the source of a back edge. 
     You put that source into a body, which is the set of nodes that are part of the natural loop. 
     Then, for each node that hasn't been put in the body but has been discovered, you put them in the body and then discover more nodes  *)
  let compute_natural_loop
      (traverser : t)
      (dominator_set : Dominator_set.t)
      ((source, dest) : Node.Key.t * Node.Key.t)
      : Natural_loop.t
    =
    let rec go (body : Node.Key.Set.t) (remaining : Node.Key.Set.t) =
      if Set.is_empty remaining then body
      else
        let new_body = Set.union body remaining in
        let remaining_pred =
          Node.Key.Set.union_list
            (Set.to_list body
            |> List.map ~f:(fun key ->
                   if Node.Key.equal key dest then Node.Key.Set.singleton dest
                   else
                     Node_traverser.Poly.predecessors traverser key
                     |> Option.to_list
                     |> List.concat
                     |> List.filter_map ~f:(fun node ->
                            let key = Node.get_key node in
                            Option.some_if (Dominator_set.mem dominator_set key dest) key)
                     |> Node.Key.Set.of_list))
        in
        let new_remaining = Set.diff remaining_pred new_body in
        go new_body new_remaining
    in
    let nodes = go Node.Key.Set.empty (Node.Key.Set.singleton source) in
    Natural_loop.{ nodes; header_node = source; back_node = dest }

  let get_predecessors (traverser : t) (current_node : Node.Key.t) : Node.Key.Set.t =
    List.concat (Option.to_list @@ Traverser.predecessors traverser current_node)
    |> List.map ~f:Node.get_key
    |> Node.Key.Set.of_list

  let rec find_max_levels_reverse
      (traverser : t)
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
        traverser
        updated_explored
        (Set.diff possible_next_level updated_explored)
        desired_dest
        (num_levels + 1)

  let rec find_all_paths_reverse_helper
      ~(explored : Node.Key.Set.t)
      (traverser : t)
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
            traverser
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
        find_all_paths_reverse_helper ~explored:Node.Key.Set.empty t source dest max_levels)
    |> List.map ~f:List.rev

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

  (*  The dominator tree can be defined as key -> children *)

  module Dominator_tree = struct
    include Multi_map_set.Make (Node.Key) (Node.Key)

    module Node = struct
      type t = Node.Key.t [@@deriving sexp, hash, compare]
      type key = Node.Key.t

      let key = Fn.id
    end
  end

  let dominator_tree (t : t) (dominator_set : Dominator_set.t) : Dominator_tree.t =
    let tree = dfs_tree t in
    let key_to_immediate_dominator : Node.Key.t option Node.Key.Map.t =
      Map.mapi dominator_set ~f:(fun ~key ~data:dominators ->
          Set.to_list (Set.remove dominators key)
          |> List.max_elt
               ~compare:
                 (Comparable.lift Int.compare ~f:(fun dominator ->
                      (Map.find_exn tree dominator).arrival_number)))
    in
    let dominator_to_children =
      List.filter_map (Map.to_alist key_to_immediate_dominator) ~f:(fun (key, dominator) ->
          Option.map dominator ~f:(fun dominator -> (dominator, key)))
    in
    Dominator_tree.of_alist (module Dominator_tree.Node) dominator_to_children
end

module Test = struct
  module IntNode = struct
    module Key = struct
      module T = struct
        type t = int [@@deriving sexp, hash, compare, to_yojson]
      end

      include T
      include Comparable.Make (T)
      include Hashable.Make (T)
    end

    type t = int * Int.Set.t

    let get_key (key, _) = key
    let children (_, children) = Set.to_list children
  end

  module Graph = Make (IntNode.Key) (IntNode)

  let edges = [ (1, 2); (2, 3); (2, 4); (3, 5); (4, 5); (5, 2); (2, 6) ]
  let expected_dominator_tree_edges = [ (1, 2); (2, 3); (2, 4); (2, 5); (2, 6) ]

  module Int_with_yojson = struct
    include Int

    let to_yojson (t : t) = `Int t

    include Comparable.Make (Int)
    include Hashable.Make (Int)

    type key = int

    let key = Fn.id
    let render = Int.to_string
  end

  module Multi_set = Multi_map_set.Make (Int_with_yojson) (Int_with_yojson)
  module Node_set = Node.Make_from_multimap (Int_with_yojson) (Multi_set)
  module Int_graph = Make (Node_set.Key) (Node_set)

  let graphviz : (int, Node_set.t) Graphviz.t =
    let render_key = Int.to_string in
    let render_node = Node_set.render in
    let get_key = Node_set.get_key in
    Graphviz.create ~render_key ~render_node ~get_key

  let%test_unit "Should be able to get Graph data easily" =
    (* Construct a multiset map first *)
    let multiset = Multi_set.of_alist (module Int_with_yojson) edges in
    let nodes = List.bind edges ~f:(fun (src, dest) -> [ src; dest ]) |> Int.Set.of_list in
    let nodes_with_multiset = Set.to_list nodes |> List.map ~f:(fun node -> (multiset, node)) in
    let node_traveser =
      Node_traverser.Poly.of_list (module Node_set) nodes_with_multiset |> Option.value_exn
    in
    let dominators = Int_graph.compute_dominators node_traveser in
    let dominator_tree = Int_graph.dominator_tree node_traveser dominators in
    let expected_dominator_tree =
      Multi_set.of_alist (module Int_with_yojson) expected_dominator_tree_edges
    in
    [%test_eq: Multi_set.t] dominator_tree expected_dominator_tree

  (* Using the map, construct a node traverser object *)
  (* Use the node traverse to construct the graph objects  *)
end