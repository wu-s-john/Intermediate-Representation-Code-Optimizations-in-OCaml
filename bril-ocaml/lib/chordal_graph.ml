open Core

module Make (Key : sig
  include Hashable
  include Comparable with type t := t
end) (Node : sig
  include Comparable

  val key : t -> Key.t
end) =
struct
  type t = (Key.t, Node.t) Undirected_graph.Poly.t

  module Queue = Priority_hash_map.Make (Key) (Int)

  (* Computes the simplicial elimination ordering of a chordal graph. It does this by iteratively choosing a node with the maximumal weight, removing it and adding more nodes *)
  let simplicial_elimination_ordering (directed_graph : t) : Key.t list =
    let queue = Queue.create () in
    Sequence.unfold ~init:() ~f:(fun () ->
        match Queue.pop queue with
        | None -> None
        | Some (key, _) ->
          let children =
            Undirected_graph.Poly.neighbors directed_graph key |> List.map ~f:Node.key
          in
          List.iter children ~f:(fun child ->
              match Queue.get queue key with
              | None -> ()
              | Some child_weight -> Queue.set queue child (child_weight + 1));
          Some (key, ()))
    |> Sequence.to_list

  (* This function determines a coloring for a chordal graph given that the number of colors is unconstrained. 
     Namely, it does this by iteratively going through the ordering of keys it is given and then tries to assign the smallest color value for that node that is not same as it's neighbors  *)
  let unbounded_greedy_coloring (directed_graph : t) (ordering : Key.t list) : int Key.Map.t =
    let highest_color = Undirected_graph.Poly.maximum_out_degree directed_graph in
    List.fold ordering ~init:Key.Map.empty ~f:(fun color_map key ->
        let children = Undirected_graph.Poly.neighbors directed_graph key |> List.map ~f:Node.key in
        let children_colors =
          List.filter_map children ~f:(fun child -> Map.find color_map child) |> Int.Set.of_list
        in
        let color_value =
          Sequence.init highest_color ~f:Fn.id
          |> Sequence.find ~f:(fun item -> not @@ Set.mem children_colors item)
          |> Option.value ~default:highest_color
        in
        Map.set color_map ~key ~data:color_value)

  let register_selection (color_map : int Key.Map.t) (num_registers : int)
      : [ `Colored of int | `Spill ] Key.Map.t
    =
    color_map |> Map.map ~f:(fun color -> if color < num_registers then `Colored color else `Spill)
end
