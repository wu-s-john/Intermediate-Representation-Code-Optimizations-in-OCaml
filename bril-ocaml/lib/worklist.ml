open Core

module type Node_operations_intf = sig
  type t
  type key
  type data [@@deriving eq]

  val transform : t -> data -> data
  val merge : data list -> data
  val zero : data
end

type ('node, 'flow_value) flow_node = {
  node : 'node;
  in_ : 'flow_value;
  out : 'flow_value;
}

module Make
    (Node : Node.S)
    (Ops : Node_operations_intf with type key := Node.Key.t and type t := Node.t) =
struct
  type data = Ops.data

  type t = {
    work_list : Node.Key.t Node.Key.Hash_queue.t;
    traverser : (Node.Key.t, (Node.t, data) flow_node) Node_traverser.Poly.t;
  }

  let rec run_forward_loop ({ work_list; traverser } as t) : unit =
    let open Ops in
    Option.iter (Hash_queue.dequeue_front_with_key work_list) ~f:(fun (key, _) ->
        let { node; in_ = _; out } = Node_traverser.Poly.find_exn traverser key in
        let predecessors =
          List.concat (Option.to_list (Node_traverser.Poly.predecessors traverser key))
        in
        let predecessors_out_data = List.map predecessors ~f:(fun { out; _ } -> out) in
        let updated_in = merge predecessors_out_data in
        let updated_out = transform node updated_in in
        let updated_node = { node; in_ = updated_in; out = updated_out } in
        Node_traverser.Poly.update traverser updated_node;
        if not @@ equal_data out updated_out then
          List.iter
            (List.concat (Option.to_list (Node_traverser.Poly.successors traverser key)))
            ~f:(fun { node = succesor_node; _ } ->
              let (_ : [ `Key_already_present | `Ok ]) =
                let key = Node.get_key succesor_node in
                Hash_queue.enqueue_back work_list key key
              in
              ());
        run_forward_loop t)

  let rec run_backwards_loop ({ work_list; traverser } as t) : unit =
    let open Ops in
    Option.iter (Hash_queue.dequeue_front_with_key work_list) ~f:(fun (key, _) ->
        let { node; in_; out = _ } = Node_traverser.Poly.find_exn traverser key in
        let successors =
          List.concat (Option.to_list (Node_traverser.Poly.successors traverser key))
        in
        let successors_in_data = List.map successors ~f:(fun { in_; _ } -> in_) in
        let updated_out = merge successors_in_data in
        let updated_in = transform node updated_out in
        let updated_node = { node; in_ = updated_in; out = updated_out } in
        Node_traverser.Poly.update traverser updated_node;
        if not @@ equal_data in_ updated_in then
          List.iter
            (List.concat (Option.to_list (Node_traverser.Poly.successors traverser key)))
            ~f:(fun { node = succesor_node; _ } ->
              let (_ : [ `Key_already_present | `Ok ]) =
                let key = Node.get_key succesor_node in
                Hash_queue.enqueue_back work_list key key
              in
              ());
        run_backwards_loop t)

  let run (traverser : (Node.Key.t, Node.t) Node_traverser.Poly.t) ~f =
    let traverser_with_data =
      Node_traverser.Poly.inv_map
        ~contra_f:(fun { node; _ } -> node)
        traverser
        ~f:(fun node -> { node; in_ = Ops.zero; out = Ops.zero })
    in
    let work_list = Node.Key.Hash_queue.create () in
    let t = { traverser = traverser_with_data; work_list } in
    f t;
    traverser_with_data

  let run_forward (traverser : (Node.Key.t, Node.t) Node_traverser.Poly.t) =
    run traverser ~f:run_forward_loop

  let run_backwards (traverser : (Node.Key.t, Node.t) Node_traverser.Poly.t) =
    run traverser ~f:run_backwards_loop
end
