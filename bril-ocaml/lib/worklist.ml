open Core

module type Node_operations_intf = sig
  type t
  type key
  type data [@@deriving eq]

  val transform : t -> data -> data
  val merge : data list -> data
  val zero : data
end

module Outer_node = Node

module Make
    (Node : Node_traverser.Node_intf)
    (Ops : Node_operations_intf with type key := Node.Key.t and type t := Node.t) =
struct
  type data = Ops.data

  module Node_with_data = struct
    module T = struct
      type t = {
        node : Node.t;
        in_ : data;
        out : data;
      }

      let f (t : Node.t) : t = { node = t; in_ = Ops.zero; out = Ops.zero }
      let contra_f ({ node; _ } : t) = node
    end

    include T
    include Node_traverser.Node.Make_map (Node) (T)
  end

  type t = {
    work_list : Node.Key.t Node.Key.Hash_queue.t;
    traverser : (Node.Key.t, Node_with_data.t) Node_traverser.Poly.t;
  }

  let rec run_forward_loop ({ work_list; traverser } as t) : unit =
    let open Ops in
    Option.iter (Hash_queue.dequeue_front_with_key work_list) ~f:(fun (key, _) ->
        let Node_with_data.T.{ node; in_ = _; out } = Node_traverser.Poly.find_exn traverser key in
        let predecessors =
          List.concat (Option.to_list (Node_traverser.Poly.predecessors traverser key))
        in
        let prededcessors_out_data = List.map predecessors ~f:(fun { out; _ } -> out) in
        let updated_in = merge prededcessors_out_data in
        let updated_out = transform node updated_in in
        let updated_node = Node_with_data.T.{ node; in_ = updated_in; out = updated_out } in
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

  let run_forward (traverser : (Node.Key.t, Node.t) Node_traverser.Poly.t)
      : (Node.Key.t, Node_with_data.t) Node_traverser.Poly.t
    =
    let traverser_with_data =
      Node_traverser.Poly.map (module Node_with_data) traverser ~f:Node_with_data.f
    in
    let work_list = Node.Key.Hash_queue.create () in
    let t = { traverser = traverser_with_data; work_list } in
    run_forward_loop t;
    traverser_with_data
end

module Reaching_defintion (Block : Program.Monomorphic_block_intf) = struct
  module Variable = String
  module Definitions0 = Multi_map_set.Make (Variable) (Block.Key)

  module Definitions = struct
    type data = Definitions0.t

    let zero = Definitions0.empty

    let merge (ins : data list) : data =
      List.fold_left ins ~init:Definitions0.empty ~f:Definitions0.merge

    let transform (block : Block.t) (definitions : data) : data =
      List.fold block.instrs ~init:definitions ~f:(fun definitions instr ->
          Option.value_map
            (Program.Instruction.dest instr)
            ~default:definitions
            ~f:(fun defintion ->
              let removed_defintion : data = Definitions0.remove definitions defintion in
              Definitions0.upsert removed_defintion defintion (Block.get_key block : Block.Key.t)))

    let equal_data = Definitions0.equal
  end

  module Worklist = Make (Block) (Definitions)

  let run (traverser : (Block.Key.t, Block.t) Node_traverser.Poly.t)
      : (Block.Key.t, Worklist.Node_with_data.t) Node_traverser.Poly.t
    =
    Worklist.run_forward traverser
end
