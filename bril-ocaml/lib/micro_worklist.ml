open Core

module type Node_operations_intf = sig
  type t
  type item
  type data [@@deriving eq]

  val items : t -> item list
  val transform : data -> item -> data
  val merge : data list -> data
  val zero : t -> data
end

(* After processing each instruction, you can put the data into some type of aggregator. 
   It could be useful for getting results like def-use or live ranges at an instruction level  *)
module type Aggregator_intf = sig
  type t
  type item
  type data

  val process : t -> item -> data -> t
  val empty : t
end

module Flow_node = struct
  module T = struct
    type ('key, 'flow_value) t = {
      key : 'key;
      before_block_val : 'flow_value;
      after_block_val : 'flow_value;
    }
  end

  include T

  module Make
      (Key : Node.Key) (Value : sig
        type t
      end) =
  struct
    type t = (Key.t, Value.t) T.t

    let key { key; _ } = key

    module Key = Key
  end
end

module Make
    (Node : Node.S)
    (Ops : Node_operations_intf with type t := Node.t)
    (Aggregator : Aggregator_intf with type item := Ops.item and type data := Ops.data) =
struct
  type t = {
    work_list : Node.t Node.Key.Hash_queue.t;
    traverser : (Node.Key.t, Node.t) Node_traverser.Poly.t;
    flow_traverser : (Node.Key.t, (Node.Key.t, Ops.data) Flow_node.t) Node_traverser.Poly.t;
    aggregate : Aggregator.t;
  }

  let rec run_forward_loop ({ work_list; traverser; flow_traverser; aggregate } as t) : t =
    let open Ops in
    Option.value_map ~default:t (Hash_queue.dequeue_front_with_key work_list) ~f:(fun (key, node) ->
        let Flow_node.{ after_block_val = old_out_data; _ } =
          Node_traverser.Poly.find_exn flow_traverser key
        in
        let predecessors_out_data =
          List.concat (Option.to_list (Node_traverser.Poly.predecessors flow_traverser key))
          |> List.map ~f:(fun node -> node.after_block_val)
        in
        let updated_in_data = merge predecessors_out_data in
        let items = Ops.items node in
        let (updated_out_data, updated_aggregator) =
          List.fold items ~init:(updated_in_data, aggregate) ~f:(fun (data, aggregate) item ->
              let updated_data = transform data item in
              let updated_aggregate = Aggregator.process aggregate item updated_data in
              (updated_data, updated_aggregate))
        in
        Node_traverser.Poly.update
          flow_traverser
          { key; before_block_val = updated_in_data; after_block_val = updated_out_data };
        if not @@ equal_data old_out_data updated_out_data then
          List.iter
            (List.concat (Option.to_list (Node_traverser.Poly.successors traverser key)))
            ~f:(fun succesor_node ->
              let (_ : [ `Key_already_present | `Ok ]) =
                let key = Node.key succesor_node in
                Hash_queue.enqueue_back work_list key succesor_node
              in
              ());
        run_forward_loop { work_list; traverser; flow_traverser; aggregate = updated_aggregator })

  let rec run_backwards_loop ({ work_list; traverser; flow_traverser; aggregate } as t) : t =
    let open Ops in
    Option.value_map ~default:t (Hash_queue.dequeue_front_with_key work_list) ~f:(fun (key, node) ->
        let Flow_node.{ before_block_val = old_out_data; _ } =
          Node_traverser.Poly.find_exn flow_traverser key
        in
        let successors_in_data =
          List.concat (Option.to_list (Node_traverser.Poly.successors flow_traverser key))
          |> List.map ~f:(fun node -> node.before_block_val)
        in
        let updated_in_data = merge successors_in_data in
        let items = Ops.items node in
        let (updated_out_data, updated_aggregator) =
          List.fold_right items ~init:(updated_in_data, aggregate) ~f:(fun item (data, aggregate) ->
              let updated_data = transform data item in
              let updated_aggregate = Aggregator.process aggregate item updated_data in
              (updated_data, updated_aggregate))
        in
        Node_traverser.Poly.update
          flow_traverser
          { key; before_block_val = updated_out_data; after_block_val = updated_in_data };
        if not @@ equal_data old_out_data updated_out_data then
          List.iter
            (List.concat (Option.to_list (Node_traverser.Poly.predecessors traverser key)))
            ~f:(fun succesor_node ->
              let (_ : [ `Key_already_present | `Ok ]) =
                let key = Node.key succesor_node in
                Hash_queue.enqueue_back work_list key succesor_node
              in
              ());
        run_backwards_loop { work_list; traverser; flow_traverser; aggregate = updated_aggregator })

  let run
      (traverser : (Node.Key.t, Node.t) Node_traverser.Poly.t)
      (direction : [ `Forward | `Backward ])
    =
    let flow_traverser =
      Node_traverser.Poly.map
        (module Flow_node.Make
                  (Node.Key)
                  (struct
                    type t = Ops.data
                  end))
        traverser
        ~f:(fun node ->
          Flow_node.
            {
              key = Node.key node;
              before_block_val = Ops.zero node;
              after_block_val = Ops.zero node;
            })
    in
    let work_list = Node.Key.Hash_queue.create () in
    let nodes = Node_traverser.Poly.reverse_postorder traverser in
    List.iter nodes ~f:(fun node ->
        let enqueue =
          match direction with
          | `Forward -> Hash_queue.enqueue_back_exn
          | `Backward -> Hash_queue.enqueue_front_exn
        in
        enqueue work_list (Node.key node) node);
    let t = { work_list; traverser; flow_traverser; aggregate = Aggregator.empty } in
    match direction with
    | `Forward -> run_forward_loop t
    | `Backward -> run_backwards_loop t
end

module Reaching_def_ops = struct
  type t = Program.Block.t
  type data = Var_def_map.t
  type item = With_loc.Instr.t

  let items (block : t) : item list =
    let label = block.label in
    List.mapi (Program.Block.all_instrs block) ~f:(fun instr_line instr ->
        { With_loc.instruction = instr; label; instr_line })

  (* First kill the existing variable in the map. Then add the new variable *)
  let set_definition (var_def_map : data) (def : With_loc.Def.t) =
    let declared_variable = With_loc.Def.declared_variable def in
    let removed_def_map = Var_def_map.remove var_def_map declared_variable in
    Var_def_map.upsert removed_def_map declared_variable def

  let transform (def_map : data) With_loc.{ label; instr_line; instruction } =
    let def = With_loc.Def.create instruction in
    Option.value_map def ~default:def_map ~f:(fun def ->
        set_definition def_map { instruction = def; label; instr_line })

  let merge (pred_outputs : data list) : data =
    List.fold_left pred_outputs ~init:Var_def_map.empty ~f:Var_def_map.merge

  let zero _ = Var_def_map.empty
  let equal_data = Var_def_map.equal
end

module Reaching_def_aggregator = struct
  type t = Var_def_map.t Instruction_location_map.t
  type item = With_loc.Instr.t
  type data = Var_def_map.t

  let empty = Instruction_location_map.empty

  let process
      (map : t)
      ({ label : string option; instr_line : int; instruction } : item)
      (var_def_map : data)
    =
    Instruction_location_map.set map label (`Index (instr_line, instruction)) var_def_map
end

module Reaching_definition = Make (Program.Block) (Reaching_def_ops) (Reaching_def_aggregator)

module Liveness_analysis_ops = struct
  type t = Program.Block.t
  type data = Variable.Set.t
  type item = With_loc.Instr.t

  let items (block : t) : item list =
    let label = block.label in
    List.mapi (Program.Block.all_instrs block) ~f:(fun instr_line instr ->
        { With_loc.instruction = instr; label; instr_line })

  let merge (successsor_before_live_vars : data list) : data =
    List.fold_left successsor_before_live_vars ~init:Variable.Set.empty ~f:Variable.Set.union

  (* transform will process an instruction. 
     If the instruction has a definition assignment, it will remove the variable associated with that definition.
     Then, all the uses of the instruction will get added to the variable set
     *)
  let transform (live_vars : data) (instr : With_loc.Instr.t) =
    let declared_variable = With_loc.Instr.declared_variable instr in
    let removed_live_vars =
      Option.value_map declared_variable ~default:live_vars ~f:(fun declared_variable ->
          Set.remove live_vars declared_variable)
    in
    let used_vars = Program.Instruction.used_vars (instr.instruction :> Program.Instruction.t) in
    Set.fold used_vars ~init:removed_live_vars ~f:(fun live_vars used_var ->
        Set.add live_vars used_var)

  let zero _ = Variable.Set.empty
  let equal_data = Variable.Set.equal
end

module Liveness_analysis_aggregator = struct
  type t = Variable.Set.t Instruction_location_map.t
  type item = With_loc.Instr.t
  type data = Variable.Set.t

  let empty = Instruction_location_map.empty

  let process
      (map : t)
      ({ label : string option; instr_line : int; instruction } : item)
      (var_def_map : data)
    =
    Instruction_location_map.set map label (`Index (instr_line, instruction)) var_def_map
end

module Liveness_analysis =
  Make (Program.Block) (Liveness_analysis_ops) (Liveness_analysis_aggregator)
