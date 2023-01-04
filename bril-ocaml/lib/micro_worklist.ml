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
    let final_t =
      match direction with
      | `Forward -> run_forward_loop t
      | `Backward -> run_backwards_loop t
    in
    final_t.flow_traverser
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

(* For each block in the map:
    You go through each instruction and then align the instructions together in order
      Make sure that the instructions goes from 1 - n
    Then, you zip all the instructions together and you get a list of reachable definitions and the possible live variables
        Then you would want to zip all the defintions together
  *)
(* let compute_definition_relations live_vars reaching_def_flow_traverser reaching_def_map reaching_def_flow_traverser = *)

let get_pairs (live_vars : Variable.Set.t) (var_def_map : Var_def_map.t) =
  let live_variable_list = Set.to_list live_vars in
  List.bind live_variable_list ~f:(fun var1 ->
      List.bind live_variable_list ~f:(fun var2 ->
          let defs1 = Var_def_map.get var_def_map var1 |> Set.to_list in
          let defs2 = Var_def_map.get var_def_map var2 |> Set.to_list in
          List.cartesian_product defs1 defs2))

let align_variables_and_instr
    (live_vars : Variable.Set.t Instruction_location_map.t)
    (liveness_analysis_flow_traverser :
      ( Program.Block.Key.t,
        (Program.Block.Key.t, Liveness_analysis_aggregator.data) Flow_node.t )
      Node_traverser.Poly.t)
    (reaching_def_map : Var_def_map.t Instruction_location_map.t)
    (reaching_def_flow_traverser :
      ( Program.Block.Key.t,
        (Program.Block.Key.t, Reaching_def_aggregator.data) Flow_node.t )
      Node_traverser.Poly.t)
  =
  Instruction_location_map.get_labels reaching_def_map
  |> List.fold_result ~init:[] ~f:(fun acc_pairings label ->
         let open Result.Let_syntax in
         let%bind sorted_reaching_def =
           Instruction_location_map.get_sorted_instrs reaching_def_map label
         in
         let%bind sorted_live_vars = Instruction_location_map.get_sorted_instrs live_vars label in
         let%map () =
           match List.zip sorted_reaching_def sorted_live_vars with
           | Unequal_lengths -> Error `Unequal_lengths
           | Ok zipped_list ->
             Result.ok_if_true
               (List.for_all zipped_list ~f:(fun ((index1, instr1, _), (index2, instr2, _)) ->
                    index1 = index2 && Program.Instruction.equal instr1 instr2))
               ~error:`Misaligned_instructions
         in
         let sorted_reaching_def_with_before_comp =
           let Flow_node.{ before_block_val; _ } =
             Node_traverser.Poly.find_exn reaching_def_flow_traverser label
           in
           before_block_val
           :: List.map sorted_reaching_def ~f:(fun (_index, _instr, var_def_map) -> var_def_map)
         in
         let liveness_analysis_with_after_comp =
           let Flow_node.{ after_block_val; _ } =
             Node_traverser.Poly.find_exn liveness_analysis_flow_traverser label
           in
           List.map sorted_live_vars ~f:(fun (_index, _instr, var_def_map) -> var_def_map)
           @ [ after_block_val ]
         in
         List.zip_exn sorted_reaching_def_with_before_comp liveness_analysis_with_after_comp
         @ acc_pairings)

let create_interference_graph
    (live_vars : Variable.Set.t Instruction_location_map.t)
    (liveness_analysis_flow_traverser :
      ( Program.Block.Key.t,
        (Program.Block.Key.t, Liveness_analysis_aggregator.data) Flow_node.t )
      Node_traverser.Poly.t)
    (reaching_def_map : Var_def_map.t Instruction_location_map.t)
    (reaching_def_flow_traverser :
      ( Program.Block.Key.t,
        (Program.Block.Key.t, Reaching_def_aggregator.data) Flow_node.t )
      Node_traverser.Poly.t)
  =
  let open Result.Let_syntax in
  let%map aligned_live_vars_and_defs =
    align_variables_and_instr
      live_vars
      liveness_analysis_flow_traverser
      reaching_def_map
      reaching_def_flow_traverser
  in
  let undirected_edges = List.bind aligned_live_vars_and_defs ~f:(fun (var_def_map, live_vars) ->
      get_pairs live_vars var_def_map) in
  Undirected_graph.Poly.of_alist (module With_loc.Def) undirected_edges
