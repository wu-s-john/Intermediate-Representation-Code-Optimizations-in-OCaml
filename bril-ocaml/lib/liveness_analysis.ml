open Core

module Make (Block : Program.Monomorphic_block_intf) = struct
  module Liveness_ops = struct
    module Used_instructions = Multi_map_set.Make (Variable) (With_loc.Instr)

    type data = Used_instructions.t

    let zero = Used_instructions.empty

    let merge (child_inputs : data list) : data =
      List.fold_left child_inputs ~init:Used_instructions.empty ~f:Used_instructions.merge

    let equal_data = Used_instructions.equal

    let set_definition (used_instructions : data) (instr : With_loc.Instr.t) =
      let declared_variable = With_loc.Instr.declared_variable instr in
      let updated_used_instructions =
        Option.value_map declared_variable ~default:used_instructions ~f:(fun declared_variable ->
            Used_instructions.remove used_instructions declared_variable)
      in
      let used_vars = Program.Instruction.used_vars (instr.instruction :> Program.Instruction.t) in
      Set.fold used_vars ~init:updated_used_instructions ~f:(fun used_instructions used_var ->
          Used_instructions.upsert used_instructions used_var instr)

    let transform (block : Block.t) (used_instructions : data) : data =
      let label = block.label in
      List.fold
        (List.rev_mapi ~f:(fun i instr -> (i, instr)) @@ Program.Block.all_instrs block)
        ~init:used_instructions
        ~f:(fun def_map (line_instr, instr) ->
          set_definition def_map { instruction = instr; label; instr_line = line_instr })
  end

  module LivenessRunner = Worklist.Make (Block) (Liveness_ops)

  let run (traverser : (Block.Key.t, Block.t) Node_traverser.Poly.t) =
    LivenessRunner.run_backwards traverser
end