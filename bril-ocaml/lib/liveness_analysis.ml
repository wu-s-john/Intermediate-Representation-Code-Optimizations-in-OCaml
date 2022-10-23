open Core

module Make (Block : Program.Monomorphic_block_intf) = struct
  module Used_instructions = Multi_map_set.Make (Variable) (With_loc.Instr)

  module Liveness_ops = struct
    type data = Used_instructions.t

    let zero _ = Used_instructions.empty

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

  type live_instr_map =
    (Block.Key.t, (Block.t, Used_instructions.t) Worklist.flow_node) Node_traverser.Poly.t

  module LivenessRunner = Worklist.Make (Block) (Liveness_ops)

  let run (traverser : (Block.Key.t, Block.t) Node_traverser.Poly.t) =
    LivenessRunner.run_backwards traverser

  (* For every definition, you can compute the use for each definition *)
  let compute_liveness_map (traverser : live_instr_map) =
    Node_traverser.Poly.fold
      traverser
      ~init:Liveness_map.empty
      ~f:(fun def_use_map { node = block; in_ = reaching_defs; out = _ } ->
        List.foldi
          (List.rev (Program.Block.all_instrs block))
          ~init:(def_use_map, reaching_defs)
          ~f:(fun instr_line (def_use_map, used_instr_map) instr ->
            (* Add instruction def usage in def_use_map *)
            let def_used_instrs =
              let open List.Let_syntax in
              let%bind def = Option.to_list @@ With_loc.Def.create instr in
              let def_with_loc : With_loc.Def.t =
                { instruction = def; label = block.label; instr_line }
              in
              let declared_variable = With_loc.Def.declared_variable def_with_loc in
              let%map used_instr =
                Used_instructions.get used_instr_map declared_variable |> Set.to_list
              in
              (def_with_loc, used_instr)
            in
            let new_def_used_map =
              List.fold def_used_instrs ~init:def_use_map ~f:(fun def_use_map (def, used_instr) ->
                  Liveness_map.upsert def_use_map (With_loc.Instr.of_def def) used_instr)
            in
            let new_liveness =
              Liveness_ops.set_definition
                used_instr_map
                { instruction = instr; label = block.label; instr_line }
            in
            (new_def_used_map, new_liveness))
        |> fst)

    
end