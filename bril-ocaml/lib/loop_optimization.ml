open Core
open Program
module Multi_map = Multi_map_set.Make (Variable) (Location)

module Marked_loop_invariant_def = Multi_map_set.Make (Variable) (Location)

(* For each def, determine the use *)
let compute_loop_invariant_instruction
    (node_traverser : Function.blocks)
    (loop : Loop.t)
    ~(use_def_map : Use_def_map.t)
    ~(var_def_map : Var_def_map.t)
    (marked_loop_invariant_definitions : Marked_loop_invariant_def.t)
  =
  let is_all_uses_outside_loop instr_with_loc variable =
    let reaching_defs = Use_def_map.get use_def_map instr_with_loc in
    let defs =
      reaching_defs
      |> Set.to_list
      |> List.filter ~f:(fun def ->
             Variable.equal (Instruction.Definition.assignment_var def.instruction) variable)
    in
    List.for_all defs ~f:(fun With_loc.{ label; _ } -> not (Set.mem loop.nodes label))
  in
  let is_defined_once (var : Variable.t) = Var_def_map.get var_def_map var |> Set.length = 1 in
  Set.fold
    loop.nodes
    ~init:marked_loop_invariant_definitions
    ~f:(fun marked_loop_invariant_definitions block_name ->
      let block = Node_traverser.Poly.find_exn node_traverser block_name in
      List.fold
        (Program.Block.all_instrs_with_line block)
        ~init:marked_loop_invariant_definitions
        ~f:(fun marked_loop_invariant_definitions (instr_line, instruction) ->
          let is_loop_invariant =
            Instruction.used_vars instruction
            |> Set.for_all ~f:(fun used_variable ->
                   is_all_uses_outside_loop
                     { With_loc.label = block_name; instruction; instr_line }
                     used_variable
                   || (is_defined_once used_variable
                      && Marked_loop_invariant_def.mem
                           marked_loop_invariant_definitions
                           used_variable
                           { Location.block_name; line = instr_line }))
          in
          if is_loop_invariant then
            Instruction.declared_var instruction
            |> Option.value_map ~default:marked_loop_invariant_definitions ~f:(fun var ->
                   Marked_loop_invariant_def.upsert
                     marked_loop_invariant_definitions
                     var
                     Location.{ block_name; line = instr_line })
          else marked_loop_invariant_definitions))

let is_dominate
    (block_dominator_graph : Block_name.Set.t Block_name.Map.t)
    (instr1 : Instruction.t With_loc.t)
    (instr2 : Instruction.t With_loc.t)
  =
  if Block_name.equal instr1.label instr2.label then instr1.instr_line < instr2.instr_line
  else
    Block_name.Map.find block_dominator_graph instr1.label
    |> Option.value_map ~default:false ~f:(fun dominators -> Set.mem dominators instr2.label)

let does_definition_dominates_all_of_its_uses
    (program : Function.blocks)
    (definition : Instruction.Definition.t With_loc.t)
    ~(def_use_map : Def_use_map.t)
    ~(block_dominator_graph : Block_name.Set.t Block_name.Map.t)
  =
  let uses = Def_use_map.get def_use_map definition in
  Set.for_all uses ~f:(fun use ->
      is_dominate block_dominator_graph (With_loc.map ~f:Instruction.of_definition definition) use)

let no_other_definitions_exists_in_loop
    (program : Function.blocks)
    (definition : Instruction.Definition.t With_loc.t)
    (loop : Loop.t)
    ~(var_def_map : Var_def_map.t)
  =
  let var = Instruction.Definition.assignment_var definition.instruction in
  let defs = Var_def_map.get var_def_map var in
  Set.for_all defs ~f:(fun def ->
      With_loc.equal Instruction.Definition.equal definition def
      || not (Set.mem loop.nodes def.label))

let instruction_dominates_all_loop_exists
    (program : Function.blocks)
    (instruction : Instruction.t With_loc.t)
    (loop : Loop.t)
    ~(block_dominator_graph : Block_name.Set.t Block_name.Map.t)
  =
  let block_name = instruction.label in
  Set.for_all loop.exits ~f:(fun exit ->
      Block_name.Map.find block_dominator_graph block_name
      |> Option.value_map ~default:false ~f:(fun dominators -> Set.mem dominators exit))

let loop_invariant_code_motion
    (program : Function.blocks)
    (loop : Loop.t)
    ~(var_def_map : Var_def_map.t)
    ~(use_def_map : Use_def_map.t)
    ~(block_dominator_graph : Block_name.t Block_name.Map.t)
  =
  let loop_invariant_instruction =
    compute_loop_invariant_instruction program loop ~use_def_map ~var_def_map Variable.Map.empty
  in
  let header_instructions = 
    failwith "Need to implement"
  in
  failwith "meatspin"
