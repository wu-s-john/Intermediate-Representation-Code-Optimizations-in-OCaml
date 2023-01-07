open Program
open Core

type phi_references = Block.Key.Set.t Variable.Map.t

(* Phi references are essentially phi node place holders  *)
module Phi_references = Multi_map_set.Make (Variable) (Block.Key)

(* 
   You have a phi function for a variable which maps block name to instruction location
   You can also also decide at runtime which variable to choose.
    - So, there is a mapping from ssa-variable to (blockname, location)
   *)
module Single_var_phi_function = struct
  type t = {
    block_to_loc_map : Int.t Block_name.Map.t;
    block_to_instruction : Location.t Variable.SSA.Map.t;
  }

  let empty =
    { block_to_loc_map = Block_name.Map.empty; block_to_instruction = Variable.SSA.Map.empty }

  let upsert (t : t) (variable : Variable.SSA.t) { Location.block_name; line } : t =
    {
      block_to_loc_map = Map.set t.block_to_loc_map ~key:block_name ~data:line;
      block_to_instruction = Map.set t.block_to_instruction ~key:variable ~data:{ block_name; line };
    }

  let find_location (t : t) (variable : Variable.SSA.t) : Location.t option =
    Map.find t.block_to_instruction variable
end

(* Data structure that contain all the phis for a block *)
module Block_phi_function = struct
  type value = {ssa_var : Variable.SSA.t; phi : Single_var_phi_function.t}
  type t = Single_var_phi_function.t Variable.Map.t

  let empty = Variable.Map.empty

  (* let create phi_reference *)

  let upsert (t : t) (variable : Variable.SSA.t) (location : Location.t) : t =
    Map.update t variable.name ~f:(function
        | None -> Single_var_phi_function.upsert Single_var_phi_function.empty variable location
        | Some phi_function -> Single_var_phi_function.upsert phi_function variable location)

  let find (t : t) (variable : Variable.t) : Single_var_phi_function.t option = Map.find t variable
  let variables (t : t) : Variable.t list = Map.keys t
end

module Phi_map = struct
  type t = Block_phi_function.t Block.Key.Map.t

  (* let empty = Variable.Map.empty

  let upsert (t : t) (variable : Variable.SSA.t) (location : Location.t) : t =
    Map.update t variable.name ~f:(function
        | None -> Single_var_phi_function.upsert Single_var_phi_function.empty variable location
        | Some phi_function -> Single_var_phi_function.upsert phi_function variable location)

  let find (t : t) (variable : Variable.t) : Single_var_phi_function.t option = Map.find t variable *)
end

module Counter_map = struct
  type t = int Variable.Map.t

  let increment (t : t) (variable : Variable.t) : t * int =
    let new_t =
      Map.update t variable ~f:(function
          | None -> 0
          | Some i -> i + 1)
    in
    let result = Map.find_exn new_t variable in
    (new_t, result)

  let create = Variable.Map.empty
end

type rename_scope = Variable.SSA.t With_loc.t Variable.Map.t

module Rename_state = struct
  type t = {
    counter_map : Counter_map.t;
    rename_scope : rename_scope;
  }

  let get_rename (rename_scope : rename_scope) (variable : Variable.t) =
    Map.find_exn rename_scope variable

  let redefine_definition
      { counter_map; rename_scope }
      ({ instruction = dest; label; instr_line } : Variable.t With_loc.t)
      : t * Variable.SSA.t
    =
    let (counter_map, counter_id) = Counter_map.increment counter_map dest in
    let new_ssa_variable = Variable.SSA.{ name = dest; counter_id } in
    let rename_scope =
      Map.set rename_scope ~key:dest ~data:{ instruction = new_ssa_variable; label; instr_line }
    in
    ({ counter_map; rename_scope }, new_ssa_variable)

  let process rename_state block_phi_function  = failwith "Bithc"

  let rename
      ({ rename_scope; _ } as t)
      ({ instruction; label; instr_line } as instr_with_loc : Regular.Instruction.normal With_loc.t)
      : t * Program.SSA.Instruction.normal
    =
    match instruction with
    | `Binary { dest; typ; op; arg1; arg2 } ->
      let renamed_arg1 = get_rename rename_scope arg1 in
      let renamed_arg2 = get_rename rename_scope arg2 in
      let (new_t, renamed_dest) =
        redefine_definition t @@ With_loc.reuse_location instr_with_loc dest
      in
      ( new_t,
        `Binary
          {
            dest = renamed_dest;
            typ;
            op;
            arg1 = renamed_arg1.instruction;
            arg2 = renamed_arg2.instruction;
          } )
    | `Unary { dest; typ; op; arg } ->
      let renamed_arg = get_rename rename_scope arg in
      let (new_t, renamed_dest) =
        redefine_definition t @@ With_loc.reuse_location instr_with_loc dest
      in
      (new_t, `Unary { dest = renamed_dest; typ; op; arg = renamed_arg.instruction })
    | `Nop -> (t, `Nop)
    | `Const { dest; value } ->
      let (new_t, dest) = redefine_definition t @@ With_loc.reuse_location instr_with_loc dest in
      (new_t, `Const { dest; value })
    | `Print args ->
      let updated_args =
        List.map args ~f:(get_rename rename_scope)
        |> List.map ~f:(fun with_loc -> with_loc.instruction)
      in
      (t, `Print updated_args)
    | `Call { func_name; args; dest } ->
      let updated_args =
        List.map args ~f:(get_rename rename_scope)
        |> List.map ~f:(fun with_loc -> with_loc.instruction)
      in
      (match dest with
      | None -> (t, `Call { func_name; args = updated_args; dest = None })
      | Some { dest; typ } ->
        let (new_t, new_dest) =
          redefine_definition t @@ With_loc.reuse_location instr_with_loc dest
        in
        (new_t, `Call { func_name; args = updated_args; dest = Some { dest = new_dest; typ } }))

  let rename_control ({ rename_scope; _ } as t) (instruction : Regular.Block.terminal_instr)
      : t * SSA.Block.terminal_instr
    =
    match instruction with
    | `Terminal -> (t, `Terminal)
    | `NextLabel label -> (t, `NextLabel label)
    | `Control (`Jmp label) -> (t, `Control (`Jmp label))
    | `Control (`Br { arg; true_label; false_label }) ->
      let renamed_arg = get_rename rename_scope arg in
      let new_br : Program.SSA.br = { arg = renamed_arg.instruction; true_label; false_label } in
      (t, `Control (`Br new_br))
    | `Control (`Ret var) ->
      Option.value_map
        var
        ~default:(t, `Control (`Ret None))
        ~f:(fun var ->
          let renamed_var = get_rename rename_scope var in
          (t, `Control (`Ret (Some renamed_var.instruction))))
end

let construct_phi_nodes
    (t : Program.Function.t)
    (dominance_frontier : Block.Key.Set.t Block.Key.Map.t)
    : Phi_references.t
  =
  Program.Function.variable_block_map t
  |> Map.fold ~init:Phi_references.empty ~f:(fun ~key:variable ~data:block_keys phi_references ->
         let phi_references =
           Set.fold block_keys ~init:phi_references ~f:(fun phi_references block_key ->
               let block_dominance_frontier = Map.find_exn dominance_frontier block_key in
               Set.fold
                 block_dominance_frontier
                 ~init:phi_references
                 ~f:(fun phi_references block_key ->
                   Phi_references.upsert phi_references variable block_key))
         in
         phi_references)

type t = {
  acc_block_map : Program.SSA.Block.t Block_name.Map.t;
  phis : Phi_map.t;
  rename_state : Rename_state.t;
}

(* TODO: There is a huge bug where  *)
let rename_block
    (block : Regular.Block.t)
    (block_phi_function : Block_phi_function.t)
    (rename_state : Rename_state.t)
    : Rename_state.t * Program.SSA.Block.t
  =
  let rename_state = Rename_state.process rename_state block_phi_function in
  let (new_rename_state, instructions) =
    List.fold_map
      (Regular.Block.all_normal_instrs_with_line block)
      ~init:rename_state
      ~f:(fun rename_state (instr_line, instruction) ->
        Rename_state.rename
          rename_state
          With_loc.{ instruction; instr_line; label = Regular.Block.key block })
  in
  let (new_rename_state, terminal_instr) =
    Rename_state.rename_control new_rename_state (Regular.Block.terminal block)
  in
  (new_rename_state, { label = block.label; instrs = instructions; terminal = terminal_instr })

(* Get all the variables that the block is demanding. Add them to the  *)
let update_phis_from_successors
    (func : Program.Regular.Function.t)
    (block_name : Block.Key.t)
    (phi_map : Phi_map.t)
    (rename_state : Rename_state.t) (* : Phis.t*)
  =
  let successors =
    Node_traverser.Poly.successors func.blocks block_name |> Option.to_list |> List.concat
  in
  List.fold successors ~init:phi_map ~f:(fun phi_map successor_block ->
      let block_phi_function =
        Map.find phi_map (Regular.Block.key successor_block)
        |> Option.value ~default:Block_phi_function.empty
      in
      let variables_to_gather = Block_phi_function.variables block_phi_function in
      List.fold variables_to_gather ~init:phi_map ~f:(fun phi_map variable ->
          let With_loc.{ instruction = ssa_variable; label; instr_line } =
            Rename_state.get_rename rename_state.rename_scope variable
          in
          let phi =
            Block_phi_function.upsert
              block_phi_function
              ssa_variable
              Location.{ block_name = label; line = instr_line }
          in
          Map.set phi_map ~key:(Regular.Block.key successor_block) ~data:phi))

let rec rename
    (func : Program.Regular.Function.t)
    (dominator_tree : Block_name.Set.t Block_name.Map.t)
    block_name
    { acc_block_map; phis; rename_state }
    : t
  =
  let phi = Map.find phis block_name |> Option.value ~default:Block_phi_function.empty in
  let (updated_rename_state, ssa_block) =
    rename_block (Node_traverser.Poly.find_exn func.blocks block_name) phi rename_state
  in
  let updated_phis = update_phis_from_successors func block_name phis rename_state in
  let updated_acc_block_map = Map.set acc_block_map ~key:block_name ~data:ssa_block in
  let updated_t =
    {
      acc_block_map = updated_acc_block_map;
      phis = updated_phis;
      rename_state = updated_rename_state;
    }
  in
  let immediate_dominators =
    Map.find dominator_tree block_name |> Option.value ~default:Block_name.Set.empty
  in
  let new_t =
    Block_name.Set.fold immediate_dominators ~init:updated_t ~f:(fun t block_name ->
        rename func dominator_tree block_name t)
  in
  { new_t with rename_state }
