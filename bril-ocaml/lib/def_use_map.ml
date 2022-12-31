open Core
include Multi_map_set.Make (With_loc.Def) (With_loc.Instr)

let create (map : Var_def_map.t Instruction_location_map.t) : t =
  Instruction_location_map.get_key_value_list map
  |> List.fold_left ~init:empty ~f:(fun map (label, loc, defs) ->
         match loc with
         | `Before -> map
         | `Index (instr_line, instruction) ->
           let defs = Var_def_map.to_list defs in
           List.fold_left defs ~init:map ~f:(fun map def ->
               upsert map def { label; instr_line; instruction }))
