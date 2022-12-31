open Core

(* Helps map values to certain values from their location index to their map values *)

type 'value location = {
  (* What is the computation value before entering the string *)
  before_block : 'value option;
  (* What is the computation after running line [INDEX] *)
  locs : (Program.Instruction.t * 'value) Int.Map.t;
}

type 'value t = 'value location Label.Map.t

let empty = Label.Map.empty

let set
    (map : 'value t)
    label
    (loc : [ `Before | `Index of int * Program.Instruction.t ])
    (value : 'value)
  =
  let old_location =
    match Label.Map.find map label with
    | Some location -> location
    | None -> { before_block = None; locs = Int.Map.empty }
  in
  let location =
    match loc with
    | `Before -> { old_location with before_block = Some value }
    | `Index (index, instr) ->
      { old_location with locs = Int.Map.set old_location.locs ~key:index ~data:(instr, value) }
  in
  Label.Map.set map ~key:label ~data:location

let find (map : 'value t) label index =
  Label.Map.find map label
  |> Option.bind ~f:(fun location ->
         match index with
         | `Before -> location.before_block
         | `Index index -> Option.map (Int.Map.find location.locs index) ~f:snd)

let get_key_value_list (map : 'value t) =
  Label.Map.to_alist map
  |> List.concat_map ~f:(fun (label, location) ->
         let before_block =
           match location.before_block with
           | Some value -> [ (label, `Before, value) ]
           | None -> []
         in
         let locs =
           Int.Map.to_alist location.locs
           |> List.map ~f:(fun (index, (instr, value)) -> (label, `Index (index, instr), value))
         in
         before_block @ locs)
