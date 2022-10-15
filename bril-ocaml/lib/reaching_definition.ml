open Core
(* You can obtain the def-use chain easily after running the worklist algorithm.
   
Namely, you go through each block from it's top and see if a definition is used for an instruction *)

module Make (Block : Program.Monomorphic_block_intf) = struct
  module Reaching_def_ops = struct
    type data = Var_def_map.t

    (* First kill the existing variable in the map. Then add it *)
    let set_definition (var_def_map : data) (def : With_loc.Def.t) =
      let declared_variable = With_loc.Def.declared_variable def in
      let removed_def_map = Var_def_map.remove var_def_map declared_variable in
      Var_def_map.upsert removed_def_map declared_variable def

    let zero = Var_def_map.empty

    let merge (pred_outputs : data list) : data =
      List.fold_left pred_outputs ~init:Var_def_map.empty ~f:Var_def_map.merge

    let equal_data = Var_def_map.equal

    let process_instruction def_map label instr_line instr =
      let def = With_loc.Def.create instr in
      Option.value_map def ~default:def_map ~f:(fun def ->
          set_definition def_map { instruction = def; label; instr_line })

    let transform (block : Block.t) (definitions : data) : data =
      let label = block.label in
      List.foldi
        (Program.Block.all_instrs block)
        ~init:definitions
        ~f:(fun instr_line def_map instr -> process_instruction def_map label instr_line instr)
  end

  module Reaching_definition_runner = Worklist.Make (Block) (Reaching_def_ops)

  module Reaching_def_block = struct
    type t = (Block.t, Var_def_map.t) Worklist.flow_node

    module Key = Block.Key

    let hi = [%show: string option]
    let render_key (key : Key.t) = Block.Key.sexp_of_t key |> Sexp.to_string

    let render_node ({ out; in_; _ } : t) : string =
      Core.sprintf
        "In:\n%s\n==============\nOut:\n%s"
        (Var_def_map.to_yojson in_ |> Yojson.Safe.to_string)
        (Var_def_map.to_yojson out |> Yojson.Safe.to_string)

    let get_key ({ node = block; _ } : t) : Key.t = Block.get_key block
    let graphviz = Graphviz.create ~render_key ~render_node ~get_key
  end

  type t = (Block.Key.t, (Block.t, Var_def_map.t) Worklist.flow_node) Node_traverser.Poly.t

  let get_key (node : (Block.t, Var_def_map.t) Worklist.flow_node) : Block.Key.t =
    Block.get_key node.node

  let run (traverser : (Block.Key.t, Block.t) Node_traverser.Poly.t) : t =
    Reaching_definition_runner.run_forward traverser

  (* 
  For all the defintions defined before the instruction, find all the defs that are used in the instruction.
  Then, add the relationship to the map.   
  
  *)

  let create_def_use_map (traverser : (Block.Key.t, Block.t) Node_traverser.Poly.t) =
    let def_use_map = Def_use_map.empty in
    let reaching_defs = run traverser in
    let def_use_map =
      Node_traverser.Poly.fold
        reaching_defs
        ~init:def_use_map
        ~f:(fun def_use_map { node = block; in_ = reaching_defs; out = _ } ->
          List.foldi
            (Program.Block.all_instrs block)
            ~init:(def_use_map, reaching_defs)
            ~f:(fun instr_line (def_use_map, reaching_defs) instr ->
              (* Find all variables used for the instr *)
              let used_variables = Program.Instruction.used_vars instr |> Set.to_list in
              (* Find all definitions that are used in the instruction *)
              let defs =
                List.bind used_variables ~f:(fun var ->
                    Var_def_map.get reaching_defs var |> Set.to_list)
              in
              (* Update reaching def *)
              let new_reaching_def =
                Reaching_def_ops.process_instruction reaching_defs block.label instr_line instr
              in
              (* Add the relationship to the map *)
              let new_def_use_map =
                List.fold_left defs ~init:def_use_map ~f:(fun def_use_map def ->
                    Def_use_map.upsert
                      def_use_map
                      def
                      { instruction = instr; label = block.label; instr_line })
              in
              (new_def_use_map, new_reaching_def))
          |> fst)
    in
    def_use_map
end

module Test = struct
  open Async
  (* We would like to test the correctness of the Def-Use Chain. So we will use two examples from online contents.
    The first test will run the Reaching Definition Algorithm and then we compare the output.
    
    Afterwards, we run the use def chain algorithm and compare the output.
  *)

  module Reaching_def = Make (Program.Block_unit)

  let%test_unit "Reaching Definitions work for one example" =
    let open Deferred.Let_syntax in
    let filename : string = "/Users/johnwu/code/bril/test/worklist/reaching_def.bril" in
    Thread_safe.block_on_async_exn (fun () ->
        let%bind deserialized_program = Test_util.read_bril_exn ~filename in
        let program : Program.t = deserialized_program in
        let node_traverser = Program.head_function_blocks_exn program in
        let reaching_def_graph = Reaching_def.run node_traverser in
        Graphviz.draw "/Users/johnwu/code/bril/reaching_def.dot" reaching_def_graph Reaching_def.Reaching_def_block.graphviz)
end