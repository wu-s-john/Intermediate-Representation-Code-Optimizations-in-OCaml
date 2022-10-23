open Core
(* You can obtain the def-use chain easily after running the worklist algorithm.
   
Namely, you go through each block from it's top and see if a definition is used for an instruction *)

module type Renderable_key = sig
  type t [@@deriving sexp]

  include Node.Key with type t := t
  include Renderable.S with type t := t
end

module Make
    (Block : Program.Monomorphic_block_intf)
    (Key_render : Renderable_key with type t = Block.Key.t) =
struct
  module Reaching_def_ops = struct
    type data = Var_def_map.t

    (* First kill the existing variable in the map. Then add it *)
    let set_definition (var_def_map : data) (def : With_loc.Def.t) =
      let declared_variable = With_loc.Def.declared_variable def in
      let removed_def_map = Var_def_map.remove var_def_map declared_variable in
      Var_def_map.upsert removed_def_map declared_variable def

    let zero _ = Var_def_map.empty

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

    let render_key (key : Key.t) = Key_render.render key

    let render_defs (map : Var_def_map.t) =
      Map.data map
      |> List.map ~f:Set.to_list
      |> List.concat
      |> List.map
           ~f:(fun ({ With_loc.instruction = _; label; instr_line } as instruction_with_loc) ->
             sprintf
               "%s-%d: %s"
               (Option.value ~default:"ROOT" label)
               instr_line
               (Program.Instruction.to_string
                  (With_loc.Def.to_program_instruction instruction_with_loc)))
      |> String.concat ~sep:"\\n"

    let render_node ({ out; in_; _ } : t) : string =
      Core.sprintf "In:\\n%s\\n==============\\nOut:\\n%s" (render_defs in_) (render_defs out)

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
end

module Test = struct
  open Async
  (* We would like to test the correctness of the Def-Use Chain. So we will use two examples from online contents.
    The first test will run the Reaching Definition Algorithm and then we compare the output.
    
    Afterwards, we run the use def chain algorithm and compare the output.
  *)

  module Reaching_def = Make (Program.Block_unit) (Program.Block.Key)

  let graphviz =
    let render_key = Program.Block.Key.render in
    let render_node = Program.Block.render in
    let get_key = Program.Block.get_key in
    Graphviz.create ~render_key ~render_node ~get_key

  let%test_unit "Reaching Definitions work for one example" =
    let open Deferred.Let_syntax in
    let filename : string = "/Users/johnwu/code/bril/test/worklist/reaching_def.bril" in
    Thread_safe.block_on_async_exn (fun () ->
        let%bind deserialized_program = Test_util.read_bril_exn ~filename in
        let program : Program.t = deserialized_program in
        let node_traverser = Program.head_function_blocks_exn program in
        let reaching_def_graph = Reaching_def.run node_traverser in
        Graphviz.draw
          "/Users/johnwu/code/bril/reaching_def.dot"
          reaching_def_graph
          Reaching_def.Reaching_def_block.graphviz)

  let%test_unit "Reaching Definitions work for euclidian example" =
    let open Deferred.Let_syntax in
    let filename : string = "/Users/johnwu/code/bril/benchmarks/euclid.bril" in
    Thread_safe.block_on_async_exn (fun () ->
        let%bind deserialized_program = Test_util.read_bril_exn ~filename in
        let program : Program.t = deserialized_program in
        let node_traverser = Program.select_block_exn program "gcd" in
        let reaching_def_graph = Reaching_def.run node_traverser in
        let%bind () =
          Graphviz.draw "/Users/johnwu/code/bril/gcd_output.dot" node_traverser graphviz
        in
        Graphviz.draw
          "/Users/johnwu/code/bril/gcd_reachng_def.dot"
          reaching_def_graph
          Reaching_def.Reaching_def_block.graphviz)
end