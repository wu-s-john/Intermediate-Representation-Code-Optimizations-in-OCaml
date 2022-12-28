open Async

module Test = struct
  let test_path = "/Users/johnwu/code/bril/test/print"

  let graphviz =
    let render_key = Program.Block.Key.render in
    let render_node = Program.Block.render in
    let get_key = Program.Block.key in
    Graphviz.create ~render_key ~render_node ~get_key

  let%test_unit "serialize and deserialize value instruction for reaching def" =
    let open Deferred.Let_syntax in
    let filename : string = "/Users/johnwu/code/bril/test/worklist/reaching_def.bril" in
    Thread_safe.block_on_async_exn (fun () ->
        let%bind deserialized_program = Test_util.read_bril_exn ~filename in
        let program : Program.t = deserialized_program in
        let node_traverser = Program.head_function_blocks_exn program in
        Graphviz.draw
          "/Users/johnwu/code/bril/should_graph_simple_program.dot"
          node_traverser
          graphviz)

  (* let%test_unit "serialize and deserialize value instruction for fib" =
    let open Deferred.Let_syntax in
    let filename : string = "/Users/johnwu/code/bril/test/interp/fib.bril" in
    Thread_safe.block_on_async_exn (fun () ->
        let%bind deserialized_program = Test_util.read_bril_exn ~filename in
        let program : Program.t = deserialized_program in
        let node_traverser = Program.head_function_blocks_exn program in
        Graphviz.draw
          "/Users/johnwu/code/bril/fib.dot"
          node_traverser
          graphviz) *)
end