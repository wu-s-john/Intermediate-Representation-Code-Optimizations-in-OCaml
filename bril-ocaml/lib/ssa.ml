open Program
open Core

type phi_references = Block.Key.Set.t String.Map.t

module Variable = String

module Phi_references = struct
  type t = Block.Key.Set.t Variable.Map.t [@@deriving sexp]

  let empty = String.Map.empty

  let upsert (t : t) (variable : string) (key : Block.Key.t) : [ `Updated of t | `Not_updated ] =
    match Map.find t variable with
    | None -> `Updated (Map.add_exn t ~key:variable ~data:(Block.Key.Set.singleton key))
    | Some referenced_predecessors ->
      if Set.mem referenced_predecessors key then `Not_updated
      else `Updated (Map.set t ~key:variable ~data:(Set.add referenced_predecessors key))
end

module Phi_placing_block = struct
  include Program.Block_util

  type meta = {
    defined_variables : Variable.Set.t;
    domaniance_frontier : Block.Key.Set.t;
    references : Phi_references.t;
  }
  [@@deriving sexp]

  type t = meta Block.t [@@deriving sexp]

  let create ~(dominator_frontier : (Block.Key.t, Block.Key.Set.t) Hashtbl.t) (block : unit Block.t)
      : t
    =
    Block.map block ~f:(fun () ->
        let defined_variables = Block.variable_defintions block in
        {
          defined_variables;
          domaniance_frontier = Hashtbl.find_exn dominator_frontier block.label;
          references = String.Map.empty;
        })

  let upsert_to_phi (t : t) (variable : Variable.t) (block_key : Block.Key.t)
      : [ `Updated of t | `Not_updated ]
    =
    match Phi_references.upsert t.meta.references variable block_key with
    | `Not_updated -> `Not_updated
    | `Updated new_references ->
      `Updated (Block.map t ~f:(fun meta -> { meta with references = new_references }))

  let is_defined (t : t) (variable : Variable.t) : bool = Map.mem t.meta.references variable
  let domaniance_frontier (t : t) = t.meta.domaniance_frontier
end

(* For a given variable, lookup the references of a block *)

type 'a t = (Block.Key.t, 'a Block.t) Hashtbl.t

let compute_variable_defintion_to_block_mapping
    (block_graph : (Block.Key.t, 'a Block.t) Node_traverser.Poly.t)
    : (Variable.t, Block.Key.Set.t) Hashtbl.t
  =
  List.bind (Node_traverser.Poly.nodes block_graph) ~f:(fun block ->
      let variables = Set.to_list (Block.variable_defintions block) in
      List.map variables ~f:(fun variable -> (variable, Block.get_key block)))
  |> Variable.Table.of_alist_multi
  |> Hashtbl.map ~f:Block.Key.Set.of_list

let variables (blocks : (Block.Key.t, Block_unit.t) Node_traverser.Poly.t) : String.Set.t =
  let result =
    List.map (Node_traverser.Poly.nodes blocks) ~f:(fun block ->
        Set.union (Block.variable_defintions block) (Block.used_variables block))
  in
  String.Set.union_list result

let add_phi_function
    (t : (Block.Key.t, Block_unit.t) Node_traverser.Poly.t)
    (dominator_frontier : (Block.Key.t, Block.Key.Set.t) Hashtbl.t)
    : (Block.Key.t, Phi_placing_block.t) Node_traverser.Poly.t
  =
  let variable_definition_to_block_mapping = compute_variable_defintion_to_block_mapping t in
  let variables = variables t in
  let block_map_with_phi : (Block.Key.t, Phi_placing_block.t) Node_traverser.Poly.t =
    Node_traverser.Poly.map
      (module Phi_placing_block)
      t
      ~f:(fun block ->
        Block.map block ~f:(fun () ->
            let defined_variables = Block.variable_defintions block in
            Phi_placing_block.
              {
                defined_variables;
                domaniance_frontier = Hashtbl.find_exn dominator_frontier block.label;
                references = String.Map.empty;
              }))
  in
  let rec go (block_work_queue : Phi_placing_block.t Block.Key.Hash_queue.t) (variable : string) =
    Option.iter (Hash_queue.dequeue_back_with_key block_work_queue) ~f:(fun (block_key, block) ->
        Set.iter block.meta.domaniance_frontier ~f:(fun successor_block_key ->
            let sucessor_block : Phi_placing_block.t =
              Node_traverser.Poly.find_exn block_map_with_phi successor_block_key
            in
            match Phi_placing_block.upsert_to_phi sucessor_block variable block_key with
            | `Updated new_successor_block ->
              let (_ : [ `No_such_key | `Ok ]) =
                Hash_queue.replace block_work_queue successor_block_key new_successor_block
              in
              Node_traverser.Poly.update block_map_with_phi sucessor_block;
              if Phi_placing_block.is_defined sucessor_block variable then
                let (_ : [ `Key_already_present | `Ok ]) =
                  Hash_queue.enqueue_back block_work_queue successor_block_key new_successor_block
                in
                ()
            | `Not_updated -> ());
        go block_work_queue variable)
  in
  Set.iter variables ~f:(fun variable ->
      let block_work_queue = Block.Key.Hash_queue.create () in
      Set.iter
        (Option.value
           (Hashtbl.find variable_definition_to_block_mapping variable)
           ~default:Block.Key.Set.empty)
        ~f:(fun block ->
          Hash_queue.enqueue_back_exn
            block_work_queue
            block
            (Node_traverser.Poly.find_exn block_map_with_phi block));
      go block_work_queue variable);
  block_map_with_phi

module Counter_map = struct
  type t = int Variable.Table.t

  let increment (t : t) (variable : Variable.t) : int =
    let result =
      Hashtbl.find_and_call t variable ~if_found:(fun i -> i + 1) ~if_not_found:(fun _ -> 0)
    in
    Hashtbl.update t variable ~f:(fun _ -> result);
    result

  let create () = Variable.Table.create ()
end

type rename_scope = Variable.t Variable.Map.t

module Rename_state = struct
  type t = {
    counter_map : Counter_map.t;
    rename_scope : rename_scope;
  }

  (* TODO: maybe throw an error *)
  let get_rename (rename_scope : rename_scope) (variable : Variable.t) =
    Map.find_exn rename_scope variable

  let redefine_definition (counter_map : Counter_map.t) (dest : Variable.t) : Variable.t =
    let new_dest_value = Counter_map.increment counter_map dest in
    sprintf !"%s.%i" dest new_dest_value

  let rename
      ~(counter_map : Counter_map.t)
      (rename_scope : rename_scope)
      (instruction : Instruction.normal)
      : Instruction.normal * rename_scope
    =
    match instruction with
    | `Binary { dest; typ; op; arg1; arg2 } ->
      let renamed_arg1 = get_rename rename_scope arg1 in
      let renamed_arg2 = get_rename rename_scope arg2 in
      let renamed_dest = redefine_definition counter_map dest in
      let new_rename_scope = Map.set rename_scope ~key:dest ~data:renamed_dest in
      (`Binary { dest; typ; op; arg1 = renamed_arg1; arg2 = renamed_arg2 }, new_rename_scope)
    | `Unary { dest; typ; op; arg } ->
      let renamed_arg = get_rename rename_scope arg in
      let renamed_dest = redefine_definition counter_map dest in
      let new_rename_scope = Map.set rename_scope ~key:dest ~data:renamed_dest in
      (`Unary { dest = renamed_dest; typ; op; arg = renamed_arg }, new_rename_scope)
    | (`Nop | `Const _) as instr -> (instr, rename_scope)
    | `Print args ->
      let updated_args = List.map args ~f:(get_rename rename_scope) in
      (`Print updated_args, rename_scope)
    | `Call { func_name; args; dest } ->
      let updated_args = List.map args ~f:(get_rename rename_scope) in
      ( match dest with
      | None -> (`Call { func_name; args = updated_args; dest = None }, rename_scope)
      | Some { dest; typ } ->
        let new_dest = redefine_definition counter_map dest in
        ( `Call { func_name; args = updated_args; dest = Some { dest = new_dest; typ } },
          rename_scope ) )
end

module Rename_block = struct
  include Program.Block_util

  module Arg_data = struct
    module T = struct
      type t = {
        renamed_variable : Variable.t;
        predecessor_label : Block.Key.t;
      }
      [@@deriving compare, sexp, hash]

      let compare =
        Comparable.lift Block.Key.compare ~f:(fun { predecessor_label; _ } -> predecessor_label)
    end

    include T
    include Comparable.Make (T)

    let create (renamed_variable : Variable.t) (predecessor_label : Block.Key.t) : t =
      { renamed_variable; predecessor_label }
  end

  module Phi_function = struct
    type out = {
      referenced_predecessors : Block.Key.Set.t;
      args : Arg_data.Set.t;
    }
    [@@deriving sexp]

    type t = out Variable.Map.t [@@deriving sexp]

    let update (t : t) (rename_scope : rename_scope) (predecessor_label : Block.Key.t) : t =
      Map.mapi t ~f:(fun ~key:variable ~data:phi_function ->
          if Set.mem phi_function.referenced_predecessors predecessor_label then
            let new_args =
              Set.add phi_function.args
              @@ Arg_data.create (Map.find_exn rename_scope variable) predecessor_label
            in
            { referenced_predecessors = phi_function.referenced_predecessors; args = new_args }
          else phi_function)
  end

  type meta = Phi_function.t
  type t = Phi_function.t Block.t [@@deriving sexp]

  let update_phi_args (t : t) (rename_scope : rename_scope) (predecessor_label : Block.Key.t) : t =
    { t with meta = Phi_function.update t.meta rename_scope predecessor_label }
end

let rec rename_block
    ~(counter_map : Counter_map.t)
    ~(rename_scope : rename_scope)
    ~(dominator_tree : (Block.Key.t, Block.Key.t) Node_traverser.Poly.t)
    (traverser : (Block.Key.t, Rename_block.t) Node_traverser.Poly.t)
    (block : Rename_block.t)
    : unit
  =
  let (updated_rename_scope, updated_instuctions) =
    List.fold_map block.instrs ~init:rename_scope ~f:(fun rename_scope instr ->
        let (instr, new_rename_scope) = Rename_state.rename ~counter_map rename_scope instr in
        (new_rename_scope, instr))
  in
  Node_traverser.Poly.update traverser { block with instrs = updated_instuctions };
  let successors =
    List.concat
    @@ Option.to_list
    @@ Node_traverser.Poly.successors traverser (Rename_block.get_key block)
  in
  List.iter successors ~f:(fun successor_block ->
      let new_successor_block =
        Rename_block.update_phi_args successor_block updated_rename_scope block.label
      in
      Node_traverser.Poly.update traverser new_successor_block);
  let successors =
    List.concat
    @@ Option.to_list
    @@ Node_traverser.Poly.successors dominator_tree (Rename_block.get_key block)
  in
  List.iter successors ~f:(fun subsequent_dominator ->
      rename_block
        ~counter_map
        ~rename_scope
        ~dominator_tree
        traverser
        (Node_traverser.Poly.find_exn traverser subsequent_dominator))

let rename
    ~(dominator_tree : (Block.Key.t, Block.Key.t) Node_traverser.Poly.t)
    (phi_placing_traverser : (Block.Key.t, Phi_placing_block.t) Node_traverser.Poly.t)
    : (Block.Key.t, Rename_block.t) Node_traverser.Poly.t
  =
  let counter_map = Counter_map.create () in
  let rename_scope = Variable.Map.empty in
  let traverser =
    Node_traverser.Poly.map
      (module Phi_placing_block)
      phi_placing_traverser
      ~f:(fun block ->
        let phi_references = block.meta.references in
        let phi_function =
          Map.map phi_references ~f:(fun references ->
              Rename_block.Phi_function.
                { referenced_predecessors = references; args = Rename_block.Arg_data.Set.empty })
        in
        { block with meta = phi_function })
  in
  rename_block
    ~counter_map
    ~rename_scope
    ~dominator_tree
    traverser
    (Node_traverser.Poly.root traverser);
  traverser
