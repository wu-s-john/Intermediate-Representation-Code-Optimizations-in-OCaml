open Core
open Async
open Program

let rec dead_code_elimination (block : 'a Block.t) : 'a Block.t =
  (* last_def are basically definitions that have been recently made, but never used *)
  let (initial_dead_indices, unused_defs) =
    List.foldi
      block.instrs
      ~init:([], String.Map.empty)
      ~f:(fun index (acc_dead_indices, unused_variables) instr ->
        let used_variables =
          Instruction.used_vars (instr :> [ Instruction.normal | Instruction.control ])
        in
        let new_unused_variables = Set.fold used_variables ~init:unused_variables ~f:Map.remove in
        match Instruction.dest instr with
        | None -> (acc_dead_indices, new_unused_variables)
        | Some variable_name ->
          let dead_index = Map.find new_unused_variables variable_name in
          let updated_last_def =
            Map.update new_unused_variables variable_name ~f:(fun _ -> index)
          in
          let updated_acc_dead_indices =
            Option.value_map dead_index ~default:acc_dead_indices ~f:(fun last_index ->
                last_index :: acc_dead_indices)
          in
          (updated_acc_dead_indices, updated_last_def))
    |> fun (acc_dead_indices, unused_variables) ->
    let used_terminal_instruction_var =
      match block.terminal with
      | `Control instr ->
        Instruction.used_vars (instr :> [ Instruction.normal | Instruction.control ])
      | _ -> String.Set.empty
    in
    ( acc_dead_indices,
      Map.filter_keys unused_variables ~f:(fun name ->
          not @@ Set.mem used_terminal_instruction_var name) )
  in
  let dead_indices = List.append initial_dead_indices (Map.data unused_defs) in
  printf !"Dead indices %{sexp:int list}\n" dead_indices;
  if List.is_empty dead_indices then block
  else
    let dead_indices = Int.Set.of_list dead_indices in
    let new_instrs = List.filteri block.instrs ~f:(fun i _ -> not @@ Set.mem dead_indices i) in
    let new_block = { block with instrs = new_instrs } in
    dead_code_elimination new_block

let nop_digest = Md5.digest_string "nop"

let compute_dest (instr : Instruction.normal) : string option =
  match instr with
  | `Binary instr -> Some instr.dest
  | `Unary instr -> Some instr.dest
  | `Const instr -> Some instr.dest
  | `Nop -> None
  | `Print _ -> None
  | `Call instr -> Option.map instr.dest ~f:(fun dest -> dest.dest)

module LVN_container = struct
  module Normalized_expression = struct
    module T = struct
      type t =
        [ `Int of int
        | `Bool of bool
        | `Not of string
        | `Binary of string * Op.Binary.t * string
        ]
      [@@deriving hash, compare, sexp]
    end

    include T
    include Hashable.Make (T)
  end

  type t = {
    optimized_instructions : Program.Instruction.normal Queue.t;
    expr_var_map : (Normalized_expression.t, string) Hashtbl.t;
    var_expr_map :
      (string, [ `Reference of string | `Expression of Normalized_expression.t ]) Hashtbl.t;
  }

  let rec compute_normalized_expr (t : t) (variable : string)
      : [ Normalized_expression.t | `Undefined_variable of string ]
    =
    match Hashtbl.find t.var_expr_map variable with
    | None -> `Undefined_variable variable
    | Some (`Reference referenced_variable) -> compute_normalized_expr t referenced_variable
    | Some (`Expression expression) ->
      (expression :> [ Normalized_expression.t | `Undefined_variable of string ])

  let run_int_op (op : [ `Add | `Mul | `Sub | `Div ]) (value1 : int) (value2 : int) : int =
    match op with
    | `Add -> value1 + value2
    | `Mul -> value1 * value2
    | `Sub -> value1 - value2
    | `Div -> value1 / value2

  let run_bool_op_bool (op : [ `Eq | `And | `Or ]) (value1 : bool) (value2 : bool) : bool =
    match op with
    | `Eq -> Bool.equal value1 value2
    | `And -> value1 && value2
    | `Or -> value1 || value2

  let run_bool_op_int (op : [ `Eq | `Lt | `Gt | `Le | `Ge ]) (value1 : int) (value2 : int) : bool =
    match op with
    | `Eq -> Int.equal value1 value2
    | `Lt -> value1 <= value2
    | `Gt -> value1 >= value2
    | `Le -> value1 < value2
    | `Ge -> value1 > value2

  type variable_reference = {
    original : string;
    dest : string;
  }

  type normalized_computation =
    | Expr of {
        expression : Normalized_expression.t;
        variable : string;
      }
    | Delete of variable_reference option
    | Unable_to_optimize of Instruction.normal

  let to_normalized_expression (optimized_instruction : Instruction.normal) : normalized_computation
    =
    match optimized_instruction with
    | `Binary { dest; typ = _; op; arg1; arg2 } ->
      Expr { expression = `Binary (arg1, op, arg2); variable = dest }
    | `Unary { dest; typ = _; op; arg } ->
      ( match op with
      | `Not -> Expr { expression = `Not arg; variable = dest }
      | `Id -> Delete (Some { original = arg; dest }) )
    | `Nop -> Delete None
    | `Const { dest; value } ->
      ( match value with
      | `Int value -> Expr { expression = `Int value; variable = dest }
      | `Bool value -> Expr { expression = `Bool value; variable = dest } )
    | (`Call _ | `Print _) as instr -> Unable_to_optimize instr

  let remove_reference_in_expression (t : t) (variable : string) : unit =
    Hashtbl.filter_keys_inplace t.expr_var_map ~f:(function
        | `Binary (arg1, _, arg2) -> List.mem ~equal:String.equal [ arg1; arg2 ] variable
        | `Not arg -> String.equal arg variable
        | `Bool _
        | `Int _ ->
          false)

  let rec get_original_variable (t : t) (var : string) : string =
    match Hashtbl.find t.var_expr_map var with
    | Some (`Reference variable_name) -> get_original_variable t variable_name
    | Some (`Expression _) -> var
    | None -> var

  type lol =
    [ Normalized_expression.t
    | `Undefined_variable of string
    ]
  [@@deriving sexp]

  let add_var_expr
      (t : t)
      (var : string)
      (data : [ `Expression of Normalized_expression.t | `Reference of string ])
      : unit
    =
    if Hashtbl.mem t.var_expr_map var then (
      (* The variable is redefined at this point if it already exists in this var_expr_map. So, remove all current references to it *)
      printf !"\nPrinting variable that has already been added";
      Hashtbl.remove t.var_expr_map var;
      remove_reference_in_expression t var;
      Hashtbl.add_exn t.var_expr_map ~key:var ~data )
    else Hashtbl.add_exn t.var_expr_map ~key:var ~data;
    match data with
    | `Expression expression -> Hashtbl.add_exn t.expr_var_map ~key:expression ~data:var
    | `Reference _ -> ()

  type int_int_op =
    [ `Add
    | `Mul
    | `Sub
    | `Div
    ]

  type int_bool_op =
    [ `Eq
    | `Lt
    | `Gt
    | `Le
    | `Ge
    ]

  let process_instruction (t : t) (instr : Program.Instruction.normal) : unit =
    let optimized_instruction : Instruction.normal =
      match instr with
      | `Unary { op; dest; typ; arg } ->
        let arg = get_original_variable t arg in
        ( match op with
        | `Not ->
          ( match compute_normalized_expr t arg with
          | `Bool value -> `Const { dest; value = `Bool (not value) }
          | _ -> `Unary { op; dest; typ; arg } )
        | `Id -> `Unary { op; dest; typ; arg } )
      | `Const { dest; value } -> `Const { dest; value }
      | `Binary { dest; typ; op; arg1; arg2 } as binary_instruction ->
        let arg1 = get_original_variable t arg1 in
        let arg2 = get_original_variable t arg2 in
        let normalized_arg1 = compute_normalized_expr t arg1 in
        let normalized_arg2 = compute_normalized_expr t arg2 in
        printf !"\nBinary Instruction processing: %{sexp:Instruction.normal}" binary_instruction;
        printf
          !"\nArg1:%s;Arg2:%s;expr1:%{sexp:lol};expr2:%{sexp:lol}"
          arg1
          arg2
          normalized_arg1
          normalized_arg2;
        ( match (normalized_arg1, normalized_arg2, op) with
        | (`Int value1, `Int value2, `Add) -> `Const { dest; value = `Int (value1 + value2) }
        | (`Int value1, `Int value2, `Sub) -> `Const { dest; value = `Int (value1 - value2) }
        | (`Int value1, `Int value2, `Mul) -> `Const { dest; value = `Int (value1 * value2) }
        | (`Int value1, `Int value2, `Div) -> `Const { dest; value = `Int (value1 / value2) }
        | (`Int value1, `Int value2, `Eq) ->
          `Const { dest; value = `Bool (Int.equal value1 value2) }
        | (`Int value1, `Int value2, `Gt) -> `Const { dest; value = `Bool (value1 > value2) }
        | (`Int value1, `Int value2, `Lt) -> `Const { dest; value = `Bool (value1 < value2) }
        | (`Int value1, `Int value2, `Ge) -> `Const { dest; value = `Bool (value1 >= value2) }
        | (`Int value1, `Int value2, `Le) -> `Const { dest; value = `Bool (value1 <= value2) }
        | (`Bool value1, `Bool value2, `Eq) ->
          `Const { dest; value = `Bool (Bool.equal value1 value2) }
        | (`Bool value1, `Bool value2, `And) -> `Const { dest; value = `Bool (value1 && value2) }
        | (`Bool value1, `Bool value2, `Or) -> `Const { dest; value = `Bool (value1 || value2) }
        | (`Bool false, _, `And)
        | (_, `Bool false, `And) ->
          `Const { dest; value = `Bool false }
        | (`Bool true, _, `Or)
        | (_, `Bool true, `Or) ->
          `Const { dest; value = `Bool true }
        | (_, _, `Eq)
        | (_, _, `And)
        | (_, _, `Or)
        | (_, _, `Add)
        | (_, _, `Mul) ->
          `Binary { dest; typ; op; arg1 = String.min arg1 arg2; arg2 = String.max arg1 arg2 }
        | (_, _, _) -> `Binary { dest; typ; op; arg1; arg2 } )
      | (`Nop | `Print _ | `Call _) as instr -> instr
    in
    printf !"\nOptimized Instruction %{sexp:Instruction.normal}" optimized_instruction;
    match to_normalized_expression optimized_instruction with
    | Expr { expression; variable } ->
      ( match Hashtbl.find t.expr_var_map expression with
      | None ->
        add_var_expr t variable (`Expression expression);
        printf !"\nAdding instruction %{sexp:Instruction.normal}" optimized_instruction;
        Queue.enqueue t.optimized_instructions optimized_instruction
      | Some original_name -> add_var_expr t variable (`Reference original_name) )
    | Delete (Some { original; dest }) ->
      Hashtbl.update t.var_expr_map dest ~f:(fun _ -> `Reference original)
    | Delete None -> ()
    | Unable_to_optimize instr -> Queue.enqueue t.optimized_instructions instr

  let get_optimized_instructions (t : t) : Program.Instruction.normal list =
    Queue.to_list t.optimized_instructions

  let optimize_branch (t : t) ({ arg; true_label; false_label } : br) : string option =
    match compute_normalized_expr t arg with
    | `Bool value -> Some (if value then true_label else false_label)
    | _ -> None

  let create () : t =
    {
      optimized_instructions = Queue.create ();
      expr_var_map = Normalized_expression.Table.create ();
      var_expr_map = String.Table.create ();
    }
end

let local_value_numbering ({ meta; label; instrs; terminal } : 'a Block.t) : 'a Block.t =
  let lvn_container = LVN_container.create () in
  printf !"\nInstructions: %{sexp:Instruction.normal list}" instrs;
  List.iter instrs ~f:(LVN_container.process_instruction lvn_container);
  let optimized_terminal =
    match terminal with
    | `Control (`Br instr) ->
      Option.value_map
        (LVN_container.optimize_branch lvn_container instr)
        ~f:(fun jmp -> `Control (`Jmp jmp))
        ~default:(`Control (`Br instr))
    | terminal_instr -> terminal_instr
  in
  {
    meta;
    label;
    instrs = LVN_container.get_optimized_instructions lvn_container;
    terminal = optimized_terminal;
  }

module Test = struct
  let%test_unit "dead code elimination works" =
    let dir = "/Users/johnwu/code/bril/examples/test/tdce" in
    Backtrace.elide := false;
    Async.Thread_safe.block_on_async_exn
    @@ fun () ->
    Deferred.List.all
    @@ List.map
         [
           ("simple.bril", "simple.out");
           ("double.bril", "double.out");
           ("combo.bril", "combo.out");
           ("reassign-dkp.bril", "reassign-dkp.out");
         ]
         ~f:(fun (filename, expected) ->
           Test_util.test_local_optimization
             ~filename:(dir ^/ filename)
             ~expected:(dir ^/ expected)
             ~f:dead_code_elimination)
    |> Deferred.ignore_m

  let%test_unit "lvn works" =
    let dir = "/Users/johnwu/code/bril/examples/test/lvn" in
    Backtrace.elide := false;
    Async.Thread_safe.block_on_async_exn
    @@ fun () ->
    Deferred.List.all
    @@ List.map
         [ ("logical-operators.bril", "logical-operators.out") ]
         ~f:(fun (filename, expected) ->
           Test_util.test_local_optimization
             ~filename:(dir ^/ filename)
             ~expected:(dir ^/ expected)
             ~f:local_value_numbering)
    |> Deferred.ignore_m
end

module String_shit = struct
  let find_prefix (s1 : string) (s2 : string) : string =
    let min_length = Int.min (String.length s1) (String.length s2) in
    let range = Sequence.init min_length ~f:succ in
    let found_index = Sequence.find range ~f:(fun i -> not @@ Char.equal s1.[i] s2.[i]) in
    String.prefix s1 (Option.value found_index ~default:min_length)

  let find_suffix (s1 : string) (s2 : string) : string =
    let min_length = Int.min (String.length s1) (String.length s2) in
    let range = Sequence.init min_length ~f:succ in
    let found_index = Sequence.find range ~f:(fun i -> not @@ Char.equal s1.[String.length s1 - i] s2.[String.length s2 - i]) in
    String.suffix s1 (Option.value found_index ~default:min_length)

  let get_unique_char (indices: int list): int option = 
      match indices with
      | [index] -> Some index
      | _ -> None
  let find_unique_char (s1: string) (s2: string): (int * int) = 
      let chars1 = Char.Table.filter_map ~f:(get_unique_char) @@ Char.Table.of_alist_multi @@ List.mapi (String.to_list s1) ~f:(fun i c -> (c, i)) in
      let chars2 = Char.Table.filter_map ~f:(get_unique_char) @@ Char.Table.of_alist_multi @@ List.mapi (String.to_list s2) ~f:(fun i c -> (c, i)) in
      match Set.min_elt @@ Set.inter (Char.Set.of_list @@ Hashtbl.keys chars1) (Char.Set.of_list @@ Hashtbl.keys chars2) with
      | None -> (-1, -1)
      | Some c -> (Hashtbl.find_exn chars1 c, Hashtbl.find_exn chars2 c) 
      

end
