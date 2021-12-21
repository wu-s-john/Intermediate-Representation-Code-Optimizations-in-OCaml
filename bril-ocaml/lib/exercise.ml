(* open! Core
open! Common

(* Create a CFG to basic blocks *)

(* Some instructions change the control flow. Some do not *)

(* type normal_instr = [`Const of {""} ] *)

(* arg can be annotated with a type *)
type arg = string [@@deriving compare, equal, sexp_of]

type 'a ptr = Ptr of 'a
module Value_Type = struct  
  type 'a t = 
    | Int_type : int t
    | Bool_type : bool t
    | Ptr : 'a ptr t
end

module Label = struct
  type t = string 
end 
module Arg = struct
  type 'a t = {
    name: string;
    typ: 'a Value_Type.t
  }
end

module Dest = struct
  type 'a t = 
    | Value : {name: string; typ: 'a Value_Type.t} -> 'a t
    | Side_effect : unit t
end 

module Arg_list = struct
  type (_, _) t =
  | [] : ('r, 'r) t
  | ( :: ) : 'a Arg.t * ('r, 'k) t -> ('r, 'a -> 'k) t
end 

module Op = struct
  type 'func t = 
    | Add: (int -> int -> int) t
    | Mul: (int -> int -> int) t
    | Sub: (int -> int -> int) t
    | Div: (int -> int -> int) t
    | Not: (bool -> bool) t
    | Id : ('a -> 'a) t
    | Call: string -> 'func t

end

module Normal_Instr = struct
  type ('ret, 'func) t = 
    {op: 'func Op.t; args: ('ret, 'func) Arg_list.t; dest: 'ret Dest.t} 
end

module Control_Instr = struct
  type t = 
  | Jmp : Label.t -> t
  | Br : {arg : bool Arg.t; true_label : Label.t; false_label : Label.t} -> t
  | Ret : ('a Arg.t) option -> t
end

module Instr = struct
  type t = 
    | Normal : ('ret, 'func) Normal_Instr.t -> t
    | Control : Control_Instr.t -> t
end



type label = string

and typ =
  | IntType
  | BoolType
  | PtrType of typ

type const = {
  dest : string;
  value : [ `Int of int | `Bool of bool ];
}

type binary = {
  dest : string;
  typ : typ;
  op : Op.Binary.t;
  arg1 : arg;
  arg2 : arg;
}

type unary = {
  dest : string;
  typ : typ;
  op : Op.Unary.t;
  arg : arg;
}

type br = {
  arg : arg;
  true_label : label;
  false_label : label;
}

type dest = {
  dest : string;
  typ : typ;
}

type call = {
  func_name : string;
  args : arg list;
  dest : dest option;
}

type ret = arg option (* It can be an effect type *)

type normal_instr =
  [ `Const of const
  | `Binary of binary
  | `Unary of unary
  | `Call of call
  | `Print of arg list
  | `Nop
  ]

type side_effect_instr = [ `Print of arg list ]

type control_instr =
  [ `Jmp of label
  | `Br of br
  | `Ret of arg option
  ]

(* TODO: Might be good to represent this as a GADT *)
type instr =
  [ `Label of label
  | `Const of const
  | `Binary of binary
  | `Unary of unary
  | `Print of arg list
  | `Nop
  | `Jmp of label
  | `Br of br
  | `Call of call
  | `Ret of arg option
  ]

(* is a list of instructions with a terminating block in the end *)
type block = {
  label : label option;
  instrs : normal_instr list;
  terminal : [ `Control of control_instr | `NextLabel of label | `Terminal ];
}

(* Each block is labelled  *)

(* TODO: This can probably be optimized as a tail recursive solution*)
let rec to_basic_blocks_helper
    (block_name : label option)
    (acc_normal_instr : normal_instr list)
    (instrs : instr list)
    : block list
  =
  match instrs with
  | [] ->
    if Option.is_none block_name && List.is_empty acc_normal_instr then []
    else [ { label = block_name; instrs = List.rev acc_normal_instr; terminal = `Terminal } ]
  | hd_instr :: tail_instr ->
    ( match hd_instr with
    (* non-control instructions should not terminate to the next block*)
    | #normal_instr as instr ->
      to_basic_blocks_helper block_name (instr :: acc_normal_instr) tail_instr
    | `Label label ->
      let constructed_block =
        { label = block_name; instrs = List.rev acc_normal_instr; terminal = `NextLabel label }
      in
      constructed_block :: to_basic_blocks_helper (Some label) [] tail_instr
    | #control_instr as terminating_instructions ->
      let constructed_block =
        {
          label = block_name;
          instrs = List.rev acc_normal_instr;
          terminal = `Control terminating_instructions;
        }
      in
      ( match tail_instr with
      | [] -> [ constructed_block ]
      | `Label next_label :: tail_tail_instr ->
        constructed_block :: to_basic_blocks_helper (Some next_label) [] tail_tail_instr
      | tail_hd_instr :: tail_tail_instr ->
        constructed_block :: to_basic_blocks_helper None [] (tail_hd_instr :: tail_tail_instr) ) )

let to_basic_blocks (instrs : instr list) : block list = to_basic_blocks_helper None [] instrs

module CFG = struct
  module Key = struct
    type t = string option [@@deriving sexp, compare]
  end

  module CFG_Map = Map.Make (Key)

  type t = block CFG_Map.t

  let create (blocks : block list) : t =
    List.map blocks ~f:(fun block -> (block.label, block)) |> CFG_Map.of_alist_exn

  type edge_list = string list CFG_Map.t

  let edge_map (map : t) : string list CFG_Map.t =
    Map.map map ~f:(fun block ->
        match block.terminal with
        | `Control control ->
          ( match control with
          | `Jmp label -> [ label ]
          | `Br { true_label; false_label; _ } -> [ true_label; false_label ]
          | `Ret _ -> [] )
        | `NextLabel label -> [ label ]
        | `Terminal -> [])
end

let used_vars (instr : normal_instr) : String.Set.t =
  match instr with
  | `Const _ -> String.Set.empty
  | `Binary { arg1; arg2; _ } -> String.Set.of_list [ arg1; arg2 ]
  | `Unary { arg; _ } -> String.Set.singleton arg
  | `Print args -> String.Set.of_list args
  | `Call { args; _ } -> String.Set.of_list args
  | `Nop -> String.Set.empty

let dest (instr : normal_instr) : string option =
  match instr with
  | `Const { dest; _ }
  | `Binary { dest; _ }
  | `Unary { dest; _ } ->
    Some dest
  | _ -> None

let rec dead_code_elimination (block : block) : block =
  (* last_def are basically definitions that have been recently made, but never used *)
  let (dead_indices, _) =
    List.foldi
      block.instrs
      ~init:([], String.Map.empty)
      ~f:(fun index (acc_dead_indices, last_def) instr ->
        let used_variables = used_vars instr in
        let removed_used_variables_last_def =
          Set.fold used_variables ~init:last_def ~f:Map.remove
        in
        match dest instr with
        | None -> (acc_dead_indices, removed_used_variables_last_def) (* No update so *)
        | Some variable_name ->
          let dead_index =
            Map.find removed_used_variables_last_def variable_name
            |> Option.map ~f:(fun (last_index, _instr) -> last_index)
          in
          let updated_last_def =
            Map.update removed_used_variables_last_def variable_name ~f:(fun _ -> (index, instr))
          in
          let updated_acc_dead_indices =
            Option.value_map dead_index ~default:acc_dead_indices ~f:(fun last_index ->
                last_index :: acc_dead_indices)
          in
          (updated_acc_dead_indices, updated_last_def))
  in
  if List.is_empty dead_indices then block
  else
    let dead_indices = Int.Set.of_list dead_indices in
    let new_instrs = List.filteri block.instrs ~f:(fun i _ -> not @@ Set.mem dead_indices i) in
    let new_block = { block with instrs = new_instrs } in
    dead_code_elimination new_block

let nop_digest = Md5.digest_string "nop"

(* transform_instruction: Expression -> Expression hash *)
(* var_2_expr_hash: Variable -> expression hash *)
(* expr_hash_2_var: Expression hash -> original variable *)

let compute_dest (instr : normal_instr) : string option =
  match instr with
  | `Binary instr -> Some instr.dest
  | `Unary instr -> Some instr.dest
  | `Const instr -> Some instr.dest
  | `Nop -> None
  | `Print _ -> None
  | `Call instr -> Option.map instr.dest ~f:(fun dest -> dest.dest)

let local_value_numbering (block : block) : block =
  let var_2_expr_hash : Md5.t String.Table.t = String.Table.create () in
  let expr_hash_2_var : string Md5.Table.t = Md5.Table.create () in
  (* If the variable is not present has it and add it to both table *)
  let hash_var (var : String.t) : Md5.t =
    match String.Table.find var_2_expr_hash var with
    | Some expr_hash -> expr_hash
    | None ->
      let new_hash = Md5.digest_string @@ sprintf "Undefined Hash %s" var in
      Hashtbl.update var_2_expr_hash var ~f:(const new_hash);
      Hashtbl.update expr_hash_2_var new_hash ~f:(const var);
      new_hash
  in
  (* This should be able to retrieve variables that are not in scope. *)
  let get_var (hash : Md5.t) : string = Hashtbl.find_exn expr_hash_2_var hash in
  let transform_binary_instruct { op; arg1; arg2; dest; typ } =
    let hashed_arg1 = hash_var arg1 in
    let hashed_arg2 = hash_var arg2 in
    let digest = Op.Binary.digest op hashed_arg1 hashed_arg2 in
    let new_instruct = { op; arg1 = get_var hashed_arg1; arg2 = get_var hashed_arg2; dest; typ } in
    (digest, `Binary new_instruct)
  in
  let transform_cummutative_binary_instruct { op = _; arg1; arg2; dest; typ } cummulative_op =
    let hashed_arg1 = hash_var arg1 in
    let hashed_arg2 = hash_var arg2 in
    let min_digest = Md5.min hashed_arg1 hashed_arg2 in
    let max_digest = Md5.max hashed_arg1 hashed_arg2 in
    let op =
      match cummulative_op with
      | `Add -> Op.Binary.Add
      | `Mul -> Mul
    in
    let digest = Op.Binary.digest op min_digest max_digest in
    let new_instruct =
      { op = Add; arg1 = get_var min_digest; arg2 = get_var max_digest; dest; typ }
    in
    (digest, `Binary new_instruct)
  in
  let transform_const_op { dest; value } =
    let digest =
      match value with
      | `Bool value -> Md5.digest_string @@ sprintf !"bool %b" value
      | `Int value -> Md5.digest_string @@ sprintf !"int %d" value
    in
    (digest, `Const { dest; value })
  in
  let optimize_instruction_with_hash (instr : normal_instr) : Md5.t * normal_instr =
    match instr with
    | `Binary ({ op; _} as instr) ->
      ( match op with
      | Add -> transform_cummutative_binary_instruct instr `Add
      | Mul -> transform_cummutative_binary_instruct instr `Mul
      | _ -> transform_binary_instruct instr )
    | `Unary { op = Not; arg; dest; typ } ->
      let hashed_arg = hash_var arg in
      let new_instruct = { op = Not; arg = get_var hashed_arg; dest; typ } in
      let digest = Md5.digest_string @@ sprintf "!%s" (Md5.to_hex hashed_arg) in
      (digest, `Unary new_instruct)
    | `Unary { op = Id; arg; dest; typ } ->
      let hashed_arg = hash_var arg in
      let digest = Md5.digest_string @@ sprintf "!%s" (Md5.to_hex hashed_arg) in
      let retrieved_variable = get_var hashed_arg in
      let new_id_instr = { op = Id; arg = retrieved_variable; dest; typ } in
      (digest, `Unary new_id_instr)
    | `Nop -> (nop_digest, `Nop)
    | `Const value -> transform_const_op value
    | `Call { func_name; args; dest } ->
      let hashed_args = List.map args ~f:(fun arg -> hash_var arg) in
      let new_args = List.map hashed_args ~f:(fun hashed_arg -> get_var hashed_arg) in
      let digest_args = String.concat ~sep:" " (List.map ~f:Md5.to_hex hashed_args) in
      let final_digest = Md5.digest_string @@ sprintf !"%s %s" func_name digest_args in
      (final_digest, `Call { func_name; args = new_args; dest })
    | `Print args ->
      let hashed_args = List.map args ~f:(fun arg -> hash_var arg) in
      let new_args = List.map hashed_args ~f:(fun hashed_arg -> get_var hashed_arg) in
      let digest_args = String.concat ~sep:" " (List.map ~f:Md5.to_hex hashed_args) in
      let final_digest = Md5.digest_string @@ sprintf !"print %s" digest_args in
      (final_digest, `Print new_args)
  in
  let instrs =
    List.filter_map block.instrs ~f:(fun instr ->
        let (instr_hash, new_instr) = optimize_instruction_with_hash instr in
        match Hashtbl.find expr_hash_2_var instr_hash with
        | None ->
          (* TODO: Helpful to have GADTs here because you can represent dest effect as a GADT *)
          Option.iter (compute_dest new_instr) ~f:(fun dest ->
              Hashtbl.add_exn expr_hash_2_var ~key:instr_hash ~data:dest;
              Hashtbl.add_exn var_2_expr_hash ~key:dest ~data:instr_hash);
          Some new_instr
        | Some _ ->
          (* The computation has already been computed, so don't run it again. Have to consider the case for print*)
          ( match instr with
          | `Print _ -> Some new_instr
          | `Call { dest = Some _; _ } -> Some new_instr
          | _ ->
            Option.iter (compute_dest new_instr) ~f:(fun dest ->
                Hashtbl.update var_2_expr_hash dest ~f:(fun _ -> instr_hash));
            None ))
  in
  { block with instrs }

type func = {name: string; args: arg list; typ: typ; instrs: instr list} *)
