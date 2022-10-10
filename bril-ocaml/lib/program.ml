open! Core

type arg = string [@@deriving compare, equal, sexp, hash]
type label = string [@@deriving compare, equal, sexp, hash]

type const = {
  dest : string;
  value : [ `Int of int | `Bool of bool ];
}
[@@deriving compare, equal, sexp, hash]

module Op = struct
  module Binary = struct
    type t =
      [ `Add
      | `Mul
      | `Sub
      | `Div
      | `Eq
      | `Lt
      | `Gt
      | `Le
      | `Ge
      | `And
      | `Or
      ]
    [@@deriving sexp, compare, hash, eq]

    let to_symbol = function
      | `Add -> "+"
      | `Mul -> "*"
      | `Sub -> "-"
      | `Div -> "/"
      | `Eq -> "="
      | `Lt -> "<"
      | `Gt -> ">"
      | `Le -> "<="
      | `Ge -> ">="
      | `And -> "&"
      | `Or -> "|"

    let digest (op : t) (hashed_arg1 : Md5.t) (hashed_arg2 : Md5.t) =
      Md5.digest_string
      @@ sprintf "%s %s %s" (Md5.to_hex hashed_arg1) (to_symbol op) (Md5.to_hex hashed_arg2)
  end

  module Unary = struct
    type t =
      [ `Not
      | `Id
      ]
    [@@deriving sexp, compare, hash, eq]
  end
end

type binary = {
  dest : string;
  typ : Type.t;
  op : Op.Binary.t;
  arg1 : arg;
  arg2 : arg;
}
[@@deriving sexp, compare, hash, eq]

type unary = {
  dest : string;
  typ : Type.t;
  op : Op.Unary.t;
  arg : arg;
}
[@@deriving sexp, compare, hash, eq]

type br = {
  arg : arg;
  true_label : label;
  false_label : label;
}
[@@deriving sexp, compare, hash, eq]

type dest = {
  dest : string;
  typ : Type.t;
}
[@@deriving compare, equal, sexp, hash]

type call = {
  func_name : string;
  args : arg list;
  dest : dest option;
}
[@@deriving compare, equal, sexp, hash]

(* To account for effect types, ret is wrapped around an option *)
type ret = arg option [@@deriving compare, equal, sexp, hash]

module Instruction = struct
  type normal =
    [ `Const of const
    | `Binary of binary
    | `Unary of unary
    | `Call of call
    | `Print of arg list
    | `Nop
    ]
  [@@deriving compare, equal, sexp, hash]

  type control =
    [ `Jmp of label
    | `Br of br
    | `Ret of arg option
    ]
  [@@deriving compare, equal, sexp, hash]

  type t =
    [ normal
    | control
    | `Label of label
    ]
  [@@deriving compare, equal, sexp, hash]

  let of_json_repr (json_instr : Json_repr.Instruction.t) : (t, Json_repr.Error.t) Result.t =
    match json_instr with
    | Label { label } -> Ok (`Label label)
    | Instr instr ->
      (match instr with
      | {
       op = `Add;
       dest = Some dest;
       typ = Some Type.Int_typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ = Type.Int_typ; op = `Add; arg1; arg2 })
      | {
       op = `Mul;
       dest = Some dest;
       typ = Some Type.Int_typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ = Type.Int_typ; op = `Mul; arg1; arg2 })
      | {
       op = `Sub;
       dest = Some dest;
       typ = Some Type.Int_typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ = Type.Int_typ; op = `Sub; arg1; arg2 })
      | {
       op = `Div;
       dest = Some dest;
       typ = Some Type.Int_typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ = Type.Int_typ; op = `Div; arg1; arg2 })
      | {
       op = `Eq;
       dest = Some dest;
       typ = Some typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ; op = `Eq; arg1; arg2 })
      | {
       op = `Gt;
       dest = Some dest;
       typ = Some Type.Bool_typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ = Type.Bool_typ; op = `Gt; arg1; arg2 })
      | {
       op = `Le;
       dest = Some dest;
       typ = Some Type.Bool_typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ = Type.Bool_typ; op = `Le; arg1; arg2 })
      | {
       op = `Ge;
       dest = Some dest;
       typ = Some Type.Bool_typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ = Type.Bool_typ; op = `Ge; arg1; arg2 })
      | {
       op = `Not;
       dest = Some dest;
       typ = Some Type.Bool_typ;
       args = [ arg ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Unary { dest; typ = Type.Bool_typ; op = `Not; arg })
      | {
       op = `And;
       dest = Some dest;
       typ = Some Type.Bool_typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ = Type.Bool_typ; op = `And; arg1; arg2 })
      | {
       op = `Or;
       dest = Some dest;
       typ = Some Type.Bool_typ;
       args = [ arg1; arg2 ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Binary { dest; typ = Type.Bool_typ; op = `Or; arg1; arg2 })
      | {
       op = `Id;
       dest = Some dest;
       typ = Some typ;
       args = [ arg ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Unary { dest; typ; op = `Id; arg })
      | { op = `Nop; dest = None; typ = None; args = []; funcs = []; labels = []; value = None } ->
        Ok `Nop
      | { op = `Print; dest = None; typ = None; args; funcs = []; labels = []; value = None } ->
        Ok (`Print args)
      | {
       op = `Jmp;
       dest = None;
       typ = None;
       args = [];
       funcs = [];
       labels = [ label ];
       value = None;
      } ->
        Ok (`Jmp label)
      | {
       op = `Br;
       dest = None;
       typ = None;
       args = [ arg ];
       funcs = [];
       labels = [ true_label; false_label ];
       value = None;
      } ->
        Ok (`Br { arg; true_label; false_label })
      | {
       op = `Call;
       dest = Some dest;
       typ = Some typ;
       args;
       funcs = [ func_name ];
       labels = [];
       value = None;
      } ->
        Ok (`Call { args; func_name; dest = Some { dest; typ } })
      | {
       op = `Call;
       dest = None;
       typ = None;
       args;
       funcs = [ func_name ];
       labels = [];
       value = None;
      } ->
        Ok (`Call { args; func_name; dest = None })
      | {
       op = `Ret;
       dest = None;
       typ = None;
       args = [ arg ];
       funcs = [];
       labels = [];
       value = None;
      } ->
        Ok (`Ret (Some arg))
      | { op = `Ret; dest = None; typ = None; args = []; funcs = []; labels = []; value = None } ->
        Ok (`Ret None)
      | {
       op = `Const;
       dest = Some dest;
       typ = Some Type.Int_typ;
       args = [];
       funcs = [];
       labels = [];
       value = Some (`Int value);
      } ->
        Ok (`Const { dest; value = `Int value })
      | {
       op = `Const;
       dest = Some dest;
       typ = Some Type.Bool_typ;
       args = [];
       funcs = [];
       labels = [];
       value = Some (`Bool value);
      } ->
        Ok (`Const { dest; value = `Bool value })
      | malformed_instr -> Error (`Malformed_instr (Instr malformed_instr)))

  let to_json_repr (instr : t) : Json_repr.Instruction.t =
    match instr with
    | `Binary { dest; typ; op; arg1; arg2 } ->
      Instr
        {
          op = (op :> Json_repr.Instruction.Op.t);
          dest = Some dest;
          typ = Some typ;
          args = [ arg1; arg2 ];
          value = None;
          funcs = [];
          labels = [];
        }
    | `Unary { dest; typ; op; arg } ->
      Instr
        {
          op = (op :> Json_repr.Instruction.Op.t);
          dest = Some dest;
          typ = Some typ;
          args = [ arg ];
          value = None;
          funcs = [];
          labels = [];
        }
    | `Nop ->
      Instr { op = `Nop; dest = None; typ = None; args = []; funcs = []; labels = []; value = None }
    | `Const { dest; value } ->
      let typ =
        match value with
        | `Bool _ -> Type.Bool_typ
        | `Int _ -> Type.Int_typ
      in
      Instr
        {
          op = `Const;
          dest = Some dest;
          typ = Some typ;
          args = [];
          funcs = [];
          labels = [];
          value = Some value;
        }
    | `Br { arg; true_label; false_label } ->
      Instr
        {
          op = `Br;
          dest = None;
          typ = None;
          args = [ arg ];
          funcs = [];
          labels = [ true_label; false_label ];
          value = None;
        }
    | `Label label -> Label { label }
    | `Ret arg_opt ->
      Instr
        {
          op = `Ret;
          dest = None;
          typ = None;
          args = Option.to_list arg_opt;
          funcs = [];
          labels = [];
          value = None;
        }
    | `Print args ->
      Instr { op = `Print; dest = None; typ = None; args; funcs = []; labels = []; value = None }
    | `Call { args; func_name; dest } ->
      let (dest, typ) =
        match dest with
        | Some { dest; typ } -> (Some dest, Some typ)
        | None -> (None, None)
      in
      Instr { op = `Ret; dest; typ; args; funcs = [ func_name ]; labels = []; value = None }
    | `Jmp label ->
      Instr
        {
          op = `Jmp;
          dest = None;
          typ = None;
          args = [];
          funcs = [];
          labels = [ label ];
          value = None;
        }

  let used_vars (instr : [ normal | control ]) : String.Set.t =
    match instr with
    | `Const _ -> String.Set.empty
    | `Binary { arg1; arg2; _ } -> String.Set.of_list [ arg1; arg2 ]
    | `Unary { arg; _ } -> String.Set.singleton arg
    | `Print args -> String.Set.of_list args
    | `Call { args; _ } -> String.Set.of_list args
    | `Nop -> String.Set.empty
    | `Br { arg; _ } -> String.Set.singleton arg
    | `Ret arg -> String.Set.of_list @@ Option.to_list arg
    | `Jmp _ -> String.Set.empty

  let dest (instr : normal) : string option =
    match instr with
    | `Const { dest; _ }
    | `Binary { dest; _ }
    | `Unary { dest; _ } ->
      Some dest
    | _ -> None
end

module type Meta_intf = sig
  type t [@@deriving compare, equal, sexp, hash]
end

module Block = struct
  type 'meta t = {
    meta : 'meta;
    label : label option;
    instrs : Instruction.normal list;
    terminal : [ `Control of Instruction.control | `NextLabel of label | `Terminal ];
  }
  [@@deriving compare, equal, sexp, hash]

  module Key = struct
    module T = struct
      type t = string option [@@deriving sexp, compare, hash]
    end

    include T
    include Hashable.Make (T)
    include Comparable.Make (T)
  end

  let get_key ({ label; _ } : 'a t) : Key.t = label

  let children ({ terminal; _ } : 'a t) : Key.t list =
    match terminal with
    | `Control control ->
      (match control with
      | `Jmp label -> [ Some label ]
      | `Br { true_label; false_label; _ } -> [ Some true_label; Some false_label ]
      | `Ret _ -> [])
    | `NextLabel label -> [ Some label ]
    | `Terminal -> []

  let to_json_repr { label; instrs; terminal; meta = _ } : Json_repr.Instruction.t list =
    List.concat
      [
        Option.map label ~f:(fun label -> Json_repr.Instruction.Label { label }) |> Option.to_list;
        List.map instrs ~f:(fun normal_instr ->
            Instruction.to_json_repr (normal_instr :> Instruction.t));
        (match terminal with
        | `Control control_instr -> [ Instruction.to_json_repr (control_instr :> Instruction.t) ]
        | `NextLabel _ -> []
        | `Terminal -> []);
      ]

  let variable_defintions { instrs; _ } : String.Set.t =
    List.bind instrs ~f:(fun instr -> Option.to_list @@ Instruction.dest instr)
    |> String.Set.of_list

  let used_variables { instrs; _ } : String.Set.t =
    String.Set.union_list
      (List.map instrs ~f:(fun instr ->
           Instruction.used_vars (instr :> [ Instruction.normal | Instruction.control ])))

  let map ({ meta; _ } as t : 'a t) ~f : 'b t = { t with meta = f meta }
end

module type Monomorphic_block_intf = sig
  type meta
  type t = meta Block.t [@@deriving sexp, compare, hash]

  include Node.S with type t := t

  val to_json_repr : meta Block.t -> Json_repr.Instruction.t list
end

(* This is useful to remove have Block module without any polymorphic type parameters *)
module Make_monomorphic_block (Meta : Meta_intf) :
  Monomorphic_block_intf with type meta := Meta.t and module Key = Block.Key = struct
  type t = Meta.t Block.t [@@deriving sexp, compare, hash]

  module Key = Block.Key

  let get_key t = Block.get_key t
  let children t = Block.children t
  let to_json_repr t = Block.to_json_repr t
end

module Block_unit = Make_monomorphic_block (Unit)

module Function = struct
  type blocks = (Block.Key.t, unit Block.t) Node_traverser.Poly.t

  type t = {
    name : string;
    args : Func_arg.t list;
    blocks : blocks;
    typ : Type.t option;
  }

  (* TODO: This can probably be optimized as a tail recursive solution*)
  let rec to_basic_blocks_helper
      (block_name : label option)
      (acc_normal_instr : Instruction.normal list)
      (instrs : Instruction.t list)
      : unit Block.t list
    =
    match instrs with
    | [] ->
      if Option.is_none block_name && List.is_empty acc_normal_instr then []
      else
        [
          {
            meta = ();
            label = block_name;
            instrs = List.rev acc_normal_instr;
            terminal = `Terminal;
          };
        ]
    | hd_instr :: tail_instr ->
      (match hd_instr with
      (* non-control instructions should not terminate to the next block*)
      | #Instruction.normal as instr ->
        to_basic_blocks_helper block_name (instr :: acc_normal_instr) tail_instr
      | `Label label ->
        let constructed_block =
          {
            meta = ();
            Block.label = block_name;
            instrs = List.rev acc_normal_instr;
            terminal = `NextLabel label;
          }
        in
        constructed_block :: to_basic_blocks_helper (Some label) [] tail_instr
      | #Instruction.control as terminating_instructions ->
        let constructed_block =
          {
            meta = ();
            Block.label = block_name;
            instrs = List.rev acc_normal_instr;
            terminal = `Control terminating_instructions;
          }
        in
        (match tail_instr with
        | [] -> [ constructed_block ]
        | `Label next_label :: tail_tail_instr ->
          constructed_block :: to_basic_blocks_helper (Some next_label) [] tail_tail_instr
        | tail_hd_instr :: tail_tail_instr ->
          constructed_block :: to_basic_blocks_helper None [] (tail_hd_instr :: tail_tail_instr)))

  let of_json_repr ({ name; args; instrs; typ } : Json_repr.Function.t)
      : (t, Json_repr.Error.t) Result.t
    =
    let open Result.Let_syntax in
    let%bind parsed_instrs = Result.all @@ List.map instrs ~f:Instruction.of_json_repr in
    let block_list = to_basic_blocks_helper None [] parsed_instrs in
    let%map traverser =
      Node_traverser.Poly.of_alist
        (module Block_unit)
        (List.map block_list ~f:(fun block -> (Block.get_key block, block)))
      |> Result.of_option ~error:`Bad_block_format
    in
    { name; args; blocks = traverser; typ }

  let to_json_repr ({ name; args; blocks; typ } : t) : Json_repr.Function.t =
    let blocks = Node_traverser.Poly.reverse_postorder blocks in
    let instrs = List.bind blocks ~f:Block.to_json_repr in
    { Json_repr.Function.name; args; typ; instrs }
end

type t = { functions : Function.t list }

let of_json_repr ({ functions } : Json_repr.Program.t) : (t, Json_repr.Error.t) Result.t =
  let open Result.Let_syntax in
  let%map parsed_functions = Result.all @@ List.map functions ~f:Function.of_json_repr in
  { functions = parsed_functions }

let to_json_repr ({ functions } : t) : Json_repr.Program.t =
  { Json_repr.Program.functions = List.map ~f:Function.to_json_repr functions }

let run_local_optimizations ({ functions } : t) ~(f : unit Block.t -> unit Block.t) : t =
  List.iter functions ~f:(fun func -> Node_traverser.Poly.map_inplace func.blocks ~f);
  { functions }
