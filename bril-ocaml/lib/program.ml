open! Core

module Make (Var : Variable.S) : Program_intf.S with module Var := Var = struct
  let lift_error (result : 'a Or_error.t) : ('a, Json_repr.Error.t) Result.t =
    match result with
    | Ok x -> Ok x
    | Error err -> Error (`Json_parse_error (Error.to_string_hum err))

  let lift_json_error (result : ('a, Json_repr.Error.t) Result.t) : ('a, string) Result.t =
    match result with
    | Ok x -> Ok x
    | Error (`Json_parse_error err) -> Error err
    | Error `Bad_block_format -> Error "Bad block format"
    | Error (`Malformed_instr instr) ->
      Error (Json_repr.Instruction.to_yojson instr |> Yojson.Safe.to_string)

  type binary = {
    dest : Var.t;
    typ : Type.t;
    op : Op.Binary.t;
    arg1 : Var.t;
    arg2 : Var.t;
  }
  [@@deriving sexp, compare, hash, eq, to_yojson]

  type unary = {
    dest : Var.t;
    typ : Type.t;
    op : Op.Unary.t;
    arg : Var.t;
  }
  [@@deriving sexp, compare, hash, eq, to_yojson]

  type br = {
    arg : Var.t;
    true_label : Label.t;
    false_label : Label.t;
  }
  [@@deriving sexp, compare, hash, eq, to_yojson]

  type dest = {
    dest : Var.t;
    typ : Type.t;
  }
  [@@deriving compare, equal, sexp, hash, to_yojson]

  type 'dest call = {
    func_name : string;
    args : Var.t list;
    dest : 'dest;
  }
  [@@deriving compare, equal, sexp, hash, to_yojson]

  type const = {
    dest : Var.t;
    value : [ `Int of int | `Bool of bool ];
  }
  [@@deriving compare, equal, sexp, hash, to_yojson]

  (* To account for effect types, ret is wrapped around an option *)
  type ret = Var.t option [@@deriving compare, equal, sexp, hash]

  module Instruction = struct
    type 'dest definition_instr =
      [ `Const of const
      | `Binary of binary
      | `Unary of unary
      | `Call of 'dest call
      ]
    [@@deriving compare, eq, sexp, hash, to_yojson]

    module Definition = struct
      type t = dest definition_instr [@@deriving compare, equal, sexp, hash, to_yojson, sexp]

      let assignment_var (t : t) : Var.t =
        match t with
        | `Const { dest; _ } -> dest
        | `Binary { dest; _ } -> dest
        | `Unary { dest; _ } -> dest
        | `Call { dest = { dest; _ }; _ } -> dest
    end

    type normal =
      [ `Const of const
      | `Binary of binary
      | `Unary of unary
      | `Call of dest option call
      | `Print of Var.t list
      | `Nop
      ]
    [@@deriving compare, equal, sexp, hash]

    type control =
      [ `Jmp of Label.t
      | `Br of br
      | `Ret of Var.t option
      ]
    [@@deriving compare, equal, sexp, hash]

    type t =
      [ normal
      | control
      | `Label of Label.t
      ]
    [@@deriving compare, equal, sexp, hash]

    let to_json_repr (instr : t) : Json_repr.Instruction.t =
      match instr with
      | `Binary { dest; typ; op; arg1; arg2 } ->
        Instr
          {
            op = (op :> Json_repr.Instruction.Op.t);
            dest = Some (Var.to_string dest);
            typ = Some typ;
            args = [ Var.to_string arg1; Var.to_string arg2 ];
            value = None;
            funcs = [];
            labels = [];
          }
      | `Unary { dest; typ; op; arg } ->
        Instr
          {
            op = (op :> Json_repr.Instruction.Op.t);
            dest = Some (Var.to_string dest);
            typ = Some typ;
            args = [ Var.to_string arg ];
            value = None;
            funcs = [];
            labels = [];
          }
      | `Nop ->
        Instr
          { op = `Nop; dest = None; typ = None; args = []; funcs = []; labels = []; value = None }
      | `Const { dest; value } ->
        let typ =
          match value with
          | `Bool _ -> Type.Bool_typ
          | `Int _ -> Type.Int_typ
        in
        Instr
          {
            op = `Const;
            dest = Some (Var.to_string dest);
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
            args = [ Var.to_string arg ];
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
            args = Option.to_list arg_opt |> List.map ~f:Var.to_string;
            funcs = [];
            labels = [];
            value = None;
          }
      | `Print args ->
        Instr
          {
            op = `Print;
            dest = None;
            typ = None;
            args = List.map ~f:Var.to_string args;
            funcs = [];
            labels = [];
            value = None;
          }
      | `Call { args; func_name; dest } ->
        let (dest, typ) =
          match dest with
          | Some { dest; typ } -> (Some dest, Some typ)
          | None -> (None, None)
        in
        Instr
          {
            op = `Ret;
            dest = Option.map ~f:Var.to_string dest;
            typ;
            args = List.map ~f:Var.to_string args;
            funcs = [ func_name ];
            labels = [];
            value = None;
          }
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

    let to_yojson = Fn.compose Json_repr.Instruction.to_yojson to_json_repr
    let control_to_yojson (control_instr : control) : Yojson.Safe.t = to_yojson (control_instr :> t)

    let of_json_repr (json_instr : Json_repr.Instruction.t) : (t, Json_repr.Error.t) Result.t =
      let open Result.Let_syntax in
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
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ = Type.Int_typ; op = `Add; arg1; arg2 }
        | {
         op = `Mul;
         dest = Some dest;
         typ = Some Type.Int_typ;
         args = [ arg1; arg2 ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ = Type.Int_typ; op = `Mul; arg1; arg2 }
        | {
         op = `Sub;
         dest = Some dest;
         typ = Some Type.Int_typ;
         args = [ arg1; arg2 ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ = Type.Int_typ; op = `Sub; arg1; arg2 }
        | {
         op = `Div;
         dest = Some dest;
         typ = Some Type.Int_typ;
         args = [ arg1; arg2 ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ = Type.Int_typ; op = `Div; arg1; arg2 }
        | {
         op = `Eq;
         dest = Some dest;
         typ = Some typ;
         args = [ arg1; arg2 ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ; op = `Eq; arg1; arg2 }
        | {
         op = `Gt;
         dest = Some dest;
         typ = Some Type.Bool_typ;
         args = [ arg1; arg2 ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ = Type.Bool_typ; op = `Gt; arg1; arg2 }
        | {
         op = `Le;
         dest = Some dest;
         typ = Some Type.Bool_typ;
         args = [ arg1; arg2 ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ = Type.Bool_typ; op = `Le; arg1; arg2 }
        | {
         op = `Ge;
         dest = Some dest;
         typ = Some Type.Bool_typ;
         args = [ arg1; arg2 ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ = Type.Bool_typ; op = `Ge; arg1; arg2 }
        | {
         op = `Not;
         dest = Some dest;
         typ = Some Type.Bool_typ;
         args = [ arg ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg = lift_error @@ Var.of_string arg in
          let%map dest = lift_error @@ Var.of_string dest in
          `Unary { dest; typ = Type.Bool_typ; op = `Not; arg }
        | {
         op = `And;
         dest = Some dest;
         typ = Some Type.Bool_typ;
         args = [ arg1; arg2 ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ = Type.Bool_typ; op = `And; arg1; arg2 }
        | {
         op = `Or;
         dest = Some dest;
         typ = Some Type.Bool_typ;
         args = [ arg1; arg2 ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg1 = lift_error @@ Var.of_string arg1 in
          let%bind arg2 = lift_error @@ Var.of_string arg2 in
          let%map dest = lift_error @@ Var.of_string dest in
          `Binary { dest; typ = Type.Bool_typ; op = `Or; arg1; arg2 }
        | {
         op = `Id;
         dest = Some dest;
         typ = Some typ;
         args = [ arg ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%bind arg = lift_error @@ Var.of_string arg in
          let%map dest = lift_error @@ Var.of_string dest in
          `Unary { dest; typ; op = `Id; arg }
        | { op = `Nop; dest = None; typ = None; args = []; funcs = []; labels = []; value = None }
          ->
          Ok `Nop
        | { op = `Print; dest = None; typ = None; args; funcs = []; labels = []; value = None } ->
          let%map args = Result.all @@ List.map ~f:(Fn.compose lift_error Var.of_string) args in
          `Print args
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
          let%map arg = lift_error @@ Var.of_string arg in
          `Br { arg; true_label; false_label }
        | {
         op = `Call;
         dest = Some dest;
         typ = Some typ;
         args;
         funcs = [ func_name ];
         labels = [];
         value = None;
        } ->
          let%bind args = Result.all @@ List.map ~f:(Fn.compose lift_error Var.of_string) args in
          let%map dest = lift_error @@ Var.of_string dest in
          `Call { args; func_name; dest = Some { dest; typ } }
        | {
         op = `Call;
         dest = None;
         typ = None;
         args;
         funcs = [ func_name ];
         labels = [];
         value = None;
        } ->
          let%map args = Result.all @@ List.map ~f:(Fn.compose lift_error Var.of_string) args in
          `Call { args; func_name; dest = None }
        | {
         op = `Ret;
         dest = None;
         typ = None;
         args = [ arg ];
         funcs = [];
         labels = [];
         value = None;
        } ->
          let%map arg = lift_error @@ Var.of_string arg in
          `Ret (Some arg)
        | { op = `Ret; dest = None; typ = None; args = []; funcs = []; labels = []; value = None }
          ->
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
          let%map dest = lift_error @@ Var.of_string dest in
          `Const { dest; value = `Int value }
        | {
         op = `Const;
         dest = Some dest;
         typ = Some Type.Bool_typ;
         args = [];
         funcs = [];
         labels = [];
         value = Some (`Bool value);
        } ->
          let%map dest = lift_error @@ Var.of_string dest in
          `Const { dest; value = `Bool value }
        | malformed_instr -> Error (`Malformed_instr (Instr malformed_instr)))

    let of_yojson (yojson : Yojson.Safe.t) : (t, string) Result.t =
      match Json_repr.Instruction.of_yojson yojson with
      | Ok instr -> lift_json_error @@ of_json_repr instr
      | Error err -> Error err

    let to_definition (instr : t) : Definition.t option =
      match instr with
      | `Const const -> Some (`Const const)
      | `Binary binary -> Some (`Binary binary)
      | `Unary unary -> Some (`Unary unary)
      | `Call { dest; func_name; args } ->
        Option.map dest ~f:(fun dest -> `Call { dest; func_name; args })
      | _ -> None

    let of_definition (definition : Definition.t) : t =
      match definition with
      | `Const const -> `Const const
      | `Binary binary -> `Binary binary
      | `Unary unary -> `Unary unary
      | `Call { dest; func_name; args } -> `Call { dest = Some dest; func_name; args }

    let used_vars (instr : t) : Var.Set.t =
      match instr with
      | `Const _ -> Var.Set.empty
      | `Binary { arg1; arg2; _ } -> Var.Set.of_list [ arg1; arg2 ]
      | `Unary { arg; _ } -> Var.Set.singleton arg
      | `Print args -> Var.Set.of_list args
      | `Call { args; _ } -> Var.Set.of_list args
      | `Nop -> Var.Set.empty
      | `Br { arg; _ } -> Var.Set.singleton arg
      | `Ret arg -> Var.Set.of_list @@ Option.to_list arg
      | `Jmp _ -> Var.Set.empty
      | `Label _ -> Var.Set.empty

    let declared_var (instr : t) : Var.t option =
      match instr with
      | `Const { dest; _ }
      | `Binary { dest; _ }
      | `Unary { dest; _ } ->
        Some dest
      | _ -> None

    let normal_declared_var (instr : normal) : Var.t option =
      match instr with
      | `Const { dest; _ }
      | `Binary { dest; _ }
      | `Unary { dest; _ } ->
        Some dest
      | _ -> None

    let to_string (instr : t) =
      match instr with
      | `Const { dest; value } ->
        let serialized_val =
          match value with
          | `Int int_value -> Int.to_string int_value
          | `Bool bool_value -> Bool.to_string bool_value
        in
        sprintf "const %s = %s" (Var.to_string dest) serialized_val
      | `Binary { dest; typ; op; arg1; arg2 } ->
        sprintf
          "%s: %s = %s %s %s"
          (Var.to_string dest)
          (Type.to_string typ)
          (Var.to_string arg1)
          (Op.Binary.to_symbol op)
          (Var.to_string arg2)
      | `Unary { dest; typ; op; arg } ->
        sprintf
          "%s: %s = %s %s"
          (Var.to_string dest)
          (Type.to_string typ)
          (Op.Unary.to_string op)
          (Var.to_string arg)
      | `Print args -> sprintf "print %s" (String.concat ~sep:" " (List.map ~f:Var.to_string args))
      | `Call { args; func_name; dest } ->
        let left_statement =
          match dest with
          | Some { dest; typ } -> sprintf "%s: %s =" (Type.to_string typ) (Var.to_string dest)
          | None -> ""
        in
        sprintf
          "%scall @%s %s"
          left_statement
          func_name
          (String.concat ~sep:" " (List.map ~f:Var.to_string args))
      | `Nop -> "nop"
      | `Br { arg; true_label; false_label } ->
        sprintf "br %s .%s .%s" (Var.to_string arg) true_label false_label
      | `Ret arg ->
        (match arg with
        | Some arg -> sprintf "ret %s" (Var.to_string arg)
        | None -> "ret")
      | `Jmp label -> sprintf "jmp .%s" label
      | `Label label -> sprintf ".%s:" label
  end

  module Block = struct
    type terminal_instr =
      [ `Control of Instruction.control
      | `NextLabel of Label.t
      | `Terminal
      ]
    [@@deriving compare, equal, sexp, hash, to_yojson]

    type t = {
      label : Label.t option;
      instrs : Instruction.normal list;
      terminal : terminal_instr;
    }
    [@@deriving compare, equal, sexp, hash]

    let convert_terminal_instr_to_instr (terminal_instr : terminal_instr) : Instruction.t option =
      match terminal_instr with
      | `Control control_instr -> Some (control_instr :> Instruction.t)
      | `NextLabel label -> Some (`Label label :> Instruction.t)
      | `Terminal -> None

    module Key = Block_name

    let key ({ label; _ } : t) : Key.t = label

    let children ({ terminal; _ } : t) : Key.t list =
      match terminal with
      | `Control control ->
        (match control with
        | `Jmp label -> [ Some label ]
        | `Br { true_label; false_label; _ } -> [ Some true_label; Some false_label ]
        | `Ret _ -> [])
      | `NextLabel label -> [ Some label ]
      | `Terminal -> []

    let to_json_repr { label; instrs; terminal } : Json_repr.Instruction.t list =
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

    let to_yojson (t : t) : Yojson.Safe.t =
      `List (to_json_repr t |> List.map ~f:Json_repr.Instruction.to_yojson)

    let defined_variables { instrs; _ } : Var.Set.t =
      List.bind instrs ~f:(fun instr -> Option.to_list @@ Instruction.normal_declared_var instr)
      |> Var.Set.of_list

    let defined_variable_names { instrs; _ } : String.Set.t =
      List.bind instrs ~f:(fun instr -> Option.to_list @@ Instruction.normal_declared_var instr)
      |> List.map ~f:Var.to_string
      |> String.Set.of_list

    let used_variables { instrs; _ } : Var.Set.t =
      Var.Set.union_list
        (List.map instrs ~f:(fun instr -> Instruction.used_vars (instr :> Instruction.t)))

    let used_variable_names t : String.Set.t =
      Var.Set.to_list (used_variables t) |> List.map ~f:Var.to_string |> String.Set.of_list

    let all_instrs ({ instrs; terminal; _ } : t) : Instruction.t list =
      List.concat
      @@ [
           List.map instrs ~f:(fun instr -> (instr :> Instruction.t));
           Option.to_list @@ convert_terminal_instr_to_instr terminal;
         ]

    let terminal (t : t) : terminal_instr = t.terminal

    let all_instrs_with_line (block : t) : (int * Instruction.t) list =
      List.mapi (all_instrs block) ~f:(fun instr_line instr -> (instr_line, instr))

    let all_normal_instrs_with_line (block : t) : (int * Instruction.normal) list =
      List.mapi block.instrs ~f:(fun instr_line instr -> (instr_line, instr))

    module Var_location_map = Multi_map_set.Make (Var) (Int)

    let variable_definitions_location_map block : Int.Set.t Var.Map.t =
      all_instrs_with_line block
      |> List.filter_map ~f:(fun (line, instr) ->
             Option.map (Instruction.declared_var instr) ~f:(fun dest -> (dest, line)))
      |> Var_location_map.of_alist

    let render (t : t) : string =
      String.concat ~sep:"\\n" (List.map ~f:Instruction.to_string (all_instrs t))
  end

  module Function = struct
    type blocks = (Block.Key.t, Block.t) Node_traverser.Poly.t

    type t = {
      name : string;
      args : Func_arg.t list;
      blocks : blocks;
      typ : Type.t option;
    }

    module Var_location_map = Multi_map_set.Make (Var) (Location)

    let variable_location_map ({ blocks; _ } : t) : Location.Set.t Var.Map.t =
      Node_traverser.Poly.to_map
        (module Block.Key)
        blocks
        ~f:Block.variable_definitions_location_map
      |> Map.to_alist
      |> List.bind ~f:(fun (block_name, var_location_map) ->
             Map.to_alist var_location_map
             |> List.bind ~f:(fun (var, location_set) ->
                    location_set
                    |> Set.to_list
                    |> List.map ~f:(fun line -> (var, Location.{ block_name; line }))))
      |> Var_location_map.of_alist

    module Var_block_name_map = Multi_map_set.Make (Var) (Block.Key)

    (* We can also create an indexing. *)
    let variable_block_map ({ blocks; _ } : t) : Block.Key.Set.t Var.Map.t =
      Node_traverser.Poly.to_map (module Block.Key) blocks ~f:Block.defined_variables
      |> Map.to_alist
      |> List.bind ~f:(fun (block_name, variables) ->
             Set.to_list variables |> List.map ~f:(fun var -> (var, block_name)))
      |> Var_block_name_map.of_alist

    let variables ({ blocks; _ } : t) : Var.Set.t =
      let result =
        List.map (Node_traverser.Poly.nodes blocks) ~f:(fun block ->
            Set.union (Block.defined_variables block) (Block.used_variables block))
      in
      Var.Set.union_list result

    let to_json_repr ({ name; args; blocks; typ } : t) : Json_repr.Function.t =
      let blocks = Node_traverser.Poly.reverse_postorder blocks in
      let instrs = List.bind blocks ~f:Block.to_json_repr in
      { Json_repr.Function.name; args; typ; instrs }

    let to_yojson (func : t) : Yojson.Safe.t = to_json_repr func |> Json_repr.Function.to_yojson

    (* TODO: This can probably be optimized as a tail recursive solution*)
    let rec to_basic_blocks_helper
        (block_name : Label.t option)
        (acc_normal_instr : Instruction.normal list)
        (instrs : Instruction.t list)
        : Block.t list
      =
      match instrs with
      | [] ->
        if Option.is_none block_name && List.is_empty acc_normal_instr then []
        else [ { label = block_name; instrs = List.rev acc_normal_instr; terminal = `Terminal } ]
      | hd_instr :: tail_instr ->
        (match hd_instr with
        (* non-control instructions should not terminate to the next block*)
        | #Instruction.normal as instr ->
          to_basic_blocks_helper block_name (instr :: acc_normal_instr) tail_instr
        | `Label label ->
          let constructed_block =
            {
              Block.label = block_name;
              instrs = List.rev acc_normal_instr;
              terminal = `NextLabel label;
            }
          in
          constructed_block :: to_basic_blocks_helper (Some label) [] tail_instr
        | #Instruction.control as terminating_instructions ->
          let constructed_block =
            {
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
        Node_traverser.Poly.of_list (module Block) block_list ~get_children:Block.children
        |> Result.of_option ~error:`Bad_block_format
      in
      { name; args; blocks = traverser; typ }

    let of_yojson (json : Yojson.Safe.t) : (t, string) Result.t =
      match Json_repr.Function.of_yojson json with
      | Ok func -> lift_json_error @@ of_json_repr func
      | Error err -> Error err
  end

  type t = { functions : Function.t list }

  let run_local_optimizations ({ functions } : t) ~(f : Block.t -> Block.t) : t =
    List.iter functions ~f:(fun func -> Node_traverser.Poly.map_inplace func.blocks ~f);
    { functions }

  let select_block_exn (t : t) function_name =
    let funct =
      List.find t.functions ~f:(fun func -> String.equal func.name function_name)
      |> Option.value_exn
    in
    funct.blocks

  let head_function_blocks_exn (t : t) = (List.hd_exn t.functions).blocks

  let to_json_repr ({ functions } : t) : Json_repr.Program.t =
    { Json_repr.Program.functions = List.map ~f:Function.to_json_repr functions }

  let to_yojson (program : t) : Yojson.Safe.t = to_json_repr program |> Json_repr.Program.to_yojson

  let of_json_repr ({ functions } : Json_repr.Program.t) : (t, Json_repr.Error.t) Result.t =
    let open Result.Let_syntax in
    let%map parsed_functions = Result.all @@ List.map functions ~f:Function.of_json_repr in
    { functions = parsed_functions }

  let of_yojson (json : Yojson.Safe.t) : (t, string) Result.t =
    match Json_repr.Program.of_yojson json with
    | Ok json_repr -> lift_json_error @@ of_json_repr json_repr
    | Error err -> Error err
end

module Regular = Make (Variable.Regular)
module SSA = Make (Variable.SSA)
include Regular
