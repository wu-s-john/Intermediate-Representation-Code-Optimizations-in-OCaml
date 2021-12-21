open Core

module type Yojson_enum = sig
  type t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

module Make_yojson_enum (Enum : Yojson_enum) : Yojson_enum with type t := Enum.t = struct
  include Enum

  let to_yojson (t : Enum.t) =
    match Enum.to_yojson t with
    | `List [ `String name ] -> `String name
    | enum_json ->
      failwithf
        !"to_yojson cannot process Enum as json format %s"
        (Yojson.Safe.pretty_to_string enum_json)
        ()

  let of_yojson (yojson : Yojson.Safe.t) : (Enum.t, string) Result.t =
    match yojson with
    | `String enum_value -> Enum.of_yojson @@ `List [ `String enum_value ]
    | yojson ->
      Error
        (sprintf !"of_yojson Cannot process json as enum %s" @@ Yojson.Safe.pretty_to_string yojson)
end

module Label = struct
  type t = { label : string } [@@deriving yojson]
end

module Type = struct
  (* TODO: Add pointer later *)
  module T = struct
    type t =
      | Int_typ [@name "int"]
      | Bool_typ [@name "bool"]
    [@@deriving yojson, eq]
  end

  include T
  module Enum = Make_yojson_enum (T)
  include Enum
end

module Const = struct
  type op = [ `Const [@name "const"] ] [@@deriving yojson, sexp]

  type t =
    | T : {
        dest : string;
        typ : 'a Primitive_type.t;
        value : 'a;
      }
        -> t

  module Literal_type = struct
    module T = struct
      type t =
        | Int_type [@name "int"]
        | Bool_type [@name "bool"]
      [@@deriving yojson, eq, sexp, compare]
    end

    include T
    module Enum = Make_yojson_enum (T)
    include Enum
  end

  let equal (t1 : t) (t2 : t) =
    match (t1, t2) with
    | ( T { dest = dest1; typ = Int_witness; value = value1 },
        T { dest = dest2; typ = Int_witness; value = value2 } ) ->
      String.equal dest1 dest2 && Int.equal value1 value2
    | ( T { dest = dest1; typ = Bool_witness; value = value1 },
        T { dest = dest2; typ = Bool_witness; value = value2 } ) ->
      String.equal dest1 dest2 && Bool.equal value1 value2
    | _ -> false

  let to_yojson : t -> Yojson.Safe.t = function
    | T { dest; typ = Primitive_type.Int_witness; value } ->
      `Assoc
        [
          ("op", `String "const");
          ("dest", `String dest);
          ("type", Literal_type.to_yojson Literal_type.Int_type);
          ("value", `Int value);
        ]
    | T { dest; typ = Primitive_type.Bool_witness; value } ->
      `Assoc
        [
          ("op", `String "const");
          ("dest", `String dest);
          ("type", Literal_type.to_yojson Literal_type.Bool_type);
          ("value", `Bool value);
        ]

  let of_yojson : Yojson.Safe.t -> (t, string) Result.t =
   fun json ->
    let open Result.Let_syntax in
    let open Yojson.Safe.Util in
    let%bind dest =
      json
      |> member "dest"
      |> to_string_option
      |> Result.of_option ~error:"Expected to dest to be type string"
    in
    match%bind json |> member "type" |> Literal_type.of_yojson with
    | Int_type ->
      let%map value =
        json
        |> member "value"
        |> Yojson.Safe.Util.to_int_option
        |> Result.of_option ~error:"Expected int"
      in
      T { dest; typ = Primitive_type.Int_witness; value }
    | Bool_type ->
      let%map value =
        json
        |> member "value"
        |> Yojson.Safe.Util.to_bool_option
        |> Result.of_option ~error:"Expected Bool"
      in
      T { dest; typ = Primitive_type.Bool_witness; value }
end

module Value_operation = struct
  module Op = struct
    module T = struct
      type t =
        [ `Add [@name "add"]
        | `Mul [@name "mul"]
        | `Sub [@name "sub"]
        | `Div [@name "div"]
        | `Eq [@name "eq"]
        | `Lt [@name "lt"]
        | `Gt [@name "gt"]
        | `Le [@name "le"]
        | `Ge [@name "ge"]
        | `Not [@name "not"]
        | `And [@name "and"]
        | `Or [@name "or"]
        ]
      [@@deriving yojson, eq]
    end

    include T
    module Enum = Make_yojson_enum (T)
    include Enum
  end

  type t = {
    op : Op.t;
    dest : string;
    typ : Type.t; [@key "type"]
    args : string list; [@default []]
  }
  [@@deriving yojson, eq]
end

module Effect_operation = struct
  module Op = struct
    module T = struct
      type t =
        [ `Nop [@name "nop"]
        | `Print [@name "print"]
        ]
      [@@deriving yojson, eq]
    end

    include T
    module Enum = Make_yojson_enum (T)
    include Enum
  end

  type t = {
    op : Op.t;
    args : string list; [@default []]
  }
  [@@deriving yojson, eq]
end

module Control = struct
  module T = struct
    type t =
      | Jmp of { labels : string list } [@name "jmp"]
      | Br of {
          labels : string list;
          args : string list;
        } [@name "br"]
      | Call of {
          dest : string option;
          typ : Type.t option; [@name "type"] [@default None]
          args : string list; [@default []]
          funcs : string list; [@default []]
        } [@name "call"]
      | Ret of { args : string list [@default []] } [@name "ret"]
    [@@deriving yojson { strict = false }, eq]
  end

  include T

  module Op = struct
    module T = struct
      type t =
        [ `Jmp [@name "jmp"]
        | `Br [@name "br"]
        | `Call [@name "call"]
        | `Ret [@name "ret"]
        ]
      [@@deriving yojson, eq]
    end

    include T
    module Enum = Make_yojson_enum (T)
    include Enum
  end

  let to_yojson : t -> Yojson.Safe.t =
   fun t ->
    let op =
      match t with
      | Jmp _ -> `Jmp
      | Br _ -> `Br
      | Call _ -> `Call
      | Ret _ -> `Ret
    in
    match T.to_yojson t with
    | `List [ _; record_json ] ->
      Yojson.Safe.Util.combine (`Assoc [ ("op", Op.to_yojson op) ]) record_json
    | json ->
      failwithf !"Cannot parse Control Instruction %s" (Yojson.Safe.pretty_to_string json) ()

  open Result.Let_syntax
  open Yojson.Safe.Util

  let of_yojson : Yojson.Safe.t -> (t, string) Result.t =
   fun json ->
    match%bind
      json
      |> member "op"
      |> Yojson.Safe.Util.to_string_option
      |> Result.of_option ~error:"Expected string"
    with
    | op -> T.of_yojson @@ `List [ `String op; json ]
end

module Instruction = struct
  module Op = struct
    module T = struct
      type t =
        [ `Add [@name "add"]
        | `Mul [@name "mul"]
        | `Sub [@name "sub"]
        | `Div [@name "div"]
        | `Eq [@name "eq"]
        | `Lt [@name "lt"]
        | `Gt [@name "gt"]
        | `Le [@name "le"]
        | `Ge [@name "ge"]
        | `Not [@name "not"]
        | `And [@name "and"]
        | `Or [@name "or"]
        | `Nop [@name "nop"]
        | `Print [@name "print"]
        | `Jmp [@name "jmp"]
        | `Br [@name "br"]
        | `Call [@name "call"]
        | `Ret [@name "ret"]
        | `Const [@name "const"]
        ]
      [@@deriving yojson, eq]
    end

    include T
    module Enum = Make_yojson_enum (T)
    include Enum
  end

  type _t =
    | Value of Value_operation.t
    | Effect of Effect_operation.t
    | Control of Control.t
    | Const of Const.t
  [@@deriving eq]

  type t = {
    op : Op.t;
    instr : _t;
  }
  [@@deriving eq]

  open Yojson.Safe.Util
  open Result.Let_syntax

  let to_yojson : t -> Yojson.Safe.t =
   fun t ->
    match t.instr with
    | Control instr -> Control.to_yojson instr
    | Const instr -> Const.to_yojson instr
    | Value value_instr -> Value_operation.to_yojson value_instr
    | Effect effect_instr -> Effect_operation.to_yojson effect_instr

  let of_yojson : Yojson.Safe.t -> (t, string) Result.t =
   fun json ->
    let%bind op = json |> member "op" |> Op.of_yojson in
    let%map instr =
      match op with
      | #Control.Op.t -> Control.of_yojson json |> Result.map ~f:(fun x -> Control x)
      | `Const -> Const.of_yojson json |> Result.map ~f:(fun x -> Const x)
      | #Value_operation.Op.t -> Value_operation.of_yojson json |> Result.map ~f:(fun x -> Value x)
      | #Effect_operation.Op.t ->
        Effect_operation.of_yojson json |> Result.map ~f:(fun x -> Effect x)
    in
    { op; instr }
end

module Function = struct
  type args = {
    name : string;
    typ : Type.t; [@key "type"]
  }
  [@@deriving yojson]

  type t = {
    name : string;
    args : args list;
    instrs : Instruction.t list;
    typ : Type.t option; [@key "type"] [@default None]
  }
  [@@deriving yojson]
end

module Program = struct
  type t = { functions : Function.t list } [@@deriving yojson]
end

module Test = struct
  open Async

  let test_path = "/Users/johnwu/code/bril/test/print"

  let%test_unit "Serailize/Deserialize Bool type" =
    let json = Const.Literal_type.to_yojson Const.Literal_type.Bool_type in
    printf !"Serialization Complete\n";
    let witness = Const.Literal_type.of_yojson json |> Result.ok_or_failwith in
    printf !"Deserialization Complete\n";
    [%test_eq: Const.Literal_type.t] witness Const.Literal_type.Bool_type

  let%test_unit "serialize and deserialize value instruction" =
    let json =
      Yojson.Safe.from_string {|{
        "args": ["ite","one"],
        "dest": "ite",
        "op": "add",
        "type": "int"
      }|}
    in
    match Value_operation.of_yojson json with
    | Ok _ -> ()
    | Error e -> failwithf !"Could not print this error: %s" e ()

  let%test_unit "serialize and deserialize const instruction" =
    let json =
      Yojson.Safe.from_string {|{ "op": "const", "type": "int", "dest": "v0", "value": 1 } |}
    in
    match Const.of_yojson json with
    | Ok _ -> ()
    | Error e -> failwithf !"Could not print this error: %s" e ()

  let%test_unit "print every Instruction op" =
    printf !"Print Instruction Op";
    List.iter [ `Add; `Mul; `Call ] ~f:(fun op ->
        printf !"Meatspin : %s\n" @@ Yojson.Safe.pretty_to_string @@ Instruction.Op.to_yojson op)

  let%test_unit "serialize and deserialize const instruction 2" =
    let json =
      Yojson.Safe.from_string {|{ "op": "const", "type": "int", "dest": "v0", "value": 1 } |}
    in
    match Instruction.of_yojson json with
    | Ok _ -> ()
    | Error e -> failwithf !"Could not print this error: %s" e ()

  let%test_unit "serialize control instruction2" =
    let control_instrs : Control.t list =
      [
        Control.Br { labels = [ "true.ret.1"; "false.loop" ]; args = [ "eq_cond_12" ] };
        Control.Call
          {
            dest = Some "icount";
            args = [ "n_1"; "queens"; "icount"; "site" ];
            typ = Some Type.Int_typ;
            funcs = [ "queen" ];
          };
        Control.Ret { args = [ "true" ] };
        Control.Jmp { labels = [ "for.cond" ] };
      ]
    in
    List.iter control_instrs ~f:(fun control_instr ->
        let json_instr = Control.to_yojson control_instr in
        assert (Control.equal (Result.ok_or_failwith (Control.of_yojson json_instr)) control_instr))

  let%test_unit "serialize control instruction 3" =
    let control_instrs : Instruction.t list =
      [
        {
          Instruction.op = `Br;
          instr =
            Control
              (Control.Br { labels = [ "true.ret.1"; "false.loop" ]; args = [ "eq_cond_12" ] });
        };
        {
          Instruction.op = `Call;
          instr =
            Control
              (Control.Call
                 {
                   dest = Some "icount";
                   args = [ "n_1"; "queens"; "icount"; "site" ];
                   typ = Some Type.Int_typ;
                   funcs = [ "queen" ];
                 });
        };
        { Instruction.op = `Ret; instr = Control (Control.Ret { args = [ "true" ] }) };
        { Instruction.op = `Jmp; instr = Control (Control.Jmp { labels = [ "for.cond" ] }) };
      ]
    in
    List.iter control_instrs ~f:(fun control_instr ->
        let json_instr = Instruction.to_yojson control_instr in
        assert (
          Instruction.equal (Result.ok_or_failwith (Instruction.of_yojson json_instr)) control_instr
        ))

  (* let%test_unit "serialize and deserialize control instruction" =
    let control_br : Control.t =
      Control.Br {labels= ["true.ret.1"; "false.loop"]; args = ["eq_cond_12"]}
    in
    let json = Instruction.to_yojson @@ {op = `Br; instr = Control control_br} in
    let deserialized_control_br_1 = Instruction.of_yojson json |> Result.ok_or_failwith in
    let json_version =  Yojson.Safe.from_string {|{"args": ["eq_cond_12"],"labels": ["true.ret.1","false.loop"],"op": "br"}|} in
    let deserialized_control_br_2 = Instruction.of_yojson json_version |> Result.ok_or_failwith in
    assert (deserialized_control_br_1 == deserialized_control_br_2) *)

  let%test_unit "serialize and deserialize json files" =
    Backtrace.elide := false;
    Thread_safe.block_on_async_exn
    @@ fun () ->
    Reader.with_file (test_path ^/ "eight-queens.json") ~f:(fun file ->
        let%map contents = Reader.contents file in
        let json = Yojson.Safe.from_string contents in
        let program =
          match Program.of_yojson json with
          | Ok result -> result
          | Error e -> failwithf !"Could not print this error: %s" e ()
        in
        let serialized_program = Program.to_yojson program in
        Yojson.Safe.pretty_print Format.std_formatter serialized_program)
end
