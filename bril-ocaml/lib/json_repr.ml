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
        | `Id [@name "id"]
        | `Nop [@name "nop"]
        | `Print [@name "print"]
        | `Jmp [@name "jmp"]
        | `Br [@name "br"]
        | `Call [@name "call"]
        | `Ret [@name "ret"]
        | `Const [@name "const"]
        (* | `Alloc [@name "alloc"]
        | `Free [@name "free"]
        | `Store [@name "store"]
        | `Load [@name "load"]
        | `Ptradd [@name "ptradd"] *)
        ]
      [@@deriving yojson, eq, sexp]
    end

    include T
    module Enum = Make_yojson_enum (T)
    include Enum
  end

  module Const_value = struct
    type t =
      [ `Int of int
      | `Bool of bool
      ]
    [@@deriving eq, sexp]

    let to_yojson (t : t) : Yojson.Safe.t = (t :> Yojson.Safe.t)

    let of_yojson (json : Yojson.Safe.t) : (t, string) result =
      match json with
      | `Int _ | `Bool _ as value -> Ok value
      | bad_json ->
        Error
          ( sprintf !"Cannot convert json to int or bool. Has form %s"
          @@ Yojson.Safe.pretty_to_string bad_json )
  end

  type instr = {
    op : Op.t;
    dest : string option [@default None];
    typ : Type.t option; [@key "type"] [@default None]
    args : string list; [@default []]
    funcs : string list; [@default []]
    labels : string list; [@default []]
    value : Const_value.t option; [@default None] (* This can be either int or bool and it is only for the value*)
  }
  [@@deriving eq, yojson, sexp]    

  type label = {label: string} [@@deriving eq, yojson, sexp]

  type t = 
    | Instr of instr
    | Label of label [@@deriving sexp]
  
  let to_yojson (t: t) : Yojson.Safe.t = 
      match  t with
      | Instr instr -> instr_to_yojson instr
      | Label label -> label_to_yojson label

  let first (t1: ('a, 'b) result) (t2: ('a, 'b) result) = 
      match (t1, t2) with 
      | (Ok result1, _) -> Ok result1
      | (_, Ok result2) -> Ok result2
      | (Error error1, Error _) -> Error error1
  let of_yojson (json: Yojson.Safe.t) = 
    first (instr_of_yojson json |> Result.map ~f:(fun x -> Instr x)) (label_of_yojson json |> Result.map ~f:(fun x -> Label x))

end

module Function = struct

  type t = {
    name : string;
    args : Func_arg.t list [@default []];
    instrs : Instruction.t list;
    typ : Type.t option; [@key "type"] [@default None]
  }
  [@@deriving yojson]
end

module Program = struct
  type t = { functions : Function.t list } [@@deriving yojson]
end

module Error = struct
  type t = [`Malformed_instr of Instruction.t | `Bad_block_format | `Json_parse_error of string] [@@deriving sexp]
end

module Test = struct

  let test_path = "/Users/johnwu/code/bril/test/print"

  let%test_unit "serialize and deserialize value instruction" =
    let json =
      Yojson.Safe.from_string
        {|{
        "args": ["ite","one"],
        "dest": "ite",
        "op": "add",
        "type": "int"
      }|}
    in
    match Instruction.of_yojson json with
    | Ok _ -> ()
    | Error e -> failwithf !"Could not print this error: %s" e ()

  let%test_unit "serialize and deserialize const instruction" =
    let json =
      Yojson.Safe.from_string {|{ "op": "const", "type": "int", "dest": "v0", "value": 1 } |}
    in
    match Instruction.of_yojson json with
    | Ok _ -> ()
    | Error e -> failwithf !"Could not print this error: %s" e ()


  let%test_unit "serialize and deserialize const instruction 2" =
    let json =
      Yojson.Safe.from_string {|{ "op": "const", "type": "int", "dest": "v0", "value": 1 } |}
    in
    match Instruction.of_yojson json with
    | Ok _ -> ()
    | Error e -> failwithf !"Could not print this error: %s" e ()


  (* let%test_unit "serialize and deserialize json files" =
    Backtrace.elide := false;
    Thread_safe.block_on_async_exn
    @@ fun () ->
    let names : string list = [ "add.json"; "call.json" ] in
    Deferred.List.iter names ~f:(fun file_name ->
        Reader.with_file (test_path ^/ file_name) ~f:(fun file ->
            let%map contents = Reader.contents file in
            let json = Yojson.Safe.from_string contents in
            let program =
              match Program.of_yojson json with
              | Ok result -> result
              | Error e -> failwithf !"Could not print this error: %s" e ()
            in
            let serialized_program = Program.to_yojson program in
            Yojson.Safe.pretty_print Format.std_formatter serialized_program)) *)
end
