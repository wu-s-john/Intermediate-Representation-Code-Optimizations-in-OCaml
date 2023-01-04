open Core

module T = struct
  type 'instr t = {
    instruction : 'instr;
    label : string option;
    instr_line : int;
  }
  [@@deriving hash, eq, sexp, compare, to_yojson]

  let map ~f (t : 'instr t) = { t with instruction = f t.instruction }
end

include T

module Def = struct
  module T = struct
    open Program

    type variabled_assigned_calls = {
      func_name : string;
      args : arg list;
      dest : dest;
    }
    [@@deriving hash, eq, sexp, compare, to_yojson]

    type instr =
      [ `Const of const
      | `Binary of binary
      | `Unary of unary
      | `Call of variabled_assigned_calls
      ]
    [@@deriving hash, eq, sexp, compare, to_yojson]

    type t = instr T.t [@@deriving hash, eq, sexp, compare, to_yojson]
  end

  let declared_variable ({ instruction; _ } : T.t) =
    match instruction with
    | `Const { dest; _ } -> dest
    | `Binary { dest; _ } -> dest
    | `Unary { dest; _ } -> dest
    | `Call { dest = { dest; _ }; _ } -> dest

  include T
  include Comparable.Make (T)
  include Hashable.Make(T)

  let create (instruction : Program.Instruction.t) : instr option =
    match instruction with
    | `Const const -> Some (`Const const)
    | `Binary binary -> Some (`Binary binary)
    | `Unary unary -> Some (`Unary unary)
    | `Call { dest; func_name; args } ->
      Option.map dest ~f:(fun dest -> `Call { dest; func_name; args })
    | _ -> None

  let def_to_program_instruction (instr : instr) : Program.Instruction.t =
    match instr with
    | `Const const -> `Const const
    | `Binary binary -> `Binary binary
    | `Unary unary -> `Unary unary
    | `Call { dest; func_name; args } -> `Call { dest = Some dest; func_name; args }

  let to_program_instruction (t : t) : Program.Instruction.t =
    def_to_program_instruction t.instruction
end

module Instr = struct
  module T = struct
    type t = Program.Instruction.t T.t [@@deriving eq, sexp, compare, to_yojson]
  end

  include T
  include Comparable.Make (T)

  let declared_variable ({ instruction; _ } : t) : string option =
    match instruction with
    | `Const { dest; _ } -> Some dest
    | `Binary { dest; _ } -> Some dest
    | `Unary { dest; _ } -> Some dest
    | `Call { dest; _ } -> Option.map dest ~f:(fun { dest; _ } -> dest)
    | _ -> None

  let of_def (def : Def.t) : t = map def ~f:Def.def_to_program_instruction
end