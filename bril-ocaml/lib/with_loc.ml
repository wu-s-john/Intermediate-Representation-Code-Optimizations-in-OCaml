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
  open Program

  module T = struct
    type t = Instruction.Definition.t T.t [@@deriving hash, eq, compare, sexp]
  end

  let declared_variable ({ instruction; _ } : T.t) =
    Instruction.Definition.assignment_var instruction

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_program_instruction (t : t) : Program.Instruction.t =
    Instruction.of_definition t.instruction
end

module Instr = struct
  module T = struct
    type t = Program.Instruction.t T.t [@@deriving eq, sexp, compare, to_yojson]
  end

  include T
  include Comparable.Make (T)

  let declared_variable ({ instruction; _ } : t) : string option =
    Program.Instruction.declared_var instruction

  let of_def (def : Def.t) : t = map def ~f:Program.Instruction.of_definition
end