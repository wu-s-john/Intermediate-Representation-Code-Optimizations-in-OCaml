open Core

module T = struct
  type 'instr t = {
    instruction : 'instr;
    label : string option;
    instr_line : int;
  }
  [@@deriving hash, eq, sexp, compare, to_yojson]

  let map ~f (t : 'instr t) = { t with instruction = f t.instruction }

  let reuse_location (t : 'instr1 t) (instruction : 'instr2) =
    { instruction; label = t.label; instr_line = t.instr_line }
end

include T

module Def = struct
  open Program

  module T = struct
    type t = Instruction.Definition.t T.t [@@deriving hash, eq, compare, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Instr = struct
  module T = struct
    type t = Program.Instruction.t T.t [@@deriving eq, sexp, compare, to_yojson]
  end

  include T
  include Comparable.Make (T)
end