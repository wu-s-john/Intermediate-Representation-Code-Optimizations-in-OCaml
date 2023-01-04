module Make (Var : Variable.S) : sig
  include Program_intf.S with module Var := Var
end

module Regular : Program_intf.S with module Var := Variable
module SSA : Program_intf.S with module Var := Variable.SSA

include Program_intf.S with module Var := Variable

val to_ssa : t -> SSA.t

val head_function_blocks_exn : t -> (Block.Key.t, Block.t) Node_traverser.Poly.t