module Make (Var : Variable.S) : sig
  include Program_intf.S with module Var := Var
end

module Regular : Program_intf.S with module Var := Variable
module SSA : Program_intf.S with module Var := Variable.SSA
include Program_intf.S with module Var := Variable