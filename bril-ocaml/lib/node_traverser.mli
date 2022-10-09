module type S = Node_traverser_intf.S
module type Poly_intf = Node_traverser_intf.Poly_intf

module Make (Node : Node.S) : S with type key := Node.Key.t and type node := Node.t
module Poly : Poly_intf
