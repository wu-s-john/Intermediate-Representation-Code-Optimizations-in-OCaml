module type S = sig
  type t
  type key
  type node

  val predecessors : t -> key -> node list option
  val successors : t -> key -> node list option
  val reverse_postorder : t -> node list
  val keys : t -> key list
  val root : t -> node
  val of_alist : (key * node) list -> t option
  val map_inplace : t -> f:(node -> node) -> t (* Could probably be in another module *)

  val edges : t -> (key * key) list
end

module type Node_intf = sig
  include Node.S

  val children : t -> Key.t list
end

module type Poly_intf = sig
  type ('key, 'node) t

  val predecessors : ('key, 'node) t -> 'key -> 'node list option
  val successors : ('key, 'node) t -> 'key -> 'node list option
  val reverse_postorder : ('key, 'node) t -> 'node list
  val nodes : ('key, 'node) t -> 'node list
  val keys : ('key, 'node) t -> 'key list
  val root : ('key, 'node) t -> 'node

  val of_alist
    :  (module Node_intf with type t = 'node and type Key.t = 'key) ->
    ('key * 'node) list ->
    ('key, 'node) t option

  val map
    :  (module Node_intf with type t = 'node_out and type Key.t = 'key) ->
    ('key, 'node_in) t ->
    f:('node_in -> 'node_out) ->
    ('key, 'node_out) t

  val find : ('key, 'node) t -> 'key -> 'node option
  val find_exn : ('key, 'node) t -> 'key -> 'node
  val map_inplace : ('key, 'node) t -> f:('node -> 'node) -> unit
  val update : ('key, 'node) t -> 'node -> unit
  val edges : ('key, 'node) t -> ('key * 'key) list
end

module Make (Node : Node_intf) : S with type key := Node.Key.t and type node := Node.t
module Poly : Poly_intf

module Node : sig
  module Make_map (N : Node_intf) (F : Node.Mapper with type _in := N.t and type key := N.Key.t) :
    Node_intf with type t = F.t and module Key = N.Key
end
