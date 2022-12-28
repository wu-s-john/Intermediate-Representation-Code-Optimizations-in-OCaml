open Core

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

module type Poly_intf = sig
  type ('key, 'node) t

  val predecessors : ('key, 'node) t -> 'key -> 'node list option
  val successors : ('key, 'node) t -> 'key -> 'node list option
  val reverse_postorder : ('key, 'node) t -> 'node list
  val nodes : ('key, 'node) t -> 'node list
  val keys : ('key, 'node) t -> 'key list
  val root : ('key, 'node) t -> 'node
  val maximum_out_degree : ('key, 'node) t -> int

  val of_list
    :  (module Node.S with type t = 'node and type Key.t = 'key) ->
    get_children:('node -> 'key list) ->
    'node list ->
    ('key, 'node) t option

  (* val of_alist : ('key * 'key) list -> ('key, 'key) t *)

  val to_map
    :  (module Comparable.S with type t = 'key and type comparator_witness = 'witness) ->
    ('key, 'node_in) t ->
    f:('node_in -> 'node_out) ->
    ('key, 'node_out, 'witness) Map.t

  val map
    :  (module Node.S with type t = 'node_out and type Key.t = 'key) ->
    ('key, 'node_in) t ->
    get_children:('node_out -> 'key list) ->
    f:('node_in -> 'node_out) ->
    ('key, 'node_out) t

  val inv_map
    :  ('key, 'node_in) t ->
    contra_f:('node_out -> 'node_in) ->
    f:('node_in -> 'node_out) ->
    ('key, 'node_out) t

  val find : ('key, 'node) t -> 'key -> 'node option
  val find_exn : ('key, 'node) t -> 'key -> 'node
  val map_inplace : ('key, 'node) t -> f:('node -> 'node) -> unit
  val update : ('key, 'node) t -> 'node -> unit
  val edges : ('key, 'node) t -> ('key * 'key) list
  val fold : ('key, 'node) t -> init:'acc -> f:('acc -> 'node -> 'acc) -> 'acc
end