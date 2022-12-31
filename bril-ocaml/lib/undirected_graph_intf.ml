module type Poly_intf = sig
  type ('key, 'node) t

  val nodes : ('key, 'node) t -> 'node list
  val keys : ('key, 'node) t -> 'key list
  val neighbors : ('key, 'node) t -> 'key -> 'node list
  val maximum_out_degree : ('key, 'node) t -> int
  val of_alist : (module Node.Key with type t = 'key) -> ('key * 'key) list -> ('key, 'key) t

  (* Creates from an edge list and a mapping from keys and nodes *)
  val create
    :  (module Node.S with type t = 'node and type Key.t = 'key) ->
    'node list ->
    ('key * 'key) list ->
    ('key, 'node) t option
end