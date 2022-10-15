open Async

type ('key, 'node) t

val create
  :  render_key:('key -> string) ->
  render_node:('node -> string) ->
  get_key:('node -> 'key) ->
  ('key, 'node) t

val map : contra_f_node:('node_out -> 'node_in) -> ('key, 'node_in) t -> ('key, 'node_out) t
val draw : string -> ('key, 'node) Node_traverser.Poly.t -> ('key, 'node) t -> unit Deferred.t
