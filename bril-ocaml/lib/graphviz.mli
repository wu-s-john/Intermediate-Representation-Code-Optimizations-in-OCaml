open Async

type ('key, 'node) t

val create : render_key:('key -> string) -> render_node:('node -> string) -> ('key, 'node) t

val map
  :  contra_f_key:('key_out -> 'key_in) ->
  contra_f_node:('node_out -> 'node_in) ->
  ('key_in, 'node_in) t ->
  ('key_out, 'node_out) t

val draw
  :  (module Node.S with type t = 'node and type Key.t = 'key) ->
  string ->
  ('key, 'node) Node_traverser.Poly.t ->
  ('key, 'node) t ->
  unit Deferred.t
