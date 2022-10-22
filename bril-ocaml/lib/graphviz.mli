open Async
open Core

type ('key, 'node) t

val create
  :  render_key:('key -> string) ->
  render_node:('node -> string) ->
  get_key:('node -> 'key) ->
  ('key, 'node) t

val map : contra_f_node:('node_out -> 'node_in) -> ('key, 'node_in) t -> ('key, 'node_out) t
val draw : string -> ('key, 'node) Node_traverser.Poly.t -> ('key, 'node) t -> unit Deferred.t
val render_instructions : Program.Instruction.t list -> string

val draw_multimap
  :  file_name:string ->
  render_key:('key -> string) ->
  ('key, ('key, 'comp) Set.t, 'comp) Map.t ->
  unit Deferred.t
