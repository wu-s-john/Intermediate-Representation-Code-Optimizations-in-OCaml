open Core

(* A Map and a priority queue combined together. 
   
It is a key-value map where the key are has values that are comparable. You can find the smallest key value pair using   *)

module type S = sig
  type t
  type key
  type value

  val create : unit -> t
  val get : t -> key -> value option
  val set : t -> key -> value -> unit
  val pop : t -> (key * value) option
  val empty : t -> bool
end

module Make (Key : Hashable) (Value : Comparable) :
  S with type key := Key.t and type value := Value.t = struct
  type t = {
    map : (Value.t * Key.t) Pairing_heap.Elt.t Key.Table.t;
    heap : (Value.t * Key.t) Pairing_heap.t;
  }

  let create () =
    {
      map = Key.Table.create ();
      heap = Pairing_heap.create ~cmp:(Comparable.lift Value.compare ~f:fst) ();
    }

  let get t key =
    Key.Table.find t.map key
    |> Option.map ~f:(fun elem ->
           let (value, _) = Pairing_heap.Elt.value_exn elem in
           value)

  let set t key (value : Value.t) =
    Key.Table.update t.map key ~f:(function
        | None -> Pairing_heap.add_removable t.heap (value, key)
        | Some elt ->
          Pairing_heap.remove t.heap elt;
          Pairing_heap.add_removable t.heap (value, key))

  let pop t =
    Option.bind (Pairing_heap.pop t.heap) ~f:(fun (_, key) ->
        Key.Table.find_and_remove t.map key
        |> Option.map ~f:(fun elt ->
               let (value, key) = Pairing_heap.Elt.value_exn elt in
               (key, value)))

  let empty t = Key.Table.is_empty t.map
end
