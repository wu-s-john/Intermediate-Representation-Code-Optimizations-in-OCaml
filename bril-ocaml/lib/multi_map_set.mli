open Core

module type S = Multi_map_set_intf.S

module Make (Key : Comparable) (Value : Comparable) :
  S with module Key = Key and module Value = Value
