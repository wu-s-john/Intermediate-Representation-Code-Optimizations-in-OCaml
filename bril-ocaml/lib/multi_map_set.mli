module type Elem = Multi_map_set_intf.Elem

module type S = Multi_map_set_intf.S

module Make (Key : Multi_map_set_intf.Elem) (Value : Multi_map_set_intf.Elem) :
  S with module Key = Key and module Value = Value
