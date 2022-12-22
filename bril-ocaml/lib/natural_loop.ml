module Make (Key : Node.Key) = struct
  type t = {
    nodes : Key.Set.t;
    header_node : Key.t;
    back_node : Key.t;
  }
end
