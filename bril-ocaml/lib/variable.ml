open Core

module Regular = struct
  module T0 = struct
    type t = string [@@deriving compare, equal, sexp, hash, to_yojson]
  end

  include T0
  include String
end

include Regular

module SSA = struct
  module T = struct
    type t = {
      name : string;
      counter_id : int;
    }
    [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module type S = sig
  type t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end