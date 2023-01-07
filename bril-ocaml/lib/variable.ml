open Core

module type S = sig
  type t [@@deriving compare, equal, sexp, hash, to_yojson]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val to_string : t -> string
  val of_string : string -> t Or_error.t
end

module Regular : S with type t = string = struct
  module T0 = struct
    type t = string [@@deriving compare, equal, sexp, hash, to_yojson]
  end

  include T0
  include String

  let of_string s = Ok s
end

include Regular

type ssa = {
  name : string;
  counter_id : int;
}
[@@deriving compare, equal, sexp, hash, to_yojson]

module SSA = struct
  module T = struct
    type t = {
      name : string;
      counter_id : int;
    }
    [@@deriving compare, equal, sexp, hash, to_yojson]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_string (s : t) = sprintf "%s.%d" s.name s.counter_id

  let of_string (s : string) =
    match String.split s ~on:'.' with
    | [ name; counter_id ] ->
      (match Int.of_string counter_id with
      | counter_id -> Ok { name; counter_id }
      | exception _ -> Or_error.errorf "Invalid counter_id: %s" counter_id)
    | _ -> Or_error.errorf "Invalid SSA name: %s" s
end