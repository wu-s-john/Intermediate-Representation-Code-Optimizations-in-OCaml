open Core

module T = struct
  type t = Label.t option [@@deriving sexp, compare, hash, yojson]
end

include T

let uuid_root =
  let rand_suffix = Uuid.to_string @@ Uuid.create_random Random.State.default in
  sprintf "root_%s" (String.prefix rand_suffix 4)

(* HACK: This avoids potential conflicts for graphviz. Labels in language my be called this *)
let render (key : t) = sprintf "\"%s\"" (Option.value ~default:uuid_root key)

include Hashable.Make (T)
include Comparable.Make (T)
