include Core

module T = struct
  type t = {block_name: string option; line: int} [@@deriving sexp, compare, hash, eq]
end

include T
include Comparable.Make(T)
include Hashable.Make(T)