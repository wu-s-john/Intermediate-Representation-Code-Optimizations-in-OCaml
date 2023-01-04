open Core

module Binary = struct
  type t =
    [ `Add
    | `Mul
    | `Sub
    | `Div
    | `Eq
    | `Lt
    | `Gt
    | `Le
    | `Ge
    | `And
    | `Or
    ]
  [@@deriving sexp, compare, hash, eq, to_yojson]

  let to_symbol = function
    | `Add -> "+"
    | `Mul -> "*"
    | `Sub -> "-"
    | `Div -> "/"
    | `Eq -> "="
    | `Lt -> "<"
    | `Gt -> ">"
    | `Le -> "<="
    | `Ge -> ">="
    | `And -> "&"
    | `Or -> "|"

  let digest (op : t) (hashed_arg1 : Md5.t) (hashed_arg2 : Md5.t) =
    Md5.digest_string
    @@ sprintf "%s %s %s" (Md5.to_hex hashed_arg1) (to_symbol op) (Md5.to_hex hashed_arg2)
end

module Unary = struct
  type t =
    [ `Not
    | `Id
    ]
  [@@deriving sexp, compare, hash, eq, to_yojson]

  let to_string = function
    | `Not -> "not"
    | `Id -> "id"
end
