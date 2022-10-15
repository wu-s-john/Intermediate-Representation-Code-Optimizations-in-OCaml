open Core
include String

let to_yojson = [%derive.to_yojson: string]