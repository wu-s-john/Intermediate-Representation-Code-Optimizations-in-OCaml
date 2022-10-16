open Core

type t =
  | Int_typ [@name "int"]
  | Bool_typ [@name "bool"]
  | Ptr_typ of t [@name "ptr"]
[@@deriving compare, equal, sexp, hash, to_yojson]

let rec to_yojson (t : t) : Yojson.Safe.t =
  match t with
  | Int_typ -> `String "int"
  | Bool_typ -> `String "bool"
  | Ptr_typ t -> `Assoc [ ("ptr", to_yojson t) ]

let rec of_yojson (json : Yojson.Safe.t) : (t, string) Result.t =
  match json with
  | `String "int" -> Ok Int_typ
  | `String "bool" -> Ok Bool_typ
  | `Assoc [ ("ptr", typ) ] -> of_yojson typ
  | bad_json -> Error (sprintf !"Cannot handle Json %s" @@ Yojson.Safe.pretty_to_string bad_json)

let rec to_string (t : t) : string =
  match t with
  | Int_typ -> "int"
  | Bool_typ -> "bool"
  | Ptr_typ t -> sprintf "ptr %s" (to_string t)
