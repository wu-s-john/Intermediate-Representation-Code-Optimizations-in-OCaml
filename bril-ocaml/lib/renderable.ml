module type S = sig
  type t

  val render : t -> string
end