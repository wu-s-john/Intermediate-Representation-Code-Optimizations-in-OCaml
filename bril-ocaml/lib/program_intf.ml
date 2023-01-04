open Core

type label = string [@@deriving compare, equal, sexp, hash, to_yojson]

module type S = sig
  module Var : Comparable.S

  type binary = {
    dest : string;
    typ : Type.t;
    op : Op.Binary.t;
    arg1 : Var.t;
    arg2 : Var.t;
  }
  [@@deriving sexp, compare, hash, eq, to_yojson]

  type unary = {
    dest : Var.t;
    (* dest may be a different type*)
    typ : Type.t;
    op : Op.Unary.t;
    arg : Var.t;
  }
  [@@deriving sexp, compare, hash, eq, to_yojson]

  (* Using a Variable *)
  type br = {
    arg : Var.t;
    true_label : label;
    false_label : label;
  }
  [@@deriving sexp, compare, hash, eq]

  type dest = {
    dest : Var.t;
    (* This dest might be a differnt type in the future*)
    typ : Type.t;
  }
  [@@deriving compare, equal, sexp, hash, to_yojson]

  type call = {
    func_name : string;
    args : Var.t list;
    dest : dest option; (* This dest might be a differnt type in the future*)
  }
  [@@deriving compare, equal, sexp, hash]

  type const = {
    dest : Var.t;
    (* This dest might be a differnt type in the future*)
    value : [ `Int of int | `Bool of bool ];
  }
  [@@deriving compare, equal, sexp, hash, to_yojson]

  type ret = Var.t option [@@deriving compare, equal, sexp, hash]

  module Instruction : sig
    type normal =
      [ `Const of const
      | `Binary of binary
      | `Unary of unary
      | `Call of call
      | `Print of Var.t list
      | `Nop
      ]
    [@@deriving compare, equal, sexp, hash, to_yojson]

    type control =
      [ `Jmp of label
      | `Br of br
      | `Ret of Var.t option
      ]
    [@@deriving compare, equal, sexp, hash, to_yojson]

    type t =
      [ normal
      | control
      | `Label of Var.t
      ]
    [@@deriving compare, equal, sexp, hash, to_yojson]

    val of_json_repr : Json_repr.Instruction.t -> (t, Json_repr.Error.t) Result.t
    val to_json_repr : t -> Json_repr.Instruction.t
    val to_string : t -> string
    val used_vars : t -> Var.Set.t
    val dest : normal -> Var.t option
  end

  module Block : sig
    module Key : sig
      type t = label option [@@deriving sexp, compare, hash, to_yojson]

      val uuid_root : string

      (* HACK: This avoids potential conflicts for graphviz. Labels in language my be called this *)
      val render : t -> string

      include Hashable.S with type t := t
      include Comparable.S with type t := t
    end

    type terminal_instr =
      [ `Control of Instruction.control
      | `NextLabel of label
      | `Terminal
      ]
    [@@deriving compare, equal, sexp, hash, to_yojson]

    type t = {
      label : Key.t;
      instrs : Instruction.normal list;
      terminal : terminal_instr;
    }
    [@@deriving compare, equal, sexp, hash, to_yojson]

    val key : t -> Key.t
    val children : t -> Key.t list
    val to_json_repr : t -> Json_repr.Instruction.t list
    val of_json_repr : Json_repr.Instruction.t -> (t, Json_repr.Error.t) Result.t
    val defined_variables : t -> Var.Set.t
    val defined_variable_names : t -> String.Set.t
    val used_variables : t -> Var.Set.t
    val used_variable_names : t -> String.Set.t
    val all_instrs : t -> Instruction.t list
    val render : t -> string
  end

  module Function : sig
    type blocks = (Block.Key.t, Block.t) Node_traverser.Poly.t

    type t = {
      name : string;
      args : dest list;
      (* This is a var with types *)
      blocks : blocks;
      typ : Type.t option;
    }
    [@@deriving compare, equal, sexp, hash, to_yojson]

    (* This might be private actually *)
    val to_json_repr : t -> Json_repr.Function.t
    val of_json_repr : Json_repr.Function.t -> (t, Json_repr.Error.t) Result.t
  end

  type t = { functions : Function.t list } [@@derive to_yojson]

  (* This might be private actually *)
  val to_json_repr : t -> Json_repr.Program.t
  val to_yojson : t -> Yojson.Safe.t

  (* might just work for one type of  *)
  val of_json_repr : Json_repr.Program.t -> (t, Json_repr.Error.t) Result.t
  val run_local_optimizations : t -> f:(Block.t -> Block.t) -> t
  val select_block_exn : t -> string -> (Block.Key.t, Block.t) Node_traverser.Poly.t
end