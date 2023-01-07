open Core

module type S = sig
  module Var : Comparable.S

  type binary = {
    dest : Var.t;
    typ : Type.t;
    op : Op.Binary.t;
    arg1 : Var.t;
    arg2 : Var.t;
  }
  [@@deriving sexp, compare, hash, eq]

  type unary = {
    dest : Var.t;
    (* dest may be a different type*)
    typ : Type.t;
    op : Op.Unary.t;
    arg : Var.t;
  }
  [@@deriving sexp, compare, hash, eq]

  (* Using a Variable *)
  type br = {
    arg : Var.t;
    true_label : Label.t;
    false_label : Label.t;
  }
  [@@deriving sexp, compare, hash, eq]

  type dest = {
    dest : Var.t;
    (* This dest might be a differnt type in the future*)
    typ : Type.t;
  }
  [@@deriving compare, equal, sexp, hash]

  type 'dest call = {
    func_name : string;
    args : Var.t list;
    dest : 'dest;
  }
  [@@deriving compare, equal, sexp, hash]

  type const = {
    dest : Var.t;
    (* This dest might be a differnt type in the future*)
    value : [ `Int of int | `Bool of bool ];
  }
  [@@deriving compare, equal, sexp, hash]

  type ret = Var.t option [@@deriving compare, equal, sexp, hash]

  module Instruction : sig
    type 'dest definition_instr =
      [ `Const of const
      | `Binary of binary
      | `Unary of unary
      | `Call of 'dest call
      ]
    [@@deriving compare, eq, sexp, hash]

    module Definition : sig
      type t =
        [ `Const of const
        | `Binary of binary
        | `Unary of unary
        | `Call of dest call
        ]
      [@@deriving compare, equal, hash]

      val sexp_of_t : t -> Base.Sexp.t
      val t_of_sexp : Base.Sexp.t -> t
      val assignment_var : t -> Var.t
    end

    type normal =
      [ `Const of const
      | `Binary of binary
      | `Unary of unary
      | `Call of dest option call
      | `Print of Var.t list
      | `Nop
      ]
    [@@deriving compare, equal, sexp, hash]

    type control =
      [ `Jmp of Label.t
      | `Br of br
      | `Ret of Var.t option
      ]
    [@@deriving compare, equal, sexp, hash]

    type t =
      [ normal
      | control
      | `Label of Label.t
      ]
    [@@deriving compare, equal, sexp, hash, yojson]

    val of_definition : Definition.t -> t
    val to_definition : t -> Definition.t option
    val to_string : t -> string
    val used_vars : t -> Var.Set.t
    val declared_var : t -> Var.t option
    val normal_declared_var : normal -> Var.t option
  end

  module Block : sig
    module Key = Block_name

    type terminal_instr =
      [ `Control of Instruction.control
      | `NextLabel of Label.t
      | `Terminal
      ]
    [@@deriving compare, equal, sexp, hash, to_yojson]

    type t = {
      label : Label.t option;
      instrs : Instruction.normal list;
      terminal : terminal_instr;
    }
    [@@deriving compare, equal, sexp, hash, to_yojson]

    val key : t -> Key.t
    val children : t -> Key.t list
    val defined_variables : t -> Var.Set.t
    val defined_variable_names : t -> String.Set.t
    val used_variables : t -> Var.Set.t
    val used_variable_names : t -> String.Set.t
    val all_instrs : t -> Instruction.t list
    val all_instrs_with_line : t -> (int * Instruction.t) list
    val all_normal_instrs_with_line : t -> (int * Instruction.normal) list
    val render : t -> string
    val terminal : t -> terminal_instr
  end

  module Function : sig
    type blocks = (Block.Key.t, Block.t) Node_traverser.Poly.t

    type t = {
      name : string;
      args : Func_arg.t list;
      blocks : blocks;
      typ : Type.t option;
    }
    [@@deriving yojson]

    val variable_location_map : t -> Location.Set.t Var.Map.t
    val variable_block_map : t -> Block.Key.Set.t Var.Map.t
    val variables : t -> Var.Set.t
  end

  type t = { functions : Function.t list } [@@deriving yojson]

  val run_local_optimizations : t -> f:(Block.t -> Block.t) -> t
  val select_block_exn : t -> string -> (Block.Key.t, Block.t) Node_traverser.Poly.t
  val head_function_blocks_exn : t -> (Block.Key.t, Block.t) Node_traverser.Poly.t
end
