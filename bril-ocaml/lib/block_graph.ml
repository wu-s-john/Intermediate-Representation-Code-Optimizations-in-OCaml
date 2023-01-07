include Graph.Make (Program.Block.Key) (Program.Block)
module Block = Program.Block
module Var_location_map = Multi_map_set.Make (Variable) (Location)

module Var_block_name_map = Multi_map_set.Make (Variable) (Block.Key)
