open Ppxlib
module List = ListLabels

let make_methods ~(loc : location) ~(is_poly : bool) (constructors : constructor_declaration list) =
  let (module Ast) = Ast_builder.make loc in
  let v_patt = match is_poly with
    | true -> fun name -> Ast.ppat_variant name None
    | false -> fun name -> Ast.ppat_construct { txt = (Lident name); loc } None
  and v_expr = match is_poly with
    | true -> fun name -> Ast.pexp_variant name None
    | false -> fun name -> Ast.pexp_construct { txt = (Lident name); loc } None
  in
  let (to_cases, of_cases) =
    List.map constructors ~f:(
      fun cd ->
        let name = cd.pcd_name.txt in
        let to_case = {
          pc_lhs = v_patt name;
          pc_guard = None;
          pc_rhs = [%expr `String [%e Ast.estring name] ];
        } in
        let of_case = {
          pc_lhs = Ast.ppat_variant "String" (Some (Ast.pstring name));
          pc_guard = None;
          pc_rhs = [%expr Ok ([%e v_expr name]) ];
        } in
        (to_case, of_case)
    )
    |> List.split
  in
  let of_default_case = {
    pc_lhs = [%pat? yj ];
    pc_guard = None;
    pc_rhs = [%expr Error (Printf.sprintf "Invalid value: %s" (Yojson.Safe.to_string yj)) ];
  } in
  let of_cases = of_cases @ [of_default_case] in
  let to_yojson = [%stri let to_yojson = [%e Ast.pexp_function to_cases]] in
  let of_yojson = [%stri let of_yojson = [%e Ast.pexp_function of_cases] ] in
  [to_yojson; of_yojson]

let type_impl ~(loc : location) (td : type_declaration) =
  match td with
  | {ptype_kind = (Ptype_abstract | Ptype_record _ | Ptype_open); _} ->
    Location.raise_errorf ~loc "Cannot derive yojson_str_enum for non variant types"
  | {ptype_kind = Ptype_variant constructors; _} -> begin
      let invalid_constructors =
        List.filter_map constructors ~f:(
          fun cd -> match cd.pcd_args with
            | (Pcstr_tuple [] | Pcstr_record []) -> None
            | _ -> Some (cd)
        )
      in
      if (List.length invalid_constructors) > 0 then
        Location.raise_errorf ~loc "Cannot derive yojson_str_enum for variant types with constructor args";
      match is_polymorphic_variant td ~sig_:false with
      | `Definitely | `Maybe -> make_methods ~loc ~is_poly:true constructors
      | `Surely_not -> make_methods ~loc ~is_poly:false constructors
    end

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  (* [loc] is "location", not "lines of code" *)
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(type_impl ~loc)
  |> List.concat

let yojson_str_enum =
  Deriving.add
    "yojson_str_enum"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate_impl)