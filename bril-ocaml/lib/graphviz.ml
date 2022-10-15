open Core

type ('key, 'node) t = {
  render_key : 'key -> string;
  render_node : 'node -> string;
  get_key : 'node -> 'key;
}

let create ~render_key ~render_node ~get_key = { render_key; render_node; get_key }

let map ~(contra_f_node : 'node_out -> 'node_in) (t : ('key, 'node_in) t) : ('key, 'node_out) t =
  {
    render_key = (fun key -> t.render_key key);
    render_node = (fun node -> t.render_node (contra_f_node node));
    get_key = (fun node -> t.get_key @@ contra_f_node node);
  }

type label = string

type node = {
  label : label;
  description : string;
}

type internal = {
  nodes : node list;
  edges : (label * label) list;
}

let render_node file_name { nodes; edges } =
  let rendered_edges =
    List.map edges ~f:(fun (a, b) -> Core.sprintf !"%s -> %s" a b) |> String.concat ~sep:"\n"
  in
  let rendered_nodes =
    List.map nodes ~f:(fun { label; description } ->
        Core.sprintf !"%s [shape=box, label=\"%s\"]\n" label description)
    |> String.concat ~sep:"\n"
  in
  Async.Writer.save
    file_name
    ~contents:(sprintf !"digraph {\n%s\n%s}" rendered_nodes rendered_edges)

let draw
    (type key node)
    (file_name : string)
    (traverser : (key, node) Node_traverser.Poly.t)
    (t : (key, node) t)
  =
  let edges =
    Node_traverser.Poly.edges traverser
    |> List.map ~f:(fun (src, dest) -> (t.render_key src, t.render_key dest))
  in
  let nodes =
    Node_traverser.Poly.nodes traverser
    |> List.map ~f:(fun node ->
           { label = t.render_node node; description = t.render_key (t.get_key node) })
  in
  render_node file_name { nodes; edges }
