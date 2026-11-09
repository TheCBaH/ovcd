type t = {
  path : string;
  parts : string list;
  scope_types : string list;
  id : string;
  width : int;
  var_type : string;
  index : string option;
}

let make names kinds (v : Vcd_ast.var_decl) =
  let parts = names @ [ v.ref ] in
  {
    path = Vcd_types.Reference.(to_string (of_list parts));
    parts;
    scope_types = kinds;
    id = Vcd_types.ID.to_string v.id;
    width = v.size;
    var_type = Vcd.string_of_var_type v.v_type;
    index = v.index;
  }

let of_header (h : Vcd_ast.header) =
  let rec scope names kinds (s : Vcd_ast.scope_node) =
    let names = names @ [ s.s_name ] and kinds = kinds @ [ Vcd.string_of_scope_type s.s_type ] in
    Seq.append
      (Seq.map (make names kinds) (List.to_seq s.vars))
      (Seq.flat_map (scope names kinds) (List.to_seq s.children))
  in
  Seq.flat_map (scope [] []) (List.to_seq h.scopes)

let jsont =
  Jsont.Object.map ~kind:"signal" (fun path parts scope_types id width var_type index ->
      { path; parts; scope_types; id; width; var_type; index })
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun s -> s.path)
  |> Jsont.Object.mem "parts" (Jsont.list Jsont.string) ~enc:(fun s -> s.parts)
  |> Jsont.Object.mem "scope_types" (Jsont.list Jsont.string) ~enc:(fun s -> s.scope_types)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun s -> s.id)
  |> Jsont.Object.mem "width" Jsont.int ~enc:(fun s -> s.width)
  |> Jsont.Object.mem "var_type" Jsont.string ~enc:(fun s -> s.var_type)
  |> Jsont.Object.opt_mem "index" Jsont.string ~enc:(fun s -> s.index)
  |> Jsont.Object.finish

let to_line s =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify jsont s with Ok l -> l | Error e -> invalid_arg e

let of_line l = Jsont_bytesrw.decode_string jsont l

let write oc signals =
  Seq.iter
    (fun s ->
      output_string oc (to_line s);
      output_char oc '\n')
    signals

let read ic =
  let rec next () =
    match input_line ic with exception End_of_file -> Seq.Nil | "" -> next () | l -> Seq.Cons (of_line l, next)
  in
  next
