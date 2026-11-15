type segment = Literal of string | Wildcard | Glob | Alt of string list | Name_pattern of string list
type pattern = segment list

let escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if c = '\\' || c = '*' || c = '{' || c = '}' || c = ',' || c = '.' || c <= ' ' then Buffer.add_char buf '\\';
      Buffer.add_char buf c)
    s;
  Buffer.contents buf

let pp_segment fmt = function
  | Literal s -> Format.pp_print_string fmt (escape s)
  | Wildcard -> Format.pp_print_char fmt '*'
  | Glob -> Format.pp_print_string fmt "**"
  | Alt ss -> Format.fprintf fmt "{%s}" (String.concat "," (List.map escape ss))
  | Name_pattern parts -> Format.pp_print_string fmt (String.concat "*" (List.map escape parts))

let pp_pattern fmt segs = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.') pp_segment fmt segs
let to_string p = Format.asprintf "%a" pp_pattern p
