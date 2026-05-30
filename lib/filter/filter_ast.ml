type segment = Literal of string | Wildcard | Glob | Alt of string list | Name_pattern of string list
type pattern = segment list

let pp_segment fmt = function
  | Literal s -> Format.pp_print_string fmt s
  | Wildcard -> Format.pp_print_char fmt '*'
  | Glob -> Format.pp_print_string fmt "**"
  | Alt ss -> Format.fprintf fmt "{%s}" (String.concat "," ss)
  | Name_pattern parts -> Format.pp_print_string fmt (String.concat "*" parts)

let pp_pattern fmt segs = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.') pp_segment fmt segs
let to_string p = Format.asprintf "%a" pp_pattern p
