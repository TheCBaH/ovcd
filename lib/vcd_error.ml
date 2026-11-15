type pos = { file : string; line : int; col : int }

type error =
  [ `Syntax of pos * string
  | `Incomplete of pos * string
  | `Fst of string * Fst.Error.t
  | `Io of string
  | `Not_fst of string
  | `Closed of string
  | `Superseded of string ]

exception E of error
exception Located of pos * string

let pos_of_lexing (p : Lexing.position) =
  { file = p.Lexing.pos_fname; line = p.Lexing.pos_lnum; col = p.Lexing.pos_cnum - p.Lexing.pos_bol }

let located p msg = raise (Located (pos_of_lexing p, msg))

let pp_pos ppf p =
  if p.file = "" then Format.fprintf ppf "line %d col %d" p.line p.col
  else Format.fprintf ppf "%s line %d col %d" p.file p.line p.col

let pp_error ppf : error -> unit = function
  | `Syntax (p, m) | `Incomplete (p, m) -> Format.fprintf ppf "%s at %a" m pp_pos p
  | `Fst (name, e) -> Format.fprintf ppf "%s: %s" name (Fst.Error.to_string e)
  | `Io m -> Format.pp_print_string ppf m
  | `Not_fst path -> Format.fprintf ppf "%s: not an FST file" path
  | `Closed name -> Format.fprintf ppf "%s: handle closed" name
  | `Superseded name -> Format.fprintf ppf "%s: stream superseded" name

let to_string e = Format.asprintf "%a" pp_error e
let catch f = try Ok (f ()) with E e -> Error e
let () = Printexc.register_printer (function E e -> Some ("Vcd.Error.E: " ^ to_string e) | _ -> None)
