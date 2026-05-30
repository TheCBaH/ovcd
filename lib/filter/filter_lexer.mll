{
  open Filter_grammar

  exception Lexer_error of string
}

(* Printable ASCII except the five DSL metacharacters *)
let name_char = [^ '*' '{' '}' ',' '.' '\000'-'\032' '\127'-'\255']

(* name_char or '*' — used to lex intra-segment globs like ba* or *ar *)
let seg_char = name_char | '*'

rule token = parse
  | "**"           { GLOBSTAR }
  (* seg_char+ matches plain names, bare *, and mixed globs like ba* or *ar.
     "**" is listed first so it takes priority when input starts with **. *)
  | seg_char+ as s {
      if s = "*" then STAR
      else if String.contains s '*' then NAME_GLOB s
      else NAME s }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | ','            { COMMA }
  | '.'            { DOT }
  | eof            { EOF }
  | _ as c         { raise (Lexer_error (Printf.sprintf "unexpected character '%c'" c)) }
