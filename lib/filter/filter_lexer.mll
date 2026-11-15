{
  open Filter_grammar

  exception Lexer_error of string

  (* Split a segment on unescaped '*'; a backslash makes the next byte literal. *)
  let split s =
    let buf = Buffer.create 16 in
    let parts = ref [] in
    let n = String.length s in
    let i = ref 0 in
    while !i < n do
      (match s.[!i] with
      | '\\' ->
          incr i;
          Buffer.add_char buf s.[!i]
      | '*' ->
          parts := Buffer.contents buf :: !parts;
          Buffer.clear buf
      | c -> Buffer.add_char buf c);
      incr i
    done;
    List.rev (Buffer.contents buf :: !parts)
}

(* Any byte except control characters, space, the metacharacters and backslash *)
let name_char = [^ '*' '{' '}' ',' '.' '\\' '\000'-'\032']

(* name_char, '*' or an escaped byte: lexes plain names, bare *, and mixed
   globs like ba* or *ar *)
let seg_item = name_char | '*' | '\\' _

rule token = parse
  (* "**" is listed first so it wins over seg_item+ on a tie. *)
  | "**"           { GLOBSTAR }
  | seg_item+ as s {
      match split s with
      | [ n ] -> NAME n
      | [ ""; "" ] -> STAR
      | parts -> NAME_GLOB parts }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | ','            { COMMA }
  | '.'            { DOT }
  | eof            { EOF }
  | '\\'           { raise (Lexer_error "dangling backslash") }
  | _ as c         { raise (Lexer_error (Printf.sprintf "unexpected character '%c'" c)) }
