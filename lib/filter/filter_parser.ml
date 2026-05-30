let parse s =
  if String.length s = 0 then Error "empty pattern"
  else
    let lexbuf = Lexing.from_string s in
    match Filter_grammar.pattern Filter_lexer.token lexbuf with
    | p -> Ok p
    | exception Filter_lexer.Lexer_error msg -> Error msg
    | exception Filter_grammar.Error ->
        let col = Lexing.(lexbuf.lex_curr_p.pos_cnum) in
        Error (Printf.sprintf "syntax error at column %d" (col + 1))

let parse_exn s =
  match parse s with
  | Ok p -> p
  | Error msg -> invalid_arg (Printf.sprintf "Filter_parser.parse_exn: %s (input: %S)" msg s)
