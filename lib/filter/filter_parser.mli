(** Parsing filter patterns from strings.

    Internally uses a Menhir grammar ([Filter_grammar]) and an ocamllex lexer ([Filter_lexer]). Callers only need this
    module; the others are internal. *)

val parse : string -> (Filter_ast.pattern, string) result
(** [parse s] returns [Ok pattern] or [Error msg] without raising. An empty string is always an error. *)

val parse_exn : string -> Filter_ast.pattern
(** Like [parse] but raises [Invalid_argument] on any error. *)
