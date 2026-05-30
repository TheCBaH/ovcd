(** AST for filter patterns.

    A {!pattern} is a non-empty list of {!segment}s in left-to-right order (outermost scope first, signal name last),
    mirroring the dot-separated notation users write.

    {2 Syntax}
    {v
      pattern  ::= segment ('.' segment)*

      segment  ::= '**'                         (* zero or more components *)
                 | '*'                           (* any single component    *)
                 | '{' name (',' name)* '}'      (* one of the listed names *)
                 | name                          (* exact component name    *)

      name     ::= (printable ASCII except  . * { } ,)+
    v} *)

type segment =
  | Literal of string  (** Exact component match: the component must equal this string. *)
  | Wildcard  (** ['*']: any single component. *)
  | Glob  (** ['**']: zero or more consecutive components. *)
  | Alt of string list
      (** ['{a,b,c}']: component equal to any of the listed strings. The list is non-empty; order is preserved from the
          source text. *)
  | Name_pattern of string list
      (** Intra-segment glob: parts split on ['*'], e.g. ["ba"; ""] for ['ba*'] or [""; "ar"] for ['*ar']. ['*'] between
          consecutive parts matches any substring within the component. A bare ['*'] is {!Wildcard}. *)

type pattern = segment list
(** A parsed pattern: a non-empty list of segments. *)

val pp_segment : Format.formatter -> segment -> unit
val pp_pattern : Format.formatter -> pattern -> unit

val to_string : pattern -> string
(** Serialise back to canonical dot-separated form; round-trips through {!Filter_parser.parse}. *)
