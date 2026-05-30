%{
  open Filter_ast
%}

%token <string> NAME
%token <string> NAME_GLOB
%token STAR GLOBSTAR LBRACE RBRACE COMMA DOT
%token EOF

%start <Filter_ast.pattern> pattern

%%

pattern:
  segs = separated_nonempty_list(DOT, segment) EOF { segs }

segment:
  | GLOBSTAR                                                   { Glob }
  | STAR                                                       { Wildcard }
  | LBRACE alts = separated_nonempty_list(COMMA, NAME) RBRACE { Alt alts }
  | n = NAME                                                   { Literal n }
  | p = NAME_GLOB                                              { Name_pattern (String.split_on_char '*' p) }
