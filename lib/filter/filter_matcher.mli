(** Matching filter patterns against VCD signal references.

    Matching is full-path and anchored: the pattern must account for every component of the reference, from the
    outermost scope down to the signal name.

    {2 Examples}
    {v
      Test_MIPS.*.pc      matches  Test_MIPS.core.pc, Test_MIPS.cpu.pc
      Test_MIPS.{core,cpu}.pc
                          matches  Test_MIPS.core.pc; NOT Test_MIPS.arm.pc
      Test_MIPS.*         matches  direct children of Test_MIPS only
      *.pc                matches  exactly 2-component paths ending in pc
      **.pc               matches  pc at any depth (0+ prefix levels)
      **                  matches  every reference
      Test_MIPS.**        matches  Test_MIPS and all its descendants
    v} *)

val anchors : Filter_ast.pattern -> string list * string list
(** [anchors pat] returns [(head, tail)] where [head] is the longest leading run of [Literal] segments and [tail] is the
    longest trailing run, both in forward (scope-first) order. Used to derive range-query bounds for indexed lookups. *)

val matches : Filter_ast.pattern -> Vcd_types.Reference.t -> bool
(** [matches pattern ref] is [true] when [pattern] covers every component of [ref]. [Glob] segments are handled by
    backtracking; alternatives are tried left-to-right with short-circuit evaluation. *)
