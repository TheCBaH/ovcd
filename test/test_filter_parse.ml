open Filter_ast

(* Round-trip helper: parse then pretty-print *)
let show s =
  match Filter_parser.parse s with
  | Error msg -> Printf.printf "ERROR: %s\n" msg
  | Ok p -> Printf.printf "%s\n" (to_string p)

(* ------------------------------------------------------------------ *)
(*  Literals                                                            *)
(* ------------------------------------------------------------------ *)

let%expect_test "literal — single component" =
  show "clk";
  [%expect {| clk |}]

let%expect_test "literal — dot-separated path" =
  show "tb.clk";
  show "Test_MIPS.core.pc";
  [%expect {|
    tb.clk
    Test_MIPS.core.pc
  |}]

let%expect_test "literal — VCD bus notation preserved" =
  show "tb.data[7:0]";
  show "tb.mem[0].addr";
  [%expect {|
    tb.data[7:0]
    tb.mem[0].addr
  |}]

(* ------------------------------------------------------------------ *)
(*  Wildcards                                                           *)
(* ------------------------------------------------------------------ *)

let%expect_test "wildcard — single *" =
  show "*";
  show "Test_MIPS.*.pc";
  show "*.pc";
  [%expect {|
    *
    Test_MIPS.*.pc
    *.pc
  |}]

let%expect_test "glob — **" =
  show "**";
  show "**.pc";
  show "tb.**.data";
  show "Test_MIPS.**";
  [%expect {|
    **
    **.pc
    tb.**.data
    Test_MIPS.**
  |}]

(* ------------------------------------------------------------------ *)
(*  Alternation                                                         *)
(* ------------------------------------------------------------------ *)

let%expect_test "alt — two choices" =
  show "Test_MIPS.{core,cpu}.pc";
  show "*.{pc,sp}";
  [%expect {|
    Test_MIPS.{core,cpu}.pc
    *.{pc,sp}
  |}]

let%expect_test "alt — three or more choices" =
  show "**.{pc,sp,ra}";
  show "{a,b,c,d}";
  [%expect {|
    **.{pc,sp,ra}
    {a,b,c,d}
  |}]

let%expect_test "alt — single element" =
  show "{clk}";
  [%expect {| {clk} |}]

(* ------------------------------------------------------------------ *)
(*  Mixed patterns                                                      *)
(* ------------------------------------------------------------------ *)

let%expect_test "mixed — glob and alt" =
  show "{a,b}.**.{x,y}";
  show "**.{pc,sp}";
  [%expect {|
    {a,b}.**.{x,y}
    **.{pc,sp}
  |}]

let%expect_test "mixed — wildcard and alt" =
  show "*.{pc,sp}.sub";
  [%expect {| *.{pc,sp}.sub |}]

(* ------------------------------------------------------------------ *)
(*  Name_pattern (intra-segment glob)                                   *)
(* ------------------------------------------------------------------ *)

let%expect_test "name_pattern — suffix glob" =
  show "ba*";
  show "foo.ba*";
  show "Test_MIPS.co*";
  [%expect {|
    ba*
    foo.ba*
    Test_MIPS.co*
  |}]

let%expect_test "name_pattern — prefix glob" =
  show "*ar";
  show "foo.*ar";
  [%expect {|
    *ar
    foo.*ar
  |}]

let%expect_test "name_pattern — infix glob" =
  show "b*r";
  show "foo.b*r.sub";
  [%expect {|
    b*r
    foo.b*r.sub
  |}]

let%expect_test "name_pattern — multiple wildcards" =
  show "b*a*";
  [%expect {| b*a* |}]

(* ------------------------------------------------------------------ *)
(*  Parse errors                                                        *)
(* ------------------------------------------------------------------ *)

let%expect_test "error — empty string" =
  show "";
  [%expect {| ERROR: empty pattern |}]

let%expect_test "error — leading dot" =
  show ".tb";
  [%expect {| ERROR: syntax error at column 2 |}]

let%expect_test "error — trailing dot" =
  show "tb.";
  [%expect {| ERROR: syntax error at column 4 |}]

let%expect_test "error — double dot" =
  show "tb..clk";
  [%expect {| ERROR: syntax error at column 5 |}]

let%expect_test "error — unterminated alternation" =
  show "{a,b";
  [%expect {| ERROR: syntax error at column 5 |}]

let%expect_test "error — empty alternation body" =
  show "{}";
  [%expect {| ERROR: syntax error at column 3 |}]

let%expect_test "error — empty name in alternation" =
  show "{a,}";
  [%expect {| ERROR: syntax error at column 5 |}]

let%expect_test "error — space in pattern" =
  show "tb clk";
  [%expect {| ERROR: unexpected character ' ' |}]

(* ------------------------------------------------------------------ *)
(*  parse_exn                                                           *)
(* ------------------------------------------------------------------ *)

let%expect_test "parse_exn — valid" =
  let p = Filter_parser.parse_exn "tb.*.clk" in
  Printf.printf "%s\n" (to_string p);
  [%expect {| tb.*.clk |}]

let%expect_test "parse_exn — raises on error" =
  (match Filter_parser.parse_exn "" with
  | exception Invalid_argument s ->
      let starts_with prefix =
        String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix
      in
      Printf.printf "raised: %b\n" (starts_with "Filter_parser.parse_exn")
  | _ -> Printf.printf "no exception\n");
  [%expect {| raised: true |}]
