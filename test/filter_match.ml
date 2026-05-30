open Vcd_types

let pat s = Filter_parser.parse_exn s
let ref_ s = Reference.of_string s

(* Print "yes" / "no" for a match result *)
let check pat_str ref_str =
  let result = Filter_matcher.matches (pat pat_str) (ref_ ref_str) in
  Printf.printf "%s %s %s\n" pat_str (if result then "=>" else "!>") ref_str

(* ------------------------------------------------------------------ *)
(*  Spec examples                                                        *)
(* ------------------------------------------------------------------ *)

let%expect_test "spec: *.pc — single wildcard" =
  (* * matches exactly one component, so only 2-component paths qualify *)
  check "*.pc" "tb.pc";
  check "*.pc" "core.pc";
  check "*.pc" "Test_MIPS.core.pc";
  (* 3 components — no match *)
  check "*.pc" "pc";
  (* 1 component — no match *)
  [%expect {|
    *.pc => tb.pc
    *.pc => core.pc
    *.pc !> Test_MIPS.core.pc
    *.pc !> pc
  |}]

let%expect_test "spec: Test_MIPS.*.pc — wildcard in middle" =
  check "Test_MIPS.*.pc" "Test_MIPS.core.pc";
  check "Test_MIPS.*.pc" "Test_MIPS.cpu.pc";
  check "Test_MIPS.*.pc" "Test_MIPS.arm.pc";
  (* any name — still matches *)
  check "Test_MIPS.*.pc" "Test_MIPS.arm.x.pc";
  (* extra level — no match *)
  check "Test_MIPS.*.pc" "Test_MIPS.pc";
  (* missing middle — no match *)
  [%expect
    {|
    Test_MIPS.*.pc => Test_MIPS.core.pc
    Test_MIPS.*.pc => Test_MIPS.cpu.pc
    Test_MIPS.*.pc => Test_MIPS.arm.pc
    Test_MIPS.*.pc !> Test_MIPS.arm.x.pc
    Test_MIPS.*.pc !> Test_MIPS.pc
  |}]

let%expect_test "spec: Test_MIPS.{core,cpu}.pc — alternation" =
  check "Test_MIPS.{core,cpu}.pc" "Test_MIPS.core.pc";
  check "Test_MIPS.{core,cpu}.pc" "Test_MIPS.cpu.pc";
  check "Test_MIPS.{core,cpu}.pc" "Test_MIPS.arm.pc";
  (* not in set *)
  [%expect
    {|
    Test_MIPS.{core,cpu}.pc => Test_MIPS.core.pc
    Test_MIPS.{core,cpu}.pc => Test_MIPS.cpu.pc
    Test_MIPS.{core,cpu}.pc !> Test_MIPS.arm.pc
  |}]

let%expect_test "spec: Test_MIPS.* — direct children only" =
  check "Test_MIPS.*" "Test_MIPS.core";
  check "Test_MIPS.*" "Test_MIPS.cpu";
  check "Test_MIPS.*" "Test_MIPS.core.pc";
  (* too deep *)
  check "Test_MIPS.*" "other.core";
  [%expect
    {|
    Test_MIPS.* => Test_MIPS.core
    Test_MIPS.* => Test_MIPS.cpu
    Test_MIPS.* !> Test_MIPS.core.pc
    Test_MIPS.* !> other.core
  |}]

let%expect_test "spec: *.{pc,sp} — wildcard with alt" =
  check "*.{pc,sp}" "tb.pc";
  check "*.{pc,sp}" "tb.sp";
  check "*.{pc,sp}" "tb.ra";
  check "*.{pc,sp}" "a.b.pc";
  (* extra level *)
  [%expect {|
    *.{pc,sp} => tb.pc
    *.{pc,sp} => tb.sp
    *.{pc,sp} !> tb.ra
    *.{pc,sp} !> a.b.pc
  |}]

(* ------------------------------------------------------------------ *)
(*  Glob (**) semantics                                                 *)
(* ------------------------------------------------------------------ *)

let%expect_test "glob: ** matches everything at any depth" =
  check "**" "clk";
  check "**" "tb.clk";
  check "**" "Test_MIPS.core.pc";
  check "**" "a.b.c.d.e";
  [%expect {|
    ** => clk
    ** => tb.clk
    ** => Test_MIPS.core.pc
    ** => a.b.c.d.e
  |}]

let%expect_test "glob: **.pc — leaf at any depth" =
  check "**.pc" "pc";
  (* ** matches 0 *)
  check "**.pc" "tb.pc";
  check "**.pc" "Test_MIPS.core.pc";
  check "**.pc" "a.b.c.pc";
  check "**.pc" "a.b.c.sp";
  (* wrong leaf *)
  [%expect
    {|
    **.pc => pc
    **.pc => tb.pc
    **.pc => Test_MIPS.core.pc
    **.pc => a.b.c.pc
    **.pc !> a.b.c.sp
  |}]

let%expect_test "glob: Test_MIPS.** — subtree including root" =
  check "Test_MIPS.**" "Test_MIPS";
  (* ** matches 0 *)
  check "Test_MIPS.**" "Test_MIPS.core";
  check "Test_MIPS.**" "Test_MIPS.core.pc";
  check "Test_MIPS.**" "Test_MIPS.a.b.c.d";
  check "Test_MIPS.**" "other.core";
  [%expect
    {|
    Test_MIPS.** => Test_MIPS
    Test_MIPS.** => Test_MIPS.core
    Test_MIPS.** => Test_MIPS.core.pc
    Test_MIPS.** => Test_MIPS.a.b.c.d
    Test_MIPS.** !> other.core
  |}]

let%expect_test "glob: tb.**.data — data anywhere under tb" =
  check "tb.**.data" "tb.data";
  (* ** matches 0 *)
  check "tb.**.data" "tb.sub.data";
  check "tb.**.data" "tb.a.b.data";
  check "tb.**.data" "tb.a.b.notdata";
  check "tb.**.data" "other.data";
  [%expect
    {|
    tb.**.data => tb.data
    tb.**.data => tb.sub.data
    tb.**.data => tb.a.b.data
    tb.**.data !> tb.a.b.notdata
    tb.**.data !> other.data
  |}]

let%expect_test "glob: ** with alt — **.{pc,sp}" =
  check "**.{pc,sp}" "pc";
  check "**.{pc,sp}" "tb.pc";
  check "**.{pc,sp}" "a.b.c.sp";
  check "**.{pc,sp}" "a.b.c.ra";
  [%expect {|
    **.{pc,sp} => pc
    **.{pc,sp} => tb.pc
    **.{pc,sp} => a.b.c.sp
    **.{pc,sp} !> a.b.c.ra
  |}]

(* ------------------------------------------------------------------ *)
(*  Literal — exact match semantics                                     *)
(* ------------------------------------------------------------------ *)

let%expect_test "literal: exact full path required" =
  check "tb.sub.data" "tb.sub.data";
  check "tb.sub.data" "tb.sub.datax";
  (* suffix differs *)
  check "tb.sub.data" "tb.sub";
  (* too short *)
  check "tb.sub.data" "tb.sub.data.x";
  (* too long *)
  [%expect
    {|
    tb.sub.data => tb.sub.data
    tb.sub.data !> tb.sub.datax
    tb.sub.data !> tb.sub
    tb.sub.data !> tb.sub.data.x
  |}]

let%expect_test "literal: single-component path" =
  check "clk" "clk";
  check "clk" "tb.clk";
  [%expect {|
    clk => clk
    clk !> tb.clk
  |}]

(* ------------------------------------------------------------------ *)
(*  VCD bus notation in names                                           *)
(* ------------------------------------------------------------------ *)

let%expect_test "bus notation preserved in match" =
  check "tb.data[7:0]" "tb.data[7:0]";
  check "tb.data[7:0]" "tb.data[3:0]";
  (* different range *)
  check "tb.*.data[7:0]" "tb.sub.data[7:0]";
  [%expect
    {|
    tb.data[7:0] => tb.data[7:0]
    tb.data[7:0] !> tb.data[3:0]
    tb.*.data[7:0] => tb.sub.data[7:0]
  |}]

(* ------------------------------------------------------------------ *)
(*  Alternation edge cases                                              *)
(* ------------------------------------------------------------------ *)

let%expect_test "alt: order of alternatives does not affect matching" =
  check "{b,a}" "a";
  check "{b,a}" "b";
  check "{b,a}" "c";
  [%expect {|
    {b,a} => a
    {b,a} => b
    {b,a} !> c
  |}]

let%expect_test "alt: single-element behaves like literal" =
  check "{pc}" "pc";
  check "{pc}" "tb.pc";
  (* only one component in pattern *)
  [%expect {|
    {pc} => pc
    {pc} !> tb.pc
  |}]

(* ------------------------------------------------------------------ *)
(*  Name_pattern — intra-segment glob                                   *)
(* ------------------------------------------------------------------ *)

let%expect_test "name_pattern: suffix glob foo.ba*" =
  (* matches ba, bar, ba1 — anything starting with ba in that slot *)
  check "foo.ba*" "foo.ba";
  check "foo.ba*" "foo.bar";
  check "foo.ba*" "foo.ba1";
  check "foo.ba*" "foo.arm";
  (* wrong prefix *)
  check "foo.ba*" "foo.ba.x";
  (* extra component *)
  [%expect
    {|
    foo.ba* => foo.ba
    foo.ba* => foo.bar
    foo.ba* => foo.ba1
    foo.ba* !> foo.arm
    foo.ba* !> foo.ba.x
  |}]

let%expect_test "name_pattern: prefix glob *ar" =
  check "*ar" "bar";
  check "*ar" "ar";
  check "*ar" "foobar";
  check "*ar" "ba";
  [%expect {|
    *ar => bar
    *ar => ar
    *ar => foobar
    *ar !> ba
  |}]

let%expect_test "name_pattern: infix glob b*r" =
  check "b*r" "br";
  check "b*r" "bar";
  check "b*r" "boor";
  check "b*r" "baz";
  [%expect {|
    b*r => br
    b*r => bar
    b*r => boor
    b*r !> baz
  |}]

let%expect_test "name_pattern: multiple wildcards b*a*" =
  check "b*a*" "ba";
  check "b*a*" "baa";
  check "b*a*" "bxayq";
  check "b*a*" "bz";
  [%expect {|
    b*a* => ba
    b*a* => baa
    b*a* => bxayq
    b*a* !> bz
  |}]

let%expect_test "name_pattern: combined with other segments" =
  check "Test_MIPS.co*.pc" "Test_MIPS.core.pc";
  check "Test_MIPS.co*.pc" "Test_MIPS.cpu.pc";
  (* no co prefix *)
  check "**.co*" "Test_MIPS.core";
  check "**.co*" "Test_MIPS.cpu";
  [%expect
    {|
    Test_MIPS.co*.pc => Test_MIPS.core.pc
    Test_MIPS.co*.pc !> Test_MIPS.cpu.pc
    **.co* => Test_MIPS.core
    **.co* !> Test_MIPS.cpu
  |}]
