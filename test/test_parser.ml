open Vcd_ast

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let parse_header s = Vcd_parser.parse_header Vcd_lexer.token (Lexing.from_string s)
let show_opt f = function None -> "(none)" | Some x -> f x

let rec pp_scope indent s =
  Format.printf "%sscope %s %s\n" indent (Vcd.string_of_scope_type s.s_type) s.s_name;
  List.iter
    (fun v ->
      Format.printf "%s  var %s %d %s%s\n" indent (Vcd.string_of_var_type v.v_type) v.size v.ref
        (match v.index with None -> "" | Some i -> " " ^ i))
    s.vars;
  List.iter (pp_scope (indent ^ "  ")) s.children

let pp_header h =
  Format.printf "date:      %s\n" (show_opt Fun.id h.date);
  Format.printf "version:   %s\n" (show_opt Fun.id h.version);
  Format.printf "timescale: %s\n" (show_opt Fun.id h.timescale);
  Format.printf "comment:   %s\n" (show_opt Fun.id h.comment);
  List.iter (pp_scope "  ") h.scopes

(* ------------------------------------------------------------------ *)
(*  Meta-data fields                                                    *)
(* ------------------------------------------------------------------ *)

let%expect_test "date parsed" =
  pp_header (parse_header "$date Sat Apr 29 09:34:13 2017 $end $enddefinitions $end");
  [%expect
    {|
    date:      Sat Apr 29 09:34:13 2017
    version:   (none)
    timescale: (none)
    comment:   (none) |}]

let%expect_test "version and timescale" =
  pp_header (parse_header "$version Icarus Verilog $end $timescale 1ns $end $enddefinitions $end");
  [%expect {|
    date:      (none)
    version:   Icarus Verilog
    timescale: 1ns
    comment:   (none) |}]

let%expect_test "comment" =
  pp_header (parse_header "$comment Csum: 1 9dbd5495 $end $enddefinitions $end");
  [%expect {|
    date:      (none)
    version:   (none)
    timescale: (none)
    comment:   Csum: 1 9dbd5495 |}]

(* ------------------------------------------------------------------ *)
(*  $var declarations                                                   *)
(* ------------------------------------------------------------------ *)

let%expect_test "var inside scope" =
  let h = parse_header "$scope module top $end $var wire 1 ! clk $end $upscope $end $enddefinitions $end" in
  List.iter (pp_scope "") h.scopes;
  [%expect {|
    scope module top
      var wire 1 clk |}]

let%expect_test "var with bit-index" =
  let h = parse_header "$scope module top $end $var wire 8 ! bus [7:0] $end $upscope $end $enddefinitions $end" in
  List.iter (pp_scope "") h.scopes;
  [%expect {|
    scope module top
      var wire 8 bus [7:0] |}]

let%expect_test "var types" =
  let h =
    parse_header
      "$scope module m $end $var reg     4 a cnt  $end $var integer 32 b idx $end $var real    1 c val  $end $var \
       event   1 d ev   $end $upscope $end $enddefinitions $end"
  in
  List.iter (pp_scope "") h.scopes;
  [%expect
    {|
    scope module m
      var reg 4 cnt
      var integer 32 idx
      var real 1 val
      var event 1 ev |}]

(* ------------------------------------------------------------------ *)
(*  $scope / $upscope nesting                                           *)
(* ------------------------------------------------------------------ *)

let%expect_test "nested scopes" =
  let h =
    parse_header
      "$scope module top $end $var wire 1 ! clk $end $scope module sub $end $var reg 8 \" data $end $upscope $end \
       $upscope $end $enddefinitions $end"
  in
  List.iter (pp_scope "") h.scopes;
  [%expect {|
    scope module top
      var wire 1 clk
      scope module sub
        var reg 8 data |}]

let%expect_test "multiple root scopes" =
  let h =
    parse_header
      "$scope module a $end $var wire 1 ! x $end $upscope $end $scope module b $end $var wire 1 \" y $end $upscope \
       $end $enddefinitions $end"
  in
  List.iter (pp_scope "") h.scopes;
  [%expect {|
    scope module a
      var wire 1 x
    scope module b
      var wire 1 y |}]

let%expect_test "scope types" =
  let h =
    parse_header
      "$scope task  t1 $end $upscope $end $scope fork  f1 $end $upscope $end $scope begin b1 $end $upscope $end \
       $enddefinitions $end"
  in
  List.iter (pp_scope "") h.scopes;
  [%expect {|
    scope task t1
    scope fork f1
    scope begin b1 |}]

(* ------------------------------------------------------------------ *)
(*  parse_vector unit tests                                             *)
(* ------------------------------------------------------------------ *)

let show_value v =
  let open Vcd_types.Value in
  let tag, body =
    match v with
    | Scalar l -> ("Scalar", String.make 1 (char_of_logic l))
    | Int n -> ("Int", string_of_int n)
    | Int64 n -> ("Int64", Int64.to_string n)
    | Bytes b -> ("Bytes", Printf.sprintf "<%dB>" (Bytes.length b))
    | Scalars ls -> ("Scalars", String.concat "" (List.map (fun l -> String.make 1 (char_of_logic l)) ls))
    | Other s -> ("Other", s)
    | Real f -> ("Real", string_of_float f)
  in
  Printf.sprintf "%s(%s)" tag body

let%expect_test "parse_vector all-zero" =
  print_string (show_value (parse_vector "0000"));
  [%expect {| Int(0) |}]

let%expect_test "parse_vector small int" =
  print_string (show_value (parse_vector "1010"));
  [%expect {| Int(10) |}]

let%expect_test "parse_vector single 1" =
  print_string (show_value (parse_vector "1"));
  [%expect {| Int(1) |}]

let%expect_test "parse_vector 23-bit all-ones" =
  let s = String.make 23 '1' in
  print_string (show_value (parse_vector s));
  [%expect {| Int(8388607) |}]

let%expect_test "parse_vector 24 bits (Int ceiling)" =
  let s = String.make 24 '1' in
  print_string (show_value (parse_vector s));
  [%expect {| Int(16777215) |}]

let%expect_test "parse_vector 25 bits (first Int64)" =
  let s = String.make 25 '1' in
  print_string (show_value (parse_vector s));
  [%expect {| Int64(33554431) |}]

let%expect_test "parse_vector 64 bits (Int64 ceiling)" =
  (* 64 all-ones = max_int64 = 9223372036854775807 … except the MSB is the
     sign bit in OCaml's Int64, so all-ones is -1 in two's-complement. *)
  let s = String.make 64 '1' in
  print_string (show_value (parse_vector s));
  [%expect {| Int64(-1) |}]

let%expect_test "parse_vector 65 bits (first Bytes)" =
  let s = String.make 65 '1' in
  print_string (show_value (parse_vector s));
  [%expect {| Bytes(<9B>) |}]

let%expect_test "parse_vector with X" =
  print_string (show_value (parse_vector "10x0"));
  [%expect {| Scalars(10x0) |}]

let%expect_test "parse_vector all-Z" =
  print_string (show_value (parse_vector "zzzz"));
  [%expect {| Scalars(zzzz) |}]

let%expect_test "parse_vector 9-state falls to Other" =
  print_string (show_value (parse_vector "10u0"));
  [%expect {| Other(10u0) |}]
