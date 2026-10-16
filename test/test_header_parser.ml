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
  Option.iter (Format.printf "timezero:  %Ld\n") h.timezero;
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
    | Port s -> ("Port", s)
    | String s -> ("String", s)
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
  [%expect {| Scalars(10u0) |}]

(* ------------------------------------------------------------------ *)
(*  SystemVerilog / GTKWave declarations                               *)
(* ------------------------------------------------------------------ *)

let%expect_test "SystemVerilog and GTKWave variable types" =
  pp_header
    (parse_header
       {|$scope module m $end
$var bit 1 ! a $end
$var byte 8 " b [7:0] $end
$var shortint 16 # c $end
$var int 32 $ d $end
$var longint 64 % e $end
$var enum 2 & f $end
$var shortreal 32 ' g $end
$var sparray 1 ( h $end
$var port 4 ) i $end
$var real_parameter 64 * j $end
$var mystery 3 + k $end
$upscope $end
$enddefinitions $end|});
  [%expect
    {|
    date:      (none)
    version:   (none)
    timescale: (none)
    comment:   (none)
      scope module m
        var bit 1 a
        var byte 8 b [7:0]
        var shortint 16 c
        var int 32 d
        var longint 64 e
        var enum 2 f
        var shortreal 32 g
        var sparray 1 h
        var port 4 i
        var real_parameter 64 j
        var mystery 3 k |}]

let%expect_test "non-1364 scope types are kept" =
  pp_header
    (parse_header
       {|$scope module top $end
$scope struct pkt $end $var logic 4 ! tag $end $upscope $end
$scope interface bus_if $end $upscope $end
$scope sv_array arr $end $upscope $end
$scope vhdl_architecture rtl $end $upscope $end
$upscope $end
$enddefinitions $end|});
  [%expect
    {|
    date:      (none)
    version:   (none)
    timescale: (none)
    comment:   (none)
      scope module top
        scope struct pkt
          var logic 4 tag
        scope interface bus_if
        scope sv_array arr
        scope vhdl_architecture rtl |}]

let%expect_test "comments, attributes and keyword-like words around and inside scopes" =
  pp_header
    (parse_header
       {|$timezero
	-42
$end
$attrbegin misc 01 wire 0 $end
$scope module m $end
$comment
	inside a scope
$end
$attrbegin misc 07 module 3 reg begin $end
$attrbegin array packed arr 4 $end
$var wire 1 ! a $end
$attrend $end
$upscope $end
$enddefinitions $end|});
  [%expect
    {|
    date:      (none)
    version:   (none)
    timescale: (none)
    timezero:  -42
    comment:   (none)
      scope module m
        var wire 1 a |}]

(* ------------------------------------------------------------------ *)
(*  $timezero                                                          *)
(* ------------------------------------------------------------------ *)

let%expect_test "negative timezero, as GTKWave writes it" =
  pp_header (parse_header "$timezero\n\t-42\n$end\n$timescale\n\t1ns\n$end\n$enddefinitions $end");
  [%expect
    {|
    date:      (none)
    version:   (none)
    timescale: 1ns
    timezero:  -42
    comment:   (none) |}]

let%expect_test "positive timezero after other declarations" =
  pp_header (parse_header "$version fst_ref $end $timescale 1ps $end $timezero 100 $end $enddefinitions $end");
  [%expect
    {|
    date:      (none)
    version:   fst_ref
    timescale: 1ps
    timezero:  100
    comment:   (none) |}]

let%expect_test "no timezero" =
  print_endline
    (match (parse_header "$timescale 1ps $end $enddefinitions $end").timezero with
    | None -> "None"
    | Some t -> Int64.to_string t);
  [%expect {| None |}]

let%expect_test "a timezero that is not an integer is a parse error" =
  (match Vcd.parse_string "$timezero soon $end $enddefinitions $end" with
  | _ -> print_endline "parsed"
  | exception Vcd.Parse_error msg -> print_endline msg);
  [%expect {| invalid $timezero: soon |}]

let%expect_test "var_type_of_keyword and scope_type_of_keyword agree with the parser" =
  let var_types =
    [ "event"; "integer"; "parameter"; "real"; "real_parameter"; "reg"; "supply0"; "supply1"; "time"; "tri";
      "triand"; "trior"; "trireg"; "tri0"; "tri1"; "wand"; "wire"; "wor"; "port"; "sparray"; "realtime"; "string";
      "bit"; "logic"; "int"; "shortint"; "longint"; "byte"; "enum"; "shortreal"; "custom" ]
  [@@ocamlformat "disable"]
  in
  let scope_types = [ "module"; "task"; "function"; "begin"; "fork"; "struct"; "vhdl_architecture"; "sv_array" ] in
  List.iter
    (fun kw ->
      let h =
        parse_header (Printf.sprintf "$scope module m $end $var %s 1 ! a $end $upscope $end $enddefinitions $end" kw)
      in
      match h.scopes with
      | [ { vars = [ v ]; _ } ] when v.v_type = var_type_of_keyword kw -> ()
      | _ -> Printf.printf "var type %s differs\n" kw)
    var_types;
  List.iter
    (fun kw ->
      let h = parse_header (Printf.sprintf "$scope %s m $end $upscope $end $enddefinitions $end" kw) in
      match h.scopes with
      | [ s ] when s.s_type = scope_type_of_keyword kw -> ()
      | _ -> Printf.printf "scope type %s differs\n" kw)
    scope_types;
  [%expect {| |}]
