module Vcd_lexer = Tlexer
module Vcd_parser = Tparser
open Vcd_ast

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let lex_header s =
  let lb = Lexing.from_string s in
  let rec loop acc =
    let tok = Vcd_lexer.token lb in
    match tok with Vcd_parser.KW_ENDDEFS -> List.rev acc | t -> loop (t :: acc)
  in
  loop []

let show_header_tokens s =
  let buf = Buffer.create 64 in
  List.iter
    (fun tok ->
      let name =
        match tok with
        | Vcd_parser.KW_END -> "$end"
        | Vcd_parser.KW_SCOPE -> "$scope"
        | Vcd_parser.KW_UPSCOPE -> "$upscope"
        | Vcd_parser.KW_VAR -> "$var"
        | Vcd_parser.KW_TIMEZERO -> "$timezero"
        | Vcd_parser.DATE t -> Printf.sprintf "DATE(%s)" t
        | Vcd_parser.VERSION t -> Printf.sprintf "VERSION(%s)" t
        | Vcd_parser.TIMESCALE t -> Printf.sprintf "TIMESCALE(%s)" t
        | Vcd_parser.COMMENT t -> Printf.sprintf "COMMENT(%s)" t
        | Vcd_parser.ID s -> Printf.sprintf "ID(%s)" s
        | _ -> "?"
      in
      Buffer.add_string buf name;
      Buffer.add_char buf ' ')
    (lex_header s);
  String.trim (Buffer.contents buf)

let show_events ?(n = 20) s =
  let lb = Lexing.from_string s in
  let buf = Buffer.create 128 in
  let rec loop i =
    if i >= n then ()
    else
      match Vcd_lexer.next_event lb with
      | None -> ()
      | Some ev ->
          let line =
            match ev with
            | Timestamp t -> Format.asprintf "#%a" Vcd_types.Timestamp.pp t
            | Change (id, v) -> Printf.sprintf "%s=%s" (Vcd_types.ID.to_string id) (Vcd_types.Value.to_string v)
            | DumpStart k -> Printf.sprintf "$%s" k
            | DumpEnd -> "$end"
            | SimComment c -> Printf.sprintf "//%s" c
          in
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          loop (i + 1)
  in
  loop 0;
  String.trim (Buffer.contents buf)

(* ------------------------------------------------------------------ *)
(*  Header token stream                                                 *)
(* ------------------------------------------------------------------ *)

let%expect_test "keywords are recognised" =
  print_string (show_header_tokens "$scope module top $end $upscope $end $var wire 8 ! bus $end $enddefinitions");
  [%expect {| $scope ID(module) ID(top) $end $upscope $end $var ID(wire) ID(8) ID(!) ID(bus) $end |}]

let%expect_test "timescale tokens" =
  print_string (show_header_tokens "$timescale 1 ns $end $enddefinitions");
  [%expect {| TIMESCALE(1 ns) |}]

let%expect_test "a comment is one text token" =
  print_string (show_header_tokens "$comment wire reg $end $enddefinitions");
  [%expect {| COMMENT(wire reg) |}]

let%expect_test "a date is one text token" =
  print_string (show_header_tokens "$date Sat Apr 29 09:34:13 2017 $end $enddefinitions");
  [%expect {| DATE(Sat Apr 29 09:34:13 2017) |}]

(* ------------------------------------------------------------------ *)
(*  Simulation event stream                                             *)
(* ------------------------------------------------------------------ *)

let%expect_test "scalar value changes" =
  print_string (show_events "0! 1\" xA zB");
  [%expect {|
    !=0
    "=1
    A=x
    B=z |}]

let%expect_test "timestamp" =
  print_string (show_events "#0 #42 #1000000");
  [%expect {|
    #0
    #42
    #1000000 |}]

let%expect_test "vector binary — small (Int)" =
  print_string (show_events "b1010 ! b0 \"");
  [%expect {|
    !=b1010
    "=b0 |}]

let%expect_test "vector binary — with X/Z (Scalars)" =
  print_string (show_events "bx ! bxz01 \"");
  [%expect {|
    !=bx
    "=bxz01 |}]

let%expect_test "real value change" =
  print_string (show_events "r3.14 !  R0.0 \"");
  [%expect {|
    !=3.1400000000000001
    "=0 |}]

let%expect_test "dumpvars block" =
  print_string (show_events "$dumpvars 0! 1\" $end");
  [%expect {|
    $dumpvars
    !=0
    "=1
    $end |}]

let%expect_test "comment in simulation section" =
  print_string (show_events "$comment hello world $end #1 0!");
  [%expect {|
    //hello world
    #1
    !=0 |}]

(* ------------------------------------------------------------------ *)
(*  SystemVerilog / GTKWave value records                              *)
(* ------------------------------------------------------------------ *)

let%expect_test "9-state scalars and '?'" =
  print_string (show_events "u! W\" l# H$ -% ?&");
  [%expect {|
    !=u
    "=w
    #=l
    $=h
    %=-
    &=b? |}]

let%expect_test "9-state vector" =
  print_string (show_events "bUX01ZWLH- ! b10?1 \"");
  [%expect {|
    !=bux01zwlh-
    "=b10?1 |}]

let%expect_test "port, string and empty string values" =
  print_string (show_events "pUDN0 ! sab\\040c \" s # S$ $");
  [%expect {|
    !=pUDN0
    "=sab\040c
    #=s
    $=s$ |}]

let%expect_test "unknown $keyword block in the simulation section is skipped" =
  print_string (show_events "#0 $attrbegin misc 07 wire 1 $end 1! $dumpoff $end");
  [%expect {|
    #0
    !=1
    $dumpoff
    $end |}]

let%expect_test "unexpected character reports its line" =
  (try print_string (show_events "#0\n1!\n  %oops")
   with Vcd_error.Located (p, m) -> Vcd.Error.pp_error Format.std_formatter (`Syntax (p, m)));
  [%expect {| unexpected '%' at line 3 col 2 |}]

let%expect_test "timezero tokens" =
  print_string (show_header_tokens "$timezero\n\t-42\n$end $enddefinitions");
  [%expect {| $timezero ID(-42) $end |}]

let%expect_test "text tokens keep keywords and collapse whitespace" =
  print_string (show_header_tokens "$comment  a $var\n b $end $version $end $enddefinitions");
  [%expect {| COMMENT(a $var b) VERSION() |}]

let%expect_test "all-0/1 vectors lex like binary_value at every width" =
  let bad = ref [] in
  for w = 1 to 70 do
    List.iter
      (fun bits ->
        match Vcd_lexer.next_event (Lexing.from_string (Printf.sprintf "b%s !" bits)) with
        | Some (Change (_, v)) when Vcd_types.Value.to_string v = Vcd_types.Value.to_string (binary_value bits) -> ()
        | _ -> bad := w :: !bad)
      [ String.make w '1'; String.make w '0'; String.init w (fun i -> if i land 1 = 0 then '1' else '0') ]
  done;
  Printf.printf "mismatches: %d\n" (List.length !bad);
  [%expect {| mismatches: 0 |}]
