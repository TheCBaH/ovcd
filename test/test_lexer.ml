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
        | Vcd_parser.KW_DATE -> "$date"
        | Vcd_parser.KW_VERSION -> "$version"
        | Vcd_parser.KW_TIMESCALE -> "$timescale"
        | Vcd_parser.KW_COMMENT -> "$comment"
        | Vcd_parser.SCP_MODULE -> "module"
        | Vcd_parser.SCP_TASK -> "task"
        | Vcd_parser.SCP_FUNCTION -> "function"
        | Vcd_parser.SCP_BEGIN -> "begin"
        | Vcd_parser.SCP_FORK -> "fork"
        | Vcd_parser.VT_WIRE -> "wire"
        | Vcd_parser.VT_REG -> "reg"
        | Vcd_parser.VT_INTEGER -> "integer"
        | Vcd_parser.VT_REAL -> "real"
        | Vcd_parser.VT_REALTIME -> "realtime"
        | Vcd_parser.VT_PARAMETER -> "parameter"
        | Vcd_parser.VT_EVENT -> "event"
        | Vcd_parser.VT_TIME -> "time"
        | Vcd_parser.VT_TRI -> "tri"
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
  [%expect {| $scope module ID(top) $end $upscope $end $var wire ID(8) ID(!) ID(bus) $end |}]

let%expect_test "timescale tokens" =
  print_string (show_header_tokens "$timescale 1 ns $end $enddefinitions");
  [%expect {| $timescale ID(1) ID(ns) $end |}]

let%expect_test "comment keyword passes through as ID in text block" =
  print_string (show_header_tokens "$comment wire reg $end $enddefinitions");
  [%expect {| $comment wire reg $end |}]

let%expect_test "unknown words become ID" =
  print_string (show_header_tokens "$date Sat Apr 29 09:34:13 2017 $end $enddefinitions");
  [%expect {| $date ID(Sat) ID(Apr) ID(29) ID(09:34:13) ID(2017) $end |}]

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
    !=3.14
    "=0. |}]

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
