[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-sonnet-5-5"]
[@@@ai_provider "Anthropic"]

let header = "$timescale 1ns $end\n$scope module tb $end\n$var wire 1 ! clk $end\n$upscope $end\n$enddefinitions $end\n"
let body = "#0\n$dumpvars\n0!\n$end\n#5\n1!\n"

(* Position-free outcome: the simulation is pulled to its end. *)
let outcome parse =
  match Seq.length (parse ()).Vcd.simulation with
  | n -> Printf.printf "ok, %d events\n" n
  | exception Vcd.Parse_error msg ->
      let rec cut i = if String.sub msg i 5 = " at <" then i else cut (i + 1) in
      Printf.printf "Parse_error: %s\n" (String.sub msg 0 (cut 0))

let both s =
  outcome (fun () -> Vcd.parse_string s);
  outcome (fun () -> Vcd.parse_string ~strict:true s)

let through_channel s =
  let path = Filename.temp_file "strict" ".vcd" in
  Out_channel.with_open_bin path (fun oc -> output_string oc s);
  In_channel.with_open_bin path (fun ic -> outcome (fun () -> Vcd.parse_channel ~strict:true ic));
  Sys.remove path

let%expect_test "complete input is accepted in both modes" =
  both (header ^ body);
  [%expect {|
    ok, 6 events
    ok, 6 events |}]

let%expect_test "header without $enddefinitions" =
  both "$timescale 1ns $end\n$scope module tb $end\n$var wire 1 ! clk $end\n$upscope $end\n";
  [%expect {|
    ok, 0 events
    Parse_error: missing $enddefinitions |}]

let%expect_test "open dump block at end of input" =
  both (header ^ "#0\n$dumpvars\n0!\n");
  [%expect {|
    ok, 3 events
    Parse_error: unterminated dump block at end of input |}]

let%expect_test "missing final newline" =
  both (header ^ "#0\n$dumpvars\n0!\n$end\n#5\n1!");
  [%expect {|
    ok, 6 events
    Parse_error: input does not end with a newline |}]

let%expect_test "cut inside a record" =
  both (header ^ "#0\n$dumpvars\n0!\n$end\n#12");
  [%expect {|
    ok, 5 events
    Parse_error: input does not end with a newline |}]

let%expect_test "cut at a record boundary is complete" =
  both (header ^ "#0\n$dumpvars\n0!\n$end\n");
  [%expect {|
    ok, 4 events
    ok, 4 events |}]

let%expect_test "channel input" =
  through_channel (header ^ body);
  through_channel "$timescale 1ns $end\n";
  through_channel (header ^ "#0\n$dumpvars\n0!\n");
  through_channel (header ^ "#0\n1!");
  through_channel "";
  [%expect {|
    ok, 6 events
    Parse_error: missing $enddefinitions
    Parse_error: unterminated dump block at end of input
    Parse_error: input does not end with a newline
    Parse_error: missing $enddefinitions |}]
