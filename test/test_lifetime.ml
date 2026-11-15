[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-sonnet-5-5"]
[@@@ai_provider "Anthropic"]

let vcd =
  "$timescale 1ns $end\n\
   $scope module tb $end\n\
   $var wire 1 ! a $end\n\
   $upscope $end\n\
   $enddefinitions $end\n\
   #0\n\
   0!\n\
   #5\n\
   1!\n"

let fds () = Array.length (Sys.readdir "/proc/self/fd")

let with_temp contents f =
  let path = Filename.temp_file "lifetime" ".vcd" in
  Out_channel.with_open_bin path (fun oc -> output_string oc contents);
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let leaked f =
  let before = fds () in
  for _ = 1 to 300 do
    f ()
  done;
  fds () - before

let%expect_test "parse_file closes at end of input" =
  with_temp vcd (fun path ->
      Printf.printf "%b\n" (leaked (fun () -> Seq.iter ignore (Vcd.parse_file path).simulation) < 10));
  [%expect {| true |}]

let%expect_test "parse_file closes on a header error" =
  with_temp "$scope $end\n" (fun path ->
      Printf.printf "%b\n" (leaked (fun () -> try ignore (Vcd.parse_file path) with Vcd.Parse_error _ -> ()) < 10));
  [%expect {| true |}]

let twice name (r : Vcd.parse_result) =
  Seq.iter ignore r.simulation;
  match Seq.iter ignore r.simulation with
  | () -> Printf.printf "%s: second traversal allowed\n" name
  | exception Seq.Forced_twice -> Printf.printf "%s: Forced_twice\n" name

let%expect_test "a second traversal raises" =
  twice "string" (Vcd.parse_string vcd);
  with_temp vcd (fun path -> twice "file" (Vcd.parse_file path));
  twice "fst" (Vcd_fst.parse_file "data/fst2vcd/clean.fst");
  [%expect {|
    string: Forced_twice
    file: Forced_twice
    fst: Forced_twice |}]
