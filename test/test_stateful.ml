open Vcd_types
open Vcd.Stateful

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let pp_map m = State.iter (fun id v -> Format.printf " %a=%a" ID.pp id Value.pp v) m

let pp_event ev =
  Format.printf "t=%a\n" Timestamp.pp ev.time;
  Format.printf "  state:";
  pp_map ev.state;
  Format.printf "\n  changes:";
  pp_map ev.changes;
  Format.printf "\n"

let stream_of vcd = (Vcd.parse_string vcd).simulation
let id_set strs = List.fold_left (fun s str -> ID_set.add (ID.of_string str) s) ID_set.empty strs
let ts s = Timestamp.of_string s
let range ?start ?stop () : Vcd.time_range = { start; stop }

(* ------------------------------------------------------------------ *)
(*  Test VCDs                                                           *)
(* ------------------------------------------------------------------ *)

(* counter_tb: 5 variables, $dumpvars block, 3 simulation timesteps. *)
let counter_vcd =
  {|
$timescale 1s $end
$scope module counter_tb $end
$var wire 2 ! out [1:0] $end
$var reg  1 " clock $end
$var reg  1 # enable $end
$var reg  1 $ reset $end
  $scope module top $end
  $var reg  2 % out [1:0] $end
  $upscope $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
bx %
0$
0#
1"
bx !
$end
#1
0"
1$
#2
b0 !
b0 %
1"
#3
0"
0$
|}

(* Two-variable VCD with a $dumpall block mid-simulation. *)
let dumpall_vcd =
  {|
$timescale 1ns $end
$scope module tb $end
$var wire 1 ! a $end
$var wire 1 " b $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
0!
0"
$end
#5
$dumpall
0!
0"
$end
1"
#10
1!
|}

(* Minimal VCD with no simulation events at all. *)
let no_sim_vcd =
  {|
$timescale 1ns $end
$scope module tb $end
$var wire 1 ! clk $end
$upscope $end
$enddefinitions $end
|}

(* ------------------------------------------------------------------ *)
(*  Basic streaming — no filters                                        *)
(* ------------------------------------------------------------------ *)

let%expect_test "basic streaming — all events, state carries dumpvars init" =
  (* The $dumpvars block at t=0 initialises state but emits no event (changes
     suppressed inside dump blocks).  The first emitted event (t=1) already
     has the fully-initialised state in its .state field. *)
  Seq.iter pp_event (stream (stream_of counter_vcd));
  [%expect
    {|
    t=1
      state: !=bx "=1 #=0 $=0 %=bx
      changes: "=0 $=1
    t=2
      state: !=bx "=0 #=0 $=1 %=bx
      changes: !=b0 "=1 %=b0
    t=3
      state: !=b0 "=1 #=0 $=1 %=b0
      changes: "=0 $=0 |}]

(* ------------------------------------------------------------------ *)
(*  Dump-block suppression                                              *)
(* ------------------------------------------------------------------ *)

let%expect_test "dumpvars — changes suppressed from .changes, state populated" =
  (* At t=1 the .state reflects the $dumpvars values, but .changes only
     contains the actual simulation changes that happened at t=1. *)
  (match Seq.uncons (stream (stream_of counter_vcd)) with
  | None -> print_string "empty\n"
  | Some (ev, _) ->
      Format.printf "state:";
      pp_map ev.state;
      Format.printf "\nchanges:";
      pp_map ev.changes;
      Format.printf "\n");
  [%expect {|
    state: !=bx "=1 #=0 $=0 %=bx
    changes: "=0 $=1 |}]

let%expect_test "dumpall mid-simulation — reassertions suppressed, real change reported" =
  (* At t=5 the $dumpall block reasserts both signals as 0 (same as current
     state).  Those reassertions must not appear in .changes; only the
     explicit b→1 assignment outside the block is reported. *)
  Seq.iter pp_event (stream (stream_of dumpall_vcd));
  [%expect {|
    t=5
      state: !=0 "=0
      changes: "=1
    t=10
      state: !=0 "=1
      changes: !=1 |}]

(* ------------------------------------------------------------------ *)
(*  tracked filter                                                      *)
(* ------------------------------------------------------------------ *)

let%expect_test "tracked filter — state only holds tracked IDs" =
  (* Only ! is tracked, so .state never contains other IDs.
     All IDs are still reported in .changes (no reported filter). *)
  Seq.iter pp_event (stream ~tracked:(id_set [ "!" ]) (stream_of counter_vcd));
  [%expect
    {|
    t=1
      state: !=bx
      changes: "=0 $=1
    t=2
      state: !=bx
      changes: !=b0 "=1 %=b0
    t=3
      state: !=b0
      changes: "=0 $=0 |}]

(* ------------------------------------------------------------------ *)
(*  reported filter                                                     *)
(* ------------------------------------------------------------------ *)

let%expect_test "reported filter — events only when reported ID changes" =
  (* Only changes to ! are reported.  ! does not change at t=1 or t=3,
     so those timesteps produce no event.  State is complete at all times. *)
  Seq.iter pp_event (stream ~reported:(id_set [ "!" ]) (stream_of counter_vcd));
  [%expect {|
    t=2
      state: !=bx "=0 #=0 $=1 %=bx
      changes: !=b0 |}]

let%expect_test "reported filter — no events when reported ID never changes" =
  (* # (enable) starts at 0 in $dumpvars and is never changed during
     simulation, so the stream is empty. *)
  let events = stream ~reported:(id_set [ "#" ]) (stream_of counter_vcd) in
  Printf.printf "event count: %d\n" (Seq.length events);
  [%expect {| event count: 0 |}]

(* ------------------------------------------------------------------ *)
(*  tracked and reported independently                                  *)
(* ------------------------------------------------------------------ *)

let%expect_test "tracked and reported independent — reported ID absent from state" =
  (* tracked={!}  reported={"}:
     - .state only ever contains !, updated when ! changes
     - .changes only contains ", emitted whenever " changes
     The two sets are orthogonal: " is surfaced in .changes but never
     appears in .state. *)
  Seq.iter pp_event (stream ~tracked:(id_set [ "!" ]) ~reported:(id_set [ "\"" ]) (stream_of counter_vcd));
  [%expect
    {|
    t=1
      state: !=bx
      changes: "=0
    t=2
      state: !=bx
      changes: "=1
    t=3
      state: !=b0
      changes: "=0 |}]

(* ------------------------------------------------------------------ *)
(*  Time range filters                                                  *)
(* ------------------------------------------------------------------ *)

let%expect_test "single range with start — state tracks all prior changes" =
  (* t=1 is outside the range (start=2) but still updates internal state.
     At t=2 (range entry) .changes is a full snapshot of all reported signals —
     including #=0 and $=1 which changed at t=1, not t=2 — so the consumer
     sees the complete current state when entering the range. *)
  Seq.iter pp_event (stream ~ranges:[ range ~start:(ts "2") () ] (stream_of counter_vcd));
  [%expect
    {|
    t=2
      state: !=bx "=0 #=0 $=1 %=bx
      changes: !=b0 "=1 #=0 $=1 %=b0
    t=3
      state: !=b0 "=1 #=0 $=1 %=b0
      changes: "=0 $=0 |}]

let%expect_test "single range with stop — snapshot at t=0 then delta; stops after stop" =
  (* Range starts from the beginning (start=None), so t=0 is the first range
     entry and triggers a snapshot of the dumpvars state in .changes.
     t=1 is a normal delta.  The sequence stops after t=1 (stop=1). *)
  Seq.iter pp_event (stream ~ranges:[ range ~stop:(ts "1") () ] (stream_of counter_vcd));
  [%expect
    {|
    t=0
      state:
      changes: !=bx "=1 #=0 $=0 %=bx
    t=1
      state: !=bx "=1 #=0 $=0 %=bx
      changes: "=0 $=1 |}]

let%expect_test "single range with start and stop — snapshot at entry, delta after" =
  (* t=0 is before start=1, so t=1 is the range entry → snapshot in .changes
     (all reported signals after t=1's changes, including !=bx and #=0).
     t=2 is a normal delta within the range. *)
  Seq.iter pp_event (stream ~ranges:[ range ~start:(ts "1") ~stop:(ts "2") () ] (stream_of counter_vcd));
  [%expect
    {|
    t=1
      state: !=bx "=1 #=0 $=0 %=bx
      changes: !=bx "=0 #=0 $=1 %=bx
    t=2
      state: !=bx "=0 #=0 $=1 %=bx
      changes: !=b0 "=1 %=b0 |}]

let%expect_test "multiple ranges — snapshot at each range entry" =
  (* Ranges: [start, stop=1] and [start=3, end].
     t=0: first entry of range 1 → snapshot of dumpvars state in .changes.
     t=1: delta within range 1.
     t=2: outside all ranges; state still updated internally.
     t=3: entry of range 2 → snapshot of full state (reflecting t=2 changes). *)
  Seq.iter pp_event (stream ~ranges:[ range ~stop:(ts "1") (); range ~start:(ts "3") () ] (stream_of counter_vcd));
  [%expect
    {|
    t=0
      state:
      changes: !=bx "=1 #=0 $=0 %=bx
    t=1
      state: !=bx "=1 #=0 $=0 %=bx
      changes: "=0 $=1
    t=3
      state: !=b0 "=1 #=0 $=1 %=b0
      changes: !=b0 "=0 #=0 $=0 %=b0 |}]

(* ------------------------------------------------------------------ *)
(*  find                                                                *)
(* ------------------------------------------------------------------ *)

let%expect_test "find — present and absent IDs" =
  (* Check find against the state carried in the first emitted event.
     The state at that point holds the $dumpvars-initialised values. *)
  (match Seq.uncons (stream (stream_of counter_vcd)) with
  | None -> print_string "empty\n"
  | Some (ev, _) ->
      let check id_str =
        match find ev.state (ID.of_string id_str) with
        | None -> Format.printf "%s -> none\n" id_str
        | Some v -> Format.printf "%s -> %a\n" id_str Value.pp v
      in
      check "!";
      check "\"";
      check "~");
  [%expect {|
    ! -> bx
    " -> 1
    ~ -> none |}]

(* ------------------------------------------------------------------ *)
(*  Edge cases                                                          *)
(* ------------------------------------------------------------------ *)

let%expect_test "no simulation events — empty sequence" =
  let n = Seq.length (stream (stream_of no_sim_vcd)) in
  Printf.printf "events: %d\n" n;
  [%expect {| events: 0 |}]

(* ------------------------------------------------------------------ *)
(*  Aliased signals (same ID, multiple scopes)                          *)
(* ------------------------------------------------------------------ *)

(* VCD where ID ! is declared in two scopes — top.clk and top.sub.clk. *)
let aliased_vcd =
  {|
$timescale 1ns $end
$scope module top $end
  $var wire 1 ! clk $end
  $scope module sub $end
    $var wire 1 ! clk $end
  $upscope $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
0!
$end
#5
1!
#10
0!
|}

let%expect_test "aliased signal — one state entry per ID, changes captured once" =
  Seq.iter pp_event (stream (stream_of aliased_vcd));
  [%expect {|
    t=5
      state: !=0
      changes: !=1
    t=10
      state: !=1
      changes: !=0 |}]

let%expect_test "aliased signal — tracked filter by ID covers all aliases" =
  Seq.iter pp_event (stream ~tracked:(id_set [ "!" ]) (stream_of aliased_vcd));
  [%expect {|
    t=5
      state: !=0
      changes: !=1
    t=10
      state: !=1
      changes: !=0 |}]

let%expect_test "aliased signal — reported filter by ID covers all aliases" =
  Seq.iter pp_event (stream ~reported:(id_set [ "!" ]) (stream_of aliased_vcd));
  [%expect {|
    t=5
      state: !=0
      changes: !=1
    t=10
      state: !=1
      changes: !=0 |}]
