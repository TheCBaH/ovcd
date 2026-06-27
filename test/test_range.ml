open Vcd_types

(* Render a parsed range as "start..stop", using "_" for an open bound, so the
   colon and dash forms can be compared at a glance. *)
let show s =
  let r = Vcd_util.parse_range s in
  let ts = function None -> "_" | Some t -> Format.asprintf "%a" Timestamp.pp t in
  Printf.printf "%-9s -> %s..%s\n" s (ts r.Vcd.start) (ts r.Vcd.stop)

let%expect_test "dash form" =
  show "0-500";
  show "2000-...";
  show "...1000";
  show "42";
  [%expect {|
    0-500     -> 0..500
    2000-...  -> 2000.._
    ...1000   -> _..1000
    42        -> 42..42
    |}]

let%expect_test "colon form mirrors dash" =
  show "0:500";
  show "25000000:32000000";
  [%expect {|
    0:500     -> 0..500
    25000000:32000000 -> 25000000..32000000
    |}]

let%expect_test "colon open bounds" =
  show "2000:";
  show ":1000";
  [%expect {|
    2000:     -> 2000.._
    :1000     -> _..1000
    |}]
