open Vcd_types

(* badframe.fst is kitchen.fst with the first byte of its second block's frame
   (the value of the first signal) replaced by 'Q'. *)
let%expect_test "a frame byte outside the 9-state alphabet is Other" =
  let state, _ = Vcd_fst.open_at "data/fst2vcd/badframe.fst" ~time:(Timestamp.of_int64 2000L) in
  (match Vcd.ID_map.min_binding_opt state with
  | Some (id, v) -> Printf.printf "%s=%s\n" (ID.to_string id) (Value.to_string v)
  | None -> print_endline "empty state");
  [%expect {| !=bQ |}]
