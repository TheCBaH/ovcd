[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5-5"]
[@@@ai_provider "Anthropic"]

open Vcd_types

(* [!] is declared twice, as an alias; [&] changes in the body without a declaration. *)
let vcd =
  {|
$timescale 1ns $end
$scope module tb $end
$var wire 1 ! a $end
$var wire 4 " b $end
$var real 1 # r $end
$scope module sub $end
$var wire 1 ! a_alias $end
$upscope $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
0!
b0000 "
r0.5 #
$end
#5
1!
1&
#10
b1010 "
$comment only b $end
#15
r1.5 #
0!
|}

let show = function
  | Vcd_ast.Timestamp t -> Format.asprintf "#%a" Timestamp.pp t
  | Change (id, v) -> Format.asprintf "%s=%s" (ID.to_string id) (Value.to_string v)
  | DumpStart s -> "$" ^ s
  | DumpEnd -> "$end"
  | SimComment c -> "$comment " ^ c

let print_seq s = print_endline (String.concat " " (List.of_seq (Seq.map show s)))

(* A predicate over a mutable set that logs its calls. *)
let logged ids =
  let set = ref ids and asked = ref [] in
  let f id =
    asked := ID.to_string id :: !asked;
    List.mem (ID.to_string id) !set
  in
  (f, set, asked)

let%expect_test "select is first called by the first pull, once per distinct declared ID" =
  let f, _, asked = logged [ "!" ] in
  let { Vcd.header; simulation } = Vcd.parse_string ~select:f vcd in
  ignore (header : Vcd_ast.header);
  Printf.printf "after the header: %d calls\n" (List.length !asked);
  (match simulation () with Seq.Cons (e, _) -> print_endline (show e) | Nil -> ());
  print_endline (String.concat " " (List.rev !asked));
  [%expect {|
    after the header: 0 calls
    #0
    ! " #
    |}]

let%expect_test "the first pull fixes the selection" =
  let f, set, asked = logged [ "!" ] in
  let { Vcd.simulation; _ } = Vcd.parse_string ~select:f vcd in
  match simulation () with
  | Nil -> ()
  | Cons (e, rest) ->
      set := [ "\""; "#" ];
      print_seq (Seq.cons e rest);
      Printf.printf "%d calls\n" (List.length !asked);
      [%expect {|
        #0 $dumpvars !=0 $end #5 !=1 #10 $comment only b #15 !=0
        3 calls
        |}]

let%expect_test "each stream has its own selection" =
  let a = Vcd.parse_string ~select:(fun id -> ID.to_string id = "!") vcd in
  let b = Vcd.parse_string ~select:(fun id -> ID.to_string id = "\"") vcd in
  print_seq a.simulation;
  print_seq b.simulation;
  [%expect
    {|
    #0 $dumpvars !=0 $end #5 !=1 #10 $comment only b #15 !=0
    #0 $dumpvars "=b0 $end #5 #10 "=b1010 $comment only b #15
    |}]

let%expect_test "undeclared IDs are dropped, and selecting nothing keeps every other event" =
  print_seq (Vcd.parse_string vcd).simulation;
  print_seq (Vcd.parse_string ~select:(fun _ -> true) vcd).simulation;
  print_seq (Vcd.parse_string ~select:(fun _ -> false) vcd).simulation;
  [%expect
    {|
    #0 $dumpvars !=0 "=b0 #=0.5 $end #5 !=1 &=1 #10 "=b1010 $comment only b #15 #=1.5 !=0
    #0 $dumpvars !=0 "=b0 #=0.5 $end #5 !=1 #10 "=b1010 $comment only b #15 #=1.5 !=0
    #0 $dumpvars $end #5 #10 $comment only b #15
    |}]
