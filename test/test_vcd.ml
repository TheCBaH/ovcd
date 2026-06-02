open Vcd_ast
open Vcd_types

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let pp_events ?(n = 100) seq =
  Seq.iter
    (function
      | Timestamp t -> Format.printf "#%a\n" Timestamp.pp t
      | Change (id, v) -> Format.printf "  %a=%a\n" ID.pp id Value.pp v
      | DumpStart k -> Format.printf "$%s\n" k
      | DumpEnd -> Format.printf "$end\n"
      | SimComment c -> Format.printf "//%s\n" c)
    (Seq.take n seq)

let parse s =
  let r = Vcd.parse_string s in
  let h = r.header in
  Format.printf "version:   %s\n" (Option.value ~default:"(none)" h.version);
  Format.printf "date:      %s\n" (Option.value ~default:"(none)" h.date);
  Format.printf "timescale: %s\n" (Option.value ~default:"(none)" h.timescale);
  let rec pp_scope ind s =
    Format.printf "%s[%s] %s\n" ind (Vcd.string_of_scope_type s.s_type) s.s_name;
    List.iter (fun v -> Format.printf "%s  %s %s id=%a\n" ind (Vcd.string_of_var_type v.v_type) v.ref ID.pp v.id) s.vars;
    List.iter (pp_scope (ind ^ "  ")) s.children
  in
  List.iter (pp_scope "") h.scopes;
  r

(* ------------------------------------------------------------------ *)
(*  Minimal VCD (no simulation data)                                    *)
(* ------------------------------------------------------------------ *)

let minimal_vcd =
  {|
$timescale 1ps $end
$scope module top $end
  $var wire 1 ! clk $end
  $var reg  8 " data $end
$upscope $end
$enddefinitions $end
|}

let%expect_test "header only" =
  let _ = parse minimal_vcd in
  [%expect
    {|
    version:   (none)
    date:      (none)
    timescale: 1ps
    [module] top
      wire clk id=!
      reg data id=" |}]

(* ------------------------------------------------------------------ *)
(*  counter_tb.vcd — end-to-end with simulation events                 *)
(* ------------------------------------------------------------------ *)

let counter_vcd =
  {|
$date Sat Apr 29 09:34:13 2017 $end
$version Icarus Verilog $end
$timescale 1s $end
$scope module counter_tb $end
$var wire 2 ! out [1:0] $end
$var reg  1 " clock $end
$var reg  1 # enable $end
$var reg  1 $ reset $end
  $scope module top $end
  $var wire 1 " clock $end
  $var wire 1 # enable $end
  $var wire 1 $ reset $end
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

let%expect_test "counter header" =
  let _ = parse counter_vcd in
  [%expect
    {|
    version:   Icarus Verilog
    date:      Sat Apr 29 09:34:13 2017
    timescale: 1s
    [module] counter_tb
      wire out id=!
      reg clock id="
      reg enable id=#
      reg reset id=$
      [module] top
        wire clock id="
        wire enable id=#
        wire reset id=$
        reg out id=% |}]

let%expect_test "counter simulation events" =
  let r = Vcd.parse_string counter_vcd in
  pp_events r.simulation;
  [%expect
    {|
    #0
    $dumpvars
      %=bx
      $=0
      #=0
      "=1
      !=bx
    $end
    #1
      "=0
      $=1
    #2
      !=b0
      %=b0
      "=1
    #3
      "=0
      $=0 |}]

(* ------------------------------------------------------------------ *)
(*  4-state / mixed vector values                                       *)
(* ------------------------------------------------------------------ *)

let mixed_vcd =
  {|
$timescale 1ns $end
$scope module tb $end
  $var reg 4  ! nibble $end
  $var reg 4  " masked $end
  $var reg 32 # word   $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
b1010 !
bxxzz "
b00000000000000000000000011111111 #
$end
#1
b0101 !
#2
b10u0 "
|}

let%expect_test "mixed vectors — Int, Scalars, Bytes, Other" =
  let r = Vcd.parse_string mixed_vcd in
  pp_events r.simulation;
  [%expect
    {|
    #0
    $dumpvars
      !=b1010
      "=bxxzz
      #=b11111111
    $end
    #1
      !=b101
    #2
      "=b10u0 |}]

let%expect_test "value variant tags" =
  let r = Vcd.parse_string mixed_vcd in
  Seq.iter
    (function
      | Change (id, v) ->
          let tag =
            match v with
            | Int _ -> "Int"
            | Int64 _ -> "Int64"
            | Bytes _ -> "Bytes"
            | Scalars _ -> "Scalars"
            | Other _ -> "Other"
            | Scalar _ -> "Scalar"
            | Real _ -> "Real"
          in
          Format.printf "%a: %s\n" ID.pp id tag
      | _ -> ())
    r.simulation;
  [%expect {|
    !: Int
    ": Scalars
    #: Int
    !: Int
    ": Other |}]

let%expect_test "Int64 value — 63-bit all-ones" =
  (* 63 ones = 0x7FFFFFFFFFFFFFFF = Int64.max_int > OCaml max_int, so stays Int64 *)
  let vcd =
    {|
$timescale 1ns $end
$scope module tb $end
  $var reg 63 ! wide $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
b111111111111111111111111111111111111111111111111111111111111111 !
$end
|}
  in
  let r = Vcd.parse_string vcd in
  Seq.iter
    (function
      | Change (id, v) ->
          let tag =
            match v with
            | Int _ -> "Int"
            | Int64 _ -> "Int64"
            | Bytes _ -> "Bytes"
            | Scalars _ -> "Scalars"
            | Other _ -> "Other"
            | Scalar _ -> "Scalar"
            | Real _ -> "Real"
          in
          Format.printf "%a: %s = %a\n" ID.pp id tag Value.pp v
      | _ -> ())
    r.simulation;
  [%expect {| !: Int64 = b111111111111111111111111111111111111111111111111111111111111111 |}]

(* ------------------------------------------------------------------ *)
(*  parse_string error handling                                         *)
(* ------------------------------------------------------------------ *)

let%expect_test "parse error is caught" =
  (match Vcd.parse_string "$scope module $end $enddefinitions $end" with
  | exception Vcd.Parse_error msg ->
      let is_err = String.length msg > 0 in
      Printf.printf "Parse_error raised: %b\n" is_err
  | _ -> Printf.printf "no error\n");
  [%expect {| Parse_error raised: true |}]

(* ------------------------------------------------------------------ *)
(*  Resolver                                                           *)
(* ------------------------------------------------------------------ *)

let resolver_vcd =
  {|
$timescale 1ns $end
$scope module tb $end
  $var wire 1 ! clk $end
  $scope module sub $end
    $var reg 8 " data $end
  $upscope $end
$upscope $end
$enddefinitions $end
|}

let%expect_test "Resolver — references by ID" =
  let r = Vcd.parse_string resolver_vcd in
  let resolver = Vcd.Resolver.make r.header in
  List.iter
    (fun id ->
      let id_t = ID.of_string id in
      let refs = Vcd.Resolver.references resolver id_t in
      if Vcd.Resolver.Ref_set.is_empty refs then Printf.printf "%s -> (not found)\n" id
      else Vcd.Resolver.Ref_set.iter (fun r -> Printf.printf "%s -> %s\n" id (Reference.to_string r)) refs)
    [ "!"; "\""; "#" ];
  [%expect {|
    ! -> tb.clk
    " -> tb.sub.data
    # -> (not found)
    |}]

let%expect_test "Resolver — entry_id and entry_references" =
  let r = Vcd.parse_string resolver_vcd in
  let resolver = Vcd.Resolver.make r.header in
  (match Vcd.Resolver.find resolver (ID.of_string "!") with
  | None -> Printf.printf "(not found)\n"
  | Some e ->
      let open Vcd.Resolver in
      let refs = Ref_set.elements (entry_references e) in
      Printf.printf "id=%s references=%s\n"
        (ID.to_string (entry_id e))
        (String.concat "," (List.map Reference.to_string refs)));
  [%expect {| id=! references=tb.clk |}]

let%expect_test "Resolver — aliased ID has multiple references" =
  let r = Vcd.parse_string counter_vcd in
  let resolver = Vcd.Resolver.make r.header in
  List.iter
    (fun id ->
      let refs = Vcd.Resolver.references resolver (ID.of_string id) in
      Printf.printf "%s (%d refs):" id (Vcd.Resolver.Ref_set.cardinal refs);
      Vcd.Resolver.Ref_set.iter (fun r -> Printf.printf " %s" (Reference.to_string r)) refs;
      print_char '\n')
    [ "!"; "\""; "#"; "$"; "%" ];
  [%expect
    {|
    ! (1 refs): counter_tb.out
    " (2 refs): counter_tb.clock counter_tb.top.clock
    # (2 refs): counter_tb.enable counter_tb.top.enable
    $ (2 refs): counter_tb.reset counter_tb.top.reset
    % (1 refs): counter_tb.top.out
    |}]

let%expect_test "Resolver — find_id reverse lookup" =
  let r = Vcd.parse_string resolver_vcd in
  let resolver = Vcd.Resolver.make r.header in
  List.iter
    (fun name ->
      let ref_ = Reference.of_string name in
      match Vcd.Resolver.find_id resolver ref_ with
      | Some id -> Printf.printf "%s -> %s\n" name (ID.to_string id)
      | None -> Printf.printf "%s -> (not found)\n" name)
    [ "tb.clk"; "tb.sub.data"; "tb.unknown" ];
  [%expect {|
    tb.clk -> !
    tb.sub.data -> "
    tb.unknown -> (not found)
  |}]
