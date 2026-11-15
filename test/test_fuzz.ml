(* Random and mutated input to the VCD parser: only Vcd.Error.E may escape, from the header or from pulling the whole
   simulation. The seed is fixed. *)

let files =
  [
    "data/verilator/sv_types.vcd";
    "data/verilator/sv_types.fst2vcd.vcd";
    "data/fst2vcd/timezero.vcd";
    "data/fst2vcd/kitchen.vcd";
  ]

let read path = In_channel.with_open_bin path In_channel.input_all

let tokens =
  [|
    "$var";
    "$scope";
    "$upscope";
    "$end";
    "$enddefinitions";
    "$timescale";
    "$date";
    "$version";
    "$comment";
    "$timezero";
    "$dumpvars";
    "$dumpall";
    "$dumpoff";
    "$dumpon";
    "$attrbegin";
    "module";
    "wire";
    "reg";
    "real";
    "string";
    "1";
    "8";
    "64";
    "99999999999999999999";
    "-5";
    "!";
    "\"";
    "#";
    "#0";
    "#10";
    "#-3";
    "#99999999999999999999";
    "b101";
    "bx";
    "b";
    "r1.5";
    "r";
    "s";
    "sab";
    "pUD0";
    "0!";
    "1\"";
    "x!";
    "z!";
    "?!";
    "[7:0]";
    "[3]";
    "\n";
    " ";
    "\t";
    "\r\n";
    "//";
    "\xff";
    "\x00";
  |]

let soup st =
  let b = Buffer.create 256 in
  for _ = 1 to Random.State.int st 60 do
    Buffer.add_string b tokens.(Random.State.int st (Array.length tokens));
    if Random.State.int st 8 > 0 then Buffer.add_char b ' '
  done;
  Buffer.contents b

let header = "$timescale 1ns $end $scope module m $end $var wire 4 ! a $end $var real 1 \" r $end $upscope $end\n"
let soup_after_header st = header ^ "$enddefinitions $end\n" ^ soup st
let noise st = String.init (Random.State.int st 200) (fun _ -> Char.chr (Random.State.int st 256))

let mutate st s =
  let n = String.length s in
  if n = 0 then s
  else
    let pos = Random.State.int st n in
    match Random.State.int st 6 with
    | 0 ->
        let b = Bytes.of_string s in
        Bytes.set b pos (Char.chr (Random.State.int st 256));
        Bytes.to_string b
    | 1 -> String.sub s 0 pos
    | 2 ->
        let len = min (n - pos) (1 + Random.State.int st 40) in
        String.sub s 0 pos ^ String.sub s (pos + len) (n - pos - len)
    | 3 -> String.sub s 0 pos ^ noise st ^ String.sub s pos (n - pos)
    | 4 ->
        let len = min (n - pos) (1 + Random.State.int st 40) in
        String.sub s 0 (pos + len) ^ String.sub s pos (n - pos)
    | _ ->
        let b = Bytes.of_string s in
        for _ = 1 to 1 + Random.State.int st 8 do
          let p = Random.State.int st n in
          Bytes.set b p (Char.chr (Char.code (Bytes.get b p) lxor (1 lsl Random.State.int st 8)))
        done;
        Bytes.to_string b

let escapes = ref []
let accepted = ref 0
let rejected = ref 0

let guard input f =
  try f () with
  | Vcd.Error.E e ->
      incr rejected;
      ignore (Vcd.Error.to_string e)
  | e -> escapes := (Printexc.to_string e, input) :: !escapes

let exercise input =
  guard input (fun () ->
      let r = Vcd.parse_string input in
      Seq.iter ignore r.simulation;
      incr accepted);
  guard input (fun () ->
      let r = Vcd.parse_string ~strict:true input in
      Seq.iter ignore r.simulation);
  guard input (fun () ->
      let r = Vcd.parse_string ~select:(fun _ -> true) input in
      let resolver = Vcd.Resolver.make r.header in
      ignore (Vcd.Resolver.find_all (Result.get_ok (Filter_parser.parse "**")) resolver);
      Seq.iter ignore (Vcd.Stateful.stream r.simulation));
  match Vcd.parse_string_result input with
  | Ok _ | Error _ -> ()
  | exception e -> escapes := (Printexc.to_string e, input) :: !escapes

let report () =
  Printf.printf "both outcomes seen: %b\n" (!accepted > 100 && !rejected > 100);
  Printf.printf "escapes: %d\n" (List.length !escapes);
  List.iteri
    (fun i (e, input) ->
      if i < 5 then Printf.printf "%s on %S\n" e (String.sub input 0 (min 200 (String.length input))))
    !escapes

let%expect_test "random bytes and token soup raise only Vcd.Error.E" =
  let st = Random.State.make [| 20261115 |] in
  for _ = 1 to 10000 do
    exercise (noise st);
    exercise (soup st);
    exercise (soup_after_header st)
  done;
  report ();
  [%expect {|
    both outcomes seen: true
    escapes: 0 |}]

let%expect_test "mutated valid VCDs raise only Vcd.Error.E" =
  let st = Random.State.make [| 20261116 |] in
  List.iter
    (fun path ->
      let s = read path in
      for _ = 1 to 1500 do
        exercise (mutate st (if Random.State.int st 3 = 0 then mutate st s else s))
      done)
    files;
  report ();
  [%expect {|
    both outcomes seen: true
    escapes: 0 |}]
