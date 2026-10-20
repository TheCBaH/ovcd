open Vcd_types

(* [pack_bits] and [binary_value] as they were before they read a digit at a time, kept to compare against. *)
module Old = struct
  let pack_bits s len =
    let nbytes = (len + 7) / 8 in
    let pad = (nbytes * 8) - len in
    let buf = Bytes.make nbytes '\x00' in
    for i = 0 to len - 1 do
      if s.[i] = '1' then begin
        let byte_idx = (i + pad) / 8 in
        let bit_pos = 7 - ((i + pad) mod 8) in
        Bytes.set buf byte_idx (Char.chr (Char.code (Bytes.get buf byte_idx) lor (1 lsl bit_pos)))
      end
    done;
    buf

  let binary_value s =
    let n = String.length s in
    if n > 0 && String.for_all (fun c -> c = '0' || c = '1') s then
      if n <= Value.int_bits then Value.int (String.fold_left (fun acc c -> (acc lsl 1) lor (Char.code c - 48)) 0 s)
      else if n <= 64 then
        Value.int64
          (String.fold_left
             (fun acc c -> Int64.logor (Int64.shift_left acc 1) (Int64.of_int (Char.code c land 1)))
             0L s)
      else Value.bytes (pack_bits s n)
    else Vcd_ast.parse_vector s
end

let%test "every binary string up to 12 digits" =
  let rec go n s =
    Vcd_ast.binary_value s = Old.binary_value s && (n = 12 || (go (n + 1) (s ^ "0") && go (n + 1) (s ^ "1")))
  in
  go 0 ""

let%test "random strings of 0..200 digits, with and without other characters" =
  let st = Random.State.make [| 20261020 |] in
  let others = "xzXZuUwWlLhH-?2a" in
  let gen n =
    let s = String.init n (fun _ -> if Random.State.bool st then '1' else '0') in
    if n > 0 && Random.State.int st 4 = 0 then begin
      let b = Bytes.of_string s in
      Bytes.set b (Random.State.int st n) others.[Random.State.int st (String.length others)];
      Bytes.to_string b
    end
    else s
  in
  let ok = ref true in
  for n = 0 to 200 do
    for _ = 1 to 200 do
      let s = gen n in
      if Vcd_ast.binary_value s <> Old.binary_value s then ok := false;
      if n > 0 && Vcd_ast.pack_bits s n <> Old.pack_bits s n then ok := false
    done
  done;
  !ok

let%test "digits at the width boundaries" =
  List.for_all
    (fun n ->
      List.for_all
        (fun s -> Vcd_ast.binary_value s = Old.binary_value s)
        [ String.make n '0'; String.make n '1'; "1" ^ String.make (n - 1) '0'; String.make (n - 1) '0' ^ "1" ])
    [ 1; 2; 23; 24; 25; 31; 32; 33; 62; 63; 64; 65; 72; 128 ]
