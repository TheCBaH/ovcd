let buf = Buffer.create 65536

let flush () =
  Buffer.output_buffer stdout buf;
  Buffer.clear buf;
  Stdlib.flush stdout

let () = at_exit flush

let text s =
  Buffer.add_string buf s;
  Buffer.add_char buf '\n';
  if Buffer.length buf >= 65536 then flush ()

let assignment indent name value =
  Buffer.add_string buf indent;
  Buffer.add_string buf name;
  Buffer.add_char buf '=';
  Buffer.add_string buf value;
  Buffer.add_char buf '\n';
  if Buffer.length buf >= 65536 then flush ()

let ts t = Int64.to_string (Vcd_types.Timestamp.to_int64 t)
