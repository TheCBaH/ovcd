[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-sonnet-5-5"]
[@@@ai_provider "Anthropic"]

(* fst_seek FST [N]: Vcd_fst.open_at, at the times around every block, must continue the full stream. With N, only the
   IDs whose hash is 0 mod N are selected. For each time, Stateful.stream ~init:state on the resumed simulation must give
   the full stream's Stateful events from the resumed stream's first timestep on, state and changes alike. *)

open Vcd_types

let show_event (e : Vcd.Stateful.event) =
  let map m =
    String.concat " " (List.map (fun (id, v) -> ID.to_string id ^ "=" ^ Value.to_string v) (Vcd.ID_map.bindings m))
  in
  Printf.sprintf "#%s state: %s | changes: %s" (Format.asprintf "%a" Timestamp.pp e.time) (map e.state) (map e.changes)

let () =
  let path = Sys.argv.(1) in
  let every = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 0 in
  let select = if every > 0 then Some (fun id -> Hashtbl.hash (ID.to_string id) mod every = 0) else None in
  let full = List.of_seq (Vcd.Stateful.stream (Vcd_fst.parse_file ?select path).simulation) in
  let blocks = Vcd_fst.blocks path in
  let i64 = Timestamp.to_int64 in
  let times =
    Array.to_list blocks
    |> List.concat_map (fun (s, e) ->
        let s = i64 s and e = i64 e in
        [ s; e; Int64.add s 1L; Int64.div (Int64.add s e) 2L; Int64.pred s ])
    |> fun l -> Int64.zero :: Int64.add (i64 (snd blocks.(Array.length blocks - 1))) 10L :: l
  in
  let times = List.sort_uniq Int64.compare (List.filter (fun t -> Int64.compare t 0L >= 0) times) in
  let resumed = ref 0 and bad = ref 0 in
  List.iter
    (fun t ->
      let state, { Vcd.simulation; _ } = Vcd_fst.open_at ?select path ~time:(Timestamp.of_int64 t) in
      if not (Vcd.ID_map.is_empty state) then incr resumed;
      let got = List.of_seq (Vcd.Stateful.stream ~init:state simulation) in
      let want =
        match got with
        | [] -> []
        | first :: _ -> List.filter (fun (e : Vcd.Stateful.event) -> Timestamp.compare e.time first.time >= 0) full
      in
      (* The frame also holds IDs that the full stream has not assigned yet, at their initial value: those are the
         only entries [state] may have beyond the full stream's. *)
      let same (g : Vcd.Stateful.event) (w : Vcd.Stateful.event) =
        Timestamp.equal g.time w.time
        && show_event { g with state = Vcd.ID_map.empty } = show_event { w with state = Vcd.ID_map.empty }
        && Vcd.ID_map.for_all
             (fun id v -> Option.map Value.to_string (Vcd.ID_map.find_opt id g.state) = Some (Value.to_string v))
             w.state
      in
      let ok = List.length got = List.length want && List.for_all2 same got want in
      if not ok then begin
        incr bad;
        Printf.printf "time %Ld: %d events, the full stream has %d from there\n" t (List.length got) (List.length want);
        let rec first g w =
          match (g, w) with
          | g0 :: g, w0 :: w -> if g0 = w0 then first g w else Printf.printf "  got  %s\n  want %s\n" g0 w0
          | g0 :: _, [] -> Printf.printf "  extra %s\n" g0
          | [], w0 :: _ -> Printf.printf "  missing %s\n" w0
          | [], [] -> ()
        in
        first (List.map show_event got) (List.map show_event want)
      end)
    times;
  Printf.printf "%d blocks, %d seeks, %d resumed after the first block, %d differ\n" (Array.length blocks)
    (List.length times) !resumed !bad
