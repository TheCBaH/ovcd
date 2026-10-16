# SystemVerilog and GTKWave test traces

Checked in, so the tests need neither Verilator nor GTKWave.

`verilator/`: `sv_types.sv` traced with Verilator 5.032 for 40 half cycles by
`main.cpp`:

    verilator --cc --exe --build --trace --top-module sv_types sv_types.sv main.cpp
    obj_dir/Vsv_types sv_types.vcd

`sv_types.vcd` is Verilator's own VCD writer (IEEE 1364 types only).
Building with `--trace-fst` instead writes `sv_types.fst`; `sv_types.fst2vcd.vcd`
is its conversion by `fst2vcd -e` (libfst's, or ocaml-fst's, which writes the
same bytes), with SystemVerilog `$var` types and enum `$attrbegin` records.

`fst2vcd/kitchen.vcd`: every construct libfst's VCD writer produces (9-state
values, strings, port values, SystemVerilog and VHDL scope and variable types,
attributes, comments, blackouts), from ocaml-fst's `test/reference/fst_ref.c`
(`a2_kitchen_z.ext.vcd`, written and read by libfst `5186181`). `fst2vcd/timezero.vcd`
is the same reference's `a2_timezero.vcd`, a small trace with `$timezero -42`.

`kitchen.fst` and `timezero.fst` are the FST files those VCDs come from
(`a2_kitchen_z.fst`, `a2_timezero.fst`). `repack.fst` (`a2_repack.fst`) is the
kitchen trace at 1 ps, gzip-wrapped by libfst's repack-on-close (first byte
`0xFE`); `repack.vcd` is its `a2_repack.ext.vcd`.
`test/fst.t` checks that each FST parses to its VCD.
