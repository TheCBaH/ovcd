Native FST input (test/data/README.md). Each FST parses to the header and
events of its fst2vcd -e conversion, reals aside:

  $ ./fst_equal data/verilator/sv_types.fst data/verilator/sv_types.fst2vcd.vcd
  292 events
  $ ./fst_equal data/fst2vcd/kitchen.fst data/fst2vcd/kitchen.vcd
  2105 events
  $ ./fst_equal data/fst2vcd/timezero.fst data/fst2vcd/timezero.vcd
  12 events
  $ ./fst_equal data/fst2vcd/repack.fst data/fst2vcd/repack.vcd
  2105 events

The tools tell FST from VCD by content, not by name:

  $ ./vcd_dump data/verilator/sv_types.fst | sed 1d > fst.out
  $ ./vcd_dump data/verilator/sv_types.fst2vcd.vcd | sed 1d > vcd.out
  $ diff fst.out vcd.out
  $ cp data/fst2vcd/repack.fst repack.vcd
  $ ./vcd_dump --summary repack.vcd
  file:      repack.vcd
  date:      Wed Oct 14 12:00:00 2026
  version:   fst_ref a2
  timescale: 1ps
  scopes:    6  vars: 23
  events:    2102 (timestamps: 401, changes: 1698, dumps: 3, comments: 0)
  $ ./vcd_tool --signal '**.cnt' data/verilator/sv_types.fst | head -6
  #5
    TOP.sv_types.cnt=h00000001
  #15
    TOP.sv_types.cnt=h00000002
  #25
    TOP.sv_types.cnt=h00000003

A truncated FST is a parse error:

  $ head -c 400 data/verilator/sv_types.fst > cut.fst
  $ ./vcd_dump cut.fst
  parse error: cut.fst: fst: malformed input: block length 830 (offset 330)
  [1]
