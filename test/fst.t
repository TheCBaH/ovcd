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

With a selection, FST and VCD keep the same changes, and ask about each ID once:

  $ ./fst_equal data/verilator/sv_types.fst data/verilator/sv_types.fst2vcd.vcd 2
  11 IDs asked once, 5 selected
  147 events
  $ ./fst_equal data/fst2vcd/kitchen.fst data/fst2vcd/kitchen.vcd 3
  19 IDs asked once, 6 selected
  882 events
  $ ./fst_equal data/fst2vcd/repack.fst data/fst2vcd/repack.vcd 1
  19 IDs asked once, 19 selected
  2105 events
  $ ./fst_equal data/fst2vcd/repack.fst data/fst2vcd/repack.vcd 1000000
  19 IDs asked once, 0 selected
  407 events

Reading can start at a block boundary: open_at gives the state before it, and
Stateful.stream ~init on the rest is the full stream from there on (the state
may also hold IDs the full stream has not assigned yet). Every block of the
multiblock trace starts in a time step that has changes in the block before,
so open_at steps back to the first; the clean trace's blocks do not:

  $ ./fst_seek data/verilator/sv_types.fst
  1 blocks, 5 seeks, 0 resumed after the first block, 0 differ
  $ ./fst_seek data/fst2vcd/kitchen.fst
  2 blocks, 9 seeks, 5 resumed after the first block, 0 differ
  $ ./fst_seek data/fst2vcd/kitchen.fst 3
  2 blocks, 9 seeks, 5 resumed after the first block, 0 differ
  $ ./fst_seek data/fst2vcd/repack.fst
  2 blocks, 9 seeks, 5 resumed after the first block, 0 differ
  $ ./fst_seek data/fst2vcd/multiblock.fst
  21 blocks, 87 seeks, 0 resumed after the first block, 0 differ
  $ ./fst_seek data/fst2vcd/multiblock.fst 2
  21 blocks, 87 seeks, 0 resumed after the first block, 0 differ
  $ ./fst_seek data/fst2vcd/clean.fst
  11 blocks, 47 seeks, 41 resumed after the first block, 0 differ
  $ ./fst_seek data/fst2vcd/clean.fst 2
  11 blocks, 47 seeks, 41 resumed after the first block, 0 differ
  $ ./fst_seek data/fst2vcd/clean.fst 3
  11 blocks, 47 seeks, 41 resumed after the first block, 0 differ
