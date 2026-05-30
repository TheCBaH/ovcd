Files from IEEE Std 1364-2005 section 18.2.4 example circuit.  Five
sources carry the same DUT declaration; they differ only in simulation
content.  Notable features: trireg var type, task scope, multi-char
id codes, and (in verilog2005-sample1.vcd) a '$'-prefixed id code.

go-vcd-parser copy — header only, single $comment event:

  $ ./vcd_dump ../test-data/go-vcd-parser/vcd/files/samples/18.2.4_01.vcd
  file:      18.2.4_01.vcd
  date:      June 26, 1989 10:05:41
  version:   VERILOG-SIMULATOR 1.0a
  timescale: 1 ns
  scope module top
    scope module m1
      var trireg 1 net1
      var trireg 1 net2
      var trireg 1 net3
    scope task t1
      var reg 32 accumulator[31:0]
      var integer 32 index
  events:    1 (timestamps: 0, changes: 0, dumps: 0, comments: 1)

go-vcd-parser copy — full simulation with four $dumpvars blocks:

  $ ./vcd_dump ../test-data/go-vcd-parser/vcd/files/samples/18.2.4_02.vcd
  file:      18.2.4_02.vcd
  date:      June 26, 1989 10:05:41
  version:   VERILOG-SIMULATOR 1.0a
  timescale: 1 ns
  scope module top
    scope module m1
      var trireg 1 net1
      var trireg 1 net2
      var trireg 1 net3
    scope task t1
      var reg 32 accumulator[31:0]
      var integer 32 index
  events:    46 (timestamps: 10, changes: 31, dumps: 4, comments: 1)

pyDigitalWaveTools sample0 — standard four-state signals only:

  $ ./vcd_dump ../test-data/pyDigitalWaveTools/tests/verilog2005-sample0.vcd
  file:      verilog2005-sample0.vcd
  date:      June 26, 1989 10:05:41
  version:   VERILOG-SIMULATOR 1.0a
  timescale: 1 ns
  scope module top
    scope module m1
      var trireg 1 net1
      var trireg 1 net2
      var trireg 1 net3
    scope task t1
      var reg 32 accumulator[31:0]
      var integer 32 index
  events:    45 (timestamps: 10, changes: 31, dumps: 4, comments: 0)

pyDigitalWaveTools sample1 — net3 uses '$' as its id code:

  $ ./vcd_dump ../test-data/pyDigitalWaveTools/tests/verilog2005-sample1.vcd
  file:      verilog2005-sample1.vcd
  date:      June 26, 1989 10:05:41
  version:   VERILOG-SIMULATOR 1.0a
  timescale: 1 ns
  scope module top
    scope module m1
      var trireg 1 net1
      var trireg 1 net2
      var trireg 1 $ net3
    scope task t1
      var reg 32 accumulator[31:0]
      var integer 32 index
  events:    46 (timestamps: 10, changes: 31, dumps: 4, comments: 1)

vcd-io copy — header-only excerpt with one timestamp and a $comment:

  $ ./vcd_dump ../test-data/vcd-io/testcases/IEEE_std_example.vcd
  file:      IEEE_std_example.vcd
  date:      June 26, 1989 10:05:41
  version:   VERILOG-SIMULATOR 1.0a
  timescale: 1 ns
  scope module top
    scope module m1
      var trireg 1 net1
      var trireg 1 net2
      var trireg 1 net3
    scope task t1
      var reg 32 accumulator[31:0]
      var integer 32 index
  events:    7 (timestamps: 1, changes: 5, dumps: 0, comments: 1)
