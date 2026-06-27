No arguments — print usage to stderr and exit 1:

  $ ./vcd_tool 2>&1; echo "exit:$?"
  usage: vcd_tool [--range SPEC]... [--signal PATTERN]... [--signal-re REGEX]... [--strip] [--when PATTERN=VALUE --show PATTERNS [--lookback N]]... <file.vcd>...
    --range SPEC  Restrict to a time range, e.g. "...1000" or "0-500" or "2000-..."; may be repeated
    --signal PATTERN  Show only signals matching this DSL pattern (may be repeated)
    --signal-re REGEX  Show only signals whose full name matches this PCRE regex (may be repeated)
    --strip  Strip the longest common scope prefix from signal names
    --when PATTERN=VALUE  Trigger on each timestamp where a matching signal takes VALUE (may be repeated)
    --show PATTERNS  Comma-separated signals to print at each --when trigger (may be repeated)
    --lookback N  Also print --show state at the previous N change-timestamps (default 0)
    -help  Display this list of options
    --help  Display this list of options
  exit:1

Basic streaming — example0.vcd (3 signals, 5 timesteps, no $dumpvars):

  $ ./vcd_tool ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    unit0.sig0=x
    unit0.sig1=x
    unit0.vect0=bxxxxxxxxxxxxxxxx
  #1
    unit0.sig0=0
  #2
    unit0.sig1=1
  #3
    unit0.vect0=h000a
  #4
    unit0.vect0=h0014

Signal filter — **.vect0 selects the vector signal only:

  $ ./vcd_tool --signal '**.vect0' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    unit0.vect0=bxxxxxxxxxxxxxxxx
  #3
    unit0.vect0=h000a
  #4
    unit0.vect0=h0014

Regex filter — PCRE pattern matching signal name:

  $ ./vcd_tool --signal-re '\.vect0$' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    unit0.vect0=bxxxxxxxxxxxxxxxx
  #3
    unit0.vect0=h000a
  #4
    unit0.vect0=h0014

Unmatched --signal emits a warning on stderr:

  $ ./vcd_tool --signal 'nonexistent' ../test-data/pyDigitalWaveTools/tests/example0.vcd 2>&1
  warning: --signal "nonexistent" matched no signals

--strip removes the common scope prefix from all signals:

  $ ./vcd_tool --strip ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    sig0=x
    sig1=x
    vect0=bxxxxxxxxxxxxxxxx
  #1
    sig0=0
  #2
    sig1=1
  #3
    vect0=h000a
  #4
    vect0=h0014

--strip with --signal strips the common prefix of matched signals only:

  $ ./vcd_tool --strip --signal 'unit0.{sig0,vect0}' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    sig0=x
    vect0=bxxxxxxxxxxxxxxxx
  #1
    sig0=0
  #3
    vect0=h000a
  #4
    vect0=h0014

Range filter — restrict to t=1..3:

  $ ./vcd_tool --range 1-3 ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #1
    unit0.sig0=0
    unit0.sig1=x
    unit0.vect0=bxxxxxxxxxxxxxxxx
  #2
    unit0.sig1=1
  #3
    unit0.vect0=h000a

Open-ended range — from t=3 to end:

  $ ./vcd_tool --range '3-...' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #3
    unit0.sig0=0
    unit0.sig1=1
    unit0.vect0=h000a
  #4
    unit0.vect0=h0014

Range snapshot — counter_tb.vcd: entering range at t=2 emits a full
state snapshot rather than just the delta, so callers see values that
changed at t=1 (while the stream was outside the range):

  $ ./vcd_tool --range 2-3 ../test-data/vcdvcd/counter_tb.vcd
  #2
    counter_tb.out=h0
    counter_tb.clock=1
    counter_tb.top.clock=1
    counter_tb.enable=0
    counter_tb.top.enable=0
    counter_tb.reset=1
    counter_tb.top.reset=1
    counter_tb.top.out=h0
  #3
    counter_tb.clock=0
    counter_tb.top.clock=0
    counter_tb.reset=0
    counter_tb.top.reset=0

Multiple --range flags (union) — t=0..1 and t=3..5; snapshot emitted at
each range entry:

  $ ./vcd_tool --range 0-1 --range 3-5 ../test-data/vcdvcd/counter_tb.vcd
  #0
    counter_tb.out=bx
    counter_tb.clock=1
    counter_tb.top.clock=1
    counter_tb.enable=0
    counter_tb.top.enable=0
    counter_tb.reset=0
    counter_tb.top.reset=0
    counter_tb.top.out=bx
  #1
    counter_tb.clock=0
    counter_tb.top.clock=0
    counter_tb.reset=1
    counter_tb.top.reset=1
  #3
    counter_tb.out=h0
    counter_tb.clock=0
    counter_tb.top.clock=0
    counter_tb.enable=0
    counter_tb.top.enable=0
    counter_tb.reset=0
    counter_tb.top.reset=0
    counter_tb.top.out=h0
  #4
    counter_tb.clock=1
    counter_tb.top.clock=1
  #5
    counter_tb.clock=0
    counter_tb.top.clock=0
    counter_tb.enable=1
    counter_tb.top.enable=1

--range with --signal — filter signals and time simultaneously:

  $ ./vcd_tool --range 2-3 --signal '**.out' ../test-data/vcdvcd/counter_tb.vcd
  #2
    counter_tb.out=h0
    counter_tb.top.out=h0

Exact alias selection — counter_tb.clock selects only the outer alias even though both share an ID:

  $ ./vcd_tool --signal 'counter_tb.clock' ../test-data/vcdvcd/counter_tb.vcd
  #1
    counter_tb.clock=0
  #2
    counter_tb.clock=1
  #3
    counter_tb.clock=0
  #4
    counter_tb.clock=1
  #5
    counter_tb.clock=0
  #6
    counter_tb.clock=1
  #7
    counter_tb.clock=0
  #8
    counter_tb.clock=1
  #9
    counter_tb.clock=0
  #10
    counter_tb.clock=1
  #11
    counter_tb.clock=0
  #12
    counter_tb.clock=1
  #13
    counter_tb.clock=0
  #14
    counter_tb.clock=1
  #15
    counter_tb.clock=0
  #16
    counter_tb.clock=1
  #17
    counter_tb.clock=0
  #18
    counter_tb.clock=1
  #19
    counter_tb.clock=0
  #20
    counter_tb.clock=1
  #21
    counter_tb.clock=0
  #22
    counter_tb.clock=1
  #23
    counter_tb.clock=0
  #24
    counter_tb.clock=1
  #25
    counter_tb.clock=0
  #26
    counter_tb.clock=1

Exact alias selection — counter_tb.top.clock selects only the inner alias:

  $ ./vcd_tool --signal 'counter_tb.top.clock' ../test-data/vcdvcd/counter_tb.vcd
  #1
    counter_tb.top.clock=0
  #2
    counter_tb.top.clock=1
  #3
    counter_tb.top.clock=0
  #4
    counter_tb.top.clock=1
  #5
    counter_tb.top.clock=0
  #6
    counter_tb.top.clock=1
  #7
    counter_tb.top.clock=0
  #8
    counter_tb.top.clock=1
  #9
    counter_tb.top.clock=0
  #10
    counter_tb.top.clock=1
  #11
    counter_tb.top.clock=0
  #12
    counter_tb.top.clock=1
  #13
    counter_tb.top.clock=0
  #14
    counter_tb.top.clock=1
  #15
    counter_tb.top.clock=0
  #16
    counter_tb.top.clock=1
  #17
    counter_tb.top.clock=0
  #18
    counter_tb.top.clock=1
  #19
    counter_tb.top.clock=0
  #20
    counter_tb.top.clock=1
  #21
    counter_tb.top.clock=0
  #22
    counter_tb.top.clock=1
  #23
    counter_tb.top.clock=0
  #24
    counter_tb.top.clock=1
  #25
    counter_tb.top.clock=0
  #26
    counter_tb.top.clock=1

Deeply nested scope — verilater.vcd (64-bit counter, no $dumpvars):

  $ ./vcd_tool ../test-data/digital-vcd-parser/test/debug/verilater.vcd
  #1
    top.clock=0
  #2
    top.clock=1
  #300
    top.clock=0
    top.leaf.counter=h000000000000f000
  #301
    top.leaf.counter=h0000000000000f00
  #302
    top.leaf.counter=h00000000000000f0
  #303
    top.leaf.counter=h000000000000000f

Colon time-range form mirrors the dash form — counter_tb.vcd (--range 1:3 == 1-3):

  $ ./vcd_tool --signal '**.out' --range 1:3 ../test-data/vcdvcd/counter_tb.vcd
  #1
    counter_tb.out=bx
    counter_tb.top.out=bx
  #2
    counter_tb.out=h0
    counter_tb.top.out=h0

  $ ./vcd_tool --signal '**.out' --range 1-3 ../test-data/vcdvcd/counter_tb.vcd
  #1
    counter_tb.out=bx
    counter_tb.top.out=bx
  #2
    counter_tb.out=h0
    counter_tb.top.out=h0

Edge correlation (--when/--show) — counter_tb.vcd: at the t=1 rising edge of
reset, show the counter and clock. The shown values are as of that timestamp
inclusive, including the $dumpvars-initialised counter (bx):

  $ ./vcd_tool --when 'counter_tb.reset=1' --show 'counter_tb.out,counter_tb.clock' ../test-data/vcdvcd/counter_tb.vcd
  @1 counter_tb.reset=1
      counter_tb.clock=0
      counter_tb.out=bx

--lookback N also prints the show signals at the previous N change-timestamps —
**.clock=1 with lookback 1, restricted to the first few cycles:

  $ ./vcd_tool --when '**.clock=1' --show '**.out' --lookback 1 --range 0-6 ../test-data/vcdvcd/counter_tb.vcd
  @0 counter_tb.top.clock=1
  @0 counter_tb.clock=1
      counter_tb.out=bx
      counter_tb.top.out=bx
  @2 counter_tb.top.clock=1
  @2 counter_tb.clock=1
      counter_tb.out=h0
      counter_tb.top.out=h0
    -1 @1
      counter_tb.out=bx
      counter_tb.top.out=bx
  @4 counter_tb.top.clock=1
  @4 counter_tb.clock=1
      counter_tb.out=h0
      counter_tb.top.out=h0
    -1 @3
      counter_tb.out=h0
      counter_tb.top.out=h0
  @6 counter_tb.top.clock=1
  @6 counter_tb.clock=1
      counter_tb.out=h1
      counter_tb.top.out=h1
    -1 @5
      counter_tb.out=h0
      counter_tb.top.out=h0

Non-trivial relational query — AxiRegTC_test_write.vcd (AXI register-write
testbench, single scope, 32-bit buses): at every rising edge of the write-data
channel valid, show the data word and byte-strobe. Each transaction writes an
incrementing value (0x64, 0x65, ...):

  $ ./vcd_tool --when '**.bus_w_valid=1' --show '**.bus_w_data,**.bus_w_strb' ../test-data/pyDigitalWaveTools/tests/AxiRegTC_test_write.vcd
  @15000 EpWithReg.bus_w_valid=1
      EpWithReg.bus_w_data=h00000064
      EpWithReg.bus_w_strb=hf
  @55000 EpWithReg.bus_w_valid=1
      EpWithReg.bus_w_data=h00000065
      EpWithReg.bus_w_strb=hf
  @75000 EpWithReg.bus_w_valid=1
      EpWithReg.bus_w_data=h00000066
      EpWithReg.bus_w_strb=hf
  @85000 EpWithReg.bus_w_valid=1
      EpWithReg.bus_w_data=h00000067
      EpWithReg.bus_w_strb=hf
  @115000 EpWithReg.bus_w_valid=1
      EpWithReg.bus_w_data=h00000068
      EpWithReg.bus_w_strb=hf

Same file with --lookback 2 and a time window — shows the write data at each
valid edge plus the two preceding change-timestamps (the first entry of the
window is the range-entry snapshot):

  $ ./vcd_tool --range 50000:90000 --when '**.bus_w_valid=1' --show '**.bus_w_data' --lookback 2 ../test-data/pyDigitalWaveTools/tests/AxiRegTC_test_write.vcd
  @55000 EpWithReg.bus_w_valid=1
      EpWithReg.bus_w_data=h00000065
    -1 @50000
      EpWithReg.bus_w_data=h00000064
  @75000 EpWithReg.bus_w_valid=1
      EpWithReg.bus_w_data=h00000066
    -1 @62500
      EpWithReg.bus_w_data=h00000065
    -2 @55000
      EpWithReg.bus_w_data=h00000065
  @85000 EpWithReg.bus_w_valid=1
      EpWithReg.bus_w_data=h00000067
    -1 @82500
      EpWithReg.bus_w_data=h00000066
    -2 @75000
      EpWithReg.bus_w_data=h00000066

A --when pattern that matches nothing warns on stderr and produces no blocks:

  $ ./vcd_tool --when '**.nonexistent=1' --show '**.out' ../test-data/vcdvcd/counter_tb.vcd 2>&1
  warning: --when "**.nonexistent=1" matched no signals

Malformed --when (missing '=') is a usage error:

  $ ./vcd_tool --when 'counter_tb.reset' ../test-data/vcdvcd/counter_tb.vcd 2>&1; echo "exit:$?"
  error: --when "counter_tb.reset" must be of the form PATTERN=VALUE
  exit:1

Negative --lookback is rejected:

  $ ./vcd_tool --when 'counter_tb.reset=1' --lookback -1 ../test-data/vcdvcd/counter_tb.vcd 2>&1; echo "exit:$?"
  error: --lookback must be >= 0
  exit:1
