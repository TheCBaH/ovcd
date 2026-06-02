No arguments — print usage to stderr and exit 1:

  $ ./vcd_tool 2>&1; echo "exit:$?"
  usage: vcd_tool [--range SPEC]... [--signal PATTERN]... [--signal-re REGEX]... [--strip] <file.vcd>...
    --range SPEC  Restrict to a time range, e.g. "...1000" or "0-500" or "2000-..."; may be repeated
    --signal PATTERN  Show only signals matching this DSL pattern (may be repeated)
    --signal-re REGEX  Show only signals whose full name matches this PCRE regex (may be repeated)
    --strip  Strip the longest common scope prefix from signal names
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
