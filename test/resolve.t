Resolve all IDs to hierarchical names — example0.vcd (single scope, scalars and vector):

  $ ./vcd_dump --resolve ../test-data/pyDigitalWaveTools/tests/example0.vcd
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

Deeply nested scope — verilater.vcd (Verilator output, 3-level scope, no $dumpvars):

  $ ./vcd_dump --resolve ../test-data/digital-vcd-parser/test/debug/verilater.vcd
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

Nested scope — counter_tb.vcd (vcdvcd), IDs shared across parent and child scopes:

  $ ./vcd_dump --resolve ../test-data/vcdvcd/counter_tb.vcd
  #0
  $dumpvars
    counter_tb.top.out=bx
    counter_tb.top.reset=0
    counter_tb.top.enable=0
    counter_tb.top.clock=1
    counter_tb.out=bx
  $end
  #1
    counter_tb.top.clock=0
    counter_tb.top.reset=1
  #2
    counter_tb.out=h0
    counter_tb.top.out=h0
    counter_tb.top.clock=1
  #3
    counter_tb.top.clock=0
    counter_tb.top.reset=0
  #4
    counter_tb.top.clock=1
  #5
    counter_tb.top.clock=0
    counter_tb.top.enable=1
  #6
    counter_tb.out=h1
    counter_tb.top.out=h1
    counter_tb.top.clock=1
  #7
    counter_tb.top.clock=0
  #8
    counter_tb.out=h2
    counter_tb.top.out=h2
    counter_tb.top.clock=1
  #9
    counter_tb.top.clock=0
  #10
    counter_tb.out=h3
    counter_tb.top.out=h3
    counter_tb.top.clock=1
  #11
    counter_tb.top.clock=0
  #12
    counter_tb.out=h0
    counter_tb.top.out=h0
    counter_tb.top.clock=1
  #13
    counter_tb.top.clock=0
  #14
    counter_tb.out=h1
    counter_tb.top.out=h1
    counter_tb.top.clock=1
  #15
    counter_tb.top.clock=0
  #16
    counter_tb.out=h2
    counter_tb.top.out=h2
    counter_tb.top.clock=1
  #17
    counter_tb.top.clock=0
  #18
    counter_tb.out=h3
    counter_tb.top.out=h3
    counter_tb.top.clock=1
  #19
    counter_tb.top.clock=0
  #20
    counter_tb.out=h0
    counter_tb.top.out=h0
    counter_tb.top.clock=1
  #21
    counter_tb.top.clock=0
  #22
    counter_tb.out=h1
    counter_tb.top.out=h1
    counter_tb.top.clock=1
  #23
    counter_tb.top.clock=0
  #24
    counter_tb.out=h2
    counter_tb.top.out=h2
    counter_tb.top.clock=1
  #25
    counter_tb.top.clock=0
    counter_tb.top.enable=0
  #26
    counter_tb.top.clock=1

Alternation filter — {sig0,vect0} selects exactly two signals:

  $ ./vcd_dump --resolve --signal 'unit0.{sig0,vect0}' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    unit0.sig0=x
    unit0.vect0=bxxxxxxxxxxxxxxxx
  #1
    unit0.sig0=0
  #3
    unit0.vect0=h000a
  #4
    unit0.vect0=h0014

Wildcard filter — unit0.* selects every signal directly under unit0:

  $ ./vcd_dump --resolve --signal 'unit0.*' ../test-data/pyDigitalWaveTools/tests/example0.vcd
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

Glob filter — **.vect0 selects vect0 at any hierarchy depth:

  $ ./vcd_dump --resolve --signal '**.vect0' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    unit0.vect0=bxxxxxxxxxxxxxxxx
  #3
    unit0.vect0=h000a
  #4
    unit0.vect0=h0014

Multiple --signal flags (OR semantics) — **.clock and **.out on counter_tb:

  $ ./vcd_dump --resolve --signal '**.clock' --signal '**.out' ../test-data/vcdvcd/counter_tb.vcd
  #0
  $dumpvars
    counter_tb.top.out=bx
    counter_tb.top.clock=1
    counter_tb.out=bx
  $end
  #1
    counter_tb.top.clock=0
  #2
    counter_tb.out=h0
    counter_tb.top.out=h0
    counter_tb.top.clock=1
  #3
    counter_tb.top.clock=0
  #4
    counter_tb.top.clock=1
  #5
    counter_tb.top.clock=0
  #6
    counter_tb.out=h1
    counter_tb.top.out=h1
    counter_tb.top.clock=1
  #7
    counter_tb.top.clock=0
  #8
    counter_tb.out=h2
    counter_tb.top.out=h2
    counter_tb.top.clock=1
  #9
    counter_tb.top.clock=0
  #10
    counter_tb.out=h3
    counter_tb.top.out=h3
    counter_tb.top.clock=1
  #11
    counter_tb.top.clock=0
  #12
    counter_tb.out=h0
    counter_tb.top.out=h0
    counter_tb.top.clock=1
  #13
    counter_tb.top.clock=0
  #14
    counter_tb.out=h1
    counter_tb.top.out=h1
    counter_tb.top.clock=1
  #15
    counter_tb.top.clock=0
  #16
    counter_tb.out=h2
    counter_tb.top.out=h2
    counter_tb.top.clock=1
  #17
    counter_tb.top.clock=0
  #18
    counter_tb.out=h3
    counter_tb.top.out=h3
    counter_tb.top.clock=1
  #19
    counter_tb.top.clock=0
  #20
    counter_tb.out=h0
    counter_tb.top.out=h0
    counter_tb.top.clock=1
  #21
    counter_tb.top.clock=0
  #22
    counter_tb.out=h1
    counter_tb.top.out=h1
    counter_tb.top.clock=1
  #23
    counter_tb.top.clock=0
  #24
    counter_tb.out=h2
    counter_tb.top.out=h2
    counter_tb.top.clock=1
  #25
    counter_tb.top.clock=0
  #26
    counter_tb.top.clock=1

Unmatched --signal pattern emits a warning on stderr:

  $ ./vcd_dump --resolve --signal 'unit0.nonexistent' ../test-data/pyDigitalWaveTools/tests/example0.vcd 2>&1
  warning: --signal "unit0.nonexistent" matched no signals

Regex filter — PCRE pattern matching full signal name:

  $ ./vcd_dump --resolve --signal-re '\.vect0$' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    unit0.vect0=bxxxxxxxxxxxxxxxx
  #3
    unit0.vect0=h000a
  #4
    unit0.vect0=h0014

Regex alternation — (sig0|sig1) selects two signals:

  $ ./vcd_dump --resolve --signal-re 'unit0\.(sig0|sig1)' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    unit0.sig0=x
    unit0.sig1=x
  #1
    unit0.sig0=0
  #2
    unit0.sig1=1

--signal and --signal-re combined (OR semantics):

  $ ./vcd_dump --resolve --signal 'unit0.sig0' --signal-re '\.vect0$' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    unit0.sig0=x
    unit0.vect0=bxxxxxxxxxxxxxxxx
  #1
    unit0.sig0=0
  #3
    unit0.vect0=h000a
  #4
    unit0.vect0=h0014

Unmatched --signal-re pattern emits a warning on stderr:

  $ ./vcd_dump --resolve --signal-re 'nonexistent' ../test-data/pyDigitalWaveTools/tests/example0.vcd 2>&1
  warning: --signal-re "nonexistent" matched no signals

--strip with a single matched signal strips all but the leaf name (min_len-1 cap):

  $ ./vcd_dump --resolve --strip --signal 'unit0.sig0' ../test-data/pyDigitalWaveTools/tests/example0.vcd
  #0
    sig0=x
  #1
    sig0=0

--strip removes the common scope prefix from matched signals:

  $ ./vcd_dump --resolve --strip --signal 'unit0.*' ../test-data/pyDigitalWaveTools/tests/example0.vcd
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

--strip with no filter strips the common prefix of all signals (verilater.vcd: top.*):

  $ ./vcd_dump --resolve --strip ../test-data/digital-vcd-parser/test/debug/verilater.vcd
  #1
    clock=0
  #2
    clock=1
  #300
    clock=0
    leaf.counter=h000000000000f000
  #301
    leaf.counter=h0000000000000f00
  #302
    leaf.counter=h00000000000000f0
  #303
    leaf.counter=h000000000000000f

--strip with signals spanning two depths — strips shared counter_tb prefix:

  $ ./vcd_dump --resolve --strip --signal '**.clock' --signal '**.out' ../test-data/vcdvcd/counter_tb.vcd
  #0
  $dumpvars
    top.out=bx
    top.clock=1
    out=bx
  $end
  #1
    top.clock=0
  #2
    out=h0
    top.out=h0
    top.clock=1
  #3
    top.clock=0
  #4
    top.clock=1
  #5
    top.clock=0
  #6
    out=h1
    top.out=h1
    top.clock=1
  #7
    top.clock=0
  #8
    out=h2
    top.out=h2
    top.clock=1
  #9
    top.clock=0
  #10
    out=h3
    top.out=h3
    top.clock=1
  #11
    top.clock=0
  #12
    out=h0
    top.out=h0
    top.clock=1
  #13
    top.clock=0
  #14
    out=h1
    top.out=h1
    top.clock=1
  #15
    top.clock=0
  #16
    out=h2
    top.out=h2
    top.clock=1
  #17
    top.clock=0
  #18
    out=h3
    top.out=h3
    top.clock=1
  #19
    top.clock=0
  #20
    out=h0
    top.out=h0
    top.clock=1
  #21
    top.clock=0
  #22
    out=h1
    top.out=h1
    top.clock=1
  #23
    top.clock=0
  #24
    out=h2
    top.out=h2
    top.clock=1
  #25
    top.clock=0
  #26
    top.clock=1

