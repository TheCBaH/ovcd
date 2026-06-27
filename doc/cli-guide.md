# VCD CLI guide: `vcd_dump` and `vcd_tool`

Two command-line tools ship with this library. They share a signal-selection
language and time-range syntax but answer different kinds of questions.

- **`vcd_dump`** — a stateless *structural* tool. It pretty-prints the header
  tree, lists signals (name ↔ ID code ↔ width), and streams raw per-change
  values. O(1) memory per change; it never holds cross-signal state.
- **`vcd_tool`** — a *stateful* tool. It reconstructs signal state over time and
  answers relational questions ("what was X when Y fired?"). It holds the
  tracked signals' current values in memory.

Rule of thumb: **structure and raw changes → `vcd_dump`; state-over-time and
relational queries → `vcd_tool`.**

Both parse lazily (the simulation body streams as a `Seq.t`), so multi-gigabyte
dumps are never fully buffered. Bound output with `--range` and `--signal`
rather than piping a full dump into `head`.

In the examples below the binaries are invoked as `vcd_dump` / `vcd_tool`; from a
build tree use `opam exec -- dune exec bin/vcd_dump.exe -- …`.

## Decision table

| You want to… | Tool | Invocation sketch |
|---|---|---|
| See the scope/signal hierarchy | `vcd_dump` | `vcd_dump file.vcd` |
| Count scopes / vars / events | `vcd_dump` | `vcd_dump --summary file.vcd` |
| Find a signal's ID code and width by name/glob | `vcd_dump` | `vcd_dump --list-signals '**.wr_strb' file.vcd` |
| Dump a few signals' transitions over a window | either | `vcd_dump --signal '**.data' --range A:B file.vcd` |
| See per-timestep state with deltas / range snapshots | `vcd_tool` | `vcd_tool --signal '**.data' file.vcd` |
| Find what A,B were when C took a value | `vcd_tool` | `vcd_tool --when '**.c=1' --show '**.a,**.b' file.vcd` |
| …plus N timestamps of history before each event | `vcd_tool` | add `--lookback N` |
| Benchmark parse throughput | `vcd_dump` | `vcd_dump --bench 10 file.vcd` |

## Signal selection

Both tools select signals by **glob DSL** (`--signal`, and `vcd_dump`'s
`--list-signals`; `vcd_tool`'s `--when`/`--show`) or **PCRE regex**
(`--signal-re`). Matching is full-path and anchored — the pattern must account
for every component of a signal's hierarchical name.

| Pattern | Matches |
|---|---|
| `tb.core.pc` | exactly that path |
| `tb.*.pc` | `pc` one scope below `tb` (`tb.core.pc`, `tb.cpu.pc`) |
| `tb.{core,cpu}.pc` | `tb.core.pc` and `tb.cpu.pc`, not `tb.arm.pc` |
| `tb.*` | direct children of `tb` only |
| `**.pc` | `pc` at any depth |
| `tb.**` | `tb` and all descendants |
| `**` | every signal |
| `*wr_strb*` (intra-segment) | one component containing `wr_strb` |

Flags accepting patterns are repeatable and combine with OR. A pattern that
matches nothing prints a warning to stderr (naming the flag) but is not fatal.

## Time ranges (`--range`, shared)

`--range` accepts, with either `-` or `:` as the bound separator:

- `A-B` / `A:B` — inclusive `[A, B]`
- `...B` / `..B` / `-B` / `:B` — from the start to `B`
- `A-...` / `A:` — from `A` to the end
- `A` — the single point `[A, A]`
- `...` — all time

`--range` is repeatable; multiple ranges form a union. `25000000:32000000` and
`25000000-32000000` are identical.

---

## `vcd_dump`

### Hierarchy (default) and `--summary`

```
$ vcd_dump test-data/vcdvcd/counter_tb.vcd
file:      counter_tb.vcd
date:      Sat Apr 29 09:34:13 2017
version:   Icarus Verilog
timescale: 1s
scope module counter_tb
  var wire 2 out [1:0]
  var reg 1 clock
  ...
events:    85 (timestamps: 27, changes: 57, dumps: 1, comments: 0)
```

`--summary` replaces the tree with scope/var counts.

### `--list-signals PATTERN` — name ↔ code ↔ width

Prints, for every signal matching the glob, a tab-separated
`path<TAB>id<TAB>width`, sorted by path, then exits without streaming the body.
This replaces the "parse the header, build a code→name map" boilerplate.

```
$ vcd_dump --list-signals '**.out' test-data/vcdvcd/counter_tb.vcd
counter_tb.out	!	2
counter_tb.top.out	%	2
```

Aliased signals (the same name reachable by multiple paths) are listed once per
path. Pipe into `cut -f2` to get just the ID codes.

### `--resolve` and `--signal` — name-resolved value stream

`--resolve` prints the raw change stream with IDs resolved to hierarchical
names. `--signal` / `--signal-re` restrict to matching signals and **imply
`--resolve`**, so name-based extraction is a one-liner:

```
$ vcd_dump --signal '**.out' --range 1:3 test-data/vcdvcd/counter_tb.vcd
#1
  counter_tb.out=bx
  counter_tb.top.out=bx
#2
  counter_tb.out=h0
  counter_tb.top.out=h0
```

Values render as bare `0`/`1` for scalars, `b…` for vectors containing X/Z, and
`h…` for fully-defined vectors. `--strip` removes the longest common scope
prefix from displayed names.

This is the stateless view: it shows **only the changes** at each timestamp, not
the full state. For full state or relational queries, use `vcd_tool`.

---

## `vcd_tool`

By default `vcd_tool` resolves names automatically and prints per-timestep
deltas. On entering a `--range` window it emits a **full state snapshot** (not
just the delta) so you see the current value of every selected signal, including
ones that last changed before the window:

```
$ vcd_tool --range 2-3 --signal '**.out' test-data/vcdvcd/counter_tb.vcd
#2
  counter_tb.out=h0
  counter_tb.top.out=h0
```

### `--when` / `--show` / `--lookback` — edge correlation

The relational core. For every timestamp where a `--when PATTERN=VALUE` signal
takes `VALUE`, print the `--show` signals at that timestamp; with `--lookback N`,
also at the previous `N` change-timestamps.

```
$ vcd_tool --when '**.bus_w_valid=1' --show '**.bus_w_data,**.bus_w_strb' \
      test-data/pyDigitalWaveTools/tests/AxiRegTC_test_write.vcd
@15000 EpWithReg.bus_w_valid=1
    EpWithReg.bus_w_data=h00000064
    EpWithReg.bus_w_strb=hf
@55000 EpWithReg.bus_w_valid=1
    EpWithReg.bus_w_data=h00000065
    EpWithReg.bus_w_strb=hf
...
```

Each AXI write transaction's data word and byte-strobe at the cycle the write
channel went valid — the incrementing `0x64, 0x65, …` confirms the correlation.

With `--lookback` and a time window:

```
$ vcd_tool --range 50000:90000 --when '**.bus_w_valid=1' --show '**.bus_w_data' \
      --lookback 2 test-data/pyDigitalWaveTools/tests/AxiRegTC_test_write.vcd
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
```

`-1`, `-2` are the preceding change-timestamps. Show values are the state *as of
that timestamp inclusive* (current changes overlaid on prior state, including
`$dumpvars`-initialised values). `--when` and `--show` patterns may be repeated;
`--show` also accepts a comma-separated list.

#### Semantics worth knowing

- **"Cycle" = change-timestamp, not a clock edge.** `--lookback 3` walks back 3
  timestamps in the (range-filtered) event stream, not 3 ticks of any clock.
- **`--when` value match is numeric.** `VALUE` is parsed as an integer (decimal,
  `0x…`, `0b…`) and compared to the signal's value. `X`/`Z` never match, so an
  all-defined transition is required to fire.
- **Triggers fire on the change.** The signal must appear in the timestamp's
  changes with the matching value; a range-entry snapshot counts as a change.

---

## Benefits & tradeoffs

| | `vcd_dump` | `vcd_tool` |
|---|---|---|
| State model | stateless; one change at a time | holds tracked signals' current state |
| Memory | O(1) per change | O(tracked signals) + O(`lookback`) snapshots |
| Shows | raw deltas, header, listings | per-step state, deltas, correlations |
| Relational queries | no | yes (`--when`/`--show`) |
| Best for | discovery, code/width lookup, fast scans | "what was X when Y happened?" |

Both stream lazily and stop early once all `--range` upper bounds are passed, so
neither needs to read an entire large file to answer a windowed query.

## Recipes (replacing two-pass scripts)

```sh
# 1. Discover a signal's ID code and width by glob
vcd_dump --list-signals '**.*wr_strb*' design.vcd

# 2. Extract a few signals' transitions over a time window
vcd_dump --signal '**.o_reg_written' --signal '**.stp_m_rbus_wr_strb' \
    --range 25000000:32000000 design.vcd

# 3. For each rising edge of one signal, show others now and N timestamps back
vcd_tool --when '**.o_reg_written=1' --show '**.i_rbus_wr_strb,**.stp_m_rbus_wr_strb' \
    --lookback 3 design.vcd
```
