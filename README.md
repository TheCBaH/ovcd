# OCaml VCD Parser

[![build](https://github.com/TheCBaH/ovcd/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/TheCBaH/ovcd/actions/workflows/build.yml)

Streaming [VCD (Value Change Dump)](https://en.wikipedia.org/wiki/Value_change_dump) parser for OCaml.
The header is parsed with Menhir; simulation events are streamed as a lazy `Seq.t` via ocamllex.

It reads IEEE 1364 VCD and the extensions GTKWave's `fst2vcd` and Verilator's
FST traces produce: SystemVerilog `$var` and `$scope` types, IEEE 1164
9-state values, strings, port values, `$attrbegin` attributes and `$timezero`. Extended VCD
(`$dumpports`) is not supported.

FST files are read natively too, through
[ocaml-fst](https://github.com/TheCBaH/ocaml-fst): `Vcd_fst.open_file`
(library `ovcd.fst`) tells FST from VCD by content and returns the same
header and lazy event stream as `fst2vcd -e` followed by `Vcd.parse_file`,
decoding one FST block at a time. Both tools accept either format.

Two command-line tools are included — `vcd_dump` (structural: hierarchy, signal
listings, raw value stream) and `vcd_tool` (stateful: per-step state and
relational `--when`/`--show` queries). See the [CLI guide](doc/cli-guide.md) for
usage patterns, when to use which, and tradeoffs.

## Get started

```
make        # build
make test   # run tests
make bench  # benchmark on large real-world VCD files (default 10 runs each)
```

Open in GitHub Codespaces: [![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=1254661213)
