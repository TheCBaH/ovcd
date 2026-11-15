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
decoding one FST block at a time. The event stream can be traversed once;
`Vcd.with_file` and `Vcd_fst.with_file` close the file on return or
exception. A viewer that seeks repeatedly opens a `Vcd_fst.File` once: its streams
share the header and the FST library's caches, borrow the handle, and are
invalidated by the next stream or by `close`. Both tools accept either format. ovcd is stricter than GTKWave on damaged
FST files: damaged chain tables and invalid time-table trailers are errors
here, where GTKWave skips the block.

Every failure is a `Vcd.Error.E` of a typed `Vcd.Error.error` (syntax and strict errors
with their file, line and column; FST errors with their kind and offset; I/O
errors), raised from the entry points and from pulling the simulation.
`Vcd.parse_string_result`, `Vcd_fst.open_file_result` and the like return the
errors raised before the simulation starts; `Vcd.Error.pp_error` prints one.

Reals print with 17 significant digits (`%.17g`), so every bit of a double survives
`float_of_string`; `0.` is now `0` and `3.14` is `3.1400000000000001`.

Two command-line tools are included — `vcd_dump` (structural: hierarchy, signal
listings, raw value stream) and `vcd_tool` (stateful: per-step state and
relational `--when`/`--show` queries). See the [CLI guide](doc/cli-guide.md) for
usage patterns, when to use which, and tradeoffs.

## Get started

```
make        # build
make test   # run tests
make benchmark  # benchmark on large real-world VCD files (default 10 runs each)
```

Open in GitHub Codespaces: [![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=1254661213)
