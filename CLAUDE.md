# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
make                  # build
make test             # run all tests
make update-tests     # run tests and auto-promote expected outputs
make benchmark        # benchmark on large real-world VCD files
make format           # auto-format with ocamlformat
make utop             # REPL with the library loaded
```

All make targets wrap `opam exec -- dune`. To run a single test:

```bash
opam exec -- dune runtest test/resolve.t      # one cram file
opam exec -- dune runtest test/test_parse.ml  # one inline-test module
```

## Architecture

Two-phase parsing: the **header** is parsed eagerly by Menhir and produces `Vcd_ast.header` (a scope tree of `scope_node` / `var_decl`); **simulation events** stream lazily as `Vcd_ast.event Seq.t` via a second ocamllex entry point, so large files are never fully buffered.

`vcd_types.ml` defines four opaque modules — `ID`, `Reference`, `Timestamp`, `Value`. The non-obvious one: `Reference.t` is a **reversed** string list; `to_string` reverses and joins with `.`.

`Vcd.Resolver` maps between IDs and hierarchical references using two balanced-tree maps (`By_id` for forward lookup, `By_ref` for reverse). `entry` is opaque — use `entry_id` / `entry_reference` or `fold`. For aliased signals (same ID in multiple scopes), `By_id` retains the deepest scope's entry while `By_ref` stores every path independently.

The public API is `lib/vcd.ml` / `lib/vcd.mli`. Everything else in `lib/` is internal.

## Tests

`test/test_*.ml` — inline expect-tests (`ppx_expect` / `ppx_inline_test`).
`test/*.t` — cram tests that drive the `vcd_dump` binary against real VCD files in `test-data/` (git submodules).

When adding a cram case, write the command with no expected output and run `make update-tests` to fill it in automatically.

## Approach

**Run `make format` before every commit.** ocamlformat is the enforced style; unformatted diffs will be rejected.

**State tradeoffs before changing API surface.** This library deliberately keeps `entry` opaque and exposes `fold` rather than raw map access. When adding public API, name the tradeoff explicitly before implementing.

**Surgical edits.** Match existing style; don't clean up adjacent code unless it is directly affected by your change.

**Keep list entries alphabetically sorted.** When adding to a list (e.g. `modules`/`libraries`/`deps` in `dune` files, or similar enumerations), insert the new entry in alphabetical order rather than appending; sort the surrounding list if it makes the addition coherent.

**Surface confusion early.** If a request has multiple valid interpretations (e.g. whether a new accessor belongs on `Resolver` or `vcd_types`), ask before implementing.
