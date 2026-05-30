default:
	opam exec dune build

test:
	opam exec -- dune runtest test/

format:
	opam exec dune fmt

utop:
	opam exec dune utop

update-tests:
	opam exec -- dune runtest test/ --auto-promote

clean:
	opam exec dune $@

# ── Benchmarks ────────────────────────────────────────────────────────────────
# Parse large VCD files BENCH_RUNS times each; timing via /usr/bin/time -f.
# Divide elapsed time by BENCH_RUNS for per-parse latency.
# Each file is run twice: once as plain parse, once with --resolve --signal
# to benchmark the resolver and filter hot path.
#
# Usage:
#   make benchmark              # default 10 runs per file
#   make benchmark BENCH_RUNS=3

BENCH_RUNS ?= 10
TIME        = /usr/bin/time -f "  elapsed: %e s  user: %U s  sys: %S s  rss: %M kB"

benchmark:
	@set -eu; \
	opam exec -- dune build bin/vcd_dump.exe; \
	BIN=_build/default/bin/vcd_dump.exe; \
	echo ""; echo "=== Icarus Verilog — pe_tb (1.5 M events, 78 MB) ==="; \
	$(TIME) $$BIN --bench $(BENCH_RUNS) test-data/digital-vcd-parser/test/debug/pe_tb.vcd; \
	echo "  -- filter: pe_tb.{clk,ych0}"; \
	$(TIME) $$BIN --bench $(BENCH_RUNS) --resolve --signal 'pe_tb.{clk,ych0}' test-data/digital-vcd-parser/test/debug/pe_tb.vcd; \
	echo ""; echo "=== Chronologic Simulation VCS — Tb_Sync_FIFO (291 k events, 14 MB) ==="; \
	$(TIME) $$BIN --bench $(BENCH_RUNS) test-data/digital-vcd-parser/test/SIMv0.1/Tb_Sync_FIFO.vcd; \
	echo "  -- filter: Tb_Sync_FIFO.{clk,rdata}"; \
	$(TIME) $$BIN --bench $(BENCH_RUNS) --resolve --signal 'Tb_Sync_FIFO.{clk,rdata}' test-data/digital-vcd-parser/test/SIMv0.1/Tb_Sync_FIFO.vcd; \
	echo ""; echo "=== Synopsys VCS — example3 / MIPS core (57 k events, 3 MB) ==="; \
	$(TIME) $$BIN --bench $(BENCH_RUNS) test-data/vcd-io/testcases/example3.vcd; \
	echo "  -- filter: Test_MIPS.**.pc (glob, all program counters)"; \
	$(TIME) $$BIN --bench $(BENCH_RUNS) --resolve --signal 'Test_MIPS.**.pc' test-data/vcd-io/testcases/example3.vcd; \
	echo ""; echo "=== NVC VHDL — tb (1.2 M events, 56 MB) ==="; \
	$(TIME) $$BIN --bench $(BENCH_RUNS) test-data/go-vcd-parser/vcd/files/samples/tb.vcd; \
	echo "  -- filter: **.{clk,tx} (glob + alternation)"; \
	$(TIME) $$BIN --bench $(BENCH_RUNS) --resolve --signal '**.{clk,tx}' test-data/go-vcd-parser/vcd/files/samples/tb.vcd

.PHONY: default clean format utop test update-tests benchmark
