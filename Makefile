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
	opam exec -- dune build bin/vcd_dump.exe bin/vcd_tool.exe; \
	DUMP=_build/default/bin/vcd_dump.exe; \
	TOOL=_build/default/bin/vcd_tool.exe; \
	echo ""; echo "=== vcd_dump — Icarus Verilog — pe_tb (1.5 M events, 78 MB) ==="; \
	$(TIME) $$DUMP --bench $(BENCH_RUNS) test-data/digital-vcd-parser/test/debug/pe_tb.vcd; \
	echo "  -- filter: pe_tb.{clk,ych0}"; \
	$(TIME) $$DUMP --bench $(BENCH_RUNS) --resolve --signal 'pe_tb.{clk,ych0}' test-data/digital-vcd-parser/test/debug/pe_tb.vcd; \
	echo ""; echo "=== vcd_dump — Chronologic Simulation VCS — Tb_Sync_FIFO (291 k events, 14 MB) ==="; \
	$(TIME) $$DUMP --bench $(BENCH_RUNS) test-data/digital-vcd-parser/test/SIMv0.1/Tb_Sync_FIFO.vcd; \
	echo "  -- filter: Tb_Sync_FIFO.{clk,rdata}"; \
	$(TIME) $$DUMP --bench $(BENCH_RUNS) --resolve --signal 'Tb_Sync_FIFO.{clk,rdata}' test-data/digital-vcd-parser/test/SIMv0.1/Tb_Sync_FIFO.vcd; \
	echo ""; echo "=== vcd_dump — Synopsys VCS — example3 / MIPS core (57 k events, 3 MB) ==="; \
	$(TIME) $$DUMP --bench $(BENCH_RUNS) test-data/vcd-io/testcases/example3.vcd; \
	echo "  -- filter: Test_MIPS.**.pc (glob, all program counters)"; \
	$(TIME) $$DUMP --bench $(BENCH_RUNS) --resolve --signal 'Test_MIPS.**.pc' test-data/vcd-io/testcases/example3.vcd; \
	echo ""; echo "=== vcd_dump — NVC VHDL — tb (1.2 M events, 56 MB) ==="; \
	$(TIME) $$DUMP --bench $(BENCH_RUNS) test-data/go-vcd-parser/vcd/files/samples/tb.vcd; \
	echo "  -- filter: **.{clk,tx} (glob + alternation)"; \
	$(TIME) $$DUMP --bench $(BENCH_RUNS) --resolve --signal '**.{clk,tx}' test-data/go-vcd-parser/vcd/files/samples/tb.vcd; \
	echo ""; echo "=== vcd_tool — Icarus Verilog — pe_tb (1.5 M events, 78 MB) ==="; \
	$(TIME) $$TOOL test-data/digital-vcd-parser/test/debug/pe_tb.vcd > /dev/null; \
	echo "  -- filter: pe_tb.{clk,ych0}"; \
	$(TIME) $$TOOL --signal 'pe_tb.{clk,ych0}' test-data/digital-vcd-parser/test/debug/pe_tb.vcd > /dev/null; \
	echo "  -- filter + range: pe_tb.{clk,ych0} t=0-500000"; \
	$(TIME) $$TOOL --signal 'pe_tb.{clk,ych0}' --range '0-500000' test-data/digital-vcd-parser/test/debug/pe_tb.vcd > /dev/null; \
	echo ""; echo "=== vcd_tool — Chronologic Simulation VCS — Tb_Sync_FIFO (291 k events, 14 MB) ==="; \
	$(TIME) $$TOOL test-data/digital-vcd-parser/test/SIMv0.1/Tb_Sync_FIFO.vcd > /dev/null; \
	echo "  -- filter: Tb_Sync_FIFO.{clk,rdata}"; \
	$(TIME) $$TOOL --signal 'Tb_Sync_FIFO.{clk,rdata}' test-data/digital-vcd-parser/test/SIMv0.1/Tb_Sync_FIFO.vcd > /dev/null; \
	echo ""; echo "=== vcd_tool — NVC VHDL — tb (1.2 M events, 56 MB) ==="; \
	$(TIME) $$TOOL test-data/go-vcd-parser/vcd/files/samples/tb.vcd > /dev/null; \
	echo "  -- filter: **.{clk,tx} (glob + alternation)"; \
	$(TIME) $$TOOL --signal '**.{clk,tx}' test-data/go-vcd-parser/vcd/files/samples/tb.vcd > /dev/null

.PHONY: default clean format utop test update-tests benchmark
