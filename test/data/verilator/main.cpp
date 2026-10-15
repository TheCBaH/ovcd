// Runs sv_types for 40 half cycles; writes argv[1] as FST when it ends in .fst, else VCD.
#include <memory>
#include <string>

#include "Vsv_types.h"
#include "verilated.h"
#if VM_TRACE_FST
#include "verilated_fst_c.h"
using Trace = VerilatedFstC;
#else
#include "verilated_vcd_c.h"
using Trace = VerilatedVcdC;
#endif

int main(int argc, char **argv) {
  auto ctx = std::make_unique<VerilatedContext>();
  ctx->traceEverOn(true);
  auto top = std::make_unique<Vsv_types>(ctx.get());
  Trace tfp;
  top->trace(&tfp, 99);
  tfp.open(argv[1]);
  for (int i = 0; i < 40; ++i) {
    top->clk = i & 1;
    top->eval();
    tfp.dump(ctx->time());
    ctx->timeInc(5);
  }
  tfp.close();
  top->final();
  return 0;
}
