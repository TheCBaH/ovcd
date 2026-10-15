// SystemVerilog data types as Verilator traces them (see README.md).
module sv_types (input logic clk);
  typedef enum logic [1:0] { IDLE, RUN, DONE } state_t;
  typedef struct packed { logic [3:0] tag; bit [7:0] data; } pkt_t;
  bit b = 0;
  byte by = 0;
  shortint si = 0;
  int i = 0;
  longint li = 0;
  logic [7:0] lg = 8'h01;
  integer cnt = 0;
  state_t st = IDLE;
  pkt_t pkt = '0;
  real r = 0.0;
  always_ff @(posedge clk) begin
    b <= ~b;
    by <= by + 8'sd3;
    si <= si - 16'sd7;
    i <= i + 1000;
    li <= li + 64'h1_0000_0001;
    lg <= {lg[6:0], lg[7]};
    cnt <= cnt + 1;
    st <= st == DONE ? IDLE : state_t'(st + 2'd1);
    pkt <= '{tag: pkt.tag + 4'd1, data: pkt.data ^ 8'h5a};
    r <= r + 0.25;
  end
endmodule
