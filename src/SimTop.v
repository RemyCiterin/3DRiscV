`timescale 1ns/100ps
`default_nettype none

module top_sim;
  reg clk;
  always #20 clk = !clk;

  reg resetq = 0;

  TestCore top(
    .clock(clk),
    .reset(resetq)
  );

  integer i;
  initial begin
    clk = 0;
    resetq = 0;

    @(negedge clk);
    resetq = 1;
    @(negedge clk);
    resetq = 0;

    for (i=0; i < 100000000; i = i + 1) begin
      @(negedge clk);
    end

    $finish;
  end

endmodule
