
module mkTop (
  input clk100mhz, // 100 MHZ clock
  input reset,

  output tx,
  input rx,
  output [7:0] led,

  output sd_clk,
  output sd_mosi,
  input sd_miso,
  output sd_cs
);

  (* mark_debug="true" *) wire RST_N;
  clk_wiz pll (
    .clk_in1(clk100mhz),
    .clk_out1(clk83mhz),        // 100mhz
    .clk_out2(clk_ref_w),    // 200mhz
    .clk_out3(clk_ddr_w),    // 400mhz
    .clk_out4(clk_ddr_dqs_w),// 400mhz @ 90°
    .reset(reset),
    .locked(RST_N)
  );

  wire RST;
  assign RST = !RST_N;

  //TestCore inst (
  //  .out(ftdi_rxd),
  //  .in0(ftdi_txd),
  //  .clock(clk100mhz),
  //  .reset(!RST_N)
  //);

  //Uart inst (
  //  .out_tx(tx),
  //  .out_led(led),
  //  .in0(rx),
  //  .clock(clk83mhz),
  //  .reset(RST)
  //);

  TestSpi inst (
    .out_0(tx),
    .out_1_miso_0(sd_miso),
    .out_1_miso_en(1),
    .out_1_clk(sd_clk),
    .out_1_mosi(sd_mosi),
    .out_1_cs(sd_cs),
    .clock(clk83mhz),
    .reset(RST)
  );

endmodule
