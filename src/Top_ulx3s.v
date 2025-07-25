module mkTopULX3S (
  input CLK, // 100 MHZ clock
  input RST_N,

  output ftdi_rxd,
  input ftdi_txd,
  output [7:0] led,

  output sd_clk,
  output sd_mosi,
  input sd_miso,
  output sd_cs,
);

  wire RST;
  assign RST = !RST_N;

  TestCore inst (
    .out_0(ftdi_rxd),
    .out_1(led),
    .in0(ftdi_txd),
    .clock(CLK),
    .reset(RST)
  );

  //Uart inst (
  //  .out_tx(ftdi_rxd),
  //  .out_led(led),
  //  .in0(ftdi_txd),
  //  .clock(CLK),
  //  .reset(RST)
  //);

  //TestSpi inst (
  //  .out_0(ftdi_rxd),
  //  .out_1_miso_0(sd_miso),
  //  .out_1_miso_en(1),
  //  .out_1_clk(sd_clk),
  //  .out_1_mosi(sd_mosi),
  //  .out_1_cs(sd_cs),
  //  .clock(CLK),
  //  .reset(RST)
  //);
endmodule
