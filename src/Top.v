
module mkTop (
  input CLK,
  input RST_N,

  output ftdi_rxd,
  input ftdi_txd,
  output [7:0] led
);
  Uart uart (
    .in0(ftdi_txd),
    .out_tx(ftdi_rxd),
    .clock(CLK),
    .reset(!RST_N),
    .out_led(led)
  );

endmodule
