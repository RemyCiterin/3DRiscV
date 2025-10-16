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

  output sdram_clk, // clock ram
  output sdram_cke,
  inout [15:0] sdram_d,
  output [12:0] sdram_a,
  output [1:0] sdram_ba,
  output [1:0] sdram_dqm,
  output sdram_csn,
  output sdram_wen,
  output sdram_rasn,
  output sdram_casn,

  output [3:0] gpdi_dp,
  input [11:11] gp,
  inout [11:11] gn
);

  parameter C_ddr = 1'b1;
  wire vga_hsync, vga_vsync, vga_blank;
  wire [7:0] r_video;
  wire [7:0] g_video;
  wire [7:0] b_video;

  wire RST;
  assign RST = !RST_N;

  wire  sdram_csn;       // chip select
  wire  sdram_clk;       // clock to SDRAM
  wire  sdram_cke;       // clock enable to SDRAM
  wire  sdram_rasn;      // SDRAM RAS
  wire  sdram_casn;      // SDRAM CAS
  wire  sdram_wen;       // SDRAM write-enable
  wire [12:0] sdram_a;   // SDRAM address bus
  wire  [1:0] sdram_ba;  // SDRAM bank-address
  wire  [1:0] sdram_dqm; // byte select
  wire [15:0] sdram_d;   // data

  assign sdram_clk = ~CLK;
  wire [15:0] sdram_dout;
  wire [15:0] sdram_din;
  wire sdram_den;

  assign sdram_din = sdram_d;
  assign sdram_d = sdram_den ? sdram_dout : 16'bzzzz;

  SocUlx3s inst (
    .out_0(ftdi_rxd),
    .out_1(led),
    .out_2_miso_0(sd_miso),
    .out_2_miso_en(1),
    .out_2_clk(sd_clk),
    .out_2_mosi(sd_mosi),
    .out_2_cs(sd_cs),
    .in0(ftdi_txd),
    .out_3_sdram_din_en(1),
    .out_3_sdram_din_0(sdram_din),
    .out_3_sdram_csn(sdram_csn),
    .out_3_sdram_rasn(sdram_rasn),
    .out_3_sdram_casn(sdram_casn),
    .out_3_sdram_wen(sdram_wen),
    .out_3_sdram_a(sdram_a),
    .out_3_sdram_ba(sdram_ba),
    .out_3_sdram_dqm(sdram_dqm),
    .out_3_sdram_dout(sdram_dout),
    .out_3_sdram_den(sdram_den),
    .out_4_hsync(vga_hsync),
    .out_4_vsync(vga_vsync),
    .out_4_blank(vga_blank),
    .out_4_red(r_video),
    .out_4_green(g_video),
    .out_4_blue(b_video),
    .clock(CLK),
    .reset(RST)
  );

  //TestCore inst (
  //  .out_0(ftdi_rxd),
  //  .out_1(led),
  //  .in0(ftdi_txd),
  //  .clock(CLK),
  //  .reset(RST)
  //);

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

  //////////////////////////////////////////////////////////////////////////////////
  // VGA 2 HDMI
  //////////////////////////////////////////////////////////////////////////////////

  wire clkdvi;
  wire clkvga;
  wire [1:0] tmds[3:0];

  vga2dvid
  #(
    .C_ddr(C_ddr),
    .C_shift_clock_synchronizer(1'b1)
  )
  vga2dvid_instance
  (
    .clk_pixel(CLK),
    .clk_shift(clkdvi),
    .in_red(r_video),
    .in_green(g_video),
    .in_blue(b_video),
    .in_hsync(vga_hsync),
    .in_vsync(vga_vsync),
    .in_blank(vga_blank),
    .out_clock(tmds[3]),
    .out_red(tmds[2]),
    .out_green(tmds[1]),
    .out_blue(tmds[0])
  );

  fake_differential
  #(
    .C_ddr(C_ddr)
  )
  fake_differential_instance
  (
    .clk_shift(clkdvi),
    .in_clock(tmds[3]),
    .in_red(tmds[2]),
    .in_green(tmds[1]),
    .in_blue(tmds[0]),
    .out_p(gpdi_dp),
    .out_n(gpdi_dn)
  );

  // clock generation for the DVI output
  clk_25_system
  clk_25_system_inst
  (
    .clk_in(CLK),
    .pll_125(clkdvi), // 125 Mhz, DDR bit rate
    .pll_25(clkvga)   //  25 Mhz, VGA pixel rate
  );
endmodule
