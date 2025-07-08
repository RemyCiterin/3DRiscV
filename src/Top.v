
module mkTop (
  input clk100mhz, // 100 MHZ clock
  input reset,

  output tx,
  input rx,
  output [7:0] led,

  output sd_clk,
  output sd_mosi,
  input sd_miso,
  output sd_cs,
  output oled_dc,
  output oled_res,
  output oled_vdd,
  output oled_sclk,
  output oled_sdin,
  output oled_vbat,

  output wire ddr3_clk_p, ddr3_clk_n,
  output wire ddr3_reset_n,
  output wire ddr3_cke, // CKE
  output wire ddr3_cs_n, // chip select signal
  output wire ddr3_ras_n, // RAS#
  output wire ddr3_cas_n, // CAS#
  output wire ddr3_we_n, // WE#
  output wire[14-1:0] ddr3_addr,
  output wire[3-1:0] ddr3_ba,
  inout wire[16-1:0] ddr3_dq,
  inout wire[1-1:0] ddr3_dqs_p, ddr3_dqs_n,
  output wire[2-1:0] ddr3_dm,
  output wire ddr3_odt // on-die termination
);

  wire clk83mhz;
  wire clk200mhz;
  wire clk333mhz;
  wire clk333mhz90;

  (* mark_debug="true" *) wire RST_N;
  clk_wiz pll (
    .clk_in1(clk100mhz),
    .clk_out1(clk83mhz),        // 100mhz
    .clk_out2(clk200mhz),    // 200mhz
    .clk_out3(clk333mhz),    // 333mhz
    .clk_out4(clk333mhz90),// 333mhz @ 90°
    .reset(reset),
    .locked(RST_N)
  );

  wire RST;
  assign RST = !RST_N;

  //TestCore inst (
  //  .out(tx),
  //  .in0(rx),
  //  .clock(clk83mhz),
  //  .reset(RST)
  //);

  //Uart inst (
  //  .out_tx(tx),
  //  .out_led(led),
  //  .in0(rx),
  //  .clock(clk83mhz),
  //  .reset(RST)
  //);

  //TestOled inst (
  //  .out_oled_vdd(oled_vdd),
  //  .out_oled_reset(oled_res),
  //  .out_oled_clk(oled_sclk),
  //  .out_oled_vbat(oled_vbat),
  //  .out_oled_dout(oled_sdin),
  //  .out_oled_isData(oled_dc),
  //  .out_oled_debug(led),
  //  .clock(clk83mhz),
  //  .reset(RST)
  //);

  wire wb_err;
  wire wb_ack;
  wire [63:0] wb_din;
  wire [31:0] wb_addr;
  wire wb_stall;
  wire wb_cyc;
  wire wb_stb;
  wire [7:0] wb_sel;
  wire [63:0] wb_dout;
  wire wb_we;

  //assign wb_err = 0;
  //assign wb_ack = 0;
  //assign wb_din = 0;
  //assign wb_stall = 0;

  TestDDR3 inst (
    .in0_wb_err(wb_err),
    .in0_wb_ack(wb_ack),
    .in0_wb_data(wb_din),
    .in0_wb_stall(wb_stall),
    .out_0_wb_cyc(wb_cyc),
    .out_0_wb_addr(wb_addr),
    .out_0_wb_stb(wb_stb),
    .out_0_wb_sel(wb_sel),
    .out_0_wb_data(wb_dout),
    .out_0_wb_we(wb_we),
    .out_1_led(),
    .out_1_tx(tx),
    .clock(clk83mhz),
    .reset(RST)
  );

  wire [31:0] o_debug1;
  assign led = o_debug1[7:0];

  //TestSpi inst (
  //  .out_0(tx),
  //  .out_1_miso_0(sd_miso),
  //  .out_1_miso_en(1),
  //  .out_1_clk(sd_clk),
  //  .out_1_mosi(sd_mosi),
  //  .out_1_cs(sd_cs),
  //  .clock(clk83mhz),
  //  .reset(RST)
  //);

  // DDR3 Controller
  ddr3_top #(
    .CONTROLLER_CLK_PERIOD(12_000), //12_000ps, clock period of the controller interface
    .DDR3_CLK_PERIOD(3_000), //3_000ps, clock period of the DDR3 RAM device (must be 1/4 of the CONTROLLER_CLK_PERIOD)
    .ROW_BITS(14), //width of row address
    .COL_BITS(10), //width of column address
    .BA_BITS(3), //width of bank address
    .DQ_BITS(8),  //width of DQ
    .BYTE_LANES(1), //number of DDR3 modules to be controlled
    .AUX_WIDTH(4), //width of aux line (must be >= 4)
    .WB2_ADDR_BITS(32), //width of 2nd wishbone address bus
    .WB2_DATA_BITS(32), //width of 2nd wishbone data bus
    .MICRON_SIM(0), //enable faster simulation for micron ddr3 model (shorten POWER_ON_RESET_HIGH and INITIAL_CKE_LOW)
    .SELF_REFRESH(1),
    .ODELAY_SUPPORTED(0), //set to 1 when ODELAYE2 is supported
    .SECOND_WISHBONE(0) //set to 1 if 2nd wishbone is needed
  ) ddr3_top_inst (
    //clock and reset
    .i_controller_clk(clk83mhz),
    .i_ddr3_clk(clk333mhz), //i_controller_clk has period of CONTROLLER_CLK_PERIOD, i_ddr3_clk has period of DDR3_CLK_PERIOD
    .i_ref_clk(clk200mhz),
    .i_ddr3_clk_90(clk333mhz90),
    .i_rst_n(!reset && RST_N),
    // Wishbone inputs
    .i_wb_cyc(wb_cyc), //bus cycle active (1 = normal operation, 0 = all ongoing transaction are to be cancelled)
    .i_wb_stb(wb_stb), //request a transfer
    .i_wb_we(wb_we), //write-enable (1 = write, 0 = read)
    .i_wb_addr(wb_addr), //burst-addressable {row,bank,col}
    .i_wb_data(wb_dout), //write data, for a 4:1 controller data width is 8 times the number of pins on the device
    .i_wb_sel(wb_sel), //byte strobe for write (1 = write the byte)
    .i_aux(wb_we), //for AXI-interface compatibility (given upon strobe)
    // Wishbone outputs
    .o_wb_stall(wb_stall), //1 = busy, cannot accept requests
    .o_wb_ack(wb_ack), //1 = read/write request has completed
    .o_wb_data(wb_din), //read data, for a 4:1 controller data width is 8 times the number of pins on the device
    .o_aux(o_aux),
    // Wishbone 2 (PHY) inputs
    .i_wb2_cyc(), //bus cycle active (1 = normal operation, 0 = all ongoing transaction are to be cancelled)
    .i_wb2_stb(), //request a transfer
    .i_wb2_we(), //write-enable (1 = write, 0 = read)
    .i_wb2_addr(), //burst-addressable {row,bank,col}
    .i_wb2_data(), //write data, for a 4:1 controller data width is 8 times the number of pins on the device
    .i_wb2_sel(), //byte strobe for write (1 = write the byte)
    // Wishbone 2 (Controller) outputs
    .o_wb2_stall(), //1 = busy, cannot accept requests
    .o_wb2_ack(), //1 = read/write request has completed
    .o_wb2_data(), //read data, for a 4:1 controller data width is 8 times the number of pins on the device
    // PHY Interface (to be added later)
    // DDR3 I/O Interface
    .o_ddr3_clk_p(ddr3_clk_p),
    .o_ddr3_clk_n(ddr3_clk_n),
    .o_ddr3_reset_n(ddr3_reset_n),
    .o_ddr3_cke(ddr3_cke), // CKE
    .o_ddr3_cs_n(/*ddr3_cs_n*/), // chip select signal (controls rank 1 only)
    .o_ddr3_ras_n(ddr3_ras_n), // RAS#
    .o_ddr3_cas_n(ddr3_cas_n), // CAS#
    .o_ddr3_we_n(ddr3_we_n), // WE#
    .o_ddr3_addr(ddr3_addr),
    .o_ddr3_ba_addr(ddr3_ba),
    .io_ddr3_dq(ddr3_dq),
    .io_ddr3_dqs(ddr3_dqs_p),
    .io_ddr3_dqs_n(ddr3_dqs_n),
    .o_ddr3_dm(ddr3_dm),
    .o_ddr3_odt(ddr3_odt), // on-die termination
    .o_debug1(o_debug1)
  );

endmodule
