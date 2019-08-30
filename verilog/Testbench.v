`timescale 1ps/1ps

module Testbench();

  reg clock = 1'b0;
  reg reset;
  reg debug;
  reg mtip;
  reg msip;
  reg meip;
  wire [7:0] leds;
  wire trap;
  
  TOPModule dut(
    .clock(clock),
    .reset(reset),
    .io_interrupts_debug(debug),
    .io_interrupts_mtip(mtip),
    .io_interrupts_msip(msip),
    .io_interrupts_meip(meip),
    .io_someleds(leds),
    .io_trap(trap)
  );
  
  always begin
    #5 clock = !clock;
  end
  
  initial begin
    
    reset = 1'b1;
    #100;
    reset = 1'b0;
  end

endmodule
