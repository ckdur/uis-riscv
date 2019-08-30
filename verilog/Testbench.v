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
    // Put the dump file
    $dumpfile("TOPModule.vcd"); // Waveform
    $dumpvars;
    // Put the memory
    $readmemh("boot.hex",dut.SysRAM.mem.mem_ext.ram);
    // We are not using interrupts. Deactivate them
    debug = 1'b0;
    mtip = 1'b0;
    msip = 1'b0;
    meip = 1'b0;
    // Put the reset
    reset = 1'b1;
    #100;
    reset = 1'b0;
    // Let the processor run 100000 ps then finish
    #100000;
    $finish;
  end

endmodule
