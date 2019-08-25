// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor

import Chisel._
import olinguitochip.config._
import olinguitochip.platform.buses._
import olinguitochip.platform.olinguito.fields._
import olinguitochip.platform.olinguito.ports._
import olinguitochip.platform.olinguito.processor.instances.adc_dac._

// Module for Olinguito without Arbiter interface
class OlinguitoNoArb(implicit val p: Parameters) extends Module
    with HasOlinguitoParameters {
  val io = new Bundle {
    val debug = new OlinguitoDebugIface
    val interrupts = new TileInterrupts().asInput
    val imem = new HastiMasterIO()(p.alterPartial({
      case HastiId => "00000"
      case HastiKey("00000") => 
        HastiParameters(
          dataBits=p(XLen),
          addrBits=p(XLen)
        )
    }))
    val dmem = new HastiMasterIO()(p.alterPartial({
      case HastiId => "00000"
      case HastiKey("00000") => 
        HastiParameters(
          dataBits=p(XLen),
          addrBits=p(XLen)
        )
    }))
    // ADC_DAC I/O interface (see ./instances/adc_dac/)
    // The pins are instantiated if the value haveADC is true
    val adc_dac_iface = if (haveADC) Some(new AdcDacIface) else None
    // RVFI interface (riscv-vormal)
    val rvfi = if(haveFormal) Some((new OlinguitoRVFI).asOutput) else None
  }

  // Control path and data path instantiation and connection
  val ctrl = Module(new Control)
  val dpath = Module(new Datapath)
  dpath.io.ctrl <> ctrl.io.dpath
  io.imem <> ctrl.io.mem.imem
  io.dmem <> ctrl.io.mem.dmem
  
  // Debug interface signals
  ctrl.io.halt := io.debug.halt
  
  // Interrupt signals
  dpath.io.interrupts := io.interrupts

  // ADC_DAC Interface. This nested match-case is used to connect two blocks
  // that use a flag (haveADC) to create IO pins (AdcDacIface).
  // See ./instances/adc_dac/Adc_Dac_Iface.scala. The same as:
  //
  //  io.adc_dac_iface match {
  //    case Some(iface_top: AdcDacIface) => 
  //      dpath.io.adc_dac_iface match {
  //        case Some(iface_dpath: AdcDacIface) => iface_top <> iface_dpath }}
  //
  AdcDacConnectIfExistent(io.adc_dac_iface, dpath.io.adc_dac_iface)

  // RVFI
  OlinguitoRVFIConnectIfExistent(io.rvfi, dpath.io.rvfi)
}
