// See LICENSE for license details.

package olinguitochip.platform.olinguito

import Chisel._
import Chisel.ImplicitConversions._
//import olinguitochip.platform.devices.debugsystem._
import olinguitochip.config._
import olinguitochip.platform.buses._
import olinguitochip.platform.olinguito.fields._
import olinguitochip.platform.olinguito.processor._
import olinguitochip.platform.olinguito.ports._
import olinguitochip.platform.olinguito.constructs._
import olinguitochip.platform.olinguito.processor.instances.adc_dac._

class Olinguito(implicit val p: Parameters) extends Module
    with HasOlinguitoParameters {
  val io = new Bundle {

    val debug = new OlinguitoDebugIface
    val interrupts = new TileInterrupts().asInput
    val iface = new HastiMasterIO()(p.alterPartial({
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

  val processor = Module(new OlinguitoNoArb)
  processor.io.interrupts := io.interrupts
  val arb = Module(new arbiter.Arbiter(cfg = p(ArbiterKey)))
  arb.io.imem <> processor.io.imem
  arb.io.dmem <> processor.io.dmem
  io.iface <> arb.io.iface

  // ADC_DAC Interface.
  // See ./instances/adc_dac/Adc_Dac_Iface.scala.
  AdcDacConnectIfExistent(io.adc_dac_iface, processor.io.adc_dac_iface)

  // RVFI
  OlinguitoRVFIConnectIfExistent(io.rvfi, processor.io.rvfi)
}
