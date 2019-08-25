// See LICENSE.SiFive for license details.

//change ROCKET(minus) ->>> ruber (subspecies olinguito)

package olinguitochip.chip

import chisel3._
import chisel3.experimental.{chiselName,IO,RawModule}
import olinguitochip.platform.olinguito._
import olinguitochip.platform.olinguito.processor._
import olinguitochip.platform.olinguito.constructs._
import olinguitochip.platform.olinguito.fields._
import olinguitochip.platform.olinguito.ports._
import olinguitochip.platform.configs._
import olinguitochip.config._
import olinguitochip.platform.buses._
import olinguitochip.platform.buses._
import olinguitochip.platform.devices.ram._
import olinguitochip.platform.devices.peripheral._

class TOPPeripherals extends Config((site, here, up) => {
  case PeripheryMaskRAMKey => List(
    MaskRAMParams(address = BigInt("10000000",16), depth = 13, name = "SysRAM"))
  case CustomPeripheryKey => new CustomPeripheryParams
  case `resetVector` => "h10000000"
})
  
class TOPConfig extends Config(
  new TOPPeripherals ++
  new Default32Config)

class TOPBundle(implicit p: Parameters) extends Bundle {
  // Mandatory Ports
  val interrupts = Input(new TileInterrupts())
  val someleds = Output(UInt(8.W))
  val trap = Output(Bool())
}

class TOPModule()(implicit p: Parameters) extends RawModule {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  val io = IO(new TOPBundle())

  withClockAndReset(clock, reset) {
    // olinguito core module
    val ahb = new hastiFabric
    val dut = Module(new OlinguitoNoArb)
    ahb.hastiMasterNodeCreator(dut.io.imem)
    ahb.hastiMasterNodeCreator(dut.io.dmem)
     
    // Connections
    dut.reset := reset
    dut.io.interrupts := io.interrupts // External interrupts
    dut.io.debug.halt := false.B // No halts

    // The RAM
    DoRAM(p, ahb)

    // And the peripheral
    io.someleds := DoPeripheral(p, ahb)

    // Misc
    io.trap := false.B
    
    ahb.hastiDoFabric
  }
}

