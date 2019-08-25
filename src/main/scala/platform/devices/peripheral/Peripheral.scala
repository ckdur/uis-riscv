package olinguitochip.platform.devices.peripheral

import chisel3._
import chisel3.util._
import chisel3.experimental._
import olinguitochip.util._
import olinguitochip.config._
import olinguitochip.platform.buses._
import olinguitochip.platform.olinguito.fields._

case object CustomPeripheryKey extends Field[CustomPeripheryParams]
case class CustomPeripheryParams(val address: BigInt = 0x20000000)

object DoPeripheral {
  def apply(p: Parameters, f: hastiFabric) : UInt = {
    val m = Module(new CustomPeriphery()(p))
    f.hastiSlaveNodeCreator(0x1000, p(CustomPeripheryKey).address, m.io.hasti)
    m.io.someleds
  }
}

class CustomPeripheryBundle(implicit val p: Parameters) extends Bundle {
  val hasti = new HastiSlaveIO()(p.alterPartial({
    case HastiId => "00000"
    case HastiKey("00000") =>
      HastiParameters(
        dataBits=p(XLen),
        addrBits=p(XLen)
      )
  }))
  val someleds = Output(UInt(8.W))
}

class CustomPeriphery(implicit val p: Parameters) extends Module {
  val io = IO(new CustomPeripheryBundle()(p))
  io.someleds := io.hasti.hwdata
  io.hasti.hrdata := BigInt("DEADBEEF", 16).U
  io.hasti.hready := true.B
  io.hasti.hresp := false.B
  // TODO: Write something interesting here
}