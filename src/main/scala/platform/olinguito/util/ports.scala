// See LICENSE for license details.

package olinguitochip.platform.olinguito.ports

import Chisel._
import Chisel.ImplicitConversions._
import olinguitochip.config._
import olinguitochip.platform.buses._
import olinguitochip.util._
import olinguitochip.platform.olinguito.fields._
import olinguitochip.platform.olinguito.constructs._

class TileInterrupts(implicit p: Parameters) extends OlinguitoBundle()(p) {
  val debug = Bool()
  val mtip = Bool()
  val msip = Bool()
  val meip = Bool()
  val seip = usingVM.option(Bool())
  val lip = Vec(coreParams.nLocalInterrupts, Bool())
}

class MemIO(implicit p: Parameters) extends Bundle {
  val imem = new HastiMasterIO()(p.alterPartial({
    case HastiId => "00000"
    case HastiKey("00000") => 
      HastiParameters(
        dataBits=p(XLen),
        addrBits=p(XLen)
      )
  }))
  val dmem = new HastiMasterIO()(p.alterPartial({
    case HastiId => "00001"
    case HastiKey("00001") => 
      HastiParameters(
        dataBits=p(XLen),
        addrBits=p(XLen)
      )
  }))
}

class OlinguitoDebugIface(implicit val p: Parameters) extends Bundle {
  val halt = Bool(INPUT)
}

class OlinguitoRVFIRegisterR(val NRET: Int = 1)(implicit p: Parameters) extends OlinguitoBundle()(p) {
  val addr = UInt((NRET*5).W)
  val rdata = UInt((NRET*xLen).W)

  //override def cloneType = new OlinguitoRVFIRegisterR(NRET)(p).asInstanceOf[this.type]
}

class OlinguitoRVFIRegisterW(val NRET: Int = 1)(implicit p: Parameters) extends OlinguitoBundle()(p) {
  val addr = UInt((NRET*5).W)
  val wdata = UInt((NRET*xLen).W)

  //override def cloneType = new OlinguitoRVFIRegisterW(NRET)(p).asInstanceOf[this.type]
}

class OlinguitoRVFIPC(val NRET: Int = 1)(implicit p: Parameters) extends OlinguitoBundle()(p) {
  val rdata = UInt((NRET*xLen).W)
  val wdata = UInt((NRET*xLen).W)

  //override def cloneType = new OlinguitoRVFIPC(NRET)(p).asInstanceOf[this.type]
}

class OlinguitoRVFIMem(val NRET: Int = 1)(implicit p: Parameters) extends OlinguitoBundle()(p) {
  val addr = UInt((NRET*xLen).W)
  val rmask = UInt((NRET*xLen/8).W)
  val wmask = UInt((NRET*xLen/8).W)
  val rdata = UInt((NRET*xLen).W)
  val wdata = UInt((NRET*xLen).W)

  //override def cloneType = new OlinguitoRVFIMem(NRET)(p).asInstanceOf[this.type]
}

class OlinguitoRVFIMetadata(implicit p: Parameters) extends OlinguitoBundle()(p) {
  val NRET: Int = 1
  val valid = UInt(NRET.W)
  val order = UInt((NRET*64).W)
  val insn = UInt((NRET*ILEN).W)
  val trap = UInt(NRET.W)
  val halt = UInt(NRET.W)
  val intr = UInt(NRET.W)
  val mode = UInt((NRET*2).W)
  val ixl = UInt((NRET*2).W)
}

class OlinguitoRVFI(implicit override val p: Parameters) extends OlinguitoRVFIMetadata {
  val rs1 = new OlinguitoRVFIRegisterR(NRET)
  val rs2 = new OlinguitoRVFIRegisterR(NRET)
  val rd = new OlinguitoRVFIRegisterW(NRET)
  val pc = new OlinguitoRVFIPC(NRET)
  val mem = new OlinguitoRVFIMem(NRET)

  //override def cloneType = (new OlinguitoRVFI(NRET)(p)).asInstanceOf[this.type]
}

object OlinguitoRVFIConnectIfExistent { // Copied from Adc_DacIface.scala
  def apply(dest: Option[OlinguitoRVFI], src: Option[OlinguitoRVFI]) = {
    dest match {
      case Some(dest: OlinguitoRVFI) =>
        src match {
          case Some(src: OlinguitoRVFI) => dest := src
          case None => /* Nothing */
        }
      case None => /* Nothing */
    }
  }
}
