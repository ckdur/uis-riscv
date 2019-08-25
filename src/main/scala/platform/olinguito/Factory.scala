package olinguitochip.platform.olinguito

import chisel3._
import chisel3.util._
import olinguitochip.config._
import olinguitochip.platform.buses._
import olinguitochip.platform.olinguito.fields._
import olinguitochip.platform.olinguito.processor._
import olinguitochip.platform.olinguito.ports._

object OlinguitoFactory {
  def apply(p: Parameters, f: hastiFabric, rst: Bool) : (TileInterrupts, OlinguitoDebugIface) = {
    withReset(rst) {
      val m = Module(new Olinguito()(p))
      f.hastiMasterNodeCreator(m.io.iface)
      m.suggestName("core")
      (m.io.interrupts, m.io.debug)
    }
  }
}

object OlinguitoNoArbFactory {
  def apply(p: Parameters, f: hastiFabric, rst: Bool) : Unit = {
    val m = Module(new OlinguitoNoArb()(p))
    f.hastiMasterNodeCreator(m.io.imem)
    f.hastiMasterNodeCreator(m.io.dmem)
    m.suggestName("core")
  }
}
