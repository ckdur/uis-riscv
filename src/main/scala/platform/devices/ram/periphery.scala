package olinguitochip.platform.devices.ram


import chisel3._
import chisel3.util._
import chisel3.experimental._
import olinguitochip.util._
import olinguitochip.config._
import olinguitochip.platform.buses._

case object PeripheryMaskRAMKey extends Field[List[MaskRAMParams]]

case class MaskRAMParams(val address: BigInt = 0x40000000, val depth:Int = 11, name: String = "SysRAM")

object DoRAM {
  def apply(p: Parameters, f: hastiFabric) : Unit = {
    p(PeripheryMaskRAMKey).foreach {
      case i =>
        val m = Module(new HastiRAM(i.depth)(p))
        f.hastiSlaveNodeCreator(1<<i.depth, i.address, m.io)
        m.suggestName(i.name)
    }
  }
}
