package olinguitochip.platform.olinguito.processor.instances

import Chisel._
import Chisel.ImplicitConversions._
import olinguitochip.config._
import olinguitochip.util._
import olinguitochip.platform.olinguito._
import olinguitochip.platform.olinguito.fields._
import olinguitochip.platform.olinguito.constructs._
import olinguitochip.platform.olinguito.ports._
import olinguitochip.platform.olinguito.constants._
import olinguitochip.platform.olinguito.constants.ScalarOpConstants._

class RegFile(n: Int, w: Int, zero: Boolean = false) {
  private val rf = Mem(n, UInt(width = w))
  private def access(addr: UInt) = rf(~addr(log2Up(n)-1,0))
  private val reads = collection.mutable.ArrayBuffer[(UInt,UInt)]()
  private var canRead = true
  def read(addr: UInt) = {
    require(canRead)
    reads += addr -> Wire(UInt())
    reads.last._2 := Mux(Bool(zero) && addr === 0.U, 0.U, access(addr))
    reads.last._2
  }
  def write(addr: UInt, data: UInt) = {
    canRead = false
    when (addr =/= 0.U) {
      access(addr) := data
      for ((raddr, rdata) <- reads)
        when (addr === raddr) { rdata := data }
    }
  }
}
