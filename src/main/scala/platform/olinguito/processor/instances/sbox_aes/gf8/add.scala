// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.gf8

import chisel3._

object add {
  def apply(n: Int, A: UInt, B: UInt) = {
    //assert(n == A.size, "A length does not match with specified length")
    //assert(n == B.size, "B length does not match with specified length")
    val m = Module(new add(n))
    m.io.A := A
    m.io.B := B
    m.io.Sum
  }
}

class add(val n: Int) extends Module {
  val io = IO(new Bundle {
    val A    = Input(UInt(n.W))
    val B    = Input(UInt(n.W))
    val Sum  = Output(UInt(n.W))
  })
  
  io.Sum := io.A^io.B
}
