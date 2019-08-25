// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.gf8

import chisel3._
import chisel3.util._

class conv_serial extends Module {
  val io = IO(new Bundle {
    val L = Input(UInt(1.W))
    val Ie = Input(UInt(8.W))
    val Is = Input(UInt(8.W))
    val Mul = Output(UInt(8.W))
  })

  // Shift register
  val De = Wire(Vec(8, Bool()))
  regis_u(De(1),io.L,io.Ie(0),De(0))
  regis_u(De(2),io.L,io.Ie(1),De(1))
  regis_u(De(3),io.L,io.Ie(2),De(2))
  regis_u(De(4),io.L,io.Ie(3),De(3))
  regis_u(De(5),io.L,io.Ie(4),De(4))
  regis_u(De(6),io.L,io.Ie(5),De(5))
  regis_u(De(7),io.L,io.Ie(6),De(6))
  regis_u(De(0),io.L,io.Ie(7),De(7))

  // Modular reduction
  val Ds = Wire(Vec(8, Bool()))
  val Wxor = Wire(Vec(3, Bool()))
  regis_u(Ds(7),io.L,io.Is(0),Ds(0))
  Wxor(0) := Ds(7)^Ds(0)
  regis_u(Wxor(0),io.L,io.Is(1),Ds(1))
  regis_u(Ds(1),io.L,io.Is(2),Ds(2))
  Wxor(1) := Ds(7)^Ds(2)
  regis_u(Wxor(1),io.L,io.Is(3),Ds(3))
  Wxor(2) := Ds(7)^Ds(3)
  regis_u(Wxor(2),io.L,io.Is(4),Ds(4))
  regis_u(Ds(4),io.L,io.Is(5),Ds(5))
  regis_u(Ds(5),io.L,io.Is(6),Ds(6))
  regis_u(Ds(6),io.L,io.Is(7),Ds(7))

  // Logic
  val Hm = Wire(Vec(8, Bool()))
  val io_Mul = Wire(Vec(8, Bool()))
  alu1(De(0), Ds(0), io_Mul(0), Hm(0), io_Mul(0))
  alu1(Hm(0), Ds(1), io_Mul(1), Hm(1), io_Mul(1))
  alu1(Hm(1), Ds(2), io_Mul(2), Hm(2), io_Mul(2))
  alu1(Hm(2), Ds(3), io_Mul(3), Hm(3), io_Mul(3))
  alu1(Hm(3), Ds(4), io_Mul(4), Hm(4), io_Mul(4))
  alu1(Hm(4), Ds(5), io_Mul(5), Hm(5), io_Mul(5))
  alu1(Hm(5), Ds(6), io_Mul(6), Hm(6), io_Mul(6))
  alu1(Hm(6), Ds(7), io_Mul(7), Hm(7), io_Mul(7))

  // Output
  io.Mul := io_Mul.asUInt
}

object alu1 {
  def apply(hm: UInt, vl: UInt, v2: UInt, ho: UInt, vo: UInt) = {
    val inst = Module(new alu1)
    inst.io.hm := hm
    inst.io.vl := vl
    inst.io.v2 := v2
    ho := inst.io.ho
    vo := inst.io.vo
  }
}

object regis_u {
  def apply(D: UInt, l: UInt, i: UInt, Q: UInt) = {
    val inst = Module(new regis_u)
    inst.io.D := D
    inst.io.l := l
    inst.io.i := i
    Q := inst.io.Q
  }
}

// ALU
class alu1 extends Module {
  val io = IO(new Bundle {
    val hm = Input(UInt(1.W))
    val vl = Input(UInt(1.W))
    val v2 = Input(UInt(1.W))
    val ho = Output(UInt(1.W))
    val vo = Output(UInt(1.W))
  })
  
  // Wires
  val p = Wire(Bool())
  val s = Wire(Bool())
  
  // Regs
  io.ho := io.hm
  p := io.hm&io.vl
  s := p^io.v2
  val vor = RegInit(0.U(1.W))
  vor := s
  io.vo := vor
}

// Register with preset
class regis_u extends Module { 
  val io = IO(new Bundle {
    val D = Input(UInt(1.W))
    val l = Input(UInt(1.W))
    val i = Input(UInt(1.W))
    val Q = Output(UInt(1.W))
  })

  val PRN = Wire(Bool())
  PRN := ~(io.l & io.i)
  val CLRN =  Wire(Bool())
  CLRN := ~(io.l & ~io.i)
  val qp = RegInit(0.U(1.W))
  when(~CLRN)
  {
    qp := 0.U
  }
  .elsewhen(~PRN)
  {
    qp := 1.U
  }
  .otherwise 
  {
    qp := io.D
  }
  io.Q := qp
}
