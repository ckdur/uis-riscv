// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.core

import chisel3._
import chisel3.util._
import olinguitochip.platform.olinguito.processor.instances.sbox_aes.gf8.add

object matrix_sbox {
  def apply(A: UInt, stages: Int = 0) : UInt = {
    assert(8 == A.getWidth, "A length does not match width in 8")
    val m = Module(new matrix_sbox(stages = stages))
    m.io.A := A
    m.io.B
  }
}

class matrix_sbox(val stages: Int = 0) extends Module {
  val io = IO(new Bundle{
    val A = Input(UInt(8.W))
    val B = Output(UInt(8.W))
  })
  
  assert(stages >= 0, "Invalid number of stages")
  
  def ShiftRightRotate(A: UInt, bits: UInt, w: Int = 8) : UInt = {
    assert(isPow2(w), "This rotator needs to be squared")
    val B = Wire(UInt(w.W))
    val Tbits = Wire(UInt(log2Ceil(w).W))
    Tbits := bits(log2Ceil(w)-1, 0)
    val B_vec = Wire(Vec(w, UInt(w.W)))
    B_vec(0) := A
    for(i <- 1 until w) {B_vec(i) := Cat(B_vec(i-1)(0), B_vec(i-1)(w-1,1))}
    B := B_vec(Tbits)
    B
  }
  
  def ShiftRightRotateFixed(A: UInt, bits: Int, w: Int = 8) : UInt = {
    assert(isPow2(w), "This rotator needs to be squared")
    val B = Wire(UInt(w.W))
    val Tbits =  bits & (w-1)
    if(bits == 0) B := A
    else B := Cat(A(Tbits-1,0), A(w-1,Tbits))
    B
  }
  
  val res = Wire(Vec(8, Bool()))
  for(i <- 0 until 8) {
    res(i) := (Reverse(ShiftRightRotateFixed("b10001111".U, i, 8)) & io.A).xorR === 1.U
  }
  
  val xres = Wire(UInt(8.W))
  xres := add(8, res.asUInt, "b01100011".U)
  
  val reg = Wire(UInt(8.W))
  reg := ShiftRegister(xres, stages)
  
  printf("A=[%b] res=[%b] xres=[%b]\n", io.A, res.asUInt, xres)
  io.B := reg
}
