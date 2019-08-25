// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.gf8

import chisel3._
import chisel3.util._

object convolution {
  def apply(n: Int, A: UInt, B: UInt, stages: Int = 0) : UInt = {
    assert(n == A.getWidth, "A length does not match with specified length")
    assert(n == B.getWidth, "B length does not match with specified length")
    assert(stages >= 0, "Invalid number of stages")
    val m = Module(new convolution(n, stages))
    m.io.A := A
    m.io.B := B
    m.io.Mul
  }
}

class convolution(val n: Int, val stages: Int = 0) extends Module {
  val m : Int = 2*n-1;
  val io = IO(new Bundle {
    val A = Input(UInt(n.W))
    val B = Input(UInt(n.W))
    val Mul = Output(UInt(m.W))
  })
  
  assert(stages >= 0, "Invalid number of stages")
  
  
  val F = Wire(UInt((2*n).W))
  F := Cat(0.U(n.W), io.A)
  val G = Wire(UInt((2*n).W))
  G := Cat(0.U(n.W), io.B)
  
  val C = Wire(Vec(m, Bool()))
  for(i <- 0 to m-1){
    val Cp = Wire(Vec(i+1,Bool()))
    for(j <- 0 to i){
      if(j == 0) {Cp(j) := F(j)&G(i-j)}
      else {Cp(j) := Cp(j-1)^(F(j)&G(i-j))}
    }
    C(i) := Cp(i)
  } 
  
  //printf("OP1=[%b] OP2=[%b] MUL=[%b]\n"
  //           , io.A
  //           , io.B
  //           , C.asUInt)
  
  // TODO: Do a more-elaborate stage divison
  // For now, just do a register shifter, then hope
  // That sintesizer do the stuff for us
  io.Mul := ShiftRegister(C.asUInt, stages)
}
