// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.core

import chisel3._
import chisel3.util._
import olinguitochip.platform.olinguito.processor.instances.sbox_aes.gf8._

class sboxBundle(val nresults: Int = 4) extends Bundle {
  val A = Input(UInt(8.W))
  val A_valid = Input(Bool()) // processor
  val A_ready = Output(Bool()) // no need
  val B = Output(Vec(nresults, UInt(8.W)))
  val calc = Input(Bool()) // calculate
  val ready = Output(Bool()) //end of calculation
  val mode = Input(Bool()) //mode=1
  
  override def cloneType = new sboxBundle(nresults).asInstanceOf[this.type]
}

class sbox(val Iimpl: Int = 1, val nresults: Int = 4, val Mstages: Int = 4) extends Module {

  assert(nresults >= 1, "Invalid number of results, at least one")
  assert(Mstages >= 0, "Invalid number of stages for sbox matrix")
  
  val io = IO(new sboxBundle(nresults))
  
  var nBresults = log2Ceil(nresults+1)
  
  // Distribute pipeline stages
  var st_conv = 0;
  var st_redu = 0;
  for(i <- 0 until (nresults-1)) { 
    if((i % 2) == 1) {st_conv = st_conv + 1} 
    else             {st_redu = st_redu + 1}
  }
  
  // Inverter
  val B = Wire(UInt(8.W))
  val B_addr = Reg(UInt( nBresults.W ))
  val invready = Wire(Bool())
  val inv = inverter_aes(impl = Iimpl, st_conv = st_conv, st_redu = st_redu, nfifo = nresults)
  inv.A := io.A
  inv.A_valid := io.A_valid
  io.A_ready := inv.A_valid
  B := inv.B
  inv.B_addr := B_addr
  inv.calc := io.calc
  invready := inv.preready // TODO: If for any reason Sbox doesn't work, you can just use ready instead of preready
  
  // Sbox matrix
  val C = Wire(UInt(8.W))
  C := matrix_sbox(A = B, stages = Mstages)
  
  // Output stage
  val K = Reg(Vec(nresults, UInt(8.W)))
  val K_addr = Reg(UInt(nBresults.W))
  val K_write = WireInit(false.B)
  val mode = Reg(Bool()) // It stores whenever is calculating Sbox (0) or just inverter (1)
  (io.B zip K).map{case(i, j) => i := j}
  when(K_write) { K(K_addr) := Mux(mode, B, C) }
  val ready = RegInit(true.B)
  io.ready := ready
  
  // Automata
  val flag = WireInit(false.B)
  val flago = Wire(Bool())
  flago := ShiftRegister(flag, Mstages)
  when(io.calc && ready) {
    printf("Start to calculate inversion\n")
    ready := false.B
    B_addr := 0.U
    K_addr := 0.U
    mode := io.mode
  }
  when(!ready && invready && (B_addr =/= nresults.U)) { // TODO: We need to lock the counter after reaching nresults?
    printf("Start to put into sbox matrix\n")
    B_addr := B_addr + 1.U
    flag := true.B
  }
  when(!ready && ((flago && !mode) || (flag && mode)) ) {
    printf("Start to save into result\n")
    K_addr := K_addr + 1.U
    K_write := true.B
    when(K_addr === (nresults-1).U) { ready := true.B }
  }
  
}
