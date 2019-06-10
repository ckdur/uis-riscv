package fibbonaci

import chisel3._
import chisel3.util._

class dpath_ctl extends Bundle {
  val r_i_1 = Input(Bool())
  val r_i_2 = Input(Bool())
  val r_cnt = Input(Bool())
  val r_n = Input(Bool())
  val e_i_1 = Input(Bool())
  val e_i_2 = Input(Bool())
  val e_cnt = Input(Bool())
  val e_n = Input(Bool())
  val maj = Output(Bool())
  val req_valid = Output(Bool())
  val req_ready = Input(Bool())
  val res_valid = Input(Bool())
  val res_ready = Output(Bool())
}

class dpath_if(nbits: Int = 32) extends Bundle {
  val req = Flipped(DecoupledIO(UInt(nbits.W))) // Request
  val res = DecoupledIO(UInt(nbits.W)) // Response
}

class dpathio(nbits: Int = 32) extends Bundle {
  val ext = new dpath_if(nbits)
  val ctl = new dpath_ctl
}

class dpath(nbits: Int = 32) extends Module {
  val io = IO(new dpathio)

  // Registers
  val n = RegInit(0.U(nbits.W))
  val a_i_1 = RegInit(1.U(nbits.W))
  val a_i_2 = RegInit(0.U(nbits.W))
  val cnt = RegInit(0.U(nbits.W))

  // Adder
  val a_i = a_i_1 + a_i_2

  // a_ i+1 logic
  when(io.ctl.r_i_1) {
    a_i_1 := 1.U
  } .elsewhen(io.ctl.e_i_1) {
    a_i_1 := a_i
  }

  // a_ i+2 logic
  when(io.ctl.r_i_2) {
    a_i_2 := 0.U
  } .elsewhen(io.ctl.e_i_2) {
    a_i_2 := a_i_1
  }

  // counter
  when(io.ctl.r_cnt) {
    cnt := 0.U
  } .elsewhen(io.ctl.e_cnt) {
    cnt := cnt + 1.U
  }

  // Major (counter >= n)
  io.ctl.maj := cnt >= n

  // n logic
  when(io.ctl.r_n) {
    n := 0.U
  } .elsewhen(io.ctl.e_n) {
    n := io.ext.req.bits
  }

  // The output
  io.ext.res.bits := a_i_1

  // Valid and Ready connections
  io.ctl.req_valid := io.ext.req.valid
  io.ext.req.ready := io.ctl.req_ready
  io.ext.res.valid := io.ctl.res_valid
  io.ctl.res_ready := io.ext.res.ready
}
