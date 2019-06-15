package fibbonaci

import chisel3._
import chisel3.util._

class cpath extends Module{
  val io = IO(Flipped(new dpath_ctl))

  def EVALID  = "b0001".U
  def RFIB    = "b0010".U
  def EFIB    = "b0011".U
  def RET     = "b0100".U
  def RET0    = "b0101".U
  def NOP     = "b0000".U

  def Y = true.B
  def N = false.B

  val decodeTable: Array[(UInt, List[UInt])] = Array(
    //             r_i_1, r_i_2, r_cnt, r_n, e_i_1, e_i_2, e_cnt, e_n, req_ready, res_valid, w_maj, w_req_valid, w_res_ready
    EVALID -> List(    N,     N,     N,   N,     N,     N,     N,   N,         Y,         N,     N,           Y,           N),
    RFIB   -> List(    Y,     Y,     Y,   Y,     N,     N,     N,   N,         N,         N,     N,           N,           N),
    EFIB   -> List(    N,     N,     N,   N,     Y,     Y,     Y,   Y,         N,         N,     Y,           N,           N),
    RET    -> List(    N,     N,     N,   N,     N,     N,     N,   N,         N,         Y,     N,           N,           Y),
    RET0   -> List(    N,     N,     N,   N,     N,     N,     N,   N,         N,         Y,     N,           N,           Y),
    NOP    -> List(    N,     N,     N,   N,     N,     N,     N,   N,         N,         N,     N,           N,           N)
  )

  val pc = RegInit(0.U(5.W))
  val instrs = VecInit(EVALID,RFIB,EFIB,RET)
  val inst = instrs(pc)

  val r_i_1 = WireInit(N)
  val r_i_2 = WireInit(N)
  val r_cnt = WireInit(N)
  val r_n = WireInit(N)
  val e_i_1 = WireInit(N)
  val e_i_2 = WireInit(N)
  val e_cnt = WireInit(N)
  val e_n = WireInit(N)
  val req_ready = WireInit(N)
  val res_valid = WireInit(N)
  val w_maj = WireInit(N)
  val w_req_valid = WireInit(N)
  val w_res_ready = WireInit(N)
  decodeTable.foreach{
    case(i,j) =>
      when(i === inst) {
        r_i_1 := j(0)
        r_i_2 := j(1)
        r_cnt := j(2)
        r_n := j(3)
        e_i_1 := j(4)
        e_i_2 := j(5)
        e_cnt := j(6)
        e_n := j(7)
        req_ready := j(8)
        res_valid := j(9)
        w_maj := j(10)
        w_req_valid := j(11)
        w_res_ready := j(12)
      }
  }

  // Control path states

  // pc
  val wait_pc = Wire(Bool())
  wait_pc := (w_maj & !io.maj) | (w_res_ready & !io.res_ready) | (w_req_valid & !io.req_valid)
  when(!wait_pc) {
    pc := pc + 1.U
    when(pc === instrs.length.U) {
      pc := 0.U
    }
  }

  // Output logic
  io.r_i_1 := r_i_1
  io.r_i_2 := r_i_2
  io.r_cnt := r_cnt
  io.r_n := r_n
  io.e_i_1 := e_i_1
  io.e_i_2 := e_i_2
  io.e_cnt := e_cnt
  io.e_n := e_n
  io.req_ready := req_ready
  io.res_valid := res_valid
}
