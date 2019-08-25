// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.wrap_sbox
import olinguitochip.platform.olinguito.processor.instances.sbox_aes.core._
import chisel3._
import chisel3.util._

object Counter{

  def counter(en: Bool): UInt = {
  val x = RegInit(0.U(2.W))
    when(en){x:= x+1.U}
    .otherwise{x:=0.U}
  x
  }
}


class wrap_sbox extends Module {
  val io = IO(new Bundle {
    val RS = Input(UInt(32.W))
    val ValidCalc = Input(Bool()) //core sbox calculation
    val ReadyGot = Input(Bool()) //core got  data
    val CalcEnd = Output(Bool()) //sbox
    val Calc = Output(Bool())  //ME calculate
    val ValidGot = Output(Bool()) //ME output data valid
    val SboxIn = Output(UInt(8.W))
    val RD = Output(UInt(32.W))
  })
  //sbox instanciation
  val Calc = WireInit(false.B)
  val sboxCore = Module(new sbox(Iimpl= 1, nresults = 4, Mstages = 4))
  io.RD := sboxCore.io.B.asUInt
  sboxCore.io.calc := Calc
  io.CalcEnd := sboxCore.io.ready
  sboxCore.io.mode := false.B
  //next state
  val sIdle :: sLoad :: sStart :: sCalc :: sReady :: Nil = Enum(5)
  val enCounter = WireInit(false.B)
  sboxCore.io.A_valid := enCounter
  io.ValidGot := false.B
  io.Calc := true.B
  //val enCap = RegInit(false.B)
  val state = RegInit(sIdle)
  val count = Counter.counter(enCounter)
  val capture = RegInit(0.U(32.W))
  when (state === sIdle) {
    when (io.ValidCalc) {
                        //enCap := true.B
                        capture := io.RS
                        state := sLoad
                        }
  }
  when (state === sLoad){ 
    enCounter:=true.B
    io.Calc := false.B
    when(count===3.U){state := sStart} 
  }
  when (state === sStart)
  {
     Calc := true.B
     state := sCalc
     io.Calc := false.B
  }
  when (state === sCalc){  
    io.Calc := false.B
    when(io.CalcEnd){ 
    state := sReady
    }
  }
  when (state === sReady) {
    io.ValidGot:=true.B
    io.Calc := false.B
    when (io.ReadyGot) { state := sIdle }
  }
   //datapath
   
   val sbin = WireInit(0.U(8.W))

    when(count===0.U)      {sbin:=capture(7,0)}
    .elsewhen(count===1.U) {sbin:=capture(15,8)}
    .elsewhen(count===2.U) {sbin:=capture(23,16)}
    .otherwise             {sbin:=capture(31,24)}
  sboxCore.io.A := sbin
  io.SboxIn := sbin
   
}



