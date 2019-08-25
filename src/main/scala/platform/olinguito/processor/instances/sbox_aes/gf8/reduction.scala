// See LICENSE.txt for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.gf8

import chisel3._
import chisel3.util._

object reduction_aes {
  def apply(n: Int, A: UInt, stages: Int = 0) : UInt = {
    assert(n == A.getWidth, "A length does not match with specified length")
    assert(8 < n, "A's length must be greater than 8")
    assert(stages >= 0, "Invalid number of stages")
    
    // Cast reduction with a polynomial of x^4 + x^3 + x^1 + x^0 (0x1B)
    reduction(n, 8, A, 0x1B, stages)
  }
}

object reduction {
  def apply(n: Int, GRADE: Int, A: UInt, P: Int, stages: Int = 0) : UInt = {
    assert(n == A.getWidth, "A length does not match with specified length")
    assert(GRADE < n, "Result length must be lower than A's")
    assert(stages >= 0, "Invalid number of stages")
    
    var dividend = new Array[Int](GRADE)
    for(i <- 0 until GRADE){
      dividend(i) = (P >> i) & 0x1
    }
    
    val mod = Module(new reduction(n,GRADE,dividend,stages))
    mod.io.A := A
    mod.io.B
  }
  def applyext(n: Int, GRADE: Int, A: UInt, P: Array[Int], stages: Int = 0) : UInt = {
    assert(n == A.getWidth, "A length does not match with specified length")
    assert(GRADE == P.size, "P length does not match with specified length")
    assert(GRADE < n, "Result length must be lower than A's")
    assert(stages >= 0, "Invalid number of stages")
    val mod = Module(new reduction(n,GRADE,P,stages))
    mod.io.A := A
    mod.io.B
  }
}

class reduction(val n: Int, val GRADE: Int, val P: Array[Int], val stages: Int = 0) extends Module {
  val io = IO(new Bundle {
    val A = Input(UInt(n.W))
    val B = Output(UInt(GRADE.W))
  })
  
  assert(stages >= 0, "Invalid number of stages")
  
  // Software creation of polynomials
  var c = Array.fill[Array[Int]](GRADE)(Array.fill[Int](GRADE)(0))
  val dividend = P :+ 1
  
  for(i <- 0 until GRADE) {
    var divisor = new Array[Int](i+GRADE+1)
    divisor(i+GRADE) = 1
    var fl = true
    while(fl)
    {
      fl = false
      var j = i+GRADE
      var found = false
      while(j >= GRADE && !found) {
        if(divisor(j) == 1)
        {
          // calculate the value to xor
          var div = Array.fill[Int](i+GRADE+1)(0)
          for(k <- 0 to GRADE){
            div(j-GRADE+k) = dividend(k)
          }
          divisor = (divisor zip div).map{ case(x,y) => x^y }
          fl = true
          found = true
        }
        j -= 1
      }
    }
    c(i) = divisor.slice(0, GRADE)
  }
  // END OF: Software creation of polynomials
  
  // Hardware implementation
  
  // Hardware to software translate
  val ch = Wire(Vec(GRADE, UInt(GRADE.W)))
  for(i <- 0 until GRADE) {
    val cha = Wire(Vec(GRADE, Bool()))
    for(j <- 0 until GRADE)
    {
      cha(j) := (c(i)(j) == 1).B
    }
    ch(i) := cha.asUInt
  }
  
  // True reduction algorithm
  val pc = Wire(UInt((GRADE*2).W))
  pc := Cat(0.U((n-GRADE).W), io.A)
  
  val pr = Wire(Vec(GRADE+1, UInt(GRADE.W)))
  pr(0) := pc(GRADE-1, 0)
  for(i <- 0 until GRADE){
    when(pc(GRADE*2-1-i)) {
      pr(i+1) := pr(i)^ch(GRADE-1-i)
    }.otherwise {
      pr(i+1) := pr(i)
    }
  }
  
  //printf("MUL=[%b] MULR=[%b]\n"
  //           , io.A
  //           , pr(GRADE))
  
  // TODO: Do a more-elaborate stage divison
  // For now, just do a register shifter, then hope
  // That sintesizer do the stuff for us
  io.B := ShiftRegister(pr(GRADE), stages)
  
  
  // END OF: Hardware implementation
}
