// See README.md for license details.

package gcd

import chisel3._

class GCDPort extends Bundle {
	val value1        = Input(UInt(16.W))
	val value2        = Input(UInt(16.W))
	val loadingValues = Input(Bool())
	val outputGCD     = Output(UInt(16.W))
	val outputValid   = Output(Bool())
}

object GCDFunc {
	def apply(io: GCDPort) = {
		val x  = Reg(UInt(16.W))
		val y  = Reg(UInt(16.W))

		when(x > y) { x := x - y } 
		.otherwise { y := y - x }

		when(io.loadingValues) {
		x := io.value1
		y := io.value2
		}

		io.outputGCD := x
		io.outputValid := y === 0.U
	}
}



/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */
  
  
class GCD extends Module {
  val io = IO(new GCDPort)
  GCDFunc(io)
}

object GCD {
	def apply(gcdp : GCDPort) : Unit = {
		val m = Module(new GCD)
		m.io <> gcdp
	}
	def do_inline(gcdp : GCDPort) : Unit = {
		GCDFunc(gcdp)
	}
}

class Top(n: Int = 2)  extends Module {
	val io = IO(new Bundle {
		val gcdp = Vec(n, new GCDPort)
		val ready = Output(Bool())
	})
	
	io.gcdp.foreach{ case g => 
		GCD.apply(g)
	}
	
	io.ready := true.B
}