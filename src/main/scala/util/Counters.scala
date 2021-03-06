// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

//change ROCKET(minus) ->>> ruber (subspecies olinguito)

package olinguitochip.util

import Chisel._


// Produces 0-width value when counting to 1
class ZCounter(val n: Int) {
  val value = Reg(init=UInt(0, log2Ceil(n)))
  def inc(): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value === UInt(n-1)
      value := Mux(Bool(!isPow2(n)) && wrap, 0.U, value + 1.U)
      wrap
    }
  }
}

object ZCounter {
  def apply(n: Int) = new ZCounter(n)
  def apply(cond: Bool, n: Int): (UInt, Bool) = {
    val c = new ZCounter(n)
    var wrap: Bool = null
    when (cond) { wrap = c.inc() }
    (c.value, cond && wrap)
  }
}

object TwoWayCounter {
  def apply(up: Bool, down: Bool, max: Int): UInt = {
    val cnt = Reg(init = UInt(0, log2Up(max+1)))
    when (up && !down) { cnt := cnt + 1.U }
    when (down && !up) { cnt := cnt - 1.U }
    cnt
  }
}

// a counter that clock gates most of its MSBs using the LSB carry-out
case class WideCounter(width: Int, inc: UInt = 1.U, reset: Boolean = true)
{
  private val isWide = width > 2*inc.getWidth
  private val smallWidth = if (isWide) inc.getWidth max log2Up(width) else width
  private val small = if (reset) Reg(init=UInt(0, smallWidth)) else Reg(UInt(width = smallWidth))
  private val nextSmall = small +& inc
  small := nextSmall

  private val large = if (isWide) {
    val r = if (reset) Reg(init=UInt(0, width - smallWidth)) else Reg(UInt(width = width - smallWidth))
    when (nextSmall(smallWidth)) { r := r + 1.U }
    r
  } else null

  val value = if (isWide) Cat(large, small) else small
  lazy val carryOut = {
    val lo = (small ^ nextSmall) >> 1
    if (!isWide) lo else {
      val hi = Mux(nextSmall(smallWidth), large ^ (large +& 1.U), 0.U) >> 1
      Cat(hi, lo)
    }
  }

  def := (x: UInt) = {
    small := x
    if (isWide) large := x >> smallWidth
  }
}


