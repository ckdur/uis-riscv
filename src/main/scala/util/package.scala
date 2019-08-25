// See LICENSE.SiFive for license details.

package olinguitochip

import Chisel._
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine, UnlocatableSourceInfo}
import scala.math.min

package object util {
  // From diplomacy
  def sourceLine(sourceInfo: SourceInfo, prefix: String = " (", suffix: String = ")") = sourceInfo match {
    case SourceLine(filename, line, col) => s"$prefix$filename:$line:$col$suffix"
    case _ => ""
  }

  def bitIndexes(x: BigInt, tail: Seq[Int] = Nil): Seq[Int] = {
    require (x >= 0)
    if (x == 0) {
      tail.reverse
    } else {
      val lowest = x.lowestSetBit
      bitIndexes(x.clearBit(lowest), lowest +: tail)
    }
  }

  // From util
  implicit class UIntIsOneOf(val x: UInt) extends AnyVal {
    def isOneOf(s: Seq[UInt]): Bool = s.map(x === _).reduce(_||_)

    def isOneOf(u1: UInt, u2: UInt*): Bool = isOneOf(u1 +: u2.toSeq)
  }

  implicit class SeqToAugmentedSeq[T <: Data](val x: Seq[T]) extends AnyVal {
    def apply(idx: UInt): T = {
      if (x.size == 1) {
        x.head
      } else if (!isPow2(x.size)) {
        // For non-power-of-2 seqs, reflect elements to simplify decoder
        (x ++ x.takeRight(x.size & -x.size)).toSeq(idx)
      } else {
        // Ignore MSBs of idx
        val truncIdx =
          if (idx.isWidthKnown && idx.getWidth <= log2Ceil(x.size)) idx
          else (idx | UInt(0, log2Ceil(x.size)))(log2Ceil(x.size)-1, 0)
        (x.head /: x.zipWithIndex.tail) { case (prev, (cur, i)) => Mux(truncIdx === i.U, cur, prev) }
      }
    }

    def asUInt(): UInt = Cat(x.map(_.asUInt).reverse)
  }

  implicit class DataToAugmentedData[T <: Data](val x: T) extends AnyVal {
    def holdUnless(enable: Bool): T = Mux(enable, x, RegEnable(x, enable))
  }

  implicit class SeqMemToAugmentedSeqMem[T <: Data](val x: SeqMem[T]) extends AnyVal {
    def readAndHold(addr: UInt, enable: Bool): T = x.read(addr, enable) holdUnless RegNext(enable)
  }

  implicit def uintToBitPat(x: UInt): BitPat = BitPat(x)
  implicit def wcToUInt(c: WideCounter): UInt = c.value

  implicit class UIntToAugmentedUInt(val x: UInt) extends AnyVal {
    def sextTo(n: Int): UInt = {
      require(x.getWidth <= n)
      if (x.getWidth == n) x
      else Cat(Fill(n - x.getWidth, x(x.getWidth-1)), x)
    }

    def padTo(n: Int): UInt = {
      require(x.getWidth <= n)
      if (x.getWidth == n) x
      else Cat(UInt(0, n - x.getWidth), x)
    }

    def extract(hi: Int, lo: Int): UInt = {
      if (hi == lo-1) 0.U
      else x(hi, lo)
    }

    def inRange(base: UInt, bounds: UInt) = x >= base && x < bounds
  }

  implicit class BooleanToAugmentedBoolean(val x: Boolean) extends AnyVal {
    def toInt: Int = if (x) 1 else 0

    // this one's snagged from scalaz
    def option[T](z: => T): Option[T] = if (x) Some(z) else None
  }

  object PopCountAtLeast {
    private def two(x: UInt): (Bool, Bool) = x.getWidth match {
      case 1 => (x.asBool, Bool(false))
      case n =>
        val half = x.getWidth / 2
        val (leftOne, leftTwo) = two(x(half - 1, 0))
        val (rightOne, rightTwo) = two(x(x.getWidth - 1, half))
        (leftOne || rightOne, leftTwo || rightTwo || (leftOne && rightOne))
    }
    def apply(x: UInt, n: Int): Bool = n match {
      case 0 => Bool(true)
      case 1 => x.orR
      case 2 => two(x)._2
      case 3 => PopCount(x) >= n.U
    }
  }
}

