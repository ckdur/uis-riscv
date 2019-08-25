// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.adc_dac

import Chisel._
import olinguitochip.config._
import olinguitochip.util._
import olinguitochip.platform.olinguito.fields._
import olinguitochip.platform.olinguito.constructs._

class AdcDacIface(implicit p: Parameters) extends OlinguitoBundle {
  
  val rd = UInt(INPUT,xLen)       // Register RD as input from ADC_DAC when used as instruction
  val rs1 = UInt(OUTPUT,xLen)         // Register RS1 as output to ADC_DAC when used as instruction
  val rs2 = UInt(OUTPUT,xLen)         // Register RS2 as output to ADC_DAC when used as instruction
  val ValidCalc = Bool(OUTPUT)
  val ReadyGot = Bool(OUTPUT)
  val ValidGot = Bool(INPUT)
  val ReadyCalc = Bool(INPUT)
}

// This nested match-case is used to connect two blocks
// that use a flag (haveADC) to create IO pins (AdcDacIface).
// Ex:
//    AdcDacConnectIfExistent(Master_or_TOP_iface, Slave_or_BOT_iface)
object AdcDacConnectIfExistent {
  def apply(iface1: Option[AdcDacIface], iface2: Option[AdcDacIface]) = {
    iface1 match {
      case Some(x: AdcDacIface) => 
        iface2 match {
          case Some(y: AdcDacIface) => x <> y
          case None => /* Nothing */
        }
      case None => /* Nothing */
    }
  }
}
