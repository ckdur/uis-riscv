// See LICENSE for license details.

package olinguitochip.platform.olinguito.constructs

import Chisel._

import olinguitochip.config._
import olinguitochip.util.ParameterizedBundle
import olinguitochip.platform.olinguito.fields._

abstract class OlinguitoModule(implicit val p: Parameters) extends Module
  with HasOlinguitoParameters
abstract class OlinguitoBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasOlinguitoParameters
