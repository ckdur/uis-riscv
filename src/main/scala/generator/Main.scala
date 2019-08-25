// See README.md for license details.

package generator

import chisel3._
import olinguitochip.config._
import olinguitochip.platform.configs._
import olinguitochip.chip._

object Main extends App {
  // Instance the configuration
  val conf = (new TOPConfig).asInstanceOf[Config]
  // Create Olinguito
  chisel3.Driver.execute(args, () =>
    new olinguitochip.platform.olinguito.Olinguito()(conf))
  // Create the top
  chisel3.Driver.execute(args, () =>
    new olinguitochip.chip.TOPModule()(conf))
}
