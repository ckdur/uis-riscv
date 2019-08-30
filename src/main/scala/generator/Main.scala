// See README.md for license details.

package generator

import chisel3._
import olinguitochip.config._
import olinguitochip.platform.configs._
import olinguitochip.chip._
import java.io.{File, FileWriter}

object Main extends App {
  // Instance the configuration
  val conf = (new TOPConfig).asInstanceOf[Config]
  // Create the top
  val circuit = Driver.elaborate(() =>
    new olinguitochip.chip.TOPModule()(conf))
  // Dump the firrtl
  chisel3.Driver.dumpFirrtl(circuit, 
    Some(new File(s"verilog", s"TOPModule.fir")))
  // Convert the firrtl to verilog
  firrtl.Driver.execute("-i verilog/TOPModule.fir -o verilog/TOPModule.v -X verilog --top-name TOPModule --repl-seq-mem -c:TOPModule:-o:verilog/TOPModule.ram.conf".split(" +"))
}
