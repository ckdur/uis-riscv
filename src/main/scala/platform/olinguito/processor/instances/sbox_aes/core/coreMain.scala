// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.core

import chisel3._

/**
  */
object coreMain extends App {
    
    chisel3.Driver.execute(new Array[String](0), () => new matrix_sbox() )  
    chisel3.Driver.execute(new Array[String](0), () => new sbox() )  
}
