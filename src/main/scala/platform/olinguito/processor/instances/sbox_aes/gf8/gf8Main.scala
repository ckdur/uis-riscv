// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.gf8

import chisel3._

/**
  */
object gf8Main extends App {
    
    val GRADE = 8
    val P = 0x1B
    val st_conv = 1;
    val st_redu = 0;
    
    val nfifo = st_conv+st_redu+1;
    var dividend = new Array[Int](GRADE)
    for(i <- 0 until GRADE){
      dividend(i) = (P >> i) & 0x1
    }
    
    chisel3.Driver.execute(new Array[String](0), () => new convolution(GRADE) )
    chisel3.Driver.execute(new Array[String](0), () => new reduction(GRADE*2-1,GRADE,dividend) )
    chisel3.Driver.execute(new Array[String](0), () => new inverter(new inverterConfig(n = GRADE, P = dividend, impl = 0, st_conv = st_conv, st_redu = st_redu, nfifo = nfifo)) )  
}
