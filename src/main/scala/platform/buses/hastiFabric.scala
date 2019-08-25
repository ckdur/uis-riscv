package olinguitochip.platform.buses

import chisel3._
import olinguitochip.config._

//****HASTI CONFIGS
class hastiMasterCfg(val io: HastiMasterIO)(implicit p: Parameters){
}

class hastiSlaveCfg(val io: HastiSlaveIO, val size: BigInt, val offset: BigInt)(implicit p: Parameters){
}

//****HASTI FABRIC
class hastiFabric(implicit p: Parameters) {
  var mas = new Array[hastiMasterCfg](0)
  var sla = new Array[hastiSlaveCfg](0)

  def hastiMasterNodeCreator(i: HastiMasterIO) : Unit = {
    mas = mas :+ (new hastiMasterCfg(io = i))
  }

  def hastiSlaveNodeCreator(size: BigInt, offset: BigInt, i: HastiSlaveIO) : Unit = {
    sla = sla :+ (new hastiSlaveCfg(io = i, size = size, offset = offset))
  }
  
  def hastiDoFabric = {
    val m = Module(new hastiInterconect(mas, sla))
    (m.io.hastiMaster zip mas).foreach { case (i, j) =>
      i.htrans    := j.io.htrans
      i.hmastlock := j.io.hmastlock
      i.haddr     := j.io.haddr
      i.hwrite    := j.io.hwrite
      i.hburst    := j.io.hburst
      i.hsize     := j.io.hsize
      i.hprot     := j.io.hprot
      i.hwdata    := j.io.hwdata
      
      j.io.hrdata := i.hrdata
      j.io.hready := i.hready
      j.io.hresp  := i.hresp
    }
    (m.io.hastiSlave zip sla).foreach { case (i, j) =>
      j.io.htrans    := i.htrans
      j.io.hmastlock := i.hmastlock
      j.io.haddr     := i.haddr
      j.io.hwrite    := i.hwrite
      j.io.hburst    := i.hburst
      j.io.hsize     := i.hsize
      j.io.hprot     := i.hprot
      j.io.hsel      := i.hsel
      j.io.hwdata    := i.hwdata
      
      i.hrdata       := j.io.hrdata 
      i.hready       := j.io.hready
      i.hresp        := j.io.hresp
    }
  }
}
