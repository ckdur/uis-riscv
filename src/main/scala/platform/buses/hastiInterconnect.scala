//package junctions
package olinguitochip.platform.buses

import chisel3._
import chisel3.util._
import olinguitochip._
import olinguitochip.config._
import olinguitochip.platform.buses.HastiConstants._
import olinguitochip.platform.olinguito.fields._

class HastiMasterI(implicit val p: Parameters) extends Bundle {
  val AddrBits = p(XLen)
  val DataBits = p(XLen)
  val DataBytes = DataBits/8
  val Alignment = log2Ceil(DataBytes)
  
  val htrans    = UInt(SZ_HTRANS.W)
  val hmastlock = Bool()
  val haddr     = UInt(AddrBits.W)
  val hwrite    = Bool()
  val hburst    = UInt(SZ_HBURST.W)
  val hsize     = UInt(SZ_HSIZE.W)
  val hprot     = UInt(SZ_HPROT.W)

  val hwdata = Bits(DataBits.W)
}
class HastiSlaveI(implicit val p: Parameters) extends Bundle {
  val AddrBits = p(XLen)
  val DataBits = p(XLen)
  val DataBytes = DataBits/8
  val Alignment = log2Ceil(DataBytes)
  
  val htrans    = UInt(SZ_HTRANS.W)
  val hmastlock = Bool()
  val haddr     = UInt(AddrBits.W)
  val hwrite    = Bool()
  val hburst    = UInt(SZ_HBURST.W)
  val hsize     = UInt(SZ_HSIZE.W)
  val hprot     = UInt(SZ_HPROT.W)
  val hsel      = Bool()

  val hwdata = Bits(DataBits.W)
}

class PortHastiInterconnect(val masters: Int, val slaves: Int)(implicit p: Parameters) extends Bundle {
  val hastiMaster = Vec(masters, Flipped(new HastiMasterIO()(p.alterPartial({
                      case HastiId => "00000"
                      case HastiKey("00000") => 
                        HastiParameters(
                          dataBits=p(XLen),
                          addrBits=p(XLen)
                        )
                    })))
                  )
  val hastiSlave  = Vec(slaves, Flipped(new HastiSlaveIO()(p.alterPartial({
                      case HastiId => "00000"
                      case HastiKey("00000") => 
                        HastiParameters(
                          dataBits=p(XLen),
                          addrBits=p(XLen)
                        )
                    })))
                  )
}

class MasterIOtoI(implicit p: Parameters) {
  def apply(inp: HastiMasterIO): HastiMasterI = {
    val i = Wire(new HastiMasterI)
    i.htrans := inp.htrans
    i.hmastlock := inp.hmastlock
    i.haddr := inp.haddr
    i.hwrite := inp.hwrite
    i.hburst := inp.hburst
    i.hsize := inp.hsize
    i.hprot := inp.hprot
    i.hwdata := inp.hwdata
    
    i
  }
}
class ShiftRightRotate{  
  def apply(A: UInt, bits: UInt, w: Int = 8) : UInt = {
    val B = Wire(UInt(w.W))
    val Tbits = Wire(UInt(log2Ceil(w).W))
    when(bits >= w.U) { Tbits := 0.U }
    .otherwise        { Tbits := bits }
    
    val B_vec = Wire(Vec(w, UInt(w.W)))
    B_vec(0) := A
    for(i <- 1 until w) {B_vec(i) := Cat(B_vec(i-1)(0), B_vec(i-1)(w-1,1))}
    B := B_vec(Tbits)
    B
  }
}

class hastiInterconect( mas: Array[hastiMasterCfg],  sla: Array[hastiSlaveCfg])(implicit p: Parameters) extends Module {

  // *** From configurations
  val sword         = p(XLen)
  val masters       = mas.size
  val slaves        = sla.size
  val numbit_master = math.max(1,log2Ceil(masters))
  val numbit_slave  = math.max(1,log2Ceil(slaves))
	
  val addr_mask = Reg(Vec(slaves,UInt(sword.W)))
  val addr_use  = Reg(Vec(slaves,UInt(sword.W)))
  
  // Checkings

  for (i <- 0 until sla.size){
    for (j <- 0 until sla.size){
      if (i != j){
        val AY1 = sla(i).offset
        val AY2 = sla(i).offset+sla(i).size-1
        val BY1 = sla(j).offset
        val BY2 = sla(j).offset+sla(j).size-1
        val cond = AY1 < BY2 && AY2 > BY1
        assert(!cond , f"Warning, overlap between slaves $i($AY1%X, $AY2%X) and $j($BY1%X, $BY2%X)")
      }
    }
    val s = sla(i).size*8
    val w = p(XLen)
    assert((s % w) == 0 , f"slave $i size cannot fit into size ($s into $w)")
    assert(isPow2(s/w), "Size of slave"+ i +" scratchpad is not power of two")
  }


   for (i <- 0 until slaves){
     addr_use(i) := sla(i).offset.U
     addr_mask(i) := (sla(i).size-1).U}
  // *** From configurations

  
  val io = IO(new PortHastiInterconnect(masters, slaves))
  
  val counter_requests = RegInit(0.U((numbit_master).W))
  val next_requests = Wire(UInt(numbit_master.W))
  val is_requests      = Wire(Vec(masters, Bool()))
  val end_requests     = Wire(Vec(masters, Bool()))
  val RhastiMaster       = Reg(Vec(masters, new HastiMasterI))
  val Master           = Wire(Vec(masters, new HastiMasterI))
  val RhreadyS         = RegInit(VecInit(Seq.fill[Bool](masters)(true.B))) // Saved
  val saveTransaction  = Wire(Vec(masters,Bool())) 
  val req_transmitted  = RegInit(false.B)
  val is_request       = RegInit(false.B)
  val end_request      = Wire(Bool())
  val enable_counter   = Wire(Bool())
  val different_future_master = Wire(Bool())
  val canSaveTransaction = WireInit(VecInit(Seq.fill[Bool](masters)(false.B))) 
  for(k <- 0 until masters) {
    // Saver of transactions
    when(saveTransaction(k)) { 
      RhastiMaster(k) := (new MasterIOtoI).apply(io.hastiMaster(k)) 
    } .elsewhen(end_requests(k)) {
      RhastiMaster(k).htrans := HTRANS_IDLE
    }
    
    // Ghost ready for masters
    when(k.U === counter_requests) { // Current processing transaction is this master
      when(enable_counter) { // About to change of master
        when((is_request && end_request) || !is_request) { // Processing a request and finished, or there is no request captured
          when(saveTransaction(k)) { RhreadyS(k) := false.B } // This is because we need to save the transaction
          .otherwise { RhreadyS(k) := true.B } // Keep ready true because we do not need to save the transaction
        }
      }
    } .otherwise {
      when(saveTransaction(k)) { RhreadyS(k) := false.B } // If not current processing, we put false if there is a request
    }
    
    // Predicter
    when(saveTransaction(k)) {
      Master(k) := (new MasterIOtoI).apply(io.hastiMaster(k))
    } .elsewhen(!RhreadyS(k)) {
      Master(k) := RhastiMaster(k)
    } .otherwise {
      Master(k) := (new MasterIOtoI).apply(io.hastiMaster(k))
    }
    
    // Saver enable (basically when can store a transaction)
    saveTransaction(k) := (io.hastiMaster(k).htrans === HTRANS_NONSEQ) && canSaveTransaction(k)
    
    // Request indicator
    when(saveTransaction(k)) { is_requests(k) := io.hastiMaster(k).htrans === HTRANS_NONSEQ }
    .otherwise               { is_requests(k) := RhastiMaster(k).htrans === HTRANS_NONSEQ }
    
    // Request terminator
    when(k.U === counter_requests) {
      end_requests(k) := end_request
    } .otherwise {
      end_requests(k) := false.B
    }
    
    // Save transaction enable
    when(k.U === counter_requests) {
      when((is_request && end_request) || !is_request) { // Processing a request and finished, or there is no request captured
        when(io.hastiMaster(k).htrans === HTRANS_NONSEQ ) { // ... and there is a transaction here
          canSaveTransaction(k) := true.B 
        }
      }
    } .otherwise { // When not counting here, always can save
      canSaveTransaction(k) := true.B
    }
  }
  
  // ** Next request (combinational)
  
  // Phase 1: Find counter_requests + 1 wrapped
  val counter_requests_1_wrapped = Wire(UInt(numbit_master.W))
  when(counter_requests < masters.U){
    counter_requests_1_wrapped := counter_requests + 1.U
  } .otherwise { 
    counter_requests_1_wrapped := 0.U
  }
  
  // Phase 2: Do shift register rotated
  val sftd = (new ShiftRightRotate).apply(is_requests.asUInt, counter_requests_1_wrapped, masters)
  
  // Phase 3: Do a priority encoder using the is_requests rotated
  val add_counter = Wire(UInt(numbit_master.W))
  when(is_requests.asUInt.orR === 0.U) { add_counter := 0.U } // No requests, so make static this thing
  .otherwise {
    add_counter := PriorityEncoder(sftd)
  }
  
  // Phase 4: Add this and wrap
  val to_add = Wire(UInt((numbit_master+1).W))
  val counter_requests_1 = Wire(UInt((numbit_master+1).W))
  val add_counter_1 = Wire(UInt((numbit_master+1).W))
  counter_requests_1 := Cat(0.U(1.W), counter_requests)
  add_counter_1 := Cat(0.U(1.W), add_counter)
  to_add := counter_requests_1+add_counter_1
  when(to_add < (masters-1).U){
    next_requests := to_add + 1.U
  } .otherwise { 
    next_requests := to_add - (masters-1).U
  }
  
  // Request counter
  when(enable_counter) { counter_requests := next_requests }

  // ** Request registering
  val req_enable = Wire(Bool())

  // General request starter (predict and save)
  val future_is_request = Wire(Bool())
  future_is_request := is_requests(next_requests)
  when(req_enable) {
      is_request  :=  future_is_request
  }
  
  // Counter enable
  enable_counter   := (!is_request && !req_transmitted) || end_request

  // Request enable (when is about to change the request counter)
  req_enable := enable_counter
  
  // Captured transaction (predict and save)
  val Master_req = Reg(new HastiMasterI)
  val Future_Master_req = Wire(new HastiMasterI)
  Future_Master_req := Master(next_requests)
  when(req_enable) {
      Master_req := Future_Master_req
  }
  
  // Address extractor (predict and save)
  val future_addr  = Wire(UInt(sword.W)) 
  future_addr := Master(next_requests).haddr
  val addr  = RegInit(0.U (sword.W)) 
  when(req_enable) {
      addr := future_addr
  }
  
  // Decode address to slaves (predicter)
  val hselp     = Wire(Vec(slaves,Bool()))
  for (k <- 0 until slaves) {
    hselp(k) := ~((addr_use(k) ^ (Master(next_requests).haddr & ~addr_mask(k))).orR)
  }
 
  // Decode address to slaves
  val hsel      = Wire(Vec(slaves,Bool()))
  for (k <- 0 until slaves) {
    hsel(k) := ~((addr_use(k) ^ (addr & ~addr_mask(k))).orR)
  }
  val slave_dec = Wire(UInt(slaves.W))  
  slave_dec := PriorityEncoder(hsel)
  val nosel = Wire(Bool()) // Error indicator
  nosel := hsel.asUInt.orR === 0.U
  
  // Different future master determination
  different_future_master := next_requests =/= counter_requests
  // Different future slave determination
  val different_future_slave = Wire(Bool())
  different_future_slave := hselp.asUInt =/= hsel.asUInt

  (io.hastiSlave zip hsel).map{ case (i,j) => i.hsel := j }
  
  val slave_hready = Wire(Bool())
  val slave_hresp  = Wire(Bool())
  
  when(nosel) {
    slave_hready := true.B
    slave_hresp := HRESP_ERROR
  } .otherwise {
    slave_hready :=  io.hastiSlave(slave_dec).hready
    slave_hresp  :=  io.hastiSlave(slave_dec).hresp
  }

  // Request end slave side
  val bypass_req_transmission = RegInit(false.B)
  when(is_request && slave_hready && !req_transmitted) { // Waiting for slave to capture our request
    req_transmitted := true.B
    when(!different_future_master && !different_future_slave) {
      bypass_req_transmission := true.B // Predicter about bypass
    }
  }
  when(req_transmitted) {
    bypass_req_transmission := !different_future_master && !different_future_slave // Bypass if there is no future master/slave
  }
  when(slave_hready && req_transmitted && (!bypass_req_transmission || different_future_master || different_future_slave)) {
    req_transmitted := false.B
  } // If is bypass, then req_transmitted still can be true
  
  // Slave request
  val Slave_req = Wire(new HastiMasterI)
  Slave_req := Master_req
  when(req_transmitted || !is_request) {Slave_req.htrans := HTRANS_IDLE}
  
  when(bypass_req_transmission) {Slave_req := Future_Master_req}
  
  
  // General request terminator
  end_request := is_request && slave_hready && req_transmitted
  

  // Master to slave interfacing
  for(i <- 0 until slaves){
    when(i.U === (slave_dec) && !nosel){
       io.hastiSlave(i).haddr        :=   Slave_req.haddr
       io.hastiSlave(i).hwrite       :=   Slave_req.hwrite
       io.hastiSlave(i).hsize        :=   Slave_req.hsize
       io.hastiSlave(i).hburst       :=   Slave_req.hburst
       io.hastiSlave(i).hprot        :=   Slave_req.hprot
       io.hastiSlave(i).hmastlock    :=   Slave_req.hmastlock
       //io.hastiSlave(i).hwdata       :=   Slave_req.hwdata
       io.hastiSlave(i).htrans       :=   Slave_req.htrans
    }
    .otherwise{
       io.hastiSlave(i).haddr        :=   0.U
       io.hastiSlave(i).hwrite       :=   false.B
       io.hastiSlave(i).hsize        :=   0.U
       io.hastiSlave(i).hburst       :=   0.U
       io.hastiSlave(i).hprot        :=   0.U
       io.hastiSlave(i).htrans       :=   HTRANS_IDLE
       io.hastiSlave(i).hmastlock    :=   false.B
       //io.hastiSlave(i).hwdata       :=   0.U
    }
    io.hastiSlave(i).hwdata       :=   io.hastiMaster(counter_requests).hwdata
  }
  
  // Slave to master interfacing
  val ToMasterHready = Wire(Bool())
  when(req_transmitted) {
    ToMasterHready := slave_hready
  } .otherwise {
    ToMasterHready := RhreadyS(counter_requests)
  }
   for(k <- 0 until masters){
    when(k.U === (counter_requests)){
       io.hastiMaster(k).hready     :=  ToMasterHready
       io.hastiMaster(k).hrdata     :=  io.hastiSlave(slave_dec).hrdata
    }
    .otherwise{
       io.hastiMaster(k).hready     :=  RhreadyS(k)
       io.hastiMaster(k).hrdata     :=  0.U
    }
    io.hastiMaster(k).hresp         :=  slave_hresp
  }
}
  
