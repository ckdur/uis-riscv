package olinguitochip.platform.olinguito.arbiter

import chisel3._
import chisel3.util._
import Chisel.ImplicitConversions._
import olinguitochip.platform.olinguito.Instructions._
import olinguitochip.platform.olinguito.constants.MemoryOpConstants._
import olinguitochip.platform.olinguito.constants.ScalarOpConstants._
import olinguitochip.platform.olinguito.processor.instances._
import olinguitochip.platform.olinguito.processor.instances.ALU._
import olinguitochip.config._
import olinguitochip.util._
import olinguitochip.platform.buses._
import olinguitochip.platform.buses.HastiConstants._
import olinguitochip.platform.olinguito._
import olinguitochip.platform.olinguito.fields._
import olinguitochip.platform.olinguito.constructs._
import olinguitochip.platform.olinguito.ports._



class ArbiterIO(cfg: arbiterConfig)(implicit p: Parameters) extends Bundle {
  val imem = Flipped(new HastiMasterIO()(p.alterPartial({
    case HastiId => "00000"
    case HastiKey("00000") => 
      HastiParameters(
        dataBits=p(XLen),
        addrBits=p(XLen)
      )
  })))
  val dmem = Flipped(new HastiMasterIO()(p.alterPartial({
    case HastiId => "00001"
    case HastiKey("00001") => 
      HastiParameters(
        dataBits=p(XLen),
        addrBits=p(XLen)
      )
  })))
  val iface = new HastiMasterIO()(p.alterPartial({
    case HastiId => "00000"
    case HastiKey("00000") => 
      HastiParameters(
        dataBits=p(XLen),
        addrBits=p(XLen)
      )
  }))
  
  override def cloneType = new ArbiterIO(cfg).asInstanceOf[this.type]
}

class Arbiter(cfg: arbiterConfig)(implicit p: Parameters) extends Module {
  
  val io = IO(new ArbiterIO(cfg))
  
  def detIntoScratchPad(addr: UInt, offset: BigInt, size: Int): Bool = {
    val is = Wire(Bool())
    val addr_mask = Wire(UInt(p(XLen).W))
    val addr_use = Wire(UInt(p(XLen).W))
    addr_mask := (size-1).U
    addr_use := offset.U
    is := ~((addr_use ^ (addr & ~addr_mask)).orR)
    is
  }
  
  // Capturing of transactions for each master
  val imem_can_save = RegInit(true.B) // TODO
  val imem_pre_show = WireInit(false.B) // TODO
  val imem_cap = Wire(new HastiMasterI)
  imem_cap := (new MasterIOtoI).apply(io.imem) holdUnless (imem_can_save || imem_pre_show)
  val dmem_can_save = RegInit(true.B) // TODO
  val dmem_pre_show = WireInit(false.B) // TODO
  val dmem_cap = Wire(new HastiMasterI)
  dmem_cap := (new MasterIOtoI).apply(io.dmem) holdUnless (dmem_can_save || dmem_pre_show)
  
  // Classic signal assumptions
  io.imem.hrdata := 0.U
  io.dmem.hrdata := 0.U
  io.imem.hready := imem_can_save // Save status is the ready for now
  io.dmem.hready := dmem_can_save
  io.imem.hresp := HRESP_OKAY // Always ok from here
  io.dmem.hresp := HRESP_OKAY
  
  // Determination of transactions
  val imem_is_scratch_pad = WireInit(false.B)
  val dmem_is_scratch_pad = WireInit(false.B)
  val imem_is_response_sp = WireInit(false.B)
  val dmem_is_response_sp = WireInit(false.B)
  
  val imem_is_bus = !imem_is_scratch_pad
  val dmem_is_bus = !dmem_is_scratch_pad
  
  val imem_is_response_bus = WireInit(false.B)
  val dmem_is_response_bus = WireInit(false.B)
  
  val imem_is_transaction = Wire(Bool())
  imem_is_transaction := imem_cap.htrans === HTRANS_NONSEQ
  val dmem_is_transaction = Wire(Bool())
  dmem_is_transaction := dmem_cap.htrans === HTRANS_NONSEQ
  
  if(cfg.has_scratch_pad) { 
    // Classic checks
    println(cfg.offset_scratch_pad >> p(XLen))
    assert((cfg.offset_scratch_pad >> p(XLen)) == 0, "Offset of scratchpad is over the address space")
    assert(isPow2((cfg.size_scratch_pad/p(XLen))), "Size of scratchpad is not power of two")
    assert((cfg.size_scratch_pad % p(XLen)) == 0 , "Size of scratchpad cannot fit into processor's word")
    assert((cfg.offset_scratch_pad % p(XLen)) == 0 , "Offset of scratchpad cannot fit into processor's word")
    assert((cfg.offset_scratch_pad % cfg.size_scratch_pad) == 0, "Offset of scratchpad cannot be fit inside specified size")
    
    // Scratchpad access
    imem_is_scratch_pad := detIntoScratchPad(imem_cap.haddr, cfg.offset_scratch_pad, cfg.size_scratch_pad)
    dmem_is_scratch_pad := detIntoScratchPad(dmem_cap.haddr, cfg.offset_scratch_pad, cfg.size_scratch_pad)
    
    // Is transaction on sp?
    val imem_is_transaction_sp = imem_is_transaction && imem_is_scratch_pad
    val dmem_is_transaction_sp = dmem_is_transaction && dmem_is_scratch_pad
    
    // Current access to scratchpad
    val next_sp = WireInit(0.U(1.W))
    val cur_sp = RegNext(next_sp, 0.U)
    when(dmem_is_transaction_sp) { // dmem has more priority over imem
      next_sp := 1.U
      // Basically, if dmem wants to do a transaction.. and also want imem, imem waits (dmem has more priority)
      when(imem_is_transaction_sp) {
        imem_can_save := false.B
      }
    }
    
    // Transaction and response determination
    // If you look well, scratch pad always responds with hready==true, so we do not need to do async
    val is_transaction_sp = imem_is_transaction_sp || dmem_is_transaction_sp
    val was_transaction_sp = RegNext(is_transaction_sp, false.B)
    imem_is_response_sp := cur_sp === 0.U && was_transaction_sp
    dmem_is_response_sp := cur_sp === 1.U && was_transaction_sp
    
    // Main memory
    val scratch_pad = Module(new scratch_mem(cfg))
    
    // Transaction signaling
    scratch_pad.io.htrans := HTRANS_IDLE
    scratch_pad.io.hsel := false.B
    scratch_pad.io.haddr := 0.U
    scratch_pad.io.hwdata := 0.U
    scratch_pad.io.hsize := 0.U
    scratch_pad.io.hburst := 0.U
    scratch_pad.io.hprot := 0.U
    scratch_pad.io.hwrite := false.B
    scratch_pad.io.hmastlock := false.B
    when(is_transaction_sp) {
      scratch_pad.io.hsel := true.B
      when(next_sp === 0.U) { // Next transaction comes from imem
        scratch_pad.io.htrans := imem_cap.htrans
        scratch_pad.io.haddr := imem_cap.haddr
        scratch_pad.io.hsize := imem_cap.hsize
        scratch_pad.io.hburst := imem_cap.hburst
        scratch_pad.io.hprot := imem_cap.hprot
        scratch_pad.io.hwrite := imem_cap.hwrite
        scratch_pad.io.hmastlock := imem_cap.hmastlock
      } .otherwise { // Next transaction comes from dmem
        scratch_pad.io.htrans := dmem_cap.htrans
        scratch_pad.io.haddr := dmem_cap.haddr
        scratch_pad.io.hsize := dmem_cap.hsize
        scratch_pad.io.hburst := dmem_cap.hburst
        scratch_pad.io.hprot := dmem_cap.hprot
        scratch_pad.io.hwrite := dmem_cap.hwrite
        scratch_pad.io.hmastlock := dmem_cap.hmastlock
      }
    }
    
    // Output response
    when(imem_is_response_sp) {
      io.imem.hready := true.B // Always true
      io.imem.hrdata := scratch_pad.io.hrdata
      scratch_pad.io.hwdata := io.imem.hwdata
    }
    when(dmem_is_response_sp) {
      io.dmem.hready := true.B // Always true
      io.dmem.hrdata := scratch_pad.io.hrdata
      scratch_pad.io.hwdata := io.dmem.hwdata
      // If there is no more transactions through dmem, imem recovers its save state
      when(!dmem_is_transaction_sp && !imem_can_save && imem_is_transaction_sp) { imem_can_save := true.B }
    }
  }
  
  // Is transaction on bus?
  val imem_is_transaction_bus = imem_is_transaction && imem_is_bus
  val dmem_is_transaction_bus = dmem_is_transaction && dmem_is_bus

  // Current access to bus
  val next_bus = WireInit(0.U(1.W))
  val cur_bus = RegNext(next_bus, 0.U)
  when(io.iface.hready) {
    when(dmem_is_transaction_bus) { // dmem has more priority over imem
      next_bus := 1.U
      // Basically, if dmem wants to do a transaction.. and also want imem, imem waits (dmem has more priority)
      when(imem_is_transaction_bus) {
        imem_can_save := false.B
      }
    }
  } .otherwise {
    next_bus := cur_bus
  }
  
  // If bus is not ready, then do not let save anymore
  when(imem_is_transaction_bus && imem_can_save && !io.iface.hready) {
    imem_can_save := false.B
  }
  when(dmem_is_transaction_bus && dmem_can_save && !io.iface.hready) {
    dmem_can_save := false.B
  }
  
  // Transaction and response determination
  val is_transaction_bus = (imem_is_transaction_bus || dmem_is_transaction_bus) && io.iface.hready
  val was_transaction_bus = RegEnable(is_transaction_bus, false.B, io.iface.hready)
  imem_is_response_bus := cur_bus === 0.U && was_transaction_bus
  dmem_is_response_bus := cur_bus === 1.U && was_transaction_bus
  
  // If bus finished transmitting, then enable save
  when(was_transaction_bus && cur_bus === 0.U && !imem_can_save && io.iface.hready) {
    imem_can_save := true.B
    imem_pre_show := true.B
  }
  when(was_transaction_bus && cur_bus === 1.U && !dmem_can_save && io.iface.hready) {
    dmem_can_save := true.B
    dmem_pre_show := true.B
  }
  
  // Transaction signaling
  io.iface.htrans := HTRANS_IDLE
  io.iface.haddr := 0.U
  io.iface.hwdata := 0.U
  io.iface.hsize := 0.U
  io.iface.hburst := 0.U
  io.iface.hprot := 0.U
  io.iface.hwrite := false.B
  io.iface.hmastlock := false.B
  when(is_transaction_bus) {
    when(next_bus === 0.U) { // Next transaction comes from imem
      io.iface.htrans := imem_cap.htrans
      io.iface.haddr := imem_cap.haddr
      io.iface.hsize := imem_cap.hsize
      io.iface.hburst := imem_cap.hburst
      io.iface.hprot := imem_cap.hprot
      io.iface.hwrite := imem_cap.hwrite
      io.iface.hmastlock := imem_cap.hmastlock
    } .otherwise { // Next transaction comes from dmem
      io.iface.htrans := dmem_cap.htrans
      io.iface.haddr := dmem_cap.haddr
      io.iface.hsize := dmem_cap.hsize
      io.iface.hburst := dmem_cap.hburst
      io.iface.hprot := dmem_cap.hprot
      io.iface.hwrite := dmem_cap.hwrite
      io.iface.hmastlock := dmem_cap.hmastlock
    }
  }
  
  // Output response
  when(imem_is_response_bus) {
    io.imem.hready := io.iface.hready
    io.imem.hrdata := io.iface.hrdata
    io.iface.hwdata := io.imem.hwdata
  }
  when(dmem_is_response_bus) {
    io.dmem.hready := io.iface.hready
    io.dmem.hrdata := io.iface.hrdata
    io.iface.hwdata :=io.dmem.hwdata
    // If there is no more transactions through dmem, imem recovers its save state, if bus' hready is true
    when(!dmem_is_transaction_bus && !imem_can_save && imem_is_transaction_bus && io.iface.hready) { imem_can_save := true.B }
  }
  
  // Some assertions
  assert(!(imem_is_response_sp && dmem_is_response_sp), "There is response from scratchpad to imem and dmem!")
  assert(!(imem_is_response_bus && dmem_is_response_bus), "There is response from bus to imem and dmem!")
  assert(!(imem_is_response_sp && imem_is_response_bus), "There is response from scratchpad and bus to imem!")
  assert(!(dmem_is_response_sp && dmem_is_response_bus), "There is response from scratchpad and bus to dmem!")
}

