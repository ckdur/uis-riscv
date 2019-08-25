// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.VecInit
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
import olinguitochip.platform.olinguito.processor.instances.sbox_aes.wrap_sbox._
import olinguitochip.platform.olinguito.processor.instances.adc_dac._

class Datapath(implicit p: Parameters) extends OlinguitoModule()(p) with olinguitochip.platform.olinguito.constants.RISCVConstants
{
  val io = new Bundle {
    val interrupts = new TileInterrupts().asInput
    val ctrl = new CtrlDpathIO().flip
    // ADC_DAC I/O interface (see ./instances/adc_dac/)
    // The pins are instantiated if the value haveADC is true
    val adc_dac_iface = if (haveADC) Some(new AdcDacIface) else None
    val rvfi = if(haveFormal) Some((new OlinguitoRVFI).asOutput) else None
  }

  val pc = Reg(init = UInt(resetVal, xLen))
  val id_br_target = Wire(UInt())
  // CSR
  val csr = Module(new CSRFile())
  
  val xcpt = io.ctrl.id.xcpt/* || io.ctrl.csr_xcpt*/  // TODO: Exceptions from CSR?

  val npc = (Mux(io.ctrl.id.j || io.ctrl.id.br && io.ctrl.br_taken, id_br_target,
             Mux(xcpt || io.ctrl.csr_eret, csr.io.evec,
                 pc + 4.U)).asSInt & SInt(-2)).asUInt

  when (!io.ctrl.stallf) {
    pc := npc
  }

  io.ctrl.repmem.imem.haddr := Mux(io.ctrl.stallf, pc, npc)

  val id_pc = Reg(init = UInt(resetVal, xLen))
  val id_npc = Reg(init = UInt(resetVal, xLen))
  val id_inst = Reg(init = BUBBLE)

  val wb_wen = Reg(init = Bool(false))
  val wb_waddr = Reg(UInt())
  val wb_wdata = Reg(Bits())

  // !io.ctrl.killf is a power optimization (clock-gating)
  
  if(xLen == 64)
  {
    when (!io.ctrl.stalldx && !io.ctrl.killf) {
      id_pc := pc
      id_npc := npc
      id_inst := Mux(pc(2), io.ctrl.repmem.imem.hrdata(63, 32), io.ctrl.repmem.imem.hrdata) 
    }
  }
  else
  {
    when (!io.ctrl.stalldx && !io.ctrl.killf) {
      id_pc := pc
      id_npc := npc
      id_inst := io.ctrl.repmem.imem.hrdata
    }
  }

  val rf = new RegFile(if (haveEExt) 15 else 31, xLen, true)
  val id_addr = Vec(id_inst(19, 15), id_inst(24,20))
  val id_rs = id_addr.map(rf.read _)
  val id_rd = id_inst(11, 7)
  val id_imm = ImmGen(io.ctrl.id.sel_imm, id_inst)
  
  // ALU
  val alu = Module(new ALU())
  alu.io.fn := io.ctrl.id.fn_alu
  alu.io.dw := io.ctrl.id.dw_alu
  alu.io.in1 := MuxLookup(io.ctrl.id.sel_alu1, SInt(0), Seq(
      A1_RS1 -> id_rs(0).asSInt,
      A1_PC -> id_pc.asSInt
    )).asUInt
  alu.io.in2 := MuxLookup(io.ctrl.id.sel_alu2, SInt(0), Seq(
      A2_SIZE -> SInt(4),
      A2_RS2 -> id_rs(1).asSInt,
      A2_IMM -> id_imm
    )).asUInt

  // BRANCH TARGET
  // jalr only takes rs1, jump and branches take pc
  id_br_target := (Mux(io.ctrl.id.j && io.ctrl.id.sel_imm === IMM_I, id_rs(0), id_pc).asSInt + id_imm).asUInt

  // CSR
  val csr_operand = alu.io.adder_out
  csr.io.rw.addr := id_inst(31, 20)
  csr.io.rw.cmd := io.ctrl.id.csr_cmd
  csr.io.rw.wdata := csr_operand

  csr.io.exception := io.ctrl.id.xcpt
  csr.io.retire := !io.ctrl.killdx
  csr.io.cause := io.ctrl.id.cause
  csr.io.pc := id_pc
  
  csr.io.decode.csr := id_inst(31, 20)     // CKDUR: Stuff added because it was inside CSRs on rocket.
  csr.io.badaddr := wb_wdata
  val hartidWire = Wire(init = UInt(hartID, width = p(MaxHartIdBits)))
  csr.io.hartid := hartidWire
  csr.io.rocc_interrupt := Bool(false)     // No rocc_interrupt. CKDUR: Accelerator should be here in some way.
  
  // All interrupts are here!
  csr.io.interrupts := io.interrupts
    
  // From: ruber-chip/src/main/scala/ruber/Ruber.scala
  /*def encodeVirtualAddress(a0: UInt, ea: UInt) = if (vaddrBitsExtended == vaddrBits) ea else {
    // efficient means to compress 64-bit VA into vaddrBits+1 bits
    // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1))
    val a = a0 >> vaddrBits-1
    val e = ea(vaddrBits,vaddrBits-1).asSInt
    val msb =
      Mux(a === 0.U || a === 1.U, e =/= SInt(0),
      Mux(a.asSInt === SInt(-1) || a.asSInt === SInt(-2), e === SInt(-1), e(0)))
    Cat(msb, ea(vaddrBits-1,0))
  }*/

  // DMEM
  val dmem_req_addr = alu.io.adder_out
  val dmem_sgen = new StoreGen(io.ctrl.id.mem_type, dmem_req_addr, id_rs(1), xLen/8)
  val dmem_load_lowaddr = if(xLen == 64) RegEnable(dmem_req_addr(2, 0), io.ctrl.id.mem_valid && !io.ctrl.id.mem_rw)
                          else RegEnable(dmem_req_addr(1, 0), io.ctrl.id.mem_valid && !io.ctrl.id.mem_rw)
  when (io.ctrl.id.mem_valid && io.ctrl.id.mem_rw) { wb_wdata := dmem_sgen.data } // share wb_wdata with store data

  io.ctrl.repmem.dmem.haddr := dmem_req_addr
  io.ctrl.repmem.dmem.hwrite := io.ctrl.id.mem_rw
  io.ctrl.repmem.dmem.hsize := dmem_sgen.size
  io.ctrl.repmem.dmem.hwdata := RegEnable(dmem_sgen.data, io.ctrl.id.mem_valid)

  val dmem_clear_sb = io.ctrl.ll.valid && !io.ctrl.ll.fn && io.ctrl.repmem.dmem.hready
  val dmem_resp_valid = dmem_clear_sb && !io.ctrl.ll.mem_rw
  val dmem_lgen = new LoadGen(io.ctrl.ll.mem_type, mtSigned(io.ctrl.ll.mem_type), dmem_load_lowaddr, io.ctrl.repmem.dmem.hrdata, Bool(false), xLen/8)
  
  // AES Sbox
  val (sboxValid: Bool, sosbox: UInt, sboxReady: Bool) = if(haveSbox) {
    val sbox = Module(new wrap_sbox)
    sbox.io.RS := id_rs(0)
    sbox.io.ValidCalc := io.ctrl.id.sbox_valid
    sbox.io.ReadyGot := true.B
    (sbox.io.ValidGot, sbox.io.RD, sbox.io.Calc)
  } else (Bool(false), 0.U, Bool(false))

  //////////////////////////////////////////////////////////////
  // ADC_DAC
  val (adc_dacValid: Bool, adc_dacData: UInt, adc_dacReady: Bool) = io.adc_dac_iface match {
    case Some(x: AdcDacIface) =>
      x.rs1 := id_rs(0)  // RS0 to output pin
      x.rs2 := id_rs(1)  // RS1 to output pin
      // Handshake signals for ADC_DAC
      x.ValidCalc := io.ctrl.id.adc_dac_valid       // Valid to ADC_DAC
      x.ReadyGot := true.B                          // Ready to ADC_DAC
      (x.ValidGot, x.rd, x.ReadyCalc)               // From ADC_DAC
    case None =>
      (Bool(false), 0.U, Bool(false))           // Non-existent
  }
  /////////////////////////////////////////////////////////////

  // MUL/DIV
  val (mulDivRespValid : Bool, mulDivRespData : UInt, mulDivReqReady : Bool) = if (haveMExt) {
    val muldiv = Module(new MulDiv(MulDivParams(mulUnroll = if(fastMulDiv) 8 else 1,
                                   divUnroll = if(fastMulDiv) 8 else 1,
                                   mulEarlyOut = fastMulDiv,
                                   divEarlyOut = fastMulDiv),
                                   width = xLen/*,  // TODO: Check this things
                                   unroll = if(fastMulDiv) 8 else 1,
                                   earlyOut = fastMulDiv*/))
    muldiv.io.req.valid := io.ctrl.id.mul_valid
    muldiv.io.req.bits.fn := io.ctrl.id.fn_alu
    muldiv.io.req.bits.dw := io.ctrl.id.dw_alu
    muldiv.io.req.bits.in1 := id_rs(0)
    muldiv.io.req.bits.in2 := id_rs(1)
    muldiv.io.kill := Bool(false)
    muldiv.io.resp.ready := Bool(true)
    (muldiv.io.resp.valid, muldiv.io.resp.bits.data, muldiv.io.req.ready)

  } else (Bool(false), 0.U, Bool(false))

  // WB (WriteBack)
  val ll_wen = dmem_resp_valid || mulDivRespValid || sboxValid || adc_dacValid
  val wen = io.ctrl.id.wen || ll_wen
  val waddr = Mux(ll_wen, io.ctrl.ll.waddr, id_rd)
  val wdata = MuxCase(
    alu.io.out, Array(
      io.ctrl.id.csr_en -> csr.io.rw.rdata,
      dmem_resp_valid -> dmem_lgen.data, 
      mulDivRespValid -> mulDivRespData,
      sboxValid       ->	sosbox,
      adc_dacValid    ->  adc_dacData
    ))

  wb_wen := wen
  when (wen) {
    wb_waddr := waddr
    wb_wdata := wdata
  }

  when (wb_wen) {
    rf.write(wb_waddr, wb_wdata)
  }

  // to control
  io.ctrl.inst := id_inst
  io.ctrl.ma_pc := pc(1)
  io.ctrl.ma_addr := dmem_sgen.misaligned
  io.ctrl.br_taken := alu.io.cmp_out      //alu.io.out(0)
  io.ctrl.mul_ready := mulDivReqReady
  io.ctrl.sbox_ready := sboxReady
  io.ctrl.adc_dac_ready := adc_dacReady
  io.ctrl.clear_sb := dmem_clear_sb || mulDivRespValid || sboxValid || adc_dacValid
  //io.ctrl.csr_xcpt := csr.io.csr_xcpt // TODO: Exceptions from CSR?
  io.ctrl.csr_eret := csr.io.eret
  io.ctrl.csr_interrupt := csr.io.interrupt
  io.ctrl.csr_interrupt_cause := csr.io.interrupt_cause

  // Logging
  printf("Z%d: %d [%d] [%x%x%x%x%x%x|%x%x%x%x] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] [%d|%x] inst=[%x] DASM(%x)\n",
    csr.io.hartid, csr.io.time(31, 0), !io.ctrl.killdx,
    Reg(init=45,next=Mux(!io.ctrl.repmem.imem.hready, 73, 45)), // I -
    Reg(init=45,next=Mux(io.ctrl.id.br && io.ctrl.br_taken, 66, 45)), // B -
    Reg(init=45,next=Mux(io.ctrl.id.j, 74, 45)), // J -
    Reg(init=45,next=Mux(io.ctrl.logging.invalidate, 86, 45)), // V -
    Reg(init=45,next=Mux(io.ctrl.csr_eret, 83, 45)), // S -
    Reg(init=45,next=Mux(xcpt, 88, 45)), // X -
    Mux(io.ctrl.logging.sb_stall, 83, 45), // S -
    Mux(io.ctrl.logging.dmem_stall, 68, 45), // D -
    Mux(io.ctrl.logging.mul_stall, 77, 45), // M -
    Mux(xcpt, 88, 45), // X -
    id_pc, waddr, wdata, wen, id_addr(0), id_rs(0), id_addr(1), id_rs(1),
    xcpt, io.ctrl.id.cause,
    id_inst, id_inst)

  // Formal interface for riscv-formal
  io.rvfi match {
    case Some(x: OlinguitoRVFI) =>
      // Metadata
      x.valid := !io.ctrl.killdx
      x.order := 0.U // TODO: What is the order?
      x.insn := id_inst
      x.trap := xcpt
      x.halt := false.B // TODO: I think this is not supported
      x.intr := 0.U // TODO: Extract this from CSR
      x.mode := 3.U // TODO: Always Machine mode?
      x.ixl := (if(xLen == 64) 2 else 1).U
      // rs1
      x.rs1.addr := id_addr(0)
      x.rs1.rdata := id_rs(0)
      // rs2
      x.rs2.addr := id_addr(1)
      x.rs2.rdata := id_rs(1)
      // rd
      x.rd.addr := Mux(wen, waddr, 0.U)
      x.rd.wdata := Mux(wen, Mux(waddr =/= 0.U, wdata, 0.U), 0.U)
      // pc
      x.pc.rdata := id_pc // TODO: Maybe is just pc
      x.pc.wdata := id_npc   // TODO: Maybe we need to pass this to execution phase (Fetched from FE phase)
      //x.pc.rdata := id_pc
      //x.pc.wdata := id_pc
      // mem (Mask extraction copied from scratch_mem.scala)
      val DataBits = p(XLen)
      val DataBytes = DataBits/8
      val Alignment = log2Ceil(DataBytes)
      val save_mask_decode = VecInit.tabulate(Alignment+1) (UInt(_) <= dmem_sgen.size)
      val save_mask_wide   = VecInit.tabulate(DataBytes) { i => save_mask_decode(log2Ceil(i+1)) }
      val save_mask_shift  = if (Alignment == 0) 1.U else
        save_mask_wide.asUInt << dmem_req_addr(Alignment-1,0)
      x.mem.addr := dmem_req_addr
      // We use here the same because only depends of mem_type (See MemGen.scala)
      x.mem.rmask := Mux(dmem_resp_valid, save_mask_shift(DataBytes-1, 0) holdUnless !dmem_resp_valid, 0.U)
      x.mem.rdata := dmem_lgen.data
      x.mem.wmask := Mux(io.ctrl.id.mem_valid, save_mask_shift(DataBytes-1, 0), 0.U)
      x.mem.wdata := dmem_sgen.data
    case None => /* Nothing */
  }
}
