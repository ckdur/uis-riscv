package olinguitochip.platform.olinguito.arbiter

import Chisel._
import chisel3.VecInit
import olinguitochip.config._
import olinguitochip.platform.buses._
import olinguitochip.platform.buses.HastiConstants._
import olinguitochip.platform.olinguito.fields._
import olinguitochip.util._

// CKDUR: Copied from: hasti.scala (HastiTestRAM)
class scratch_mem(cfg: arbiterConfig)(implicit p: Parameters) extends Module {
  val io = IO(new HastiSlaveIO()(p.alterPartial({
    case HastiId => "00000"
    case HastiKey("00000") => 
      HastiParameters(
        dataBits=p(XLen),
        addrBits=p(XLen)
      )
  })))
  
  val depth = log2Ceil(cfg.size_scratch_pad)
  val DataBits = p(XLen)
  val DataBytes = DataBits/8
  val Alignment = log2Ceil(DataBytes)
  
  // Calculate the bitmask of which bytes are being accessed
  val mask_decode = VecInit.tabulate(Alignment+1) (UInt(_) <= io.hsize)
  val mask_wide   = VecInit.tabulate(DataBytes) { i => mask_decode(log2Ceil(i+1)) }
  val mask_shift  = if (Alignment == 0) 1.U else
                    mask_wide.asUInt << io.haddr(Alignment-1,0)
  
  // The request had better have been aligned! (AHB-lite requires this)
  if (Alignment >= 1) {
    assert (io.htrans === HTRANS_IDLE || io.htrans === HTRANS_BUSY ||
      (io.haddr & mask_decode.asUInt()(Alignment,1)) === 0.U,
      "request not aligned")
  }
  
  // The mask and address during the address phase
  val a_request   = io.hsel && io.htrans === HTRANS_NONSEQ
  val a_mask      = Wire(UInt(width = DataBytes))
  val a_address   = io.haddr(depth-1, Alignment)
  val a_write     = io.hwrite

  // for backwards compatibility with chisel2, we needed a static width in definition
  a_mask := mask_shift(DataBytes-1, 0)
  
  // The data phase signals
  val d_read  = RegNext(a_request && !a_write, false.B)
  val d_mask  = RegEnable(a_mask, a_request)
  val d_wdata = VecInit.tabulate(DataBytes) { i => io.hwdata(8*(i+1)-1, 8*i) }
  
  // AHB writes must occur during the data phase; this poses a structural
  // hazard with reads which must occur during the address phase. To solve
  // this problem, we delay the writes until there is a free cycle.
  //
  // The idea is to record the address information from address phase and
  // then as soon as possible flush the pending write. This cannot be done
  // on a cycle when there is an address phase read, but on any other cycle
  // the write will execute. In the case of reads following a write, the
  // result must bypass data from the pending write into the read if they
  // happen to have matching address.
  
  // Pending write?
  val p_valid     = RegInit(Bool(false))
  val p_address   = Reg(a_address)
  val p_mask      = Reg(a_mask)
  val p_latch_d   = RegNext(a_request && a_write, false.B)
  val p_wdata     = d_wdata holdUnless p_latch_d
  
  // Use single-ported memory with byte-write enable
  val mem = SeqMem(1 << (depth-Alignment), Vec(DataBytes, Bits(width = 8)))
  
  // Decide is the SRAM port is used for reading or (potentially) writing
  val read = a_request && !a_write
  // In case we are stalled, we need to hold the read data
  val d_rdata = mem.readAndHold(a_address, read)
  // Whenever the port is not needed for reading, execute pending writes
  when (!read && p_valid) { mem.write(p_address, p_wdata, p_mask.asBools) }
  when (!read) { p_valid := Bool(false) }
  
  // Record the request for later?
  when (a_request && a_write) {
    p_valid   := Bool(true)
    p_address := a_address
    p_mask    := a_mask
  }
  
  // Does the read need to be muxed with the previous write?
  val a_bypass = a_address === p_address && p_valid
  val d_bypass = RegEnable(a_bypass, a_request)
  
  // Mux in data from the pending write
  val muxdata = Vec((p_mask.asBools zip (p_wdata zip d_rdata))
                    map { case (m, (p, r)) => Mux(d_bypass && m, p, r) })
  // Wipe out any data the master should not see (for testing)
  val outdata = Vec((d_mask.asBools zip muxdata)
                    map { case (m, p) => Mux(d_read && m, p, Bits(0)) })

  // Finally, the outputs
  io.hrdata := outdata.asUInt
  io.hready := true.B
  io.hresp  := HRESP_OKAY
}

class scratch_mem_old(cfg: arbiterConfig)(implicit p: Parameters) extends Module {
  val io = IO(new HastiSlaveIO()(p.alterPartial({
    case HastiId => "00000"
    case HastiKey("00000") =>
      HastiParameters(
        dataBits=p(XLen),
        addrBits=p(XLen)
      )
  })))

  val depth = log2Ceil(cfg.size_scratch_pad)
  val DataBits = p(XLen)
  val DataBytes = DataBits/8
  val Alignment = log2Ceil(DataBytes)
  val AddrBits = p(XLen)

  val wdata = Reg(Vec(DataBits/8, Bits(width = 8)))
  val waddr = Reg(UInt(width = AddrBits))
  val wvalid = Reg(init = Bool(false))
  val wsize = Reg(UInt(width = SZ_HSIZE))
  val ram = SeqMem(1 << (depth-Alignment), Vec(DataBits/8, Bits(width = 8)))

  val wmask_lut = MuxLookup(wsize, Bits(0xf), Seq(
    0.U -> Bits(0x1),
    1.U -> Bits(0x3)))
  val wmask = Wire(UInt(width = 4))
  wmask := wmask_lut << waddr(1,0)

  val s_w1 :: s_w2 :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_w1)

  when (state === s_w2) {
    wdata := VecInit.tabulate(DataBits/8)(i => io.hwdata(8*(i+1)-1,8*i))
    state := s_w1
  }

  val raddr = io.haddr >> 2.U
  val ren = Wire(init=Bool(false))
  val bypass = Reg(Bool())

  when (io.hsel && (io.htrans === HTRANS_NONSEQ)) {
    when (io.hwrite) {
      waddr := io.haddr
      wsize := io.hsize
      wvalid := Bool(true)
      when (wvalid) {
        ram.write(waddr >> 2.U, wdata, wmask.asBools)
      }
      state := s_w2
    } .otherwise {
      ren := Bool(true)
      bypass := ((waddr >> 2.U) === raddr) && wvalid
    }
  }

  val rdata = ram.read(raddr, ren).asUInt()
  val rmask = FillInterleaved(8, wmask & Fill(DataBits / 8, bypass))
  io.hrdata := (wdata.asUInt() & rmask) | (rdata & ~rmask)

  io.hready := Bool(true)
  io.hresp := HRESP_OKAY
}



