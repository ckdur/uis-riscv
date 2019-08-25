// See LICENSE for license details.

package olinguitochip.platform.olinguito.processor.instances.sbox_aes.gf8

import chisel3._
import chisel3.util._

class inverterConfig (
  val n: Int, 
  val P: Array[Int], 
  val impl: Int = 0, 
  val st_conv: Int = 0, 
  val st_redu: Int = 0, 
  val nfifo: Int = 1
)
{
  val bits_fifo = Math.max(log2Ceil(nfifo), 1)
}

class inverterBundle(val n: Int, nfifo: Int = 1) extends Bundle {
  val bits_fifo = Math.max(log2Ceil(nfifo), 1)
  val A = Input(UInt(n.W))
  val A_valid = Input(Bool())
  val A_ready = Output(Bool())
  val B = Output(UInt(n.W))
  val B_addr = Input(UInt(bits_fifo.W))
  val calc = Input(Bool())
  val ready = Output(Bool())
  val preready = Output(Bool())

  override def cloneType = new inverterBundle(n, nfifo).asInstanceOf[this.type]
}

object inverter_aes {
  def apply(impl: Int = 0, st_conv: Int = 0, st_redu: Int = 0, nfifo: Int = 1) : inverterBundle = {
    // Put GF8 polynomial from AES
    val Pol = 0x1B
    var P = new Array[Int](8)
    for(i <- 0 until 8){
      P(i) = (Pol >> i) & 0x1
    }
    inverter(new inverterConfig(n = 8, P = P, impl = impl, st_conv = st_conv, st_redu = st_redu, nfifo = nfifo) )
  }
}

object inverter {
  def apply(cfg: inverterConfig) : inverterBundle = {
    val m = Module(new inverter(cfg))
    val io = Wire(new inverterBundle(cfg.n, cfg.nfifo))
    io <> m.io
    io
  }
}

class inverter(val cfg: inverterConfig) extends Module {
  val n = cfg.n 
  val P = cfg.P  
  val impl = cfg.impl
  val st_conv = cfg.st_conv
  val st_redu = cfg.st_redu
  val nfifo = cfg.nfifo
  val bits_fifo = cfg.bits_fifo
  val io = IO(new inverterBundle(n, nfifo) )
  
  assert(n == P.size, "P length does not match with specified length")
  assert(impl >= 0 && impl <= 1, "We do not support this inverter implementation")
  assert(st_conv >= 0, "Invalid number of stages for convolution")
  assert(st_redu >= 0, "Invalid number of stages for reduction")
  assert(nfifo <= (st_conv+st_redu+1), "Invalid number of fifo stages for reduction")
  assert(nfifo >= 1, "Invalid number of fifo stages, at least one")
  
  // m1 and m calculation
  val m1a = n - 1
  val m1bits = log2Ceil(m1a)
  val m1 = Wire(UInt(m1bits.W))
  m1 := m1a.U
  val m = n*2 - 1
  
  // t bit test and early calculation
  var t_test: Int = 1
  var tsa = Array.fill[Int](m1bits-1)(0)
  for(i <- (m1bits-2) to 0 by -1){
    tsa(i) = t_test
    if(((m1a >> i) & 1) == 1) {
      t_test = (t_test*2) + 1
    }
    else {
      t_test = t_test*2
    }
  }
  val tbits = log2Ceil(t_test)
  val ts = Wire(Vec(m1bits-1, UInt(tbits.W)))
  for(i <- (m1bits-2) to 0 by -1){
    ts(i) := tsa(i).U
  }
  
  // Some constants
  val OP1_OP1 = 0.U
  val OP1_OP2 = 1.U
  val OP1_BTA = 2.U
  val OP1_ALP = 3.U
  val OP1_AL0 = 4.U
  val OP1_OP0 = 5.U
  val OP1_SZ = 6
  val OP1_SZB = log2Ceil(OP1_SZ)
  
  val OP2_OP2 = 0.U
  val OP2_ALP = 1.U
  val OP2_AL0 = 2.U
  val OP2_SZ = 3
  val OP2_SZB = log2Ceil(OP2_SZ)
  
  def FIFO_COUNTER(r: Bool, u: Bool, d: Bool, nfifo: Int) : UInt = {
    val bits_fifo = Math.max(log2Ceil(nfifo), 1)
    val cnt = RegInit(0.U(bits_fifo.W))
    
    when(r) {
      when(u) {
        if(nfifo <= 1) {cnt := 0.U}
        else {cnt := 1.U}
      }
      .otherwise {
        cnt := 0.U
      }
    }
    .elsewhen(u) {
      when(cnt === (nfifo-1).U) { cnt := 0.U }
      .otherwise { cnt := cnt+1.U }
    }
    .elsewhen(d) {
      when(cnt === 0.U) { cnt := (nfifo-1).U }
      .otherwise { cnt := cnt-1.U }
    }
    cnt
  }
  
  def FIFO_GEN(n: Int, nfifo: Int, in: UInt) : (UInt, UInt, Bool, Bool, Bool, Bool, Vec[UInt]) = {
    val bits_fifo = Math.max(log2Ceil(nfifo), 1)
    
    val s = Wire(Bool())
    val r = Wire(Bool())
    val u = Wire(Bool())
    val d = Wire(Bool())
    val f = Reg(Vec(nfifo, UInt(n.W)))
    val cnt = Wire(UInt(bits_fifo.W))
    val ext = Wire(UInt(n.W))
    cnt := FIFO_COUNTER(r, u, d, nfifo)
    when(s) { f(cnt) := in } // When control says that can be saved
    ext := f(cnt) // For us, btaf is the final result (final beta)
    
    (ext, cnt, s, r, u, d, f)
  }
  
  if(impl == 0)
  {
    // ** FIRST IMPLEMENTATION: Just use one convolution and reduction
    
    // Calculation phase
    val conv_op1 = Wire(UInt(n.W))
    val conv_op2 = Wire(UInt(n.W))
    val conv_result = Wire(UInt(m.W))
    val redu_result_noreg = Wire(UInt(n.W))
    val redu_result = Reg(UInt(n.W)) // Makes sure at least one pipeline stage
    conv_result := convolution(n, conv_op1, conv_op2, st_conv)
    redu_result_noreg := reduction.applyext(m, n, conv_result, P, st_redu)
    redu_result := redu_result_noreg
    
    // Controls
    val A_ctl_ready = Wire(Bool())
    val conv_op1_sel = Wire(UInt(OP1_SZB.W))
    val conv_op2_sel = Wire(UInt(OP2_SZB.W))
    io.A_ready := A_ctl_ready
    val flag = Wire(Bool())
    
    // Flag sync
    val flago = Wire(Bool())
    flago := ShiftRegister(flag, st_conv+st_redu+1)
    
    // Temporals and asignations
    val op2 = Wire(UInt(n.W))
    op2 := redu_result
    val bta2 = Wire(UInt(n.W))
    bta2 := redu_result
    val bta = Wire(UInt(n.W))
    bta := redu_result
    
    // FIFOs (with counters)
    val (op1, op1_cnt, op1s, op1r, op1u, op1d, op1f) = FIFO_GEN(n, nfifo, redu_result) // Predictable result
    val (alp, alp_cnt, alps, alpr, alpu, alpd, alpf) = FIFO_GEN(n, nfifo, io.A)
    io.B := op1f(io.B_addr) // For us, btaf is the final result (final beta)
    
    // Multiplexers for operator selection
    val conv_op1_mux = Wire(Vec(OP1_SZ, UInt(n.W)))
    conv_op1_mux(OP1_OP1) := op1
    conv_op1_mux(OP1_ALP) := alp
    conv_op1_mux(OP1_OP2) := op2
    conv_op1_mux(OP1_BTA) := bta
    conv_op1_mux(OP1_AL0) := alpf(0)
    conv_op1_mux(OP1_OP0) := op1f(0)
    conv_op1 := conv_op1_mux(conv_op1_sel)
    val conv_op2_mux = Wire(Vec(OP2_SZ, UInt(n.W)))
    conv_op2_mux(OP2_OP2) := op2
    conv_op2_mux(OP2_ALP) := alp
    conv_op2_mux(OP2_AL0) := alpf(0)
    conv_op2 := conv_op2_mux(conv_op2_sel)
    
    // ** Control machine
    
    // States and substates
    val sIdle :: s_two_powers :: s_beta_square :: s_alpha_beta :: s_beta_powers :: s_final_inv :: s_save_inv :: Nil = Enum(7)
    val state = RegInit(sIdle)
    val i = RegInit(0.U(log2Ceil(n).W))
    val j = RegInit(0.U(tbits.W))
    val t = Wire(UInt(tbits.W))
    val t1 = Wire(UInt(tbits.W))
    t := ts(i)        // Limit tester checks for t+1 here instead of t
    t1 := ts(i)-1.U   // Limit tester checks for t here instead of t-1
    val begin_two_powers = RegInit(false.B)
    val last_saved = RegInit(false.B)
    val save_op1 = RegInit(false.B)
    val last_reachd = RegInit(false.B)
    
    // Default outputs
    op1s := false.B
    op1r := false.B
    op1u := false.B
    op1d := false.B
    alps := false.B
    alpr := false.B
    alpu := false.B
    alpd := false.B
    A_ctl_ready := false.B
    conv_op1_sel := 0.U
    conv_op2_sel := 0.U
    flag := 0.U
    io.ready := false.B
    io.preready := false.B
    
    // Main FSM
    
    // Debug status
    val conv_op1_delayed = Wire(UInt(n.W))
    val conv_op2_delayed = Wire(UInt(n.W))
    val conv_result_delayed = Wire(UInt(m.W))
    conv_op1_delayed := ShiftRegister(conv_op1, st_conv+st_redu+1)
    conv_op2_delayed := ShiftRegister(conv_op2, st_conv+st_redu+1)
    conv_result_delayed := ShiftRegister(conv_result, st_redu+1)
    when (state =/= sIdle) {
      printf("OP1=[%b] OP2=[%b] MUL=[%b] MULR=[%b] %b\n"
             , conv_op1_delayed
             , conv_op2_delayed
             , conv_result_delayed
             , redu_result, flago)
    }
    
    // Idle state:
    when (state === sIdle) {
      
      // Output logic
      A_ctl_ready := !last_saved
      io.ready := true.B
      io.preready := true.B
      
      // Additional calculations
      when (io.A_valid && !last_saved) { 
        alps := true.B  // Save and increase alp FIFO
        alpu := true.B 
      }
      when(alp_cnt === (nfifo-1).U && io.A_valid){
        last_saved := true.B // FIFO reached limit, trigger last_saved
      }
      
      // Next state stuff
      when (io.calc)   { 
        // Calculation operation: Do beta^(2^t+1) or beta^2^t
        state := s_two_powers
        flag := 1.U

        alpr := true.B
        alpu := false.B
        op1r := true.B
        op1u := false.B
        
        save_op1 := false.B; begin_two_powers := true.B
        
        // Put i and j accordingly
        i := (m1bits-2).U
        j := 0.U
        
        // Reset misc
        last_saved := false.B
        
        printf("Changing to: s_two_powers\n")
      }
    }
    
    // beta^(2^t+1) or beta^2^t
    when (state === s_two_powers) {
    
      printf("j=[%d] t=[%d] i=[%d]\n", j, t, i)
      // Precalculation
      val stop_cond = Wire(Bool())
      when(m1(i)) { stop_cond := j === t }
      .otherwise  { stop_cond := j === t1 }
      
      when(flago === 1.U) { 
        j := j + 1.U // Increase j here, so it can be replaced later
        flag := 1.U
      }
      
      // Output logic
      when(begin_two_powers) {
        conv_op1_sel := OP1_ALP // When beggining, we need to compute alpha*alpha
        conv_op2_sel := OP2_ALP
      }
      .otherwise
      {
        conv_op1_sel := OP1_OP2 // Default operand is OP2
        conv_op2_sel := OP2_OP2 // Default operand is OP2
      }
      
      // Additional calculations
      when (save_op1) { 
        op1s := true.B  // Save and increase op1 FIFO
        op1u := true.B 
      }
      when (begin_two_powers) { 
        alpu := true.B  // Load alpha when beggining two powers
      }
      when (j === 0.U && flago === 1.U && !save_op1) { save_op1 := true.B; begin_two_powers := false.B }
      when (op1_cnt === (nfifo-1).U && save_op1)     { save_op1 := false.B }
      
      // Next state stuff
      when (stop_cond && flago === 1.U)   { 
        // Calculation operation: 
        when(m1(i)) { 
          // Do beta^2
          state := s_beta_square
          printf("Changing to: s_beta_square\n")
        }
        .otherwise  { 
          // Do beta*beta^2^t
          state := s_beta_powers
          printf("Changing to: s_beta_powers\n")
        }
        flag := 1.U
        
        // Reset stuff used here
        j := 0.U
        save_op1 := false.B // TODO: Paranoia
        op1r := true.B
        op1u := false.B // Prevent to be 1.U
        alpr := true.B
        alpu := false.B // Prevent to be 1.U
      }
    }
    
    // beta^2
    when (state === s_beta_square) { 
      
      // Output logic
      conv_op1_sel := OP1_OP1
      conv_op2_sel := OP2_OP2
      
      // Additional calculations
      op1u := true.B // Just increase the OP1 FIFO
      
      // Next state stuff
      when (flago === 1.U)   { 
        // Calculation operation: 
        // Do alpha*beta
        state := s_alpha_beta
        flag := 1.U
        
        // Reset stuff used here
        op1r := true.B
        op1u := false.B // Prevent to be 1.U
        alpr := true.B
        alpu := false.B // Prevent to be 1.U
        printf("Changing to: s_alpha_beta\n")
      }
    }
    
    // alpha*beta
    when (state === s_alpha_beta) { 
      
      // Output logic
      conv_op1_sel := OP1_ALP
      conv_op2_sel := OP2_OP2
      
      // Additional calculations
      alpu := true.B // Just increase the alpha FIFO
      
      // Next state stuff
      when (flago === 1.U)   { 
        // Calculation operation: 
        when(i === 0.U) {
          // Do Final Inversion Calculation
          state := s_final_inv
          printf("Changing to: s_final_inv\n")
        }
        .otherwise {
          // Do beta^(2^t+1) or beta^2^t
          i := i - 1.U
          state := s_two_powers
          printf("Changing to: s_two_powers\n")
        
          // Predict if save_op1 flag needs to be 1
          save_op1 := false.B
        }
        flag := 1.U
        
        // Reset stuff used here
        op1r := true.B
        op1u := false.B // Prevent to be 1.U
        alpr := true.B
        alpu := false.B // Prevent to be 1.U
      }
    }
    
    // beta*beta^2^t
    when (state === s_beta_powers) { 
      
      // Output logic
      conv_op1_sel := OP1_BTA
      conv_op2_sel := OP2_OP2
      
      // Additional calculations
      
      // Next state stuff
      when (flago === 1.U)   { 
        // Calculation operation: 
        when(i === 0.U) {
          // Do Final Inversion Calculation
          state := s_final_inv
          printf("Changing to: s_final_inv\n")
        }
        .otherwise {
          // Do beta^(2^t+1) or beta^2^t
          i := i - 1.U
          state := s_two_powers
          printf("Changing to: s_two_powers\n")
        
          // Predict if save_op1 flag needs to be 1
          save_op1 := false.B
        }
        flag := 1.U
        
        // Reset stuff used here
        op1r := true.B
        op1u := false.B
        alpr := true.B
        alpu := false.B
      }
    }
    
    // Final Inversion Calculation
    when (state === s_final_inv) { 
      
      // Output logic
      conv_op1_sel := OP1_OP2
      conv_op2_sel := OP2_OP2
      
      // Additional calculations
      
      // Next state stuff
      when (flago === 1.U)   { 
        // Calculation operation: 
        // Do Final Inversion Saving
        flag := 1.U
        
        // Predict if next state is continue saving... or Idle
        state := s_save_inv; printf("Changing to: s_save_inv\n")
        op1r := true.B
        op1u := false.B // Start in 1
        
        // Reset stuff used here
        alpr := true.B
        alpu := false.B
        
      }
    }
    
    // Final Inversion Saving
    when (state === s_save_inv) { 
      
      // Output logic
      A_ctl_ready := !last_saved
      when(op1_cnt =/= 0.U) {io.preready := true.B} // Indicating at least the first is calculated
      
      // Additional calculations
      when(!last_reachd)
      {
        op1u := true.B // Increase and save inside op1
        op1s := true.B
      }
      when(op1_cnt === (nfifo-1).U && !last_reachd) // If limit of saving is reached, do not save anumore
      {
        last_reachd := true.B
      }
      when (io.A_valid && !last_saved) { 
        alps := true.B  // Save and increase alp FIFO
        alpu := true.B 
      }
      when(alp_cnt === (nfifo-1).U && io.A_valid){
        last_saved := true.B // FIFO reached limit, trigger last_saved
      }
      
      // Next state stuff
      when (flago === 1.U || last_reachd)   { 
        // Goto Idle
        state := sIdle
        
        // Reset stuff used here
        op1r := true.B
        op1u := false.B // Prevent to update to 1.U
        last_reachd := false.B
        printf("Changing to: sIdle\n")
      }
    }
  }
  else if(impl == 1)
  {
    // ** SECOND IMPLEMENTATION: Algorithm without using convolutioner or reductor
    
    // Convert the polinomial into bits
    val Pol = Wire(Vec(n+1, Bool()))
    for(j <- 0 until n)
    {
      Pol(j) := (P(j) == 1).B
    }
    Pol(n) := true.B // Add the final '1'
    
    // Custom bundle for signals in this impl
    val CalcSignals_length = 4*(n+1) + 2 // TODO: A more automated way to do this
    class CalcSignals extends Bundle {
      val u = UInt((n+1).W)
      val v = UInt((n+1).W)
      val g1 = UInt((n+1).W)
      val g2 = UInt((n+1).W)
      val u_reach = Bool()
      val v_reach = Bool()
    }
    
    // Helper function to do the calculations for this implementation
    def next_calc_m1(cur: CalcSignals, flag: Bool, f: UInt, n: Int, stages: Int = 0, rst: Bool = false.B) : (CalcSignals, Bool) = {
      assert(stages >= 0, "Invalid number of stages")
      
      val u = cur.u
      val v = cur.v
      val g1 = cur.g1
      val g2 = cur.g2
      val reachd = cur.u_reach || cur.v_reach
      
      // Main algorithm
      val next_u = Wire(UInt((n+1).W))
      val next_v = Wire(UInt((n+1).W))
      val next_g1 = Wire(UInt((n+1).W))
      val next_g2 = Wire(UInt((n+1).W))
      val mid_u = Wire(UInt((n+1).W))
      val mid_v = Wire(UInt((n+1).W))
      val mid_g1 = Wire(UInt((n+1).W))
      val mid_g2 = Wire(UInt((n+1).W))
      val pol_u = Wire(UInt((n+1).W))
      val pol_v = Wire(UInt((n+1).W))
      val u_reach = Wire(Bool())
      val v_reach = Wire(Bool())
      
      when(reachd) { // Only do calculations when limit not reached
        next_u := u
        next_v := v
        next_g1 := g1
        next_g2 := g2
        u_reach := cur.u_reach
        v_reach := cur.v_reach
        mid_u := 0.U
        mid_v := 0.U
        mid_g1 := 0.U
        mid_g2 := 0.U
        pol_u := 0.U
        pol_v := 0.U
      } .otherwise {
      
        when (!u(0)) {
          mid_u := u >> 1
          when(!g1(0)) {
            mid_g1 := g1 >> 1
          }
          .otherwise {
            mid_g1 := (g1 ^ f) >> 1
          }
        }
        .otherwise {
          mid_u := u
          mid_g1 := g1
        }
        
        when (!v(0)) {
          mid_v := v >> 1
          when(!g2(0)) {
            mid_g2 := g2 >> 1
          }
          .otherwise {
            mid_g2 := (g2 ^ f) >> 1
          }
        }
        .otherwise {
          mid_v := v
          mid_g2 := g2
        }
        
        val gradu = Wire(UInt(log2Ceil(n+1).W))
        val jud_u = Reverse(mid_u)
        gradu := PriorityEncoder(jud_u)
        val gradv = Wire(UInt(log2Ceil(n+1).W))
        val jud_v = Reverse(mid_v)
        gradv := PriorityEncoder(jud_v)
        
        //printf("gradu=[%d] gradv=[%d] u=[%b] v=[%b]\n", gradu, gradv, mid_u, mid_v)
	
	      // Finding polynomial degree (Note: grad_ are reversed)
	      when(gradu < gradv) {
		      pol_u := mid_u ^ mid_v
		      next_g1 := mid_g1 ^ mid_g2
		      pol_v := mid_v
		      next_g2 := mid_g2
	      } .otherwise {
		      pol_u := mid_u
		      next_g1 := mid_g1
		      pol_v := mid_u ^ mid_v
		      next_g2 := mid_g2 ^ mid_g1
	      }
	      
	      u_reach := pol_u === 1.U
	      v_reach := pol_v === 1.U
	      
	      next_u := pol_u
	      when(u_reach) { next_v := next_g1; printf("Saving g1\n") }
	      .elsewhen(v_reach) { next_v := next_g2; printf("Saving g2\n") }
	      .otherwise {next_v := pol_v}
	      
	    }
      
      // Deliver result in the bundle, TODO: Do a better pipeline staging
      val reg = Wire(new CalcSignals)
      val flago = Wire(Bool())
      reg.u := ShiftRegister(next_u, stages+1)
      reg.v := ShiftRegister(next_v, stages+1)
      reg.g1 := ShiftRegister(next_g1, stages+1)
      reg.g2 := ShiftRegister(next_g2, stages+1)
      reg.u_reach := ShiftRegister(u_reach, stages+1)
      reg.v_reach := ShiftRegister(v_reach, stages+1)
      flago := ShiftRegister(flag, stages+1)
      
	    (reg, flago)
    }
    
    // FIFO declaration
    val data_rst = Wire(Bool())
    val data_in = Wire(new CalcSignals)
    val (data, data_cnt, datas, datar, datau, datad, dataf) = FIFO_GEN(CalcSignals_length, nfifo, data_in.asUInt)
    val data_cur = data.asTypeOf(new CalcSignals)
    val data_cur_bits = Wire(new CalcSignals)
    data_cur_bits := data_cur
    
    when(data_rst) { 
      dataf.map {
        case i => 
          val curv = i.asTypeOf(new CalcSignals)
          val cur = Wire(new CalcSignals)
          cur.u := curv.u
          cur.v := Pol.asUInt
          cur.g1 := 1.U
          cur.g2 := 0.U
          when(curv.u === 0.U) { // If the input is already 0, prevent to do calculations
            cur.u_reach := true.B
          } .otherwise {
            cur.u_reach := false.B
          }
          cur.v_reach := false.B
          i := cur.asUInt
          printf("RESET: u=[%b] v=[%b] g1=[%b] g2=[%b] ur=[%b] vr=[%b]\n"
                 , cur.u
                 , cur.v
                 , cur.g1
                 , cur.g2
                 , cur.u_reach
                 , cur.v_reach)
      }
    }
    
    // Output definition (inefficent)
    val data_for_B = Wire(UInt(CalcSignals_length.W))  
    data_for_B := dataf(io.B_addr)
    val data_B = data_for_B.asTypeOf(new CalcSignals)
    io.B := data_B.v(7,0)
    
    // Signals driven by control machine
    val flag = Wire(Bool())
    val rst_flag = Wire(Bool())
    val A_ctl_ready = Wire(Bool())
    io.A_ready := A_ctl_ready
    
    // Main calculation
    val (data_next, flago) = next_calc_m1(data_cur_bits, flag, Pol.asUInt, n, st_conv+st_redu, rst_flag)
    
    // ** Control machine
    
    // States and substates
    val sIdle :: sCalc :: Nil = Enum(2)
    val state = RegInit(sIdle)
    val last_saved = RegInit(false.B)
    val last_reachd = RegInit(false.B)
    val first_calc = RegInit(false.B)
    
    // Default values
    data_in := data_next
    flag := false.B
    data_rst := false.B
    datas := false.B
    datar := false.B
    datau := false.B
    datad := false.B
    io.ready := false.B
    io.preready := false.B
    A_ctl_ready := false.B
    rst_flag := false.B
    
    // Done signal, TODO: Pre-detect for current saving point on fifo about (data_next.u_reach || data_next.v_reach)
    val done = Wire(Bool())
    val donev = Wire(Vec(nfifo, Bool()))
    (donev zip dataf).map {
      case (j,i) => 
        val cur = i.asTypeOf(new CalcSignals)
        j := cur.u_reach || cur.v_reach
    }
    done := false.B
    
    // Main
    when(state === sIdle)
    {
      // Output logic
      A_ctl_ready := !last_saved
      io.ready := true.B
      io.preready := true.B
      data_in := data_cur
      data_in.u := io.A
      
      // Additional calculations
      when (io.A_valid && !last_saved) { 
        datas := true.B  // Save and increase alp FIFO
        datau := true.B 
      }
      when(data_cnt === (nfifo-1).U && io.A_valid){
        last_saved := true.B // FIFO reached limit, trigger last_saved
      }
      
      // Next state stuff
      when(io.calc)
      {
        state := sCalc
        datar := true.B
        datau := false.B
        flag := true.B
        last_saved := false.B
        first_calc := true.B
        data_rst := true.B
        printf("Changing state: sCalc\n")
      }
    }
    
    when(state === sCalc)
    {
      // Output logic
      // Additional calculations
      flag := flago
      done := donev.asUInt.andR === 1.U
      when(!last_reachd)
      {
        datau := true.B // Increase data FIFO
        when(!first_calc) { datas := data_next.u_reach || data_next.v_reach } // Save inside FIFO if necessary
      }
      
      when(!first_calc) {
        data_cur_bits := data_next
        printf("u=[%b] v=[%b] g1=[%b] g2=[%b] ur=[%b] vr=[%b] %b %d\n"
               , data_next.u
               , data_next.v
               , data_next.g1
               , data_next.g2
               , data_next.u_reach
               , data_next.v_reach
               , datas, data_cnt)
      }
      .otherwise {
        printf("BEGIN: u=[%b] v=[%b] g1=[%b] g2=[%b] ur=[%b] vr=[%b] %b %d\n"
               , data_cur_bits.u
               , data_cur_bits.v
               , data_cur_bits.g1
               , data_cur_bits.g2
               , data_cur_bits.u_reach
               , data_cur_bits.v_reach
               , datas, data_cnt)
      }
      
      when(flago) { 
        datar := true.B
        datau := false.B // Prevent to update to 1.U
        last_reachd := false.B
        first_calc := false.B
        printf(" -- Separator -- \n")
      } .otherwise {
        when(data_cnt === (nfifo-1).U && !last_reachd) // If limit of saving is reached, do not increment anymore
        {
          last_reachd := true.B
        }
      }
      
      
      // Next state stuff
      when(done) {
        state := sIdle
        flag := false.B
        rst_flag := true.B
        datar := true.B
        datau := false.B
        last_reachd := false.B
        printf("Changing state: sIdle\n")
      }
    }
    
  }
}
