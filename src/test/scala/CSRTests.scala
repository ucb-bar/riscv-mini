// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import chisel3.testers._
import Control._

class CSRTester(c: => CSR, trace: Boolean = false)(implicit p: config.Parameters)
   extends BasicTester with TestUtils {
  val dut = Module(c)
  val ctrl = Module(new Control)
  val xlen = p(XLEN)

  override val insts = 
    (CSR.regs map (csr => I(rand_fn3, 0, rand_rs1.litValue(), csr.litValue()))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRW, 0, csr, rand_rs1.litValue()))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRS, 0, csr, rand_rs1.litValue()))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRC, 0, csr, rand_rs1.litValue()))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRWI, 0, csr, rand_rs1.litValue()))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRSI, 0, csr, rand_rs1.litValue()))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRCI, 0, csr, rand_rs1.litValue()))) ++
    (CSR.regs map (csr => I(rand_fn3, 0, rand_rs1.litValue(), csr.litValue()))) ++ List[UInt](
    // system insts
    Instructions.ECALL,  SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    Instructions.EBREAK, SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    Instructions.ERET,   SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    // illegal addr
    J(rand_rd.litValue(), rnd.nextInt), SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    JR(rand_rd.litValue(), rand_rs1.litValue(), rnd.nextInt), 
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    L(Funct3.LW, rand_rd.litValue(), rand_rs1.litValue(), rand_rs2.litValue()),
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    L(Funct3.LH, rand_rd.litValue(), rand_rs1.litValue(), rand_rs2.litValue()), 
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    L(Funct3.LHU, rand_rd.litValue(), rand_rs1.litValue(), rand_rs2.litValue()),
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    S(Funct3.SW, rand_rd.litValue(), rand_rs1.litValue(), rand_rs2.litValue()), 
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    S(Funct3.SH, rand_rd.litValue(), rand_rs1.litValue(), rand_rs2.litValue()),
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    // illegal inst
    rand_inst, SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    // Check counters
    SYS(Funct3.CSRRC, 0, CSR.time, 0),
    SYS(Funct3.CSRRC, 0, CSR.cycle, 0),
    SYS(Funct3.CSRRC, 0, CSR.instret, 0),
    SYS(Funct3.CSRRC, 0, CSR.mfromhost, 0))

  val (cntr, done) = Counter(true.B, insts.size)
  val pc   = Seq.fill(insts.size)(rnd.nextInt()) map toBigInt
  val addr = Seq.fill(insts.size)(rnd.nextInt()) map toBigInt
  val data = Seq.fill(insts.size)(rnd.nextInt()) map toBigInt
  val regs = (CSR.regs map (_.litValue()) map { addr => addr -> RegInit(
    if (addr == CSR.mcpuid.litValue()) (BigInt(1) << ('I' - 'A') | BigInt(1) << ('U' - 'A')).U(xlen.W)
    else if (addr == CSR.mstatus.litValue()) (CSR.PRV_M.litValue() << 4 | CSR.PRV_M.litValue() << 1).U(xlen.W)
    else if (addr == CSR.mtvec.litValue()) Const.PC_EVEC.U(xlen.W) else 0.U(xlen.W)
  )}).toMap

  ctrl.io.inst    := Vec(insts)(cntr)
  dut.io.inst     := ctrl.io.inst
  dut.io.cmd      := ctrl.io.csr_cmd
  dut.io.illegal  := ctrl.io.illegal
  dut.io.st_type  := ctrl.io.st_type
  dut.io.ld_type  := ctrl.io.ld_type
  dut.io.pc_check := ctrl.io.pc_sel === PC_ALU
  dut.io.pc       := Vec(pc map (_.U))(cntr)
  dut.io.addr     := Vec(addr map (_.U))(cntr)
  dut.io.in       := Vec(data map (_.U))(cntr)

  dut.io.stall := false.B
  dut.io.host.fromhost.valid := false.B

  // values known statically
  val _csr_addr  = insts map csr
  val _rs1_addr  = insts map rs1
  val _csr_ro    = _csr_addr map (x => ((((x >> 11) & 0x1) > 0x0) && (((x >> 10) & 0x1) > 0x0)) || 
    x == CSR.mtvec.litValue() || x == CSR.mtdeleg.litValue())
  val _csr_valid = _csr_addr map (x => CSR.regs exists (_.litValue() == x))
  // should be <= prv in runtime
  val _prv_level = _csr_addr map (x => (x >> 8) & 0x3)
  // should consider prv in runtime
  val _is_ecall  = _csr_addr map (x => ((x & 0x1) == 0x0) && (((x >> 8) & 0x1) == 0x0))
  val _is_ebreak = _csr_addr map (x => ((x & 0x1)  > 0x0) && (((x >> 8) & 0x1) == 0x0))
  val _is_eret   = _csr_addr map (x => ((x & 0x1) == 0x0) && (((x >> 8) & 0x1)  > 0x0))
  // should consider pc_check in runtime
  val _iaddr_invalid = addr map (x => ((x >> 1) & 0x1) > 0)
  // should consider ld_type & st_type
  val _waddr_invalid = addr map (x => ((x >> 1) & 0x1) > 0 || (x & 0x1) > 0)
  val _haddr_invalid = addr map (x => (x & 0x1) > 0)
  
  // values known in runtime
  val csr_addr  = Wire(UInt())
  val rs1_addr  = Wire(UInt())
  val csr_ro    = Wire(Bool())
  val csr_valid = Wire(Bool())
  csr_addr  := Vec(_csr_addr map (_.U))(cntr)
  rs1_addr  := Vec(_rs1_addr map (_.U))(cntr)
  csr_ro    := Vec(_csr_ro map (_.B))(cntr)
  csr_valid := Vec(_csr_valid map (_.B))(cntr)
  val wen  = dut.io.cmd === CSR.W || dut.io.cmd(1) && rs1_addr != 0.U
  val prv1 = (regs(CSR.mstatus.litValue()) >> 4.U) & 0x3.U
  val ie1  = (regs(CSR.mstatus.litValue()) >> 3.U) & 0x1.U
  val prv  = (regs(CSR.mstatus.litValue()) >> 1.U) & 0x3.U 
  val ie   =  regs(CSR.mstatus.litValue()) & 0x1.U
  val prv_inst  = dut.io.cmd === CSR.P
  val prv_valid = Vec(_prv_level map (_.U))(cntr) <= prv
  val iaddr_invalid = Vec(_iaddr_invalid map (_.B))(cntr) && dut.io.pc_check
  val laddr_invalid =
    Vec(_haddr_invalid map (_.B))(cntr) && (dut.io.ld_type === LD_LH || dut.io.ld_type === LD_LHU) ||
    Vec(_waddr_invalid map (_.B))(cntr) && (dut.io.ld_type === LD_LW)
  val saddr_invalid =
    Vec(_haddr_invalid map (_.B))(cntr) && dut.io.st_type === ST_SH ||
    Vec(_waddr_invalid map (_.B))(cntr) && dut.io.st_type === ST_SW
  val is_ecall = prv_inst && Vec(_is_ecall map (_.B))(cntr)
  val is_ebreak = prv_inst && Vec(_is_ebreak map (_.B))(cntr)
  val is_eret = prv_inst && Vec(_is_eret map (_.B))(cntr)
  val exception = dut.io.illegal || iaddr_invalid || laddr_invalid || saddr_invalid ||
    (((dut.io.cmd & 0x3.U) > 0.U) && (!csr_valid || !prv_valid)) ||
    (csr_ro && wen) || (prv_inst && !prv_valid) || is_ecall || is_ebreak
  val instret = dut.io.inst =/= nop && (!exception || is_ecall || is_ebreak)

  val rdata = Lookup(csr_addr, UInt(0), regs.toSeq map {
    case (addr, reg) => BitPat(addr.U(12.W)) -> reg }) 
  val wdata = Lookup(dut.io.cmd, UInt(0), Seq(
    BitPat(CSR.W) -> dut.io.in,
    BitPat(CSR.S) -> (dut.io.in | rdata),
    BitPat(CSR.C) -> (~dut.io.in & rdata)
  ))

  // compute state
  regs(CSR.time.litValue())   := regs(CSR.time.litValue())   + 1.U
  regs(CSR.timew.litValue())  := regs(CSR.timew.litValue())  + 1.U
  regs(CSR.mtime.litValue())  := regs(CSR.mtime.litValue())  + 1.U
  regs(CSR.cycle.litValue())  := regs(CSR.cycle.litValue())  + 1.U
  regs(CSR.cyclew.litValue()) := regs(CSR.cyclew.litValue()) + 1.U
  when(regs(CSR.time.litValue()).andR) {
    regs(CSR.mtime.litValue()) := regs(CSR.mtime.litValue()) + 1.U
    regs(CSR.timeh.litValue()) := regs(CSR.timeh.litValue()) + 1.U
    regs(CSR.timehw.litValue()) := regs(CSR.timehw.litValue()) + 1.U
  }
  when(regs(CSR.cycle.litValue()).andR) {
    regs(CSR.cycleh.litValue()) := regs(CSR.cycleh.litValue()) + 1.U
    regs(CSR.cyclehw.litValue()) := regs(CSR.cyclehw.litValue()) + 1.U
  }
  when(instret) {
    regs(CSR.instret.litValue()) := regs(CSR.instret.litValue()) + 1.U
    regs(CSR.instretw.litValue()) := regs(CSR.instret.litValue()) + 1.U
    when(regs(CSR.instret.litValue()).andR) {
      regs(CSR.instreth.litValue()) := regs(CSR.instreth.litValue()) + 1.U
      regs(CSR.instrethw.litValue()) := regs(CSR.instrethw.litValue()) + 1.U
    }
  }

  when(exception) {
    regs(CSR.mepc.litValue()) := (dut.io.pc >> 2.U) << 2.U
    regs(CSR.mcause.litValue()) :=
      Mux(iaddr_invalid, Cause.InstAddrMisaligned,
      Mux(laddr_invalid, Cause.LoadAddrMisaligned,
      Mux(saddr_invalid, Cause.StoreAddrMisaligned,
      Mux(prv_inst && Vec(is_ecall)(cntr), Cause.Ecall + prv,
      Mux(prv_inst && Vec(is_ebreak)(cntr), Cause.Breakpoint, Cause.IllegalInst)))))
    regs(CSR.mstatus.litValue()) := (prv << 4.U) | (ie << 3.U) | (CSR.PRV_M << 1.U) | 0.U
    when(iaddr_invalid || laddr_invalid || saddr_invalid) {
      regs(CSR.mbadaddr.litValue()) := dut.io.addr
    }
  }.elsewhen(is_eret) {
    regs(CSR.mstatus.litValue()) := (CSR.PRV_U << 4.U) | (1.U << 3.U) | (prv1 << 1.U) | ie1
  }.elsewhen(wen) {
    when(csr_addr === CSR.mstatus) {
      regs(CSR.mstatus.litValue()) := wdata(5, 0)
    }.elsewhen(csr_addr === CSR.mip) {
      regs(CSR.mip.litValue()) := (wdata(7) << 7.U) | (wdata(3) << 3.U)
    }.elsewhen(csr_addr === CSR.mie) {
      regs(CSR.mie.litValue()) := (wdata(7) << 7.U) | (wdata(3) << 3.U)
    }.elsewhen(csr_addr === CSR.mepc) {
      regs(CSR.mepc.litValue()) := (wdata >> 2.U) << 2.U
    }.elsewhen(csr_addr === CSR.mcause) {
      regs(CSR.mcause.litValue()) := wdata & ((BigInt(1) << 31) | 0xf).U
    }.elsewhen(csr_addr === CSR.timew || csr_addr === CSR.mtime) {
      regs(CSR.time.litValue()) := wdata
      regs(CSR.timew.litValue()) := wdata
      regs(CSR.mtime.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.timehw || csr_addr === CSR.mtimeh) {
      regs(CSR.timeh.litValue()) := wdata
      regs(CSR.timehw.litValue()) := wdata
      regs(CSR.mtimeh.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.cyclew) {
      regs(CSR.cycle.litValue()) := wdata
      regs(CSR.cyclew.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.cyclehw) {
      regs(CSR.cycleh.litValue()) := wdata
      regs(CSR.cyclehw.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.instretw) {
      regs(CSR.instret.litValue()) := wdata
      regs(CSR.instretw.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.instrethw) {
      regs(CSR.instreth.litValue()) := wdata
      regs(CSR.instrethw.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.mtimecmp) {
      regs(CSR.mtimecmp.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.mscratch) {
      regs(CSR.mscratch.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.mbadaddr) {
      regs(CSR.mbadaddr.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.mtohost) {
      regs(CSR.mtohost.litValue()) := wdata
    }.elsewhen(csr_addr === CSR.mfromhost) {
      regs(CSR.mfromhost.litValue()) := wdata
    }
  }

  val epc = Wire(UInt())
  val evec = Wire(UInt())
  epc := regs(CSR.mepc.litValue())
  evec := regs(CSR.mtvec.litValue()) + (prv << 6.U)

  when(done) { stop(); stop() } // from VendingMachine example...
  when(cntr.orR) {
    assert(dut.io.out  === rdata)
    assert(dut.io.epc  === epc)
    assert(dut.io.evec === evec)
    assert(dut.io.expt === exception)
  }

  if (trace) {
    printf("*** Counter: %d ***\n", cntr)
    printf("[in] inst: 0x%x, pc: 0x%x, daddr: 0x%x, in: 0x%x\n",
           dut.io.inst, dut.io.pc, dut.io.addr, dut.io.in)
    printf("     cmd: 0x%x, st_type: 0x%x, ld_type: 0x%x, illegal: %d, pc_check: %d\n",
           dut.io.cmd, dut.io.st_type, dut.io.ld_type, dut.io.illegal, dut.io.pc_check)
    printf("[state] csr addr: %x\n", csr_addr)
    regs.toSeq sortWith (_._1 < _._1) foreach {
      case (addr, reg) => printf(s" ${addr.toString(16)} -> 0x%x\n", reg) }
    printf("[out] read: 0x%x =? 0x%x, epc: 0x%x =? 0x%x, evec: 0x%x ?= 0x%x, expt: %d ?= %d\n",
           dut.io.out, rdata, dut.io.epc, epc, dut.io.evec, evec, dut.io.expt, exception)
  }
}

class CSRTests extends org.scalatest.FlatSpec {
  implicit val p = config.Parameters.root((new MiniConfig).toInstance)
  "CSR" should "pass" in {
    assert(TesterDriver execute (() => new CSRTester(new CSR)))
  }
}
