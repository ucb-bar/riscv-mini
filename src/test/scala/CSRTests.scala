package mini

import Chisel._
import Chisel.iotesters.ClassicTester
import scala.collection.mutable.HashMap

case class CSRIn(cmd: BigInt, value: BigInt, inst: UInt, pc: BigInt, addr: BigInt, 
                 illegal: Boolean, pc_check: Boolean, st_type: BigInt, ld_type: BigInt)
case class CSROut(value: BigInt, epc: BigInt, evec: BigInt, expt: Boolean)

class GoldCSR extends RISCVCommon {
  implicit def uintToBigInt(x: UInt) = x.litValue()
  private val regs = HashMap[BigInt, BigInt](CSR.regs map (reg => reg.litValue() -> 
    (if (reg === CSR.mcpuid) BigInt(1) << ('I' - 'A') | BigInt(1) << ('U' - 'A')
    else if (reg === CSR.mstatus) CSR.PRV_M.litValue() << 4 | CSR.PRV_M.litValue() << 1
    else if (reg === CSR.mtvec) Const.PC_EVEC.litValue()
    else BigInt(0))):_*)
  private def mtvec = regs(CSR.mtvec)
  private def mepc  = regs(CSR.mepc)
  private def mepc_=(data: BigInt) { regs(CSR.mepc) = data }
  private def mcause = regs(CSR.mcause)
  private def mcause_=(data: BigInt) { regs(CSR.mcause) = data }
  private def mbadaddr = regs(CSR.mbadaddr)
  private def mbadaddr_=(data:BigInt) { regs(CSR.mbadaddr) = data }
  private def status(prv: BigInt, ie: BigInt, prv1: BigInt, ie1: BigInt) {
    regs(CSR.mstatus) = (prv1 << 4) | (ie1 << 3) | (prv << 1) | ie
  }
  private def ip(mtip: BigInt, msip: BigInt) {
    regs(CSR.mip) = mtip << 7 | msip << 3
  }
  private def ie(mtie: BigInt, msie: BigInt) {
    regs(CSR.mie) = mtie << 7 | msie << 3
  }
  private def prv1 = (regs(CSR.mstatus) >> 4) & 0x3
  private def ie1  = (regs(CSR.mstatus) >> 3) & 0x1
  private def prv  = (regs(CSR.mstatus) >> 1) & 0x3
  private def ie   = regs(CSR.mstatus) & 0x1
  private def read(addr: BigInt) = regs getOrElse (addr, BigInt(0))

  private def count(instret: Boolean) {
    // TODO: overflow
    regs(CSR.time)   += 1
    regs(CSR.timew)  += 1
    regs(CSR.mtime)  += 1
    regs(CSR.cycle)  += 1
    regs(CSR.cyclew) += 1
    if (instret) {
      regs(CSR.instret)  += 1
      regs(CSR.instretw) += 1
    }
  }

  private def write(addr: BigInt, data: BigInt) {
    if (addr == CSR.mstatus.litValue()) { 
      status((data >> 1) & 0x3, data & 0x1, (data >> 4) & 0x3, (data >> 1) & 0x1)
    } else if (addr == CSR.mip.litValue()) {
      ip((data >> 7) & 0x1, (data >> 3) & 0x1)
    } else if (addr == CSR.mie.litValue()) {
      ie((data >> 7) & 0x1, (data >> 3) & 0x1)
    } else if (addr == CSR.mepc.litValue()) {
      regs(CSR.mepc) = data & -4
    } else if (addr == CSR.mcause.litValue()) {
      regs(CSR.mcause) = data & (BigInt(1) << 31 | 0xf)
    } else if (addr == CSR.timew.litValue() || addr == CSR.mtime.litValue()) {
      regs(CSR.time)  = data
      regs(CSR.timew) = data
      regs(CSR.mtime) = data
    } else if (addr == CSR.timehw.litValue() || addr == CSR.mtimeh.litValue()) {
      regs(CSR.timeh)  = data
      regs(CSR.timehw) = data
      regs(CSR.mtimeh) = data
    } else if (addr == CSR.cyclew.litValue()) {
      regs(CSR.cycle)  = data
      regs(CSR.cyclew) = data
    } else if (addr == CSR.cyclehw.litValue()) {
      regs(CSR.cycleh)  = data
      regs(CSR.cyclehw) = data
    } else if (addr == CSR.instretw.litValue()) {
      regs(CSR.instret)  = data
      regs(CSR.instretw) = data
    } else if (addr == CSR.instrethw.litValue()) {
      regs(CSR.instreth)  = data
      regs(CSR.instrethw) = data
    } else regs(addr) = data
  }

  def apply(in: CSRIn) = {
    val csr_addr = csr(in.inst)
    val rs1_addr = rs1(in.inst)
    val privValid = ((csr_addr >> 8) & 0x3) <= prv
    val privInst  = in.cmd == CSR.P.litValue()
    val isEcall  = privInst && (csr_addr & 0x1) == 0 && ((csr_addr >> 8) & 0x1) == 0
    val isEbreak = privInst && (csr_addr & 0x1) == 1 && ((csr_addr >> 8) & 0x1) == 0
    val isEret   = privInst && (csr_addr & 0x1) == 0 && ((csr_addr >> 8) & 0x1) == 1
    val csrValid = regs contains csr_addr
    val csrRO    = ((csr_addr >> 10) & 0x3) == 0x3 ||
        csr_addr == CSR.mtvec.litValue() || csr_addr == CSR.mtdeleg.litValue()
    val wen = in.cmd == CSR.W.litValue() || ((in.cmd >> 1) & 0x1) == 0x1 && rs1_addr != 0
    val iaddrInvalid = in.pc_check && ((in.addr >> 0x1) & 0x1) > 0
    val laddrInvalid = in.ld_type == Control.LD_LW.litValue() && (in.addr & 0x3) > 0 ||
        (in.ld_type == Control.LD_LH.litValue() || 
         in.ld_type == Control.LD_LHU.litValue()) && (in.addr & 0x1) > 0
    val saddrInvalid = in.st_type == Control.ST_SW.litValue() && (in.addr & 0x3) > 0 ||
         in.st_type == Control.ST_SH.litValue() && (in.addr & 0x1) > 0
    val exception = in.illegal || iaddrInvalid || laddrInvalid || saddrInvalid || 
         (in.cmd & 0x3) > 0 && (!csrValid || !privValid) || wen && csrRO ||
         (privInst && !privValid) || isEcall || isEbreak
    val out = new CSROut(read(csr_addr), mepc, mtvec + (prv << 6), exception)
    val wdata = if (in.cmd == CSR.W.litValue()) in.value
      else if (in.cmd == CSR.S.litValue()) in.value | read(csr_addr)
      else if (in.cmd == CSR.C.litValue()) ~in.value & read(csr_addr)
      else BigInt(0)
    val isInstRet = (in.inst =/= nop).litValue() == 1 && (!exception || isEcall || isEbreak)
    count(isInstRet)
    if (exception) {
      mepc = in.pc & -4
      mcause = if (iaddrInvalid) Cause.InstAddrMisaligned
        else if (laddrInvalid) Cause.LoadAddrMisaligned
        else if (saddrInvalid) Cause.StoreAddrMisaligned
        else if (isEcall) Cause.Ecall + prv
        else if (isEbreak) Cause.Breakpoint 
        else Cause.IllegalInst
      status(CSR.PRV_M, 0, prv, ie)
      if (iaddrInvalid || laddrInvalid || saddrInvalid) mbadaddr = in.addr
    } else if (isEret) {
      status(prv1, ie1, CSR.PRV_U, 1)
    } else if (wen) {
      write(csr_addr, wdata) 
    }
    out
  }
}

class CSRTests(c: CSR) extends ClassicTester(c) with RandInsts {
  type DUT = CSR
  override val insts: List[UInt] = 
    (CSR.regs map (csr => I(rand_fn3, 0, int(rand_rs1), int(csr)))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRW, 0, csr, int(rand_rs1)))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRS, 0, csr, int(rand_rs1)))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRC, 0, csr, int(rand_rs1)))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRWI, 0, csr, int(rand_rs1)))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRSI, 0, csr, int(rand_rs1)))) ++
    (CSR.regs map (csr => SYS(Funct3.CSRRCI, 0, csr, int(rand_rs1)))) ++
    (CSR.regs map (csr => I(rand_fn3, 0, int(rand_rs1), int(csr)))) ++ List[UInt](
    // system insts
    Instructions.ECALL,  SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    Instructions.EBREAK, SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    Instructions.ERET,   SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    // illegal addr
    J(int(rand_rd), rnd.nextInt), SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    JR(int(rand_rd), int(rand_rs1), rnd.nextInt), 
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    L(Funct3.LW, int(rand_rd), int(rand_rs1), int(rand_rs2)),
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    L(Funct3.LH, int(rand_rd), int(rand_rs1), int(rand_rs2)), 
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    L(Funct3.LHU, int(rand_rd), int(rand_rs1), int(rand_rs2)),
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    S(Funct3.SW, int(rand_rd), int(rand_rs1), int(rand_rs2)), 
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    S(Funct3.SH, int(rand_rd), int(rand_rs1), int(rand_rs2)),
    SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    // illegal inst
    rand_inst, SYS(Funct3.CSRRC, 0, CSR.mcause, 0),
    // Check counters
    SYS(Funct3.CSRRC, 0, CSR.time, 0),
    SYS(Funct3.CSRRC, 0, CSR.cycle, 0),
    SYS(Funct3.CSRRC, 0, CSR.instret, 0),
    SYS(Funct3.CSRRC, 0, CSR.mfromhost, 0))

  def poke(in: CSRIn) {
    poke(c.io.cmd,      in.cmd)
    poke(c.io.in,       in.value)
    poke(c.io.inst,     in.inst)
    poke(c.io.pc,       in.pc)
    poke(c.io.addr,     in.addr)
    poke(c.io.illegal,  in.illegal)
    poke(c.io.pc_check, in.pc_check)
    poke(c.io.st_type,  in.st_type)
    poke(c.io.ld_type,  in.ld_type)
  }

  def expect(out: CSROut) {
    expect(c.io.out,  out.value)
    expect(c.io.epc,  out.epc)
    expect(c.io.evec, out.evec)
    expect(c.io.expt, out.expt)
  }

  c.csrFile foreach (x => poke(x._2, 0))
  poke(c.io.stall, 0)
  poke(c.io.host.fromhost.valid, 0)
  val goldCSR = new GoldCSR

  for (inst <- insts) {
    val value = rand_data
    val ctrl = GoldControl(new ControlIn(inst))
    val in   = new CSRIn(ctrl.csr_cmd, value, inst, rand_addr, int(rnd.nextInt|0x3), 
      ctrl.illegal, ctrl.pc_sel == Control.PC_ALU.litValue(), ctrl.st_type, ctrl.ld_type)
    println(s"*** inst: ${dasm(inst)}, csr: ${csrName(csr(inst))}, value: %x ***".format(value))
    poke(in)
    expect(goldCSR(in))
    step(1) // update registers
  } 
}
