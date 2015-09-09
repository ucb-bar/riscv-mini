package mini

import Chisel._
import TestCommon._
import scala.collection.mutable.HashMap

class CSRTests(c: CSR) extends Tester(c) {
  implicit def toBigInt(x: UInt) = x.litValue()
  implicit def toBoolean(x: BigInt) = x != 0
  val csrFile = c.csrFile map { case (k, y) => (k.litValue(), y) }
  val values = HashMap[BigInt, BigInt]()
  val pc = int(rnd.nextInt())
  var instret = BigInt(0)
  def nextInstret = {
    instret = rnd.nextInt() & 0x1
    instret
  }
  def prv = (values(CSR.mstatus) >> 1) & 0x3
  def nextIn = int(rnd.nextInt() & 0xffffffff)
  def nextSrc = int(rnd.nextInt() & 0x1f)
  def updateTime(value: BigInt) {
    values(CSR.time) = value
    values(CSR.timew) = value
    values(CSR.mtime) = value
  }
  def updateCycle(value: BigInt) {
    values(CSR.cycle) = value
    values(CSR.cyclew) = value
  }
  def updateInstret(value: BigInt) {
    values(CSR.instret) = value
    values(CSR.instretw) = value
  }
  def updateTimeh(value: BigInt) {
    values(CSR.timeh) = value
    values(CSR.timehw) = value
    values(CSR.mtimeh) = value
  }
  def updateCycleh(value: BigInt) {
    values(CSR.cycleh) = value
    values(CSR.cyclehw) = value
  }
  def updateInstreth(value: BigInt) {
    values(CSR.instreth) = value
    values(CSR.instrethw) = value
  }
  def pokeCSR(csr: BigInt, value: BigInt) {
    if (csr == CSR.mstatus.litValue()) {
      poke(c.PRV1, (value >> 4) & 0x3)
      poke(c.IE1,  (value >> 3) & 0x1)
      poke(c.PRV,  (value >> 1) & 0x3)
      poke(c.IE,   (value >> 0) & 0x1)
      values(csr) = value & (0x3 << 4 | 0x1 << 3 | 0x3 << 1 | 0x1)
    } else if (csr == CSR.mip.litValue()) {
      poke(c.MTIP, (value >> 7) & 0x1)
      poke(c.MSIP, (value >> 3) & 0x1)
      values(csr) = value & (0x1 << 7 | 0x1 << 3)
    } else if (csr == CSR.mie.litValue()) {
      poke(c.MTIE, (value >> 7) & 0x1)
      poke(c.MSIE, (value >> 3) & 0x1)
      values(csr) = value & (0x1 << 7 | 0x1 << 3)
    } else if (csr == CSR.mcause.litValue()) {
      val mcause = value & (BigInt(1) << (c.xlen-1) | 0xf)
      poke(c.mcause, mcause)
      values(csr) = mcause
    } else if (csr == CSR.mepc.litValue()) {
      val mepc = value & int(-4)
      poke(c.mepc, mepc)
      values(csr) = mepc
    } else if (csrCycle(csr)) {
      poke(c.cycle, value) ; updateCycle(value)
    } else if (csrTime(csr)) {
      poke(c.time, value) ; updateTime(value)  
    } else if (csrInstret(csr)) {
      poke(c.instret, value) ; updateInstret(value)
    } else if (csrCycleh(csr)) {
      poke(c.cycleh, value) ; updateCycleh(value)
    } else if (csrTimeh(csr)) {
      poke(c.timeh, value) ; updateTimeh(value)
    } else if (csrInstreth(csr)) {
      poke(c.instreth, value) ; updateInstreth(value)
    } else if (!csrRO(csr)) {
      poke(csrFile(csr), value) ; values(csr) = value
    } else {
      values(csr) = value
    }
  }
  def expectOut(csr: BigInt, value: BigInt) {
    if (csr == CSR.mstatus.litValue()) {
      expect(c.io.out, value & (0x3 << 4 | 0x1 << 3 | 0x3 << 1 | 0x1))
    } else if (csr == CSR.mip.litValue()) {
      expect(c.io.out, value & (0x1 << 7 | 0x1 << 3))
    } else if (csr == CSR.mie.litValue()) {
      expect(c.io.out, value & (0x1 << 7 | 0x1 << 3))
    } else if (csr == CSR.mcause.litValue()) {
      expect(c.io.out, value & (BigInt(1) << (c.xlen-1) | 0xf))
    } else if (csr == CSR.mepc.litValue()) {
      expect(c.io.out, value & int(-4))
    } else {
      expect(c.io.out, value)
    }
  }
  def expectCSR(csr: BigInt, value: BigInt) {
    if (csr == CSR.mstatus.litValue()) {
      expect(c.PRV1, (value >> 4) & 0x3)
      expect(c.IE1,  (value >> 3) & 0x1)
      expect(c.PRV,  (value >> 1) & 0x3)
      expect(c.IE,   (value >> 0) & 0x1)
      // return to M mode
      values(CSR.mstatus) = CSR.PRV_M << 4 | CSR.PRV_M << 1
      poke(c.PRV1, CSR.PRV_M)
      poke(c.IE1, 0)
      poke(c.PRV, CSR.PRV_M)
      poke(c.IE, 0)
    } else if (csr == CSR.mip.litValue()) {
      expect(c.MTIP, (value >> 7) & 0x1)
      expect(c.MSIP, (value >> 3) & 0x1)
      values(csr) = value & (0x1 << 7 | 0x1 << 3)
    } else if (csr == CSR.mie.litValue()) {
      expect(c.MTIE, (value >> 7) & 0x1)
      expect(c.MSIE, (value >> 3) & 0x1)
      values(csr) = value & (0x1 << 7 | 0x1 << 3)
    } else if (csr == CSR.mcause.litValue()) {
      val mcause = value & (BigInt(1) << (c.xlen-1) | 0xf)
      expect(c.mcause, mcause)
      values(csr) = mcause
    } else if (csr == CSR.mepc.litValue()) {
      val mepc = value & int(-4)
      expect(c.mepc, mepc)
      values(csr) = mepc
    } else if (csrCycle(csr)) {
      expect(c.cycle, value) ; updateCycle(value)
    } else if (csrTime(csr)) {
      expect(c.time, value) ; updateTime(value)
    } else if (csrInstret(csr)) {
      expect(c.instret, value) ; updateInstret(value)
    } else if (csrCycleh(csr)) {
      expect(c.cycleh, value) ; updateCycleh(value)
    } else if (csrTimeh(csr)) {
      expect(c.timeh, value) ; updateTimeh(value)
    } else if (csrInstreth(csr)) {
      expect(c.instreth, value) ; updateInstreth(value)
    } else { 
      expect(csrFile(csr), value) ; values(csr) = value
    }
  }
  def expectException(cause: BigInt) {
    expect(c.io.expt, 1)
    expect(c.io.eret, 0)
    val evec = if (prv) pc_mtvec else pc_utvec
    expect(c.io.evec, evec)
    values(CSR.mepc) = pc & int(-4)
    values(CSR.mcause) = cause 
    step(1)
    expect(c.mepc,   values(CSR.mepc))
    expect(c.mcause, values(CSR.mcause))
    expect(c.PRV1,  (values(CSR.mstatus) >> 1) & 0x3)
    expect(c.IE1,    values(CSR.mstatus) & 0x1)
    expect(c.PRV,    CSR.PRV_M)
    expect(c.IE,     0)
    values(CSR.mstatus) = (values(CSR.mstatus) & 0x7) << 3 | CSR.PRV_M << 1
  }
  def expectEret {
    expect(c.io.expt, 0)
    expect(c.io.eret, 1)
    expect(c.io.evec, values(CSR.mepc))
    step(1)
    expect(c.PRV, (values(CSR.mstatus) >> 4) & 0x3)
    expect(c.IE,  (values(CSR.mstatus) >> 3) & 0x1)
    expect(c.PRV1, CSR.PRV_U)
    expect(c.IE1,  1)
    values(CSR.mstatus) = (values(CSR.mstatus) >> 3) & 0x7 | CSR.PRV_U << 4 | 1 << 3
  }
  override def step(n: Int) {
    values(CSR.time)   += 1
    values(CSR.timew)  += 1
    values(CSR.mtime)  += 1
    values(CSR.cycle)  += 1
    values(CSR.cyclew) += 1
    if (instret) {
      values(CSR.instret)  += 1
      values(CSR.instretw) += 1
    }
    poke(c.io.instret, instret)
    super.step(n)
  }

  poke(c.io.pc, pc)
  poke(c.io.illegal_inst, 0)
  poke(c.io.iaddr_invalid, 0)
  poke(c.io.daddr_invalid, 0)
  poke(c.io.addr, 0)
  poke(c.io.instret, 0)
  csrNames foreach { case (csr, name) => 
    val value = if (!csrRO(csr)) nextIn else peek(csrFile(csr))
    pokeCSR(csr, value)
  }

  poke(c.io.cmd, csr_n)
  csrNames foreach { case (csr, name) => 
    val reg = csrFile(csr)
    println("*** CSR.N: %s ***".format(name))
    poke(c.io.in, nextIn)
    poke(c.io.src, nextSrc)
    poke(c.io.csr, csr)
    expectOut(csr, values(csr))
    step(1)
    expectCSR(csr, values(csr))
  }

  poke(c.io.cmd, csr_w)
  csrNames foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    println("*** CSR.W: %s <- %x (x%d) ***".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr)
    expectOut(csr, values(csr))
    if (!csrPrv(csr, prv) || csrRO(csr)) { 
      expectException(Cause.IllegalInst)
    } else if (!csrRO(csr)) {
      step(1)
      expectCSR(csr, in)
    }
  }

  poke(c.io.cmd, csr_s)
  csrNames foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    println("*** CSR.S: %s <- %x (x%d) ****".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr)
    expectOut(csr, values(csr))
    if (!csrPrv(csr, prv) || (csrRO(csr) && src != 0)) { 
      expectException(Cause.IllegalInst)
    } else if (!csrRO(csr)) {
      val value = if (src != 0) values(csr) | in 
        else if (csrTime(csr) || csrCycle(csr) || csrInstret(csr) && nextInstret) values(csr) + 1 
        else values(csr)
      step(1)
      expectCSR(csr, value)
    } 
  }

  poke(c.io.cmd, csr_c)
  csrNames foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    println("*** CSR.C: %s <- %x (x%d) ***".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr)
    expectOut(csr, values(csr))
    if (!csrPrv(csr, prv) && (csrRO(csr) && src != 0)) { 
      expectException(Cause.IllegalInst)
    } else if (!csrRO(csr)) {
      val value = if (src != 0) values(csr) & int(~in.toInt) 
        else if (csrTime(csr) || csrCycle(csr) || csrInstret(csr) && nextInstret) values(csr) + 1 
        else values(csr)
      step(1)
      expectCSR(csr, value) 
    } 
  }

  poke(c.io.cmd, csr_p)
  println("*** CSR.P: ECALL ***")
  poke(c.io.in, nextIn)
  poke(c.io.csr, Funct12.ECALL)
  expectException(Cause.Ecall + ((values(CSR.mstatus) >> 1) & 0x3))

  println("*** CSR.P: ERET ***")
  poke(c.io.in, nextIn)
  poke(c.io.csr, Funct12.ERET)
  expectEret

  println("*** CSR.P: EBREAK ***")
  poke(c.io.in, nextIn)
  poke(c.io.csr, Funct12.EBREAK)
  expectException(Cause.Breakpoint)
}
