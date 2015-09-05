package mini

import Chisel._
import TestCommon._
import scala.collection.mutable.HashMap

class CSRTests(c: CSR) extends Tester(c) {
  implicit def toBigInt(x: UInt) = x.litValue()
  implicit def toBoolean(x: BigInt) = x != 0
  val csrFile = c.csrFile map { case (k, y) => (k.litValue(), y) }
  val values = HashMap[BigInt, BigInt]()
  def prv = (values(CSR.mstatus) >> 1) & 0x3
  val pc = int(rnd.nextInt())
  def nextIn = int(rnd.nextInt() & 0xffffffff)
  def nextSrc = int(rnd.nextInt() & 0x1f)
  def pokeCSR(csr: BigInt, value: BigInt) {
    if (csr == CSR.mstatus.litValue()) {
      poke(c.PRV, (value >> 1) & 0x3)
      poke(c.IE,  (value >> 0) & 0x1)
    } else if (csr == CSR.mip.litValue()) {
      poke(c.MTIP, (value >> 7) & 0x1)
      poke(c.MSIP, (value >> 3) & 0x1)
    } else if (csr == CSR.mie.litValue()) {
      poke(c.MTIE, (value >> 7) & 0x1)
      poke(c.MSIE, (value >> 3) & 0x1)
    } else if (csr == CSR.mcause.litValue()) {
      poke(c.mcause, value & (BigInt(1) << (c.instLen-1) | 0xf))
    } else if (csr == CSR.mepc.litValue()) {
      poke(c.mepc, value & int(-4))
    } else if (!csrRO(csr)) {
      poke(csrFile(csr), value)
    }
  }
  def expectOut(csr: BigInt, value: BigInt) {
    if (csr == CSR.mstatus.litValue()) {
      expect(c.io.out, value & (0x3 << 1 | 0x1))
    } else if (csr == CSR.mip.litValue()) {
      expect(c.io.out, value & (0x1 << 7 | 0x1 << 3))
    } else if (csr == CSR.mie.litValue()) {
      expect(c.io.out, value & (0x1 << 7 | 0x1 << 3))
    } else if (csr == CSR.mcause.litValue()) {
      expect(c.io.out, value & (BigInt(1) << (c.instLen-1) | 0xf))
    } else if (csr == CSR.mepc.litValue()) {
      expect(c.io.out, value & int(-4))
    } else {
      expect(c.io.out, value)
    }
  }
  def expectCSR(csr: BigInt, value: BigInt) {
    if (csr == CSR.mstatus.litValue()) {
      expect(c.PRV, (value >> 1) & 0x3)
      expect(c.IE,  (value >> 0) & 0x1)
      // return to M mode
      values(CSR.mstatus) = CSR.PRV_M << 1
      poke(c.PRV, CSR.PRV_M)
      poke(c.IE, 0)
    } else if (csr == CSR.mip.litValue()) {
      expect(c.MTIP, (value >> 7) & 0x1)
      expect(c.MSIP, (value >> 3) & 0x1)
    } else if (csr == CSR.mie.litValue()) {
      expect(c.MTIE, (value >> 7) & 0x1)
      expect(c.MSIE, (value >> 3) & 0x1)
    } else if (csr == CSR.mcause.litValue()) {
      expect(c.mcause, value & (BigInt(1) << (c.instLen-1) | 0xf))
    } else if (csr == CSR.mepc.litValue()) {
      expect(c.mepc, value & int(-4))
    } else { 
      expect(csrFile(csr), value)
    }
  }
  def expectException(cause: BigInt) {
    expect(c.io.expt, 1)
    expect(c.io.eret, 0)
    val evec = if (prv) pc_mtvec else pc_utvec
    expect(c.io.evec, evec)
    values(CSR.mepc) = pc & int(-4)
    values(CSR.mcause) = cause 
    values(CSR.mstatus) = CSR.PRV_M << 1
    step(1)
    expect(c.mepc, values(CSR.mepc))
    expect(c.mcause, values(CSR.mcause))
    expect(c.PRV, CSR.PRV_M)
    expect(c.IE, 0)
  }
  def expectEret {
    expect(c.io.expt, 0)
    expect(c.io.eret, 1)
    expect(c.io.evec, values(CSR.mepc))
    values(CSR.mstatus) = (CSR.PRV_U << 1) | BigInt(1)
    step(1)
    expect(c.PRV, CSR.PRV_U)
    expect(c.IE, 1)
  }
  override def step(n: Int) {
    super.step(n)
    values(CSR.mtime) += 1
  }

  poke(c.io.pc, pc)
  poke(c.io.illegal_inst, 0)
  csrNames foreach { case (csr, name) => 
    val value = if (!csrRO(csr)) nextIn else peek(csrFile(csr))
    pokeCSR(csr, values getOrElseUpdate (csr, value)) 
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
    if (!csrPrv(csr, prv) || (csrRO(csr) && src != 0)) { 
      expectException(Cause.IllegalInst)
    } else if (!csrRO(csr)) {
      val value = if (src != 0) in 
        else if (csr == CSR.mtime.litValue()) values(csr) + 1 
        else values(csr)
      step(1)
      values(csr) = value
      expectCSR(csr, value)
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
        else if (csr == CSR.mtime.litValue()) values(csr) + 1 
        else values(csr)
      step(1)
      values(csr) = value
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
        else if (csr == CSR.mtime.litValue()) values(csr) + 1 
        else values(csr)
      step(1)
      values(csr) = value
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
