package mini

import Chisel._
import TestCommon._
import scala.collection.mutable.HashMap

class CSRTests(c: CSR) extends Tester(c) {
  val csrFile = c.csrFile map { case (k, y) => (k.litValue(), y) }
  val values = HashMap[BigInt, BigInt]()
  val pc = int(rnd.nextInt() & -4)
  def nextIn = int(rnd.nextInt() & 0xffffffff)
  def nextSrc = int(rnd.nextInt() & 0x1f)
  def isRO(csr: BigInt) = ((csr >> 10) & 0x3) == 0x3 || csr == CSR.mtvec.litValue() || csr == CSR.mtdeleg.litValue()
  def pokeCSR(csr: BigInt, value: BigInt) {
    if (csr == CSR.mstatus.litValue()) {
      poke(c.PRV1, (value >> 4) & 0x3)
      poke(c.IE1,  (value >> 3) & 0x1)
      poke(c.PRV0, (value >> 1) & 0x3)
      poke(c.IE0,  (value >> 0) & 0x1)
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
    } else if (!isRO(csr)) {
      poke(csrFile(csr), value)
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
      expect(c.io.out, value & (BigInt(1) << (c.instLen-1) | 0xf))
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
      expect(c.PRV0, (value >> 1) & 0x3)
      expect(c.IE0,  (value >> 0) & 0x1)
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
  def expectException {
    values(CSR.mepc.litValue())    = pc
    values(CSR.mcause.litValue())  = Cause.IllegalInst.litValue()
    values(CSR.mstatus.litValue()) = (values(CSR.mstatus.litValue()) << 3) & 0xffffffff
    expect(c.mepc,   values(CSR.mepc.litValue()))
    expect(c.mcause, values(CSR.mcause.litValue()))
  }
  override def step(n: Int) {
    super.step(n)
    values(CSR.mtime.litValue()) += 1
  }

  poke(c.io.pc, pc)
  poke(c.io.stall, 0)
  csrNames foreach { case (csr, name) => 
    val value = if (!isRO(csr)) nextIn else peek(csrFile(csr))
    pokeCSR(csr, values getOrElseUpdate (csr, value)) 
  }

  poke(c.io.cmd, csr_n)
  csrNames foreach { case (csr, name) => 
    val reg = csrFile(csr)
    println("*** CSR.N: %s ****".format(name))
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
    println("*** CSR.W: %s <- %x (x%d) ****".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr)
    expectOut(csr, values(csr))
    if (isRO(csr) && src != 0) { 
      step(1)
      expectException 
    } else if (!isRO(csr)) {
      val value = if (src != 0) in 
        else if (csr == CSR.mtime.litValue()) values(csr) + 1 
        else values(csr)
      step(1)
      expectCSR(csr, value)
      values(csr) = value
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
    if (isRO(csr) && src != 0) { 
      step(1)
      expectException 
    } else if (!isRO(csr)) {
      val value = if (src != 0) values(csr) | in 
        else if (csr == CSR.mtime.litValue()) values(csr) + 1 
        else values(csr)
      step(1)
      expectCSR(csr, value)
      values(csr) = value
    } 
  }

  poke(c.io.cmd, csr_c)
  csrNames foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    println("*** CSR.C: %s <- %x (x%d) ****".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr)
    expectOut(csr, values(csr))
    if (isRO(csr) && src != 0) { 
      step(1)
      expectException 
    } else if (!isRO(csr)) {
      val value = if (src != 0) values(csr) & int(~in.toInt) 
        else if (csr == CSR.mtime.litValue()) values(csr) + 1 
        else values(csr)
      step(1)
      expectCSR(csr, value) 
      values(csr) = value
    } 
  }
}
