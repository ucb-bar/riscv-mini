package mini

import Chisel._
import TestCommon._
import scala.collection.mutable.HashMap

class CSRTests(c: CSR) extends Tester(c) {
  val values = HashMap[UInt, BigInt]()
  val pc = int(rnd.nextInt() & -4)
  def nextIn = int(rnd.nextInt() & 0xffffffff)
  def nextSrc = int(rnd.nextInt() & 0x1f)
  def isRO(csr: UInt) = ((csr.litValue() >> 10) & 0x3) == 0x3 || csr == CSR.mtvec || csr == CSR.mtdeleg
  def pokeCSR(csr: UInt, value: BigInt) {
    if (csr == CSR.mstatus) {
      poke(c.PRV1, (value >> 4) & 0x3)
      poke(c.IE1,  (value >> 3) & 0x1)
      poke(c.PRV0, (value >> 1) & 0x3)
      poke(c.IE0,  (value >> 0) & 0x1)
    } else if (csr == CSR.mip) {
      poke(c.MTIP, (value >> 7) & 0x1)
      poke(c.MSIP, (value >> 3) & 0x1)
    } else if (csr == CSR.mie) {
      poke(c.MTIE, (value >> 7) & 0x1)
      poke(c.MSIE, (value >> 3) & 0x1)
    } else if (csr == CSR.mcause) {
      poke(c.mcause, value & (BigInt(1) << (c.instLen-1) | 0xf))
    } else if (csr == CSR.mepc) {
      poke(c.mepc, value & int(-4))
    } else if (!isRO(csr)) {
      poke(c.csrFile(csr), value)
    }
  }
  def expectOut(csr: UInt, value: BigInt) {
    if (csr == CSR.mstatus) {
      expect(c.io.out, value & (0x3 << 4 | 0x1 << 3 | 0x3 << 1 | 0x1))
    } else if (csr == CSR.mip) {
      expect(c.io.out, value & (0x1 << 7 | 0x1 << 3))
    } else if (csr == CSR.mie) {
      expect(c.io.out, value & (0x1 << 7 | 0x1 << 3))
    } else if (csr == CSR.mcause) {
      expect(c.io.out, value & (BigInt(1) << (c.instLen-1) | 0xf))
    } else if (csr == CSR.mepc) {
      expect(c.io.out, value & int(-4))
    } else {
      expect(c.io.out, value)
    }
  }
  def expectCSR(csr: UInt, value: BigInt) {
    if (csr == CSR.mstatus) {
      expect(c.PRV1, (value >> 4) & 0x3)
      expect(c.IE1,  (value >> 3) & 0x1)
      expect(c.PRV0, (value >> 1) & 0x3)
      expect(c.IE0,  (value >> 0) & 0x1)
    } else if (csr == CSR.mip) {
      expect(c.MTIP, (value >> 7) & 0x1)
      expect(c.MSIP, (value >> 3) & 0x1)
    } else if (csr == CSR.mie) {
      expect(c.MTIE, (value >> 7) & 0x1)
      expect(c.MSIE, (value >> 3) & 0x1)
    } else if (csr == CSR.mcause) {
      expect(c.mcause, value & (BigInt(1) << (c.instLen-1) | 0xf))
    } else if (csr == CSR.mepc) {
      expect(c.mepc, value & int(-4))
    } else { 
      expect(c.csrFile(csr), value)
    }
  }
  def expectException {
    println("hello")
    values(CSR.mepc)    = pc
    values(CSR.mcause)  = Cause.IllegalInst.litValue()
    values(CSR.mstatus) = (values(CSR.mstatus) << 3) & 0xffffffff
    expect(c.mepc,   values(CSR.mepc))
    expect(c.mcause, values(CSR.mcause))
  }
  override def step(n: Int) {
    super.step(n)
    values(CSR.mtime) += 1
  }

  poke(c.io.pc, pc)
  poke(c.io.stall, 0)
  csrMap foreach { case (csr, name) => 
    val value = if (!isRO(csr)) nextIn else peek(c.csrFile(csr))
    pokeCSR(csr, values getOrElseUpdate (csr, value)) 
  }

  poke(c.io.cmd, csr_n)
  csrMap foreach { case (csr, name) => 
    val reg = c.csrFile(csr)
    println("*** CSR.N: %s ****".format(name))
    poke(c.io.in, nextIn)
    poke(c.io.src, nextSrc)
    poke(c.io.csr, csr.litValue())
    expectOut(csr, values(csr))
    step(1)
    expectCSR(csr, values(csr))
  }

  poke(c.io.cmd, csr_w)
  csrMap foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    println("*** CSR.W: %s <- %x (x%d) ****".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr.litValue())
    expectOut(csr, values(csr))
    if (!isRO(csr) && src != 0) {
      val value = in
      step(1)
      expectCSR(csr, value)
      values(csr) = value
    } else {
      step(1)
      expectException
    }
  }

  poke(c.io.cmd, csr_s)
  csrMap foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    println("*** CSR.S: %s <- %x (x%d) ****".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr.litValue())
    expectOut(csr, values(csr))
    if (!isRO(csr) && src != 0) {
      val value = values(csr) | in
      step(1)
      expectCSR(csr, value)
      values(csr) = value
    } else {
      step(1)
      expectException
    }
  }

  poke(c.io.cmd, csr_c)
  csrMap foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    println("*** CSR.C: %s <- %x (x%d) ****".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr.litValue())
    expectOut(csr, values(csr))
    if (!isRO(csr) && src != 0) {
      val value = values(csr) & int(~in.toInt) 
      step(1)
      expectCSR(csr, value) 
      values(csr) = value
    } else {
      step(1)
      expectException
    }
  }
}
