package mini

import Chisel._
import TestCommon._
import scala.collection.mutable.HashMap

class CSRTests(c: CSR) extends Tester(c) {
  val values = HashMap[UInt, BigInt]()
  def nextIn = int(rnd.nextInt() & 0xffffffff)
  def nextSrc = int(rnd.nextInt() & 0x1f)
  def isRO(csr: UInt) = ((csr.litValue() >> 10) & 0x3) == 0x3 || csr == CSR.mtvec || csr == CSR.mtdeleg
  def pokeCSR(csr: UInt, value: BigInt) {
    if (csr == CSR.mstatus) {
      poke(c.VM,   (value >> 17) & 0x1f)
      poke(c.MPRV, (value >> 16) & 0x1)
      poke(c.PRV1, (value >> 4)  & 0x3)
      poke(c.IE1,  (value >> 3)  & 0x1)
      poke(c.PRV0, (value >> 1)  & 0x3)
      poke(c.IE0,  (value >> 0)  & 0x1)
    } else if (csr == CSR.mip) {
      poke(c.MTIP, (value >> 7) & 0x1)
      poke(c.MSIP, (value >> 3) & 0x1)
    } else if (csr == CSR.mie) {
      poke(c.MTIE, (value >> 7) & 0x1)
      poke(c.MSIE, (value >> 3) & 0x1)
    } else if (csr == CSR.mcause) {
      poke(c.Interrupt, (value >> (c.instLen-1)) & 0x1)
      poke(c.Exception, value & 0xf)
    } else if (!isRO(csr)) {
      poke(c.csrFile(csr), value)
    }
  }
  def expectOut(csr: UInt, value: BigInt) {
    if (csr == CSR.mstatus) {
      expect(c.io.out, value & (0x1f << 17 | 0x1 << 16 | 0x3 << 4 | 0x1 << 3 | 0x3 << 1 | 0x1))
    } else if (csr == CSR.mip) {
      expect(c.io.out, value & (0x1 << 7 | 0x1 << 3))
    } else if (csr == CSR.mie) {
      expect(c.io.out, value & (0x1 << 7 | 0x1 << 3))
    } else if (csr == CSR.mcause) {
      expect(c.io.out, value & (0x1 << (c.instLen-1) | 0xf))
    } else {
      expect(c.io.out, value)
    }
  }
  def expectCSR(csr: UInt, value: BigInt) {
    if (csr == CSR.mstatus) {
      expect(c.VM,   (value >> 17) & 0x1f)
      expect(c.MPRV, (value >> 16) & 0x1)
      expect(c.PRV1, (value >> 4)  & 0x3)
      expect(c.IE1,  (value >> 3)  & 0x1)
      expect(c.PRV0, (value >> 1)  & 0x3)
      expect(c.IE0,  (value >> 0)  & 0x1)
    } else if (csr == CSR.mip) {
      expect(c.MTIP, (value >> 7) & 0x1)
      expect(c.MSIP, (value >> 3) & 0x1)
    } else if (csr == CSR.mie) {
      expect(c.MTIE, (value >> 7) & 0x1)
      expect(c.MSIE, (value >> 3) & 0x1)
    } else if (csr == CSR.mcause) {
      expect(c.Interrupt, (value >> (c.instLen-1)) & 0x1)
      expect(c.Exception, value & 0xf)
    } else if (isRO(csr) && values(csr) != value) {
      expect(c.Interrupt, 0x1)
      expect(c.Exception, 0x2)
      values(CSR.mcause) = (0x1 << (c.instLen-1)) | 0x2
    } else { 
      expect(c.csrFile(csr), value)
    }
  }

  csrNames foreach { case (csr, name) => 
    val value = if (!isRO(csr)) nextIn else peek(c.csrFile(csr))
    pokeCSR(csr, values getOrElseUpdate (csr, value)) 
  }

  poke(c.io.cmd, csr_n)
  csrNames foreach { case (csr, name) => 
    val value = values(csr)
    val reg = c.csrFile(csr)
    println("*** CSR.N: %s ****".format(name))
    poke(c.io.in, nextIn)
    poke(c.io.src, nextSrc)
    poke(c.io.csr, csr.litValue())
    expectOut(csr, value)
    step(1)
    expectCSR(csr, value)
  }

  poke(c.io.cmd, csr_w)
  csrNames foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    val value = if (src != 0) in else values(csr)
    println("*** CSR.W: %s <- %x (x%d) ****".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr.litValue())
    expectOut(csr, values(csr))
    step(1)
    expectCSR(csr, value)
    if (!isRO(csr)) values(csr) = value
  }

  poke(c.io.cmd, csr_c)
  csrNames foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    val value = if (src != 0) values(csr) & in else values(csr)
    println("*** CSR.C: %s <- %x (x%d) ****".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr.litValue())
    expectOut(csr, values(csr))
    step(1)
    expectCSR(csr, value)
    if (!isRO(csr)) values(csr) = value
  }

  poke(c.io.cmd, csr_s)
  csrNames foreach { case (csr, name) => 
    val in = nextIn
    val src = nextSrc
    val value = if (src != 0) values(csr) & int(~in.toInt) else values(csr)
    println("*** CSR.S: %s <- %x (x%d) ****".format(name, in, src))
    poke(c.io.in, in)
    poke(c.io.src, src)
    poke(c.io.csr, csr.litValue())
    expectOut(csr, values(csr))
    step(1)
    expectCSR(csr, value)
    if (!isRO(csr)) values(csr) = value
  }
}
