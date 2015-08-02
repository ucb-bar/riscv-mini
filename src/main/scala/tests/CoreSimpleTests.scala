package mini

import Chisel._
import TestCommon._
import scala.collection.mutable.HashMap

class CoreSimpleTests(c: Core) extends Tester(c) {
  def removeSign(x: Int) = BigInt(x >>> 1) << 1 | x & 1
  def doTest(test: Array[UInt]) {
    val mem = HashMap[BigInt, BigInt]() // mock memory
    do {
      // InstMem
      val iaddr = peek(c.io.icache.addr)
      val daddr = peek(c.io.dcache.addr)
      val idx = (iaddr - Const.PC_START.litValue()).toInt / 4
      val inst = if (idx < test.size) test(idx) else nop
      val dout = mem getOrElse (daddr, BigInt(0))
      val din  = peek(c.io.dcache.din)
      val ire = peek(c.io.icache.re) == 1
      val dre = peek(c.io.dcache.re) == 1
      val dwe = peek(c.io.dcache.we) > 1
      
      step(1)
      if (ire) {
        println("FEED: " + dasm(inst))
        poke(c.io.icache.dout, inst.litValue())
      }
      if (dre) {
        poke(c.io.dcache.dout, dout)
      }
      if (dwe) {
        mem(daddr) = din
      }
    } while (peek(c.io.host.tohost) == 0)

    for ((rd, expected) <- testResults(test)) {
      val result = peekAt(c.dpath.regFile.regs, rd)
      expect(result == expected, "RegFile[%d] = %d == %d".format(rd, result, expected))
    }
  }

  // Reset
  for (i <- 0 until c.dpath.regFile.regs.n) {
    if (i == 0)
      pokeAt(c.dpath.regFile.regs, 0, i)
    else
      pokeAt(c.dpath.regFile.regs, removeSign(rnd.nextInt() & 0xffffffff), i)
  }
  poke(c.io.stall, 0)
  doTest(bypassTest)
}
