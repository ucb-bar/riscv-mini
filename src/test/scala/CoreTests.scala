package mini

import Chisel._
import scala.collection.mutable.HashMap

class CoreSimpleTests(c: Core, log: Option[java.io.PrintStream]) 
    extends LogTester(c, log) with RandInsts {
  val evec = Const.PC_EVEC.litValue()
  def doTest(test: List[UInt]) {
    val mem = HashMap[Int, Byte]() // mock memory
    // Reset
    reset(5)
    for (i <- 0 until c.dpath.regFile.regs.n) {
      if (i == 0)
        pokeAt(c.dpath.regFile.regs, 0, i)
      else
        pokeAt(c.dpath.regFile.regs, int(rnd.nextInt() & 0xffffffff), i)
    }

    do {
      // InstMem
      val iaddr = peek(c.io.icache.req.bits.addr)
      val daddr = peek(c.io.dcache.req.bits.addr)
      val idx = (iaddr - Const.PC_START.litValue()).toInt / 4
      val inst = if (iaddr == evec + (3 << 6)) fin 
            else if (idx < test.size && idx >= 0) test(idx) else nop
      val dout = ((0 until 4) foldLeft BigInt(0)){
        (res, i) => res | int(mem getOrElse(daddr+i, 0.toByte)) << 8*i }
      val dwe = peek(c.io.dcache.req.bits.mask)
      val din = peek(c.io.dcache.req.bits.data)
      val ire = peek(c.io.icache.req.valid)
      val dre = peek(c.io.dcache.req.valid)
      
      step(1)
      if (ire) {
        addEvent(new DumpEvent(s"FEED: ${dasm(inst)}"))
        poke(c.io.icache.resp.bits.data, inst.litValue())
      }
      if (dre) {
        if (dwe) {
          (0 until 4) filter (i => (dwe >> i) & 1) foreach { i => 
            mem(daddr+i) = (din >> 8*i).toByte
            addEvent(new DumpEvent("MEM[%x] <- %x".format(daddr+i, mem(daddr+i))))
          }
        } else {
          addEvent(new DumpEvent("MEM[%x] -> %x".format(daddr, dout)))
          poke(c.io.dcache.resp.bits.data, dout)
        }
      }
    } while (peek(c.io.host.tohost) == 0)

    for ((rd, expected) <- testResults(test)) {
      val result = peekAt(c.dpath.regFile.regs, rd)
      expect(result == expected, "RegFile[%d] = %d == %d".format(rd, result, expected))
    }
  }

  poke(c.io.icache.resp.valid, true)
  poke(c.io.dcache.resp.valid, true)
  doTest(bypassTest)
  doTest(exceptionTest)
}
