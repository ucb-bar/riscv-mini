package mini

import Chisel._

trait CoreTests extends MemCommon {
  def run(c: Core, maxcycles: Int, verbose: Boolean) = {
    for (i <- 0 until c.dpath.regFile.regs.n) {
      if (i == 0)
        pokeAt(c.dpath.regFile.regs, 0, i)
      else
        pokeAt(c.dpath.regFile.regs, int(rnd.nextInt() & 0xffffffff), i)
    }
    poke(c.io.icache.resp.valid, 1)
    poke(c.io.dcache.resp.valid, 1)
    var tohost = BigInt(0)
    do {
      val iaddr = peek(c.io.icache.req.bits.addr)
      val daddr = peek(c.io.dcache.req.bits.addr)
      val inst  = readMem(iaddr)
      val ire   = peek(c.io.icache.req.valid)
      val dre   = peek(c.io.dcache.req.valid)
      val dwe   = peek(c.io.dcache.req.bits.mask)
      val din   = peek(c.io.dcache.req.bits.data)
      val dout  = readMem(daddr)
      step(1)
      if (ire) {
        if (verbose) println("MEM[%x] -> %s".format(iaddr, dasm(UInt(inst))))
        poke(c.io.icache.resp.bits.data, inst)
      } 
      if (dre) {
        if (verbose) println("MEM[%x] -> %x".format(daddr, dout))
        poke(c.io.dcache.resp.bits.data, dout)
      }
      if (dwe && !peek(c.io.dcache.abort)) {
        if (verbose) println("MEM[%x] <- %x".format(daddr, din))
        writeMem(daddr, din, dwe)
      }
      tohost = peek(c.io.host.tohost)
      val log = testOutputString
      // if (verbose && !log.isEmpty) println(log)
    } while (!tohost && cycles < maxcycles)

    val reason = if (cycles < maxcycles) "tohost = " + tohost else "timeout"
    val ok = tohost == 1
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (ok) "PASSED" else "FAILED", reason, cycles))
    ok
  }
}

class CoreTester(c: Core, args: Array[String]) extends MemTester(c, args) with CoreTests {
  def regFile(x: Int) = peekAt(c.dpath.regFile.regs, x)
  def runTests(maxcycles: Int, verbose: Boolean) = {
    ok &= run(c, maxcycles, verbose)
  }
}
