package mini

import Chisel._

class CoreTests(c: Core, args: Array[String]) extends MemTester(c, args) {
  def regFile(x: Int) = peekAt(c.dpath.regFile.regs, x)
  def runTests(maxcycles: Int, verbose: Boolean) = {
    t = 0
    ok = true
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
      val inst  = mem.read(iaddr)
      val ire   = peek(c.io.icache.req.valid)
      val dre   = peek(c.io.dcache.req.valid)
      val dwe   = peek(c.io.dcache.req.bits.mask)
      val din   = peek(c.io.dcache.req.bits.data)
      val dout  = mem.read(daddr)
      step(1)
      if (ire) {
        if (verbose) println("MEM[%x] -> %s".format(iaddr, TestCommon.dasm(UInt(inst))))
        poke(c.io.icache.resp.bits.data, inst)
      } 
      if (dre) {
        if (verbose) println("MEM[%x] -> %x".format(daddr, dout))
        poke(c.io.dcache.resp.bits.data, dout)
      }
      if (dwe) {
        if (verbose) println("MEM[%x] <- %x".format(daddr, din))
        mem.write(daddr, din, dwe)
      }
      tohost = peek(c.io.host.tohost)
      val log = testOutputString
      // if (verbose && !log.isEmpty) println(log)
    } while (!tohost && cycles < maxcycles)

    val reason = if (cycles < maxcycles) "tohost = " + tohost else "timeout"
    ok &= tohost == 1
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (tohost == 1) "PASSED" else "FAILED", reason, cycles))
  }
}
