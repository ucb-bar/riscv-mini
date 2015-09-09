package mini

import Chisel._
import TestCommon._

class CoreTests(c: Core, args: Array[String]) extends Tester(c, false) {
  implicit def bigIntToBoolean(b: BigInt) = b != 0
  implicit def bigIntToInt(b: BigInt) = b.toInt

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
        if (verbose) println("MEM[%x] -> %s".format(iaddr, dasm(UInt(inst))))
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
    } while (!tohost && t < maxcycles)

    val reason = if (t < maxcycles) "tohost = " + tohost else "timeout"
    ok &= tohost == 1
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (ok) "PASSED" else "FAILED", reason, t))
  }

  val mem = new MagicMem
  val (dir, tests, maxcycles, verbose) = parseOpts(args)
  tests match {
    case SimpleTests =>
      mem.loadMem(bypassTest)
      runTests(maxcycles, verbose)
      for ((rd, expected) <- testResults(bypassTest)) {
        val result = peekAt(c.dpath.regFile.regs, rd)
        println("[%s] RegFile[%d] = %d == %d".format(
                if (result == expected) "PASS" else "FAIL", rd, result, expected))
      }
      reset(5)
      mem.loadMem(exceptionTest)
      runTests(maxcycles, verbose)
      for ((rd, expected) <- testResults(exceptionTest)) {
        val result = peekAt(c.dpath.regFile.regs, rd)
        println("[%s] RegFile[%d] = %d == %d".format(
                if (result == expected) "PASS" else "FAIL", rd, result, expected))
      }
    case ISATests => for (test <- isaTests) {
      reset(5)
      println("\n***** ISA Test: %s ******".format(test))
      mem.loadMem(dir + "/" + test)
      runTests(maxcycles, verbose)
    }
    case Benchmarks => for (test <- bmarksTest) {
      reset(5)
      println("\n***** Benchmark: %s ******".format(test))
      mem.loadMem(dir + "/" + test)
      runTests(maxcycles, verbose)
    } 
  }
}
