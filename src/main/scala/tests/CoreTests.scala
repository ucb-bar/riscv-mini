package mini

import Chisel._
import TestCommon._
import HexCommon._

class CoreTests(c: Core, args: Array[String]) extends Tester(c, false) {
  def runTests(maxcycles: Int, verbose: Boolean) = {
    pokeAt(c.dpath.regFile.regs, 0, 0)
    poke(c.io.stall, 0)
    do {
      val iaddr = peek(c.io.icache.addr)
      val daddr = (peek(c.io.dcache.addr) >> 2) << 2
      val data  = peek(c.io.dcache.din)
      val dwe   = peek(c.io.dcache.we)
      val ire   = peek(c.io.icache.re) == 1
      val dre   = peek(c.io.dcache.re) == 1

      step(1)

      if (dwe > 0) {
        writeMem(daddr, data, dwe)
      } else if (ire) {
        val inst = readMem(iaddr)
        poke(c.io.icache.dout, inst)
      } else if (dre) {
        val data = readMem(daddr)
        poke(c.io.dcache.dout, data)
      }

      if (verbose) {
        val pc     = peek(c.dpath.ew_pc)
        val inst   = UInt(peek(c.dpath.ew_inst), 32)
        val wb_en  = peek(c.ctrl.io.ctrl.wb_en)
        val wb_val = 
          if (wb_en == 1) peek(c.dpath.regWrite) 
          else peekAt(c.dpath.regFile.regs, rd(inst)) 
        println("[%h] %s -> RegFile[%d] = %h".format(
                pc, dasm(inst), rd(inst), wb_val))
      }
    } while (peek(c.io.host.tohost) == 0 && t < maxcycles) 

    val tohost = peek(c.io.host.tohost)
    val reason = if (t < maxcycles) "tohost = " + tohost else "timeout"
    ok &= tohost == 1
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (ok) "PASSED" else "FAILED", reason, t))
  }

  val (filename, maxcycles, verbose) = parseOpts(args)
  loadMem(filename)
  runTests(maxcycles, verbose)
}
