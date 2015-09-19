package mini

import Chisel._
import junctions.MemIO

trait TileTests extends MemCommon {
  private var memrw     = false
  private var memtag    = 0 
  private var memaddr   = 0 
  private var memcycles = -1

  def tick(c: Tile, verbose: Boolean) {
    if (memcycles > 0) {
      poke(c.io.mem.req_cmd.ready, 0)
      poke(c.io.mem.req_data.ready, 0)
      poke(c.io.mem.resp.valid, 0)  
      memcycles -= 1
    } else if (memcycles < 0) {
      if (peek(c.io.mem.req_cmd.valid)) {
        memrw   = peek(c.io.mem.req_cmd.bits.rw) 
        memtag  = peek(c.io.mem.req_cmd.bits.tag)
        memaddr = peek(c.io.mem.req_cmd.bits.addr) << c.blen
        // Memread
        if (!memrw) {
          memcycles = 0
          poke(c.io.mem.req_cmd.ready, 1)
        }
      }
      if (peek(c.io.mem.req_data.valid) == 1) {
        val data = peek(c.io.mem.req_data.bits.data)
        if (verbose) println("MEM[%x] <- %s".format(memaddr, data.toString(16)))
        poke(c.io.mem.req_cmd.ready, 1)
        poke(c.io.mem.req_data.ready, 1)
        writeMem(memaddr, data)
        memcycles = -1
      }
    } else {
      if (!memrw) {
        val read = readMem(memaddr)
        if (verbose) println("MEM[%x] -> %s".format(memaddr, read.toString(16)))
        poke(c.io.mem.resp.bits.data, read)
        poke(c.io.mem.resp.bits.tag, memtag)
        poke(c.io.mem.resp.valid, 1)
      }
      memcycles -= 1
    }
    step(1)
    poke(c.io.mem.req_cmd.ready, 0)
    poke(c.io.mem.req_data.ready, 0)
    poke(c.io.mem.resp.valid, 0)
  }

  def tick(n: Int, verbose: Boolean): Unit

  def run(c: Tile, maxcycles: Int, verbose: Boolean) = {
    pokeAt(c.core.dpath.regFile.regs, 0, 0)
    var tohost = BigInt(0)
    do {
      tick(1, verbose)
      val log = testOutputString
      if (verbose && !log.isEmpty) println(log)
      tohost = peek(c.io.htif.host.tohost)
    } while (tohost == 0 && cycles < maxcycles)
    val reason = if (cycles < maxcycles) "tohost = " + tohost else "timeout"
    val ok = tohost == 1
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (ok) "PASSED" else "FAILED", reason, cycles))
    ok
  }
}

class TileTester(c: Tile, args: Array[String]) extends MemTester(c, args, 16) with TileTests {
  def tick(n: Int, verbose: Boolean) {
    (0 until n) foreach (_ => tick(c, verbose)) 
  }
  def regFile(x: Int) = peekAt(c.core.dpath.regFile.regs, x)
  def runTests(maxcycles: Int, verbose: Boolean) {
    ok &= run(c, maxcycles, verbose)
  } 
}
