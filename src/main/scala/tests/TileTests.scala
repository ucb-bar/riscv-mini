package mini

import Chisel._
import junctions.MemIO

trait TileMem extends MemCommon {
  private var memrw     = false
  private var memtag    = 0 
  private var memaddr   = 0 
  private var memcycles = -1
  def tick(mem_io: MemIO, verbose: Boolean) {
    if (memcycles > 0) {
      poke(mem_io.req_cmd.ready, 0)
      poke(mem_io.req_data.ready, 0)
      poke(mem_io.resp.valid, 0)  
      memcycles -= 1
    } else if (memcycles < 0) {
      if (peek(mem_io.req_cmd.valid)) {
        memrw   = peek(mem_io.req_cmd.bits.rw) 
        memtag  = peek(mem_io.req_cmd.bits.tag)
        memaddr = peek(mem_io.req_cmd.bits.addr) << 4 // TODO: use params
        // Memread
        if (!memrw) {
          memcycles = 2 // 10
          poke(mem_io.req_cmd.ready, 1)
        }
      }
      if (peek(mem_io.req_data.valid) == 1) {
        val data = peek(mem_io.req_data.bits.data)
        if (verbose) println("MEM[%x] <- %s".format(memaddr, data.toString(16)))
        poke(mem_io.req_cmd.ready, 1)
        poke(mem_io.req_data.ready, 1)
        mem.write(memaddr, data)
        memcycles = 1 // 5
      }
    } else {
      if (!memrw) {
        val read = mem.read(memaddr)
        if (verbose) println("MEM[%x] -> %s".format(memaddr, read.toString(16)))
        poke(mem_io.resp.bits.data, read)
        poke(mem_io.resp.bits.tag, memtag)
        poke(mem_io.resp.valid, 1)
      }
      memcycles -= 1
    }
    step(1)
    poke(mem_io.req_cmd.ready, 0)
    poke(mem_io.req_data.ready, 0)
    poke(mem_io.resp.valid, 0)
  }

  def run(c: Tile, maxcycles: Int, verbose: Boolean) = {
    pokeAt(c.core.dpath.regFile.regs, 0, 0)
    do {
      tick(c.io.mem, verbose)
      val log = testOutputString
      if (verbose && !log.isEmpty) println(log)
    } while (peek(c.io.htif.host.tohost) == 0 && cycles < maxcycles)

    val tohost = peek(c.io.htif.host.tohost)
    val reason = if (cycles < maxcycles) "tohost = " + tohost else "timeout"
    val ok = tohost == 1
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (ok) "PASSED" else "FAILED", reason, cycles))
    ok
  }
}

class TileTests(c: Tile, args: Array[String]) extends MemTester(c, args, 16) with TileMem {
  def regFile(x: Int) = peekAt(c.core.dpath.regFile.regs, x)
  def runTests(maxcycles: Int, verbose: Boolean) {
    ok &= run(c, maxcycles, verbose)
  } 
}
