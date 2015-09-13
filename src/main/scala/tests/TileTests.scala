package mini

import Chisel._
import TestCommon._
import scala.io.Source

class TileTests(c: Tile, args: Array[String]) extends MemTester(c, args, 16) {
  def regFile(x: Int) = peekAt(c.core.dpath.regFile.regs, x)
  def runTests(maxcycles: Int, verbose: Boolean) = {
    var memrw     = false
    var memtag    = 0 
    var memaddr   = 0 
    var memcycles = -1
    pokeAt(c.core.dpath.regFile.regs, 0, 0)
    do {
      if (memcycles > 0) {
        poke(c.io.mem.req_cmd.ready, 0)
        poke(c.io.mem.req_data.ready, 0)
        poke(c.io.mem.resp.valid, 0)  
        memcycles -= 1
      } else if (memcycles < 0) {
        if (peek(c.io.mem.req_cmd.valid)) {
          memrw   = peek(c.io.mem.req_cmd.bits.rw) 
          memtag  = peek(c.io.mem.req_cmd.bits.tag)
          memaddr = peek(c.io.mem.req_cmd.bits.addr) << 4 // TODO: use params
          // Memread
          if (!memrw) {
            memcycles = 2 // 10
            poke(c.io.mem.req_cmd.ready, 1)
          }
        }
        if (peek(c.io.mem.req_data.valid) == 1) {
          val data = peek(c.io.mem.req_data.bits.data)
          if (verbose) println("MEM[%x] <- %s".format(memaddr, data.toString(16)))
          poke(c.io.mem.req_cmd.ready, 1)
          poke(c.io.mem.req_data.ready, 1)
          mem.write(memaddr, data)
          memcycles = 1 // 5
        }
      } else {
        if (!memrw) {
          val read = mem.read(memaddr)
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
      val log = testOutputString
      if (verbose && !log.isEmpty) println(log)
    } while (peek(c.io.htif.host.tohost) == 0 && cycles < maxcycles)

    val tohost = peek(c.io.htif.host.tohost)
    val reason = if (cycles < maxcycles) "tohost = " + tohost else "timeout"
    ok &= tohost == 1
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (tohost == 1) "PASSED" else "FAILED", reason, cycles))
  } 
}
