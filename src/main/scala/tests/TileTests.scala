package mini

import Chisel._
import TestCommon._
import HexCommon._
import scala.io.Source

class TileTests(c: Tile, args: Array[String]) extends Tester(c, false) {
  // Emulate AXI Slave
  private var memrw   = false
  private var memtag  = BigInt(0)
  private var memaddr = BigInt(0)
  private var memmask = BigInt(0)
  private var memcycles = -1
  def tickMem {
    if (memcycles > 0) {
      poke(c.io.mem.req_cmd.ready, 0)
      poke(c.io.mem.req_data.ready, 0)
      poke(c.io.mem.resp.valid, 0)  
      memcycles -= 1
    } else if (memcycles < 0) {
      if (peek(c.io.mem.req_cmd.valid) == 1) {
        memrw   = if (peek(c.io.mem.req_cmd.bits.rw) == 1) true else false
        memtag  = peek(c.io.mem.req_cmd.bits.tag)
        memaddr = peek(c.io.mem.req_cmd.bits.addr) << 2
        memmask = peek(c.io.mem.req_cmd.bits.mask)
        // Memread
        if (!memrw) {
          memcycles = 10
          poke(c.io.mem.req_cmd.ready, 1)
        }
      }
      if (peek(c.io.mem.req_data.valid) == 1) {
        val data = peek(c.io.mem.req_data.bits.data)
        poke(c.io.mem.req_cmd.ready, 1)
        poke(c.io.mem.req_data.ready, 1)
        writeMem(memaddr, data, memmask)
        memcycles = 5
      }
    } else {
      if (!memrw) {
        val read = readMem(memaddr)
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

  def runTests(maxcycles: Int, verbose: Boolean) {
    pokeAt(c.core.dpath.regFile.regs, 0, 0)
    do {
      tickMem
      if (verbose) {
        val pc     = peek(c.core.dpath.ew_pc)
        val inst   = UInt(peek(c.core.dpath.ew_inst), 32)
        val wb_en  = peek(c.core.ctrl.io.ctrl.wb_en)
        val wb_val = 
          if (wb_en == 1) peek(c.core.dpath.regWrite) 
          else peekAt(c.core.dpath.regFile.regs, rd(inst)) 
        println("[%h] %s -> RegFile[%d] = %h".format(
                pc, instStr(inst), rd(inst), wb_val))
      }
    } while (peek(c.io.htif.host.tohost) == 0 && t < maxcycles)

    val tohost = peek(c.io.htif.host.tohost)
    val reason = if (t < maxcycles) "tohost = " + tohost else "timeout"
    ok &= tohost == 1
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (ok) "PASSED" else "FAILED", reason, t))
  }

  val (filename, maxcycles, verbose) = parseOpts(args)
  loadMem(filename)
  runTests(maxcycles, verbose)
}
