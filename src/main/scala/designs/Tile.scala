package mini

import Chisel._
import junctions.MemIO

class HTIFIO extends Bundle {
  val host = new HostIO
}

class TileIO extends Bundle {
  val htif = new HTIFIO
  val mem  = new MemIO 
}

class Tile extends Module {
  val io     = new TileIO
  val core   = Module(new Core)
  val icache = Module(new Cache)
  val dcache = Module(new Cache)
  val mem    = Module(new MemArbiter)
  
  io.htif.host <> core.io.host
  core.io.icache <> icache.io.cpu
  core.io.dcache <> dcache.io.cpu
  mem.io.icache <> icache.io.mem
  mem.io.dcache <> dcache.io.mem
  io.mem <> mem.io.mem
}
