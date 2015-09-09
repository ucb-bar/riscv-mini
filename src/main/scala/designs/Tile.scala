package mini

import Chisel._

class HTIFIO extends Bundle {
  val host     = new HostIO
}

class TileIO extends Bundle {
  val htif = new HTIFIO
  val mem  = new MemIO 
}

class Tile extends Module {
  val io = new TileIO
 
  val core = Module(new Core)
  val mem = Module(new Memory)
  
  io.htif.host <> core.io.host
  io.mem <> mem.io.mem
  core.io.icache <> mem.io.icache
  core.io.dcache <> mem.io.dcache
}
