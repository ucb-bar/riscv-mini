package mini

import Chisel._
import cde.Parameters
import junctions.{MemIO, NastiIO}

class HTIFIO(implicit p: Parameters) extends junctions.ParameterizedBundle {
  val host = new HostIO
}

class TileIO(implicit p: Parameters) extends junctions.ParameterizedBundle {
  val htif  = new HTIFIO
  val mem   = new MemIO
  val nasti = new NastiIO
}

class Tile(implicit val p: Parameters) extends Module with CacheParams {
  val io     = new TileIO
  val core   = Module(new Core)
  val icache = Module(new Cache)
  val dcache = Module(new Cache)
  
  io.htif.host <> core.io.host
  core.io.icache <> icache.io.cpu
  core.io.dcache <> dcache.io.cpu

  if (core.useNasti) {
    val arb = Module(new NastiIOArbiter)
    arb.io.icache <> icache.io.nasti
    arb.io.dcache <> dcache.io.nasti
    io.nasti <> arb.io.nasti
  } else {
    val arb = Module(new MemIOArbiter)
    arb.io.icache <> icache.io.mem
    arb.io.dcache <> dcache.io.mem
    io.mem <> arb.io.mem
  }
}
