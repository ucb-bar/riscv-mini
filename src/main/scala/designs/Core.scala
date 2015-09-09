package mini

import Chisel._

class HostIO extends CoreBundle {
  val fromhost = Valid(UInt(width=xlen)).flip
  val tohost   = UInt(OUTPUT, xlen)
}

/*
class CacheIO extends CoreBundle {
  val addr = UInt(OUTPUT, xlen)
  val din  = UInt(OUTPUT, xlen)
  val dout = UInt(INPUT, xlen)
  val re   = Bool(OUTPUT)
  val we   = UInt(OUTPUT, 4)
}
*/

class CoreIO extends CoreBundle {
  val host = new HostIO
  val icache = (new CacheIO).flip
  val dcache = (new CacheIO).flip
}

class Core extends Module {
  val io = new CoreIO
  val dpath = Module(new Datapath) 
  val ctrl  = Module(new Control)

  io.host <> dpath.io.host
  io.icache <> dpath.io.icache
  io.dcache <> dpath.io.dcache
  dpath.io.ctrl <> ctrl.io.ctrl
}
