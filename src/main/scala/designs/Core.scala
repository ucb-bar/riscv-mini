package mini

import Chisel._

abstract trait CoreParams extends UsesParameters {
  val xlen = params(XLEN)
}

abstract trait CoreBundle extends Bundle with CoreParams

class HostIO extends CoreBundle {
  val fromhost = Valid(UInt(width=xlen)).flip
  val tohost   = UInt(OUTPUT, xlen)
}

class CoreIO extends Bundle {
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
