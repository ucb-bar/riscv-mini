package mini

import Chisel._

class HostIO extends CoreBundle {
  val id = UInt(INPUT, 1)
  val tohost = UInt(OUTPUT, instLen)
  val status = UInt(OUTPUT, instLen)
}

class CacheIO extends CoreBundle {
  val addr = UInt(OUTPUT, addrLen)
  val din  = UInt(OUTPUT, instLen)
  val dout = UInt(INPUT, instLen)
  val re   = Bool(OUTPUT)
  val we   = UInt(OUTPUT, 4)
}

class CoreIO extends CoreBundle {
  val stall = Bool(INPUT)
  val host = new HostIO
  val icache = new CacheIO
  val dcache = new CacheIO
}

class Core extends Module {
  val io = new CoreIO
  val dpath = Module(new Datapath) 
  val ctrl  = Module(new Control)

  io.host <> dpath.io.host
  io.icache <> dpath.io.icache
  io.dcache <> dpath.io.dcache
  dpath.io.ctrl <> ctrl.io.ctrl
  dpath.io.stall := io.stall
}
