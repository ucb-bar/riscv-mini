package mini

import Chisel._

class RegFileIO extends CoreBundle {
  val raddr1 = UInt(INPUT, 5)
  val raddr2 = UInt(INPUT, 5)
  val rdata1 = UInt(OUTPUT, instLen)
  val rdata2 = UInt(OUTPUT, instLen)
  val wen    = Bool(INPUT)
  val waddr  = UInt(INPUT, 5)
  val wdata  = UInt(INPUT, instLen)
}

class RegFile extends Module with CoreParams {
  val io = new RegFileIO
  val regs = Mem(UInt(width=instLen), 32)
  io.rdata1 := Mux(io.raddr1.orR, regs(io.raddr1), UInt(0))
  io.rdata2 := Mux(io.raddr2.orR, regs(io.raddr2), UInt(0))
  when(io.wen & io.waddr.orR) {
    regs(io.waddr) := io.wdata
  }
}
