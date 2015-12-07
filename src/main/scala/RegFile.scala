package mini

import Chisel._
import cde.Parameters

class RegFileIO(implicit p: Parameters)  extends CoreBundle()(p) {
  val raddr1 = UInt(INPUT, 5)
  val raddr2 = UInt(INPUT, 5)
  val rdata1 = UInt(OUTPUT, xlen)
  val rdata2 = UInt(OUTPUT, xlen)
  val wen    = Bool(INPUT)
  val waddr  = UInt(INPUT, 5)
  val wdata  = UInt(INPUT, xlen)
}

class RegFile(implicit val p: Parameters) extends Module with CoreParams {
  val io = new RegFileIO
  val regs = Mem(32, UInt(width=xlen))
  io.rdata1 := Mux(io.raddr1.orR, regs(io.raddr1), UInt(0))
  io.rdata2 := Mux(io.raddr2.orR, regs(io.raddr2), UInt(0))
  when(io.wen & io.waddr.orR) {
    regs(io.waddr) := io.wdata
  }
}
