package mini

import Chisel._

object CSR {
  val N = UInt(0, 2)
  val W = UInt(1, 2)
  val S = UInt(2, 2)
  val C = UInt(3, 2)

  val STATUS = UInt(0x50a, 12)
  val HARTID = UInt(0x50b, 12)
  val TOHOST = UInt(0x51e, 12)
}

class CSRIO extends CoreBundle {
  val host = new HostIO
  val cmd = Bits(INPUT, 2)
  val addr = Bits(INPUT, 12)
  val src  = Bits(INPUT, instLen)
  val data = Bits(OUTPUT, instLen)
}

class CSR extends Module {
  val io = new CSRIO
  val reg_tohost = RegInit(UInt(0, 32))
  val reg_status = RegInit(UInt(0, 32))

  io.host.tohost := reg_tohost
  io.host.status := reg_status

  io.data := MuxLookup(io.addr, UInt(0), Seq(
    CSR.TOHOST -> reg_tohost,
    CSR.STATUS -> reg_status,
    CSR.HARTID -> io.host.id)).zext

  when(io.cmd === CSR.W) {
    when(io.addr === CSR.TOHOST) { reg_tohost := io.src }
    when(io.addr === CSR.STATUS) { reg_status := io.src }
  }
  when(io.cmd === CSR.S && io.src != UInt(0)) {
    when(io.addr === CSR.TOHOST) { reg_tohost := io.data | (UInt(1) << io.src) }
    when(io.addr === CSR.STATUS) { reg_status := io.data | (UInt(1) << io.src) }
  }
  when(io.cmd === CSR.C && io.src != UInt(0)) {
    when(io.addr === CSR.TOHOST) { reg_tohost := io.data & (UInt(0) << io.src) }
    when(io.addr === CSR.STATUS) { reg_status := io.data & (UInt(0) << io.src) }
  }  
}
