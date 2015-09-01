package mini

import Chisel._
import scala.collection.immutable.ListMap

object CSR {
  val N = UInt(0, 2)
  val W = UInt(1, 2)
  val S = UInt(2, 2)
  val C = UInt(3, 2)

  // Machine-level CSR addrs
  // Machine Information Registers
  val mcpuid   = UInt(0xf00, 12)
  val mimpid   = UInt(0xf01, 12)
  val mhartid  = UInt(0xf10, 12)
  // Machine Trap Setup
  val mstatus  = UInt(0x300, 12)
  val mtvec    = UInt(0x301, 12)
  val mtdeleg  = UInt(0x302, 12) 
  val mie      = UInt(0x304, 12)
  val mtimecmp = UInt(0x321, 12)
  // Machine Timers and Counters
  val mtime    = UInt(0x701, 12)
  val mtimeh   = UInt(0x741, 12)
  // Machine Trap Handling
  val mscratch = UInt(0x340, 12)
  val mepc     = UInt(0x341, 12)
  val mcause   = UInt(0x342, 12)
  val mbadaddr = UInt(0x343, 12)
  val mip      = UInt(0x344, 12)
  // Machine Protection and Translation
  val mbase    = UInt(0x380, 12)
  val mbound   = UInt(0x381, 12)
  val mibase   = UInt(0x382, 12)
  val mibound  = UInt(0x383, 12)
  val mdbase   = UInt(0x384, 12)
  val mdbound  = UInt(0x385, 12)
  // Hachine HITF
  val mtohost   = UInt(0x780, 12)
  val mfromhost = UInt(0x781, 12)
}

class CSRIO extends CoreBundle {
  val cmd  = UInt(INPUT, 2)
  val csr  = UInt(INPUT, 12)
  val src  = UInt(INPUT, 5)
  val in   = UInt(INPUT, instLen)
  val out  = UInt(OUTPUT, instLen)

  val host = new HostIO
}

class CSR extends Module with CoreParams {
  val io = new CSRIO

  val mcpuid  = Cat(UInt(0, 2) /* RV32I */, UInt(0, instLen-28), 
                   UInt(1 << ('I' - 'A') /* Base ISA */| 
                        1 << ('U' - 'A') /* User Mode */, 26))
  val mimpid  = UInt(0, instLen) // not implemented
  val mhartid = UInt(0, instLen) // only one hart

  // interrupt enable stack
  val PRV0 = RegInit(UInt(0, 2))
  val PRV1 = RegInit(UInt(0, 2))
  val PRV2 = UInt(0, 2)
  val PRV3 = UInt(0, 2)
  val IE0 = RegInit(Bool(true))
  val IE1 = RegInit(Bool(true))
  val IE2 = Bool(false)
  val IE3 = Bool(false)
  // virtualization management field
  val VM = RegInit(UInt(0, 5)) 
  // memory privilege
  val MPRV = RegInit(Bool(true))
  // extention context status
  val XS = UInt(0, 2)
  val FS = UInt(0, 2)
  val SD = UInt(0, 1)
  val mstatus = Cat(SD, UInt(0, instLen-23), VM, MPRV, XS, FS, PRV3, IE3, PRV2, IE2, PRV1, IE1, PRV0, IE0)
  val mtvec   = UInt(0x200, instLen)
  val mtdeleg = UInt(0x0,   instLen)
  
  // interrupt registers
  val MTIP = RegInit(Bool(false))
  val HTIP = Bool(false)
  val STIP = Bool(false)
  val MTIE = RegInit(Bool(false))
  val HTIE = Bool(false)
  val STIE = Bool(false)
  val MSIP = RegInit(Bool(false))
  val HSIP = Bool(false)
  val SSIP = Bool(false)
  val MSIE = RegInit(Bool(false))
  val HSIE = Bool(false)
  val SSIE = Bool(false)
  val mip = Cat(UInt(0, instLen-8), MTIP, HTIP, STIP, Bool(false), MSIP, HSIP, SSIP, Bool(false))
  val mie = Cat(UInt(0, instLen-8), MTIE, HTIE, STIE, Bool(false), MSIE, HSIE, SSIE, Bool(false))

  val mtime = RegInit(UInt(0, instLen))
  val mtimeh = RegInit(UInt(0, instLen))
  val mtimecmp = Reg(UInt(width=instLen)) 

  val mscratch = Reg(UInt(width=instLen))

  val mepc = Reg(UInt(width=instLen))
  val Interrupt = RegInit(Bool(false))
  val Exception = RegInit(UInt(0, 4))
  val mcause = Cat(Interrupt, UInt(0, instLen-5), Exception)
  val mbadaddr = Reg(UInt(width=instLen))

  val mbase = Reg(UInt(width=instLen))
  val mbound = Reg(UInt(width=instLen))
  val mibase = Reg(UInt(width=instLen))
  val mibound = Reg(UInt(width=instLen))
  val mdbase = Reg(UInt(width=instLen))
  val mdbound = Reg(UInt(width=instLen))

  val mtohost = RegInit(UInt(0, instLen))
  val mfromhost = Reg(UInt(width=instLen))
  io.host.tohost := mtohost
  when(io.host.fromhost.valid) {
    mfromhost := io.host.fromhost.bits
  }
 
  val csrFile = ListMap(
    CSR.mcpuid    -> mcpuid,
    CSR.mimpid    -> mimpid,
    CSR.mhartid   -> mhartid,
    CSR.mstatus   -> mstatus,
    CSR.mtvec     -> mtvec,
    CSR.mtdeleg   -> mtdeleg,
    CSR.mie       -> mie,
    CSR.mtimecmp  -> mtimecmp,
    CSR.mtime     -> mtime,
    CSR.mtimeh    -> mtimeh,
    CSR.mscratch  -> mscratch,
    CSR.mepc      -> mepc,
    CSR.mcause    -> mcause,
    CSR.mbadaddr  -> mbadaddr,
    CSR.mip       -> mip,
    CSR.mbase     -> mbase,
    CSR.mbound    -> mbound,
    CSR.mibase    -> mibase,
    CSR.mibound   -> mibound,
    CSR.mdbase    -> mdbase,
    CSR.mdbound   -> mdbound,
    CSR.mtohost   -> mtohost,
    CSR.mfromhost -> mfromhost)
  val csr = Lookup(io.csr, UInt(0), csrFile.toSeq)
  io.out := csr.zext

  val isM  = io.csr(9, 8).andR
  val isRO = io.csr(11, 10).andR || io.csr === CSR.mtvec || io.csr === CSR.mtdeleg
  val wen = (io.cmd != CSR.N && io.src.orR)
  val wdata = MuxLookup(io.cmd, UInt(0), Seq(
    CSR.W -> io.in,
    CSR.C -> (io.out & io.in),
    CSR.S -> (io.out & ~io.in)
  ))

  when(!isM || (wen && isRO)) {
    Interrupt := Bool(true)
    Exception := UInt(2)
  }.elsewhen(wen) {
    when(io.csr === CSR.mstatus) { 
      VM   := wdata(21, 17)
      MPRV := wdata(16)
      PRV1 := wdata(5, 4)
      IE1  := wdata(3)
      PRV0 := wdata(2, 1)
      IE0  := wdata(0)
    }
    .elsewhen(io.csr === CSR.mip) {
      MTIP := wdata(7)
      MSIP := wdata(3)
    }
    .elsewhen(io.csr === CSR.mie) {
      MTIE := wdata(7)
      MSIE := wdata(3)
    }
    .elsewhen(io.csr === CSR.mtime) { mtime := wdata }
    .elsewhen(io.csr === CSR.mtimeh) { mtimeh := wdata }
    .elsewhen(io.csr === CSR.mtimecmp) { mtimecmp := wdata }
    .elsewhen(io.csr === CSR.mscratch) { mscratch := wdata }
    .elsewhen(io.csr === CSR.mepc) { mepc := wdata }
    .elsewhen(io.csr === CSR.mcause) {
      Interrupt := wdata(instLen-1)
      Exception := wdata(3, 0)
    }
    .elsewhen(io.csr === CSR.mbadaddr) { mbadaddr := wdata }
    .elsewhen(io.csr === CSR.mbase) { mbase := wdata }
    .elsewhen(io.csr === CSR.mbound) { mbound := wdata }
    .elsewhen(io.csr === CSR.mibase) { mibase := wdata }
    .elsewhen(io.csr === CSR.mibound) { mibound := wdata }
    .elsewhen(io.csr === CSR.mdbase) { mdbase := wdata }
    .elsewhen(io.csr === CSR.mdbound) { mdbound := wdata }
    .elsewhen(io.csr === CSR.mtohost) { mtohost := wdata }
    .elsewhen(io.csr === CSR.mfromhost) { mfromhost := wdata }
  }
}
